# ==============================================================================
# Dengue Brasil | Modulo: mapa coropletico (echarts4r)
#
# GRANULARIDADE: visao Brasil = as 27 UFs (BR_UF); estado ou municipio = os
#   municipios da UF (MUN_<uf>). Clique numa UF -> recorte no estado; clique
#   num municipio -> recorte no municipio, que fica DESTACADO (estado 'select'
#   do ECharts via handler JS 'dengue_map_highlight', sem re-renderizar).
#   Trocar de municipio dentro da mesma UF nao redesenha o mapa.
#
# Metrica: incidencia = sum(casos) / pop * 100.000 (pop = max por municipio
#   no periodo, somada na UF) - mesma conta do dengue_ride.
#
# Malhas: texto JSON cru (global.R::malha_do_recorte); no mapa municipal o
#   contorno da UF vai anexado ao fim da malha (codarea "CONTORNO").
# ==============================================================================

COR_BORDA_MUN  <- "#CBD5DB"   # entre municipios
COR_BORDA_UF   <- "#94A3AC"   # divisa entre estados / contorno da UF

#' Desenha o contorno da UF por cima do mapa municipal.
#' A feicao de contorno ("CONTORNO", anexada ao fim da malha municipal em
#' global.R::malha_municipios) ganha um item de dado sem valor (o visualMap a
#' ignora), sem preenchimento e `silent` (clique, hover e tooltip atravessam
#' para o municipio de baixo) - so a borda. opacity = 1 no proprio item: o
#' destaque (dengue_map_highlight) baixa a opacidade da serie inteira.
com_contorno <- function(e) {
  e$x$opts$series[[1]]$data <- c(e$x$opts$series[[1]]$data, list(list(
    name = "CONTORNO", value = NULL,
    itemStyle = list(areaColor = "none", opacity = 1, borderColor = COR_BORDA_UF, borderWidth = 1.2),
    emphasis = list(disabled = TRUE), select = list(disabled = TRUE),
    tooltip = list(show = FALSE), silent = TRUE
  )))
  e
}

# PECAS_INCIDENCIA vive em global.R: a mesma escala de cor e usada aqui e no
# heatmap de sazonalidade, para que a leitura seja comparavel entre os dois.


mod_mapa_controles <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::radioButtons(
      ns("metrica"), "Métrica",
      choices = c("Incidência /100 mil" = "incidencia",
                  "Casos notificados"   = "casos"),
      selected = "incidencia"
    ),
    shiny::sliderInput(
      ns("anos"), "Período (ano)",
      min   = ANO_MIN,
      max   = ANO_MAX,
      value = c(max(ANO_MIN, ANO_MAX - 1), ANO_MAX),
      step  = 1,
      sep   = ""
    ),
    shiny::helpText(
      shiny::tags$small("No Brasil, clique em um estado para abrir os municípios dele; no estado, clique em um município para focar o painel nele.")
    )
  )
}

mod_mapa_saida <- function(id, height = "540px") {
  ns <- shiny::NS(id)

  shiny::tagList(
    # Script unico da pagina (singleton): (a) handler de destaque do mapa e
    # (b) "wake up" do canal do input de clique.
    #
    # (a) DESTAQUE. Mecanismo: estado 'select' do ECharts (persistente,
    #     casado por nome = geocodigo) para o poligono escolhido + setOption
    #     reduzindo a opacidade do resto da serie. NAO usa highlight/downplay
    #     (emphasis): num map series com coordinateSystem 'geo' o dispatch de
    #     'highlight' por nome nao aplica estado de forma confiavel; 'select'
    #     aplica. Handler proprio (e nao e_dispatch_action_p direto) porque na
    #     troca de UF o mapa e RE-RENDERIZADO e a mensagem chega junto do
    #     novo widget, sem ordem garantida - a aplicacao e re-afirmada 4x
    #     numa janela de ~480 ms para cobrir essa corrida.
    #
    # (b) WAKE UP. O primeiro Shiny.setInputValue disparado de dentro do
    #     handler de clique do ECharts as vezes fica so no cache do cliente e
    #     nao chega ao servidor - o pipeline de inputs do Shiny ainda nao
    #     "acordou" para aquele input. Ao conectar, registramos a chave do
    #     input de clique com "" (SEM priority:'event', entao nada dispara no
    #     servidor: o eventReactive de input$clique cai no req(!is.na(cod))).
    #     Assim o primeiro clique real ja encontra o canal pronto.
    shiny::singleton(shiny::tags$script(shiny::HTML(sprintf("
      Shiny.addCustomMessageHandler('dengue_map_highlight', function(m){
        var n = 0;
        function apply(){
          n++;
          var el = document.getElementById(m.id);
          var chart = null;
          if (el && window.echarts) {
            chart = echarts.getInstanceByDom(el) ||
                    (el.firstElementChild && echarts.getInstanceByDom(el.firstElementChild));
          }
          if (chart) {
            var opt = chart.getOption();
            var nData = (opt.series && opt.series[0] && opt.series[0].data) ? opt.series[0].data.length : 0;
            var hasSel = (m.name !== null && m.name !== undefined && m.name !== '');
            // opacidade do restante da serie: reduzida quando ha selecao
            chart.setOption({ series: [{
              selectedMode: 'single',
              itemStyle: { opacity: hasSel ? 0.3 : 1 },
              select: { itemStyle: { opacity: 1 } }
            }]});
            // limpa qualquer selecao anterior (unselect sem alvo nao zera tudo)
            if (nData > 0) {
              var all = [];
              for (var i = 0; i < nData; i++) all.push(i);
              chart.dispatchAction({ type: 'unselect', seriesIndex: 0, dataIndex: all });
            }
            if (hasSel) {
              chart.dispatchAction({ type: 'select', seriesIndex: 0, name: String(m.name) });
            }
          }
          if (n < 4) setTimeout(apply, 120);
        }
        apply();
      });

      (function(){
        var cliqueId = '%s';
        function wake(){ try { Shiny.setInputValue(cliqueId, ''); } catch (e) {} }
        if (window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected &&
            Shiny.shinyapp.isConnected()) {
          wake();
        } else {
          document.addEventListener('shiny:connected', wake);
        }
      })();
    ", ns("clique"))))),

    # Botao "Visao geral" sobreposto ao canto do mapa: limpa a selecao e
    # volta a visualizacao padrao (toda a base, sem destaque). Fica sempre
    # visivel - quando nada esta selecionado, e um no-op inofensivo. Canto
    # superior esquerdo: o toolbox (salvar imagem) fica no direito e o
    # visual map no inferior esquerdo, entao esse canto esta livre.
    # html-fill-*: o mapa estica ate a altura do card, que acompanha a coluna
    # da direita (Rt + evolucao temporal) em ui.R.
    shiny::div(
      class = "position-relative html-fill-item html-fill-container",
      shiny::actionButton(
        ns("limpar"),
        label = "Visão geral",
        icon  = shiny::icon("rotate-left"),
        class = "btn btn-sm btn-light shadow-sm",
        style = paste(
          "position:absolute; top:8px; left:8px; z-index:5;",
          "padding:.15rem .5rem; font-size:.8rem;"
        )
      ),
      echarts4r::echarts4rOutput(ns("mapa"), height = height)
    )
  )
}


mod_mapa_server <- function(id, geo, selected_location = shiny::reactive(NULL)) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # Unidades do recorte (UFs no Brasil, municipios da UF num estado). So a
    # troca de UF/Brasil muda o mapa; o municipio em foco e so destaque.
    chave_un <- shiny::reactive({
      g <- geo()
      if (identical(g$nivel, "brasil")) "brasil" else paste0("uf_", g$uf_ibge)
    })
    un <- shiny::reactive(unidades_do_recorte(geo())) |> shiny::bindEvent(chave_un())

    # -- consulta agregada (tudo no DuckDB) ---------------------------------
    dados <- shiny::reactive({
      shiny::req(input$anos)
      u <- un()
      d1 <- as.Date(sprintf("%d-01-01", input$anos[1]))
      d2 <- as.Date(sprintf("%d-12-31", input$anos[2]))

      q <- tbl_dengue() |> dplyr::filter(data_ini_se >= !!d1, data_ini_se <= !!d2)
      if (identical(u$tipo, "municipio")) q <- dplyr::filter(q, uf_ibge == !!u$uf)
      d <- q |>
        dplyr::group_by(uf_ibge, municipio_geocodigo) |>
        dplyr::summarise(
          casos   = sum(casos, na.rm = TRUE),
          pop     = max(pop, na.rm = TRUE),
          .groups = "drop"
        ) |>
        dplyr::collect() |>
        dplyr::mutate(
          uf_ibge = as.integer(uf_ibge),
          municipio_geocodigo = as.integer(municipio_geocodigo),
          casos   = as.numeric(casos),
          pop     = as.numeric(pop)
        )
      if (identical(u$tipo, "uf")) {
        d <- d |>
          dplyr::group_by(id = uf_ibge) |>
          dplyr::summarise(casos = sum(casos), pop = sum(pop, na.rm = TRUE), .groups = "drop")
      } else {
        d <- dplyr::transmute(d, id = municipio_geocodigo, casos, pop)
      }
      d
    }) |>
      shiny::bindCache(input$anos, chave_un())

    # -- junta com a lista completa de areas (sem dado != ausente) ----------
    dados_mapa <- shiny::reactive({
      u <- un()
      d <- dados()

      areas <- if (identical(u$tipo, "uf")) {
        dplyr::transmute(UF_LOOKUP, id = uf_ibge, rotulo = uf_nome)
      } else {
        GEO_DICT[GEO_DICT$uf_ibge == u$uf, , drop = FALSE] |>
          dplyr::transmute(id = municipio_geocodigo, rotulo = sprintf("%s (%s)", municipio_nome, uf_sigla))
      }

      areas |>
        dplyr::left_join(d, by = "id") |>
        dplyr::mutate(
          chave = as.character(id),
          casos = dplyr::coalesce(casos, 0),
          valor = if (identical(input$metrica, "incidencia")) {
            dplyr::if_else(!is.na(pop) & pop > 0, casos / pop * 1e5, NA_real_)
          } else {
            casos
          },
          valor = round(valor, 1)
        )
    })

    rotulo_metrica <- shiny::reactive({
      if (identical(input$metrica, "incidencia")) "Incidência /100 mil" else "Casos"
    })

    # -- render --------------------------------------------------------------
    output$mapa <- echarts4r::renderEcharts4r({
      u <- un()
      m <- malha_do_recorte(u)
      d <- dados_mapa()
      shiny::req(nrow(d) > 0)
      eh_uf <- identical(u$tipo, "uf")

      nomes_js <- jsonlite::toJSON(
        stats::setNames(as.list(d$rotulo), d$chave),
        auto_unbox = TRUE
      )

      js_tooltip <- htmlwidgets::JS(sprintf(
        "function(p){
           var nomes = %s;
           var n = nomes[p.name] || p.name;
           if (p.value === undefined || p.value === null || isNaN(p.value)) {
             return '<b>' + n + '</b><br/>sem dado';
           }
           return '<b>' + n + '</b><br/>%s: ' +
                  p.value.toLocaleString('pt-BR', {maximumFractionDigits: 1})%s;
         }",
        nomes_js, rotulo_metrica(),
        if (eh_uf) " + '<br/><span style=\"opacity:.7\">Clique para abrir o estado</span>'" else ""
      ))

      js_click <- htmlwidgets::JS(sprintf(
        "function(p){
           if (p && p.name && p.name !== 'CONTORNO') {
             var alvo = p.name;
             setTimeout(function(){
               Shiny.setInputValue('%s', alvo, {priority: 'event'});
             }, 60);
           }
         }",
        ns("clique")
      ))

      grafico <- d |>
        echarts4r::e_charts(chave) |>
        echarts4r::e_map_register(m$nome, m$malha) |>
        echarts4r::e_map(
          valor,
          map          = m$nome,
          name         = rotulo_metrica(),
          nameProperty = GEO_NAME_PROPERTY,
          roam         = TRUE,
          itemStyle    = list(borderColor = if (eh_uf) COR_BORDA_UF else COR_BORDA_MUN,
                              borderWidth = if (eh_uf) 0.8 else 0.4),
          label        = list(show = FALSE),
          emphasis     = list(
            itemStyle = list(areaColor = TONS_AZUL[["300"]]),
            label     = list(show = FALSE)
          ),
          selectedMode = "single",
          select       = list(
            itemStyle = list(areaColor = PALETA_DENGUE[["azul_acao"]], opacity = 1),
            label     = list(show = FALSE)
          )
        )

      grafico <- if (identical(input$metrica, "incidencia")) {
        grafico |>
          echarts4r::e_visual_map(
            type   = "piecewise",
            pieces = PECAS_INCIDENCIA,
            orient = "vertical",
            left   = "left",
            bottom = "6%",
            itemWidth = 14, itemHeight = 12
          )
      } else {
        grafico |>
          echarts4r::e_visual_map(
            min    = 0,
            max    = max(d$valor, na.rm = TRUE),
            orient = "vertical",
            left   = "left",
            bottom = "6%",
            inRange = list(color = RAMPA_MAPA),
            formatter = htmlwidgets::JS(
              "function(v){return v.toLocaleString('pt-BR',{maximumFractionDigits:0});}"
            )
          )
      }

      grafico <- grafico |>
        echarts4r::e_tooltip(trigger = "item", formatter = js_tooltip) |>
        echarts4r::e_toolbox_feature("saveAsImage", title = "Salvar imagem") |>
        echarts4r::e_text_style(fontFamily = FONTE_GRAFICO) |>
        echarts4r::e_on(query = NULL, handler = js_click, event = "click")
      if (!eh_uf) grafico <- com_contorno(grafico)
      grafico
    })

    # -- feedback visual: destaque do municipio selecionado ------------------
    shiny::observe({
      sl     <- selected_location()
      chaves <- tryCatch(dados_mapa()$chave, error = function(e) character(0))

      cod <- NULL
      if (!is.null(sl) && identical(sl$nivel, "municipio") &&
          !is.null(sl$geocodigo) && !is.na(sl$geocodigo)) {
        c0 <- as.character(sl$geocodigo)
        if (c0 %in% chaves) cod <- c0
      }

      session$sendCustomMessage("dengue_map_highlight", list(
        id   = session$ns("mapa"),
        name = cod
      ))
    })

    # -- pedido de foco para o filtro global ------------------------------
    #   - clique numa UF (codigo de 2 digitos) -> nivel = "uf";
    #   - clique num municipio -> nivel = "municipio";
    #   - botao "Visao geral" -> nivel = "brasil" (reset).
    pedido_foco <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$clique, {
      cod <- suppressWarnings(as.integer(input$clique))
      shiny::req(!is.na(cod))
      pedido_foco(if (cod < 100L) {
        list(nivel = "uf", uf_ibge = cod, geocodigo = NA_integer_, .ts = as.numeric(Sys.time()))
      } else {
        list(nivel = "municipio", uf_ibge = cod %/% 100000L, geocodigo = cod)
      })
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$limpar, {
      pedido_foco(list(
        nivel = "brasil", uf_ibge = NA_integer_, geocodigo = NA_integer_,
        .ts = as.numeric(Sys.time())
      ))
    }, ignoreInit = TRUE)

    pedido_foco
  })
}

