# ==============================================================================
# Dengue Brasil | global.R
# Irmao nacional do dengue_ride: mesma base (InfoDengue) e mesmos graficos, para
# os 5.570 municipios. O que muda por causa da escala: malhas por UF (texto
# JSON cru), serie por unidade lida sob demanda (UFs no Brasil, municipios da
# UF num estado) em vez de SERIE_MUNICIPIOS inteira na memoria, e previsao
# pre-ajustada so para Brasil e UFs.
# Ambiente, fonte de dados (Parquet via DuckDB), dicionario geografico e
# helpers de consulta. Executado UMA vez, antes de ui.R e server.R.
# ==============================================================================

library(shiny)
library(bslib)
library(DBI)
library(duckdb)
library(dplyr)
library(echarts4r)
library(DT)  # tabela da aba Analises (R/mod_epi_scanner_server.R)

# Cache do CSS do tema (Sass) em disco, em app_cache/ (pasta criada no
# servidor com dono shiny, fora do git): o Bootstrap do tema e compilado uma
# vez e reaproveitado entre processos e reinicios (touch restart.txt), em vez
# de ser recompilado a cada processo R novo - a primeira visita depois de um
# reinicio deixa de pagar essa compilacao.
if (dir.exists("app_cache") && file.access("app_cache", 2) == 0) {
  options(sass.cache = sass::sass_file_cache(file.path("app_cache", "sass")))
}


# ------------------------------------------------------------ fonte de dados --
# Parquet local lido pelo DuckDB.
#
# Por que DuckDB e nao o backend dplyr do arrow:
#   O arrow traduz um subconjunto de dplyr. As consultas deste painel usam
#   n_distinct(), if_else() dentro de sum(), max(na.rm=) e um summarise()
#   aninhado em dois estagios (mod_mapa). O arrow nao cobre tudo isso e cairia
#   para collect() antecipado - justamente o que a lazy evaluation deveria
#   evitar. O DuckDB fala SQL completo via dbplyr, entao as consultas dos
#   modulos seguem identicas as da versao PostgreSQL, e o pushdown de filtros
#   e de colunas acontece na leitura do Parquet.
PARQUET_DENGUE <- Sys.getenv(
  "DENGUE_PARQUET",
  file.path("data", "dengue_br.parquet")
)

if (!file.exists(PARQUET_DENGUE)) {
  stop(
    "Arquivo Parquet nao encontrado em '", PARQUET_DENGUE, "'. ",
    "Verifique o caminho ou defina DENGUE_PARQUET em ~/.Renviron.",
    call. = FALSE
  )
}

# Uma unica conexao, sem pool. O pool existia para nao esgotar conexoes de um
# servidor Postgres remoto; um DuckDB em memoria no mesmo processo nao tem esse
# custo, e o Shiny processa as consultas de forma serial de qualquer modo.
# bigint = "numeric": municipio_geocodigo tem 7 digitos e cabe em double sem
# perda; evita a dependencia de integer64, que quebra selectInput.
#
# CONEXAO AUTO-RECUPERAVEL. Num servidor (RStudio Server) a MESMA sessao do R
# roda o app varias vezes. Ao parar o app o onStop() abaixo faz
# dbDisconnect(shutdown = TRUE), que DESTROI o banco em memoria. Se algo
# ainda segurar o `con` antigo na re-execucao, toda consulta falha com
# "Invalid connection". Por isso a conexao e (re)criada por uma funcao e
# tbl_dengue() reconecta sozinho quando o `con` esta invalido.
con <- NULL

.dengue_conectar <- function() {
  con <<- DBI::dbConnect(
    duckdb::duckdb(),
    dbdir  = ":memory:",
    bigint = "numeric"
  )

  # Deixa o DuckDB usar os nucleos disponiveis. Falha silenciosamente em
  # versoes que nao aceitam o PRAGMA - nao vale derrubar o app por isso.
  try(DBI::dbExecute(con, "SET threads TO 4"), silent = TRUE)

  # View sobre o Parquet: o arquivo NAO e carregado na RAM aqui. Cada consulta
  # le so as colunas e as linhas que o filtro exige.
  caminho <- normalizePath(PARQUET_DENGUE, winslash = "/", mustWork = TRUE)
  caminho <- gsub("'", "''", caminho, fixed = TRUE)   # escapa aspas no path
  DBI::dbExecute(con, sprintf(
    "CREATE OR REPLACE VIEW dengue AS SELECT * FROM read_parquet('%s')",
    caminho
  ))

  invisible(con)
}

.dengue_conectar()

onStop(function() {
  if (!is.null(con) && DBI::dbIsValid(con)) {
    message("[Dengue Brasil] Encerrando DuckDB.")
    try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE)
  }
})

#' Referencia lazy para a base de dengue.
#'
#' Sem filtro de escopo: o Parquet ja chega com o recorte correto (Brasil,
#' todos os municipios). Qualquer trava aqui seria uma segunda fonte de verdade a
#' manter em sincronia com o arquivo - e a primeira a ficar desatualizada.
#'
#' Reconecta sozinho se o `con` tiver caido (ver nota sobre RStudio Server
#' acima) - assim o app roda N vezes na mesma sessao do R sem reiniciar.
tbl_dengue <- function() {
  if (is.null(con) || !DBI::dbIsValid(con)) .dengue_conectar()
  dplyr::tbl(con, "dengue")
}

message(sprintf(
  "[Dengue Brasil] Fonte: %s (%.1f MB).",
  PARQUET_DENGUE, file.size(PARQUET_DENGUE) / 1024^2
))

# --------------------------------------------------------- escopo da base ----
# A base cobre o Brasil inteiro. O rotulo do nivel agregado reflete
# isso; a chave interna continua "brasil" para nao alterar aplicar_filtro_geo()
# nem o contrato de saida do modulo geografico.
ROTULO_BASE <- Sys.getenv("DENGUE_ROTULO_BASE", "Brasil")

# ------------------------------------------------------- dominio das 27 UFs --
# A coluna `uf` guarda o codigo IBGE em texto ("53"), nao a sigla.
# Esta tabela traduz codigo -> sigla/nome. Continua com as 27 UFs de proposito:
# e uma tabela de dominio, e quem restringe o escopo e a BASE, nao a lookup.
UF_LOOKUP <- data.frame(
  uf_ibge = c(11L, 12L, 13L, 14L, 15L, 16L, 17L,
              21L, 22L, 23L, 24L, 25L, 26L, 27L, 28L, 29L,
              31L, 32L, 33L, 35L,
              41L, 42L, 43L,
              50L, 51L, 52L, 53L),
  uf_sigla = c("RO", "AC", "AM", "RR", "PA", "AP", "TO",
               "MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA",
               "MG", "ES", "RJ", "SP",
               "PR", "SC", "RS",
               "MS", "MT", "GO", "DF"),
  uf_nome = c("Rondônia", "Acre", "Amazonas", "Roraima", "Pará", "Amapá", "Tocantins",
              "Maranhão", "Piauí", "Ceará", "Rio Grande do Norte", "Paraíba",
              "Pernambuco", "Alagoas", "Sergipe", "Bahia",
              "Minas Gerais", "Espírito Santo", "Rio de Janeiro", "São Paulo",
              "Paraná", "Santa Catarina", "Rio Grande do Sul",
              "Mato Grosso do Sul", "Mato Grosso", "Goiás", "Distrito Federal"),
  regiao = c(rep("Norte", 7), rep("Nordeste", 9), rep("Sudeste", 4),
             rep("Sul", 3), rep("Centro-Oeste", 4)),
  stringsAsFactors = FALSE
)

# ------------------------------------------------- dicionario geografico -----
# Um DISTINCT sobre o Parquet. E AQUI que o escopo do painel se define: os
# seletores de Estado e Municipio sao construidos a partir deste resultado,
# entao so aparece o que existe na base. Nao ha lista de municipios codificada
# em lugar nenhum do app - trocar o Parquet troca os seletores.
message("[Dengue Brasil] Carregando dicionário geográfico...")
.t0 <- Sys.time()

GEO_DICT <- tbl_dengue() |>
  dplyr::distinct(uf, uf_ibge, municipio_geocodigo, municipio_nome) |>
  dplyr::collect() |>
  dplyr::mutate(
    uf_ibge             = as.integer(uf_ibge),
    municipio_geocodigo = as.integer(municipio_geocodigo),
    # Fallback: se uf_ibge vier NA, deriva da raiz do geocodigo.
    uf_ibge             = dplyr::coalesce(uf_ibge, municipio_geocodigo %/% 100000L)
  ) |>
  dplyr::left_join(UF_LOOKUP, by = "uf_ibge") |>
  dplyr::mutate(
    # Se a coluna `uf` trouxer sigla de verdade, ela tem prioridade.
    uf_sigla = dplyr::if_else(grepl("^[A-Za-z]{2}$", uf), toupper(uf), uf_sigla),
    uf_nome  = dplyr::coalesce(uf_nome, uf_sigla, as.character(uf_ibge))
  ) |>
  dplyr::filter(!is.na(municipio_geocodigo)) |>
  dplyr::arrange(uf_nome, municipio_nome)

message(sprintf(
  "[Dengue Brasil] Dicionário pronto: %d municípios / %d UF(s) em %.1fs.",
  nrow(GEO_DICT), dplyr::n_distinct(GEO_DICT$uf_ibge),
  as.numeric(difftime(Sys.time(), .t0, units = "secs"))
))
rm(.t0)

# Tabela de UFs derivada da BASE - so entra UF que tem municipio no Parquet.
UF_DISPONIVEIS <- GEO_DICT |>
  dplyr::distinct(uf_ibge, uf_sigla, uf_nome, regiao) |>
  dplyr::arrange(uf_nome)

# UF padrao dos seletores: DF se estiver na base, senao a primeira disponivel.
UF_PADRAO <- if (53L %in% UF_DISPONIVEIS$uf_ibge) {
  53L
} else {
  UF_DISPONIVEIS$uf_ibge[1]
}

message(sprintf(
  "[Dengue Brasil] Escopo: %s — %d município(s) em %s.",
  ROTULO_BASE, nrow(GEO_DICT), paste(UF_DISPONIVEIS$uf_sigla, collapse = ", ")
))

# ------------------------------------------------- cobertura temporal --------
PERIODO_DADOS <- tbl_dengue() |>
  dplyr::summarise(
    inicio = min(data_ini_se, na.rm = TRUE),
    fim    = max(data_ini_se, na.rm = TRUE)
  ) |>
  dplyr::collect()

ANO_MIN <- as.integer(format(as.Date(PERIODO_DADOS$inicio), "%Y"))
ANO_MAX <- as.integer(format(as.Date(PERIODO_DADOS$fim),    "%Y"))

message(sprintf("[Dengue Brasil] Cobertura temporal: %d a %d.", ANO_MIN, ANO_MAX))

# ------------------------------------------------- ano sanitario (jul-jun) ---
# O canal endemico e a curva epidemica usam o ANO SANITARIO, nao o calendario:
# a temporada comeca na semana epidemiologica 27 (fim de junho / inicio de
# julho) e termina na semana 26 do ano seguinte. Ancorar no inicio da baixa
# transmissao (inverno), em vez de 1o de janeiro, evita partir ao meio a
# temporada de pico (verao) - que e exatamente o periodo que o canal precisa
# comparar inteiro contra o historico.
SEMANA_CORTE_SANITARIA <- 27L

#' Ano sanitario (temporada) a que uma semana epidemiologica pertence.
#' Semanas >= 27 ficam na temporada = ano calendario; semanas < 27 pertencem
#' a temporada do ano anterior (ex.: semana 10 de 2025 -> temporada 2024).
ano_sanitario <- function(ano, semana) {
  ano - as.integer(semana < SEMANA_CORTE_SANITARIA)
}

#' Rotulo "2024/2025" de uma temporada (ano sanitario).
rotulo_ano_sanitario <- function(ano_san) {
  sprintf("%d/%d", ano_san, ano_san + 1L)
}

# Ordem de exibicao das semanas no eixo dos graficos por temporada: comeca em
# 27 e fecha em 1..26 - sempre 52 semanas. A semana 53 (existe so em alguns
# anos) NAO vira categoria propria: e dobrada metade em 52, metade em 1 antes
# de chegar aqui (ver dobrar_semana_53() abaixo), porque raramente tem anos
# de referencia suficientes pra formar uma estatistica propria (quartil,
# mediana) e apareceria como um buraco isolado no meio do grafico.
NIVEIS_SEMANA_SANITARIA <- c(
  seq(SEMANA_CORTE_SANITARIA, 52L),
  seq(1L, SEMANA_CORTE_SANITARIA - 1L)
)

#' Dobra a semana epidemiologica 53 nas semanas vizinhas da MESMA temporada.
#'
#' A semana 53 so existe em anos "longos" do calendario ISO, entao raramente
#' tem anos de referencia suficientes pra formar uma estatistica propria.
#' Em vez de virar categoria propria no eixo, ela e DOBRADA: metade do valor
#' de cada coluna em `colunas` vai pra semana 52, a outra metade pra semana 1
#' - ambas da MESMA temporada (ano_san), ja que a semana 1 pertence ao ano
#' sanitario que comecou na semana 27 anterior. O resultado e que toda
#' temporada, tenha ela 52 ou 53 semanas de calendario, ocupa exatamente 52
#' posicoes no eixo.
#'
#' @param d `data.frame` com colunas `semana`, `ano_san` e as colunas listadas
#'   em `colunas`.
#' @param colunas nomes das colunas numericas a dobrar (ex.: c("inc",
#'   "inc_nowcast"), ou c("casos", "casos_est")).
dobrar_semana_53 <- function(d, colunas) {
  com_53 <- d[d$semana == 53L, , drop = FALSE]
  d <- d[d$semana != 53L, , drop = FALSE]
  if (!nrow(com_53)) return(d)
  
  for (i in seq_len(nrow(com_53))) {
    linha <- com_53[i, ]
    idx52 <- which(d$ano_san == linha$ano_san & d$semana == 52L)
    idx1  <- which(d$ano_san == linha$ano_san & d$semana == 1L)
    
    for (col in colunas) {
      metade <- linha[[col]] / 2
      if (is.na(metade)) next
      if (length(idx52)) d[[col]][idx52] <- sum(d[[col]][idx52], metade, na.rm = TRUE)
      if (length(idx1))  d[[col]][idx1]  <- sum(d[[col]][idx1],  metade, na.rm = TRUE)
    }
  }
  d
}

# --------------------------------------------------------- malhas (GeoJSON) --
# BR_UF para a visao Brasil e MUN_<uf> para um estado. A malha vai para o
# navegador como TEXTO JSON cru marcado com htmlwidgets::JS(): passar como
# lista R obrigava o jsonlite a serializar milhares de coordenadas a cada
# renderizacao (~11-14 s so para Minas Gerais). Lida uma vez por UF, em cache.
GEO_DIR <- Sys.getenv("DENGUE_GEO_DIR", file.path("data", "geo"))
GEO_NAME_PROPERTY <- Sys.getenv("DENGUE_GEO_NAME_PROP", "codarea")

ler_texto <- function(arq) paste(readLines(arq, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

MALHA_UF_TXT <- ler_texto(file.path(GEO_DIR, "BR_UF.geojson"))
MALHA_UF     <- htmlwidgets::JS(MALHA_UF_TXT)
.FEICOES_UF  <- jsonlite::fromJSON(MALHA_UF_TXT, simplifyVector = FALSE)$features

.cache_malha <- new.env(parent = emptyenv())

#' Malha dos municipios de uma UF com o contorno da UF anexado ao FIM
#' (codarea "CONTORNO"), desenhado por cima e junto com zoom/arraste.
malha_municipios <- function(uf) {
  chave <- as.character(uf)
  if (!is.null(.cache_malha[[chave]])) return(.cache_malha[[chave]])
  txt <- ler_texto(file.path(GEO_DIR, sprintf("MUN_%d.geojson", uf)))
  contorno <- Filter(function(f) identical(as.character(f$properties$codarea), chave), .FEICOES_UF)
  m <- if (length(contorno)) {
    contorno[[1]]$properties$codarea <- "CONTORNO"
    htmlwidgets::JS(sprintf("(function(){ var m = %s; m.features.push(%s); return m; })()",
                            txt, jsonlite::toJSON(contorno[[1]], auto_unbox = TRUE, digits = NA)))
  } else htmlwidgets::JS(txt)
  .cache_malha[[chave]] <- m
  m
}

#' Nome da malha registrada e a malha das unidades de um recorte.
malha_do_recorte <- function(un) {
  if (identical(un$tipo, "uf")) list(nome = "BR_UF", malha = MALHA_UF)
  else list(nome = paste0("MUN_", un$uf), malha = malha_municipios(un$uf))
}

# ------------------------------------------ serie semanal por unidade ------
# Mapa de tendencia (R/mod_mapa_rt.R), mapa e Epi Scanner comparam UNIDADES:
# as 27 UFs na visao Brasil e os municipios da UF num estado/municipio. O
# dengue_ride guardava a serie de todos os municipios em memoria
# (SERIE_MUNICIPIOS); para o Brasil seriam 3,4 mi de linhas, entao a serie
# das UFs e calculada no start e a dos municipios de uma UF e lida do Parquet
# na primeira vez que a UF e aberta (cache por processo).
#
# Colunas no formato de SERIE_MUNICIPIOS: municipio_geocodigo/municipio_nome
# guardam o id/nome da unidade (na visao Brasil, a UF); `sigla` e o rotulo
# curto do ponto. rt: no municipio o valor da coluna; na UF, a media
# ponderada pelos casos (aproximacao, como em calcular_indicadores()).
message("[Dengue Brasil] Carregando série semanal por UF...")

SERIE_UFS <- tbl_dengue() |>
  dplyr::group_by(uf_ibge, data_ini_se) |>
  dplyr::summarise(
    se      = max(se, na.rm = TRUE),
    casos   = sum(casos, na.rm = TRUE),
    pop     = sum(pop, na.rm = TRUE),
    rt_num  = sum(rt * casos, na.rm = TRUE),
    rt_peso = sum(dplyr::if_else(!is.na(rt), casos, 0L), na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::collect() |>
  dplyr::mutate(
    municipio_geocodigo = as.integer(uf_ibge),
    data_ini_se = as.Date(data_ini_se),
    se     = as.integer(se),
    ano    = se %/% 100L,
    semana = se %%  100L,
    casos  = as.numeric(casos),
    pop    = as.numeric(pop),
    rt     = dplyr::if_else(rt_peso > 0, as.numeric(rt_num) / as.numeric(rt_peso), NA_real_)
  ) |>
  dplyr::left_join(dplyr::transmute(UF_LOOKUP, municipio_geocodigo = uf_ibge,
                                    municipio_nome = uf_nome, sigla = uf_sigla),
                   by = "municipio_geocodigo") |>
  dplyr::select(municipio_geocodigo, municipio_nome, sigla, data_ini_se, se, ano, semana,
                casos, pop, rt) |>
  dplyr::arrange(municipio_geocodigo, data_ini_se)

.cache_serie_uf <- new.env(parent = emptyenv())

#' Serie semanal dos municipios de uma UF (lida do Parquet, em cache).
serie_municipios_uf <- function(uf) {
  chave <- as.character(uf)
  if (!is.null(.cache_serie_uf[[chave]])) return(.cache_serie_uf[[chave]])
  uf <- as.integer(uf)
  d <- tbl_dengue() |>
    dplyr::filter(uf_ibge == !!uf) |>
    dplyr::group_by(municipio_geocodigo, data_ini_se) |>
    dplyr::summarise(
      se    = max(se, na.rm = TRUE),
      casos = sum(casos, na.rm = TRUE),
      pop   = max(pop, na.rm = TRUE),
      rt    = max(rt, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::collect() |>
    dplyr::mutate(
      municipio_geocodigo = as.integer(municipio_geocodigo),
      data_ini_se = as.Date(data_ini_se),
      se     = as.integer(se),
      ano    = se %/% 100L,
      semana = se %%  100L,
      casos  = as.numeric(casos),
      pop    = as.numeric(pop),
      rt     = as.numeric(rt)
    ) |>
    dplyr::left_join(dplyr::distinct(GEO_DICT, municipio_geocodigo, municipio_nome),
                     by = "municipio_geocodigo") |>
    dplyr::mutate(sigla = sub("\\s.*$", "", municipio_nome)) |>
    dplyr::arrange(municipio_geocodigo, data_ini_se)
  if (length(ls(.cache_serie_uf)) >= 30L) rm(list = ls(.cache_serie_uf), envir = .cache_serie_uf)
  .cache_serie_uf[[chave]] <- d
  d
}

#' Unidades do recorte: UFs no Brasil, municipios da UF num estado/municipio.
unidades_do_recorte <- function(geo) {
  if (identical(geo$nivel, "brasil")) {
    list(tipo = "uf", uf = NA_integer_, chave = "brasil", rotulo = "UF", rotulo_plural = "UFs")
  } else {
    list(tipo = "municipio", uf = as.integer(geo$uf_ibge), chave = paste0("uf_", geo$uf_ibge),
         rotulo = "município", rotulo_plural = "municípios")
  }
}

#' Serie semanal por unidade do recorte.
serie_unidades <- function(un) {
  if (identical(un$tipo, "uf")) SERIE_UFS else serie_municipios_uf(un$uf)
}

# ------------------------------------------------------------- paleta dengue --
# Identidade visual comum aos tres paineis (dengue_df, dengue_ride e
# PVdengue_df), baseada no guia Cenarios+: azul Cenarios na interface e uma
# rampa terracota que representa a dengue (nao ha cor oficial no MS nem na
# SES-DF; a familia laranja-avermelhada e a associacao mais comum). Mesmos
# valores em config/global.R do dengue_df e R/utils_app_theme.R do PV.
#
# Definida AQUI, e nao em R/utils_ui.R, porque os modulos em R/ sao
# carregados em ordem alfabetica: utils_ui.R viria por ultimo e as constantes
# de cor no topo dos modulos nao a encontrariam.
#
# Regras de uso:
#   - magnitude (incidencia, casos)   -> TONS_DENGUE, claro = pouco, escuro = muito
#   - polaridade (Rt, razao historica) -> azul abaixo do limiar, terracota acima
#   - estado (alerta, obito)           -> ok/atencao/grave/critico, sempre com rotulo
#   - categorias (modelos, classes)    -> SERIES_CENARIOS, na ordem, sem reciclar
# Validado para daltonismo (protan/deutan) e contraste sobre fundo branco.
TONS_DENGUE <- c(
  `50`  = "#FFF2EB", `100` = "#FDDDCF", `200` = "#F7C0A8", `300` = "#ED9E7C",
  `400` = "#D97F55", `500` = "#C0663C", `600` = "#A34F25", `700` = "#853B15",
  `800` = "#682B0A", `900` = "#4E1D03"
)

TONS_AZUL <- c(
  `50`  = "#ECF7FE", `100` = "#CFE9F9", `200` = "#A8D5F1", `300` = "#7ABEE6",
  `400` = "#4FA4D3", `500` = "#3891C0", `600` = "#0E7BA8", `700` = "#00638C",
  `800` = "#004565", `900` = "#00324C"
)

PALETA_DENGUE <- c(
  fundo       = "#F2F6F9",
  superficie  = "#FFFFFF",
  linha       = "#D8E3EA",
  tinta       = "#12222B",  # titulos de grafico, numeros, serie em foco
  texto       = "#4A4A4A",  # corpo
  sub         = "#7A8B95",  # eixos, legendas, linhas de referencia
  neutro      = "#B8C4CC",  # "sem dado", "ignorado"
  azul        = TONS_AZUL[["500"]],
  azul_acao   = TONS_AZUL[["600"]],
  azul_titulo = TONS_AZUL[["700"]],
  dengue      = TONS_DENGUE[["500"]],
  dengue_forte = TONS_DENGUE[["700"]],
  ok          = "#17895B",
  atencao     = "#DCA744",
  grave       = "#C65D26",
  critico     = "#8F2639"
)

SERIES_CENARIOS <- c("#3891C0", "#C0663C", "#6C5CA8", "#5A9E2E",
                     "#8E3B46", "#B08A1E", "#3F4E9E", "#1B9AAA")

#' Rampa continua da dengue. Para e_visual_map(type = "continuous").
RAMPA_DENGUE <- unname(TONS_DENGUE[c("50", "200", "300", "500", "700", "900")])

# Fonte dos graficos echarts (o echarts nao herda a fonte do CSS da pagina).
FONTE_GRAFICO <- "'DM Sans', system-ui, sans-serif"

#' Converte "#RRGGBB" em "rgba(r,g,b,a)" (echarts aceita os dois; rgba
#' quando a cor precisa de transparencia).
cor_alpha <- function(hex, alpha) {
  rgb <- grDevices::col2rgb(hex)[, 1]
  sprintf("rgba(%d,%d,%d,%s)", rgb[1], rgb[2], rgb[3], alpha)
}

# ------------------------------------------------- escala de incidencia -----
# Cortes usados pelo Ministerio da Saude (casos/100 mil hab.):
# < 100 baixa, 100-299 media, >= 300 alta. Os dois ultimos niveis separam
# situacoes epidemicas que a faixa ">= 300" sozinha achataria.
#
# Cores dos MAPAS e do heatmap: amarelo -> marrom (base ColorBrewer YlOrBr,
# a mesma do dengue_pe), deslocada um passo para o laranja escuro. O tom mais
# claro ainda se separa do fundo branco do mapa.
ESCALA_MAPA <- c("#FEE391", "#FEB24C", "#F07818", "#C4500A", "#7F2704")
RAMPA_MAPA  <- ESCALA_MAPA

PECAS_INCIDENCIA <- list(
  list(min = 1000,             label = "≥ 1.000",   color = ESCALA_MAPA[[5]]),
  list(min = 500,  max = 1000, label = "500 – 999", color = ESCALA_MAPA[[4]]),
  list(min = 300,  max = 500,  label = "300 – 499", color = ESCALA_MAPA[[3]]),
  list(min = 100,  max = 300,  label = "100 – 299", color = ESCALA_MAPA[[2]]),
  list(min = 0.01, max = 100,  label = "< 100",     color = ESCALA_MAPA[[1]]),
  list(value = 0,              label = "Sem casos", color = PALETA_DENGUE[["fundo"]])
)

# ----------------------------------------------------------------- helpers ---

#' Aplica o recorte geografico a uma query lazy.
#'
#' O filtro entra no WHERE; nada e materializado aqui. A chave "brasil"
#' significa o Brasil inteiro.
aplicar_filtro_geo <- function(dados, geo) {
  switch(
    geo$nivel,
    brasil    = dados,
    uf        = dplyr::filter(dados, uf_ibge == !!geo$uf_ibge),
    municipio = dplyr::filter(dados, municipio_geocodigo == !!geo$geocodigo),
    stop("Nivel geografico desconhecido: ", geo$nivel, call. = FALSE)
  )
}

#' Serie temporal semanal agregada para um recorte geografico.
#'
#' O filtro desce ate a leitura do Parquet (predicate pushdown) e so as colunas
#' citadas sao lidas do disco (projection pushdown). Volta uma linha por semana.
#'
#' Colunas devolvidas, por semana:
#'   data_ini_se  inicio da semana epidemiologica
#'   se           semana epidemiologica no formato AAAASS
#'   casos        notificados
#'   casos_est    estimados pelo nowcast do InfoDengue
#'   pop          populacao somada dos municipios do recorte naquela semana
#'   rt_direto    rt da linha (so e o proprio rt no nivel municipal)
#'   rt_num/rt_peso  numerador e peso da media de rt ponderada por casos
#'   n_mun        municipios com registro na semana
#'   n_mun_rt1    municipios com rt > 1 na semana
consultar_serie_semanal <- function(geo) {
  aplicar_filtro_geo(tbl_dengue(), geo) |>
    dplyr::group_by(data_ini_se) |>
    dplyr::summarise(
      # se e constante dentro de uma data_ini_se (a semana epidemiologica e
      # nacional); max() e so a forma de carrega-la pela agregacao.
      se        = max(se, na.rm = TRUE),
      casos     = sum(casos, na.rm = TRUE),
      casos_est = sum(casos_est, na.rm = TRUE),
      pop       = sum(pop, na.rm = TRUE),
      rt_direto = max(rt, na.rm = TRUE),
      rt_num    = sum(rt * casos, na.rm = TRUE),
      rt_peso   = sum(dplyr::if_else(!is.na(rt), casos, 0L), na.rm = TRUE),
      n_mun     = dplyr::n_distinct(municipio_geocodigo),
      n_mun_rt1 = sum(dplyr::if_else(rt > 1, 1L, 0L), na.rm = TRUE),
      .groups   = "drop"
    ) |>
    dplyr::collect() |>
    dplyr::mutate(
      data_ini_se = as.Date(data_ini_se),
      se          = as.integer(se),
      dplyr::across(c(casos, casos_est, pop, rt_direto, rt_num, rt_peso), as.numeric),
      n_mun       = as.integer(n_mun),
      n_mun_rt1   = as.integer(n_mun_rt1)
    ) |>
    dplyr::arrange(data_ini_se)
}

#' Reagrupa a serie semanal por mes, ou devolve como esta.
#'
#' A semana e atribuida inteira ao mes em que COMECA (data_ini_se). Semanas a
#' cavalo entre dois meses nao sao rateadas - convencao usual em vigilancia,
#' que mantem a semana epidemiologica como unidade indivisivel.
reagrupar_periodo <- function(d, agrupamento = c("semana", "mes")) {
  agrupamento <- match.arg(agrupamento)
  
  if (identical(agrupamento, "semana")) {
    d$periodo <- d$data_ini_se
    return(d)
  }
  
  d |>
    dplyr::mutate(periodo = as.Date(format(data_ini_se, "%Y-%m-01"))) |>
    dplyr::group_by(periodo) |>
    dplyr::summarise(
      casos     = sum(casos, na.rm = TRUE),
      casos_est = sum(casos_est, na.rm = TRUE),
      # Populacao nao se soma ao longo do tempo: o denominador do mes e o
      # mesmo conjunto de pessoas das semanas que o compoem.
      pop       = suppressWarnings(max(pop, na.rm = TRUE)),
      rt_direto = mean(rt_direto, na.rm = TRUE),
      rt_num    = sum(rt_num, na.rm = TRUE),
      rt_peso   = sum(rt_peso, na.rm = TRUE),
      n_mun     = suppressWarnings(max(n_mun, na.rm = TRUE)),
      n_mun_rt1 = round(mean(n_mun_rt1, na.rm = TRUE)),
      .groups   = "drop"
    ) |>
    dplyr::mutate(
      pop   = dplyr::if_else(is.finite(pop), pop, NA_real_),
      n_mun = dplyr::if_else(is.finite(n_mun), n_mun, NA_integer_)
    )
}

#' Deriva incidencia e Rt a partir da serie agregada.
#'
#' Rt depende do nivel do recorte: no municipio e o valor da coluna; acima
#' disso e a media ponderada pelos casos, que e aproximacao - ver mod_rt.R.
calcular_indicadores <- function(d, geo) {
  d |>
    dplyr::mutate(
      incidencia     = dplyr::if_else(pop > 0, casos     / pop * 1e5, NA_real_),
      incidencia_est = dplyr::if_else(pop > 0, casos_est / pop * 1e5, NA_real_),
      rt = if (identical(geo$nivel, "municipio")) {
        rt_direto
      } else {
        dplyr::if_else(rt_peso > 0, rt_num / rt_peso, NA_real_)
      }
    )
}

#' Expressao JS que formata a data do eixo (pt-BR), operando sobre a variavel `v`.
#'
#' O eixo X do echarts4r e categorico e entrega a data como texto "AAAA-MM-DD".
#' A formatacao e feita por manipulacao de string, NAO por `new Date(v)`:
#' "2024-01-05" e interpretado como UTC pelo JS, e toLocaleDateString num
#' navegador em UTC-3 devolveria 04/01/2024 - um dia a menos.
js_formato_data <- function(escala = c("mes", "dia")) {
  escala <- match.arg(escala)
  meses <- "['jan','fev','mar','abr','mai','jun','jul','ago','set','out','nov','dez']"
  
  if (identical(escala, "mes")) {
    sprintf(
      "(function(s){var p=String(s).split('-');return %s[parseInt(p[1],10)-1]+'/'+p[0];})(v)",
      meses
    )
  } else {
    "(function(s){var p=String(s).split('-');return p[2]+'/'+p[1]+'/'+p[0];})(v)"
  }
}

#' Formata inteiros no padrao brasileiro (12.345).
fmt_int <- function(x) {
  if (length(x) == 0 || all(is.na(x))) return("-")
  format(round(as.numeric(x)), big.mark = ".", decimal.mark = ",",
         scientific = FALSE, trim = TRUE)
}

#' Formata numeros com casas decimais no padrao brasileiro (1.234,5).
fmt_num <- function(x, digits = 1) {
  if (length(x) == 0 || all(is.na(x))) return("-")
  format(round(as.numeric(x), digits), big.mark = ".", decimal.mark = ",",
         nsmall = digits, scientific = FALSE, trim = TRUE)
}

#' Coalesce nulo/vazio (util nos modulos; forecasting/ tambem define o seu).
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# ---------------------------------------------------- forecasting (previsao) --
# Backend isolado em forecasting/ (treino offline via Rscript; ver
# README-FORECAST.md). Aqui so expomos get_dengue_forecast() aos modulos e
# carregamos as metricas do backtest. Se os pacotes (forecast/mgcv) ou os
# artefatos .rds nao existirem, o painel segue funcionando SEM a camada de
# previsao - FORECAST_DISPONIVEL controla isso nos modulos.
FORECAST_DISPONIVEL <- FALSE
FORECAST_METRICS    <- NULL   # linhas "todos" do model_metrics.csv (com coluna `classe`)
FORECAST_CLASSES    <- NULL   # c(chave = rotulo) das classes com modelo

local({
  arq_fun <- file.path("forecasting", "get_forecast.R")
  arq_cfg <- file.path("forecasting", "models", "ensemble_config.rds")
  arq_met <- file.path("forecasting", "model_metrics.csv")

  if (!file.exists(arq_fun) || !file.exists(arq_cfg)) {
    message("[Dengue Brasil] Forecasting: artefatos ausentes - camada de previsao desativada.")
    return(invisible())
  }

  ok <- tryCatch({
    source(arq_fun, local = FALSE, encoding = "UTF-8")
    is.function(get_dengue_forecast)
  }, error = function(e) {
    message("[Dengue Brasil] Forecasting indisponivel: ", conditionMessage(e))
    FALSE
  })
  if (!isTRUE(ok)) return(invisible())

  if (file.exists(arq_met)) {
    m <- tryCatch(utils::read.csv(arq_met, stringsAsFactors = FALSE, encoding = "UTF-8"),
                  error = function(e) NULL)
    if (!is.null(m) && "horizonte" %in% names(m)) {
      keep <- intersect(c("classe", "modelo", "RMSE", "MAE", "MAPE", "sMAPE", "MASE", "peso"),
                        names(m))
      FORECAST_METRICS <<- m[m$horizonte == "todos", keep]
    }
  }
  # Classes com modelo registrado (FC_CLASSES / fc_classes_com_modelo vem de
  # utils_forecast.R, ja carregado pelo source de get_forecast.R acima).
  if (exists("fc_classes_com_modelo")) {
    cc <- fc_classes_com_modelo()
    FORECAST_CLASSES <<- stats::setNames(vapply(cc, fc_rotulo_classe, character(1)), cc)
  }
  FORECAST_DISPONIVEL <<- TRUE
  message(sprintf("[Dengue Brasil] Forecasting ativo (%d modelo(s), %d classe(s): %s).",
                  if (exists("FC_MODELOS")) length(FC_MODELOS) else 0L,
                  length(FORECAST_CLASSES),
                  paste(FORECAST_CLASSES, collapse = ", ")))
})

# Sem os artefatos do forecasting (antes do primeiro train_models.R) o
# diagrama ainda monta a legenda com o horizonte da faixa do IP.
if (!exists("FC_HORIZON_DEFAULT")) FC_HORIZON_DEFAULT <- 4L

# Rotulo amigavel por chave de modelo/ensemble - usado pelos modulos do painel.
FORECAST_ROTULO <- function(chave) {
  if (!isTRUE(FORECAST_DISPONIVEL)) return(chave)
  if (identical(chave, "global"))    return("Ensemble global")
  if (startsWith(chave, "ens_"))     return(sprintf("Ensemble %s",
                                    fc_rotulo_classe(sub("^ens_", "", chave))))
  fc_rotulo_modelo(chave)
}

# Cache em DISCO da previsao (app_cache/previsao), compartilhado entre
# processos e reinicios: o calculo (alguns modelos sao reajustados ao vivo)
# era refeito pela primeira visita apos cada `touch restart.txt` ou troca de
# processo. A chave inclui o dia e VERSAO_DADOS (tamanho + data do Parquet),
# entao uma atualizacao do ETL nunca serve previsao velha. Sem app_cache/
# gravavel, cai no cache em memoria padrao ("app").
VERSAO_DADOS <- paste(file.size(PARQUET_DENGUE), as.numeric(file.mtime(PARQUET_DENGUE)))
CACHE_PREVISAO <- if (dir.exists("app_cache") && file.access("app_cache", 2) == 0) {
  cachem::cache_disk(file.path("app_cache", "previsao"),
                     max_size = 200 * 1024^2, max_age = 3 * 24 * 3600)
} else {
  "app"
}

# --------------------------------------------------------- modulos do app ----
# Carregamento explicito: nao depende da ordem de auto-source do diretorio R/.
for (.f in list.files("R", pattern = "\\.R$", full.names = TRUE)) {
  source(.f, local = FALSE, encoding = "UTF-8")
}
rm(.f)
