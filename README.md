# Dengue Brasil — Painel de monitoramento de dengue

Versão nacional do **dengue_ride**: mesma base (InfoDengue,
`"Info_Dengue".dengue`) e os mesmos gráficos — KPIs, mapa, tendência
(quadrantes), série temporal, curva epidêmica, Rt, diagrama de controle com
previsão, sazonalidade e Epi Scanner — para os 5.570 municípios, com recorte
**Brasil → Estado → Município**. A documentação completa do painel original
está em `README-dengue_ride.md` (vale para tudo que não está listado abaixo).

## Atualizar os dados e a previsão

```bash
cd /srv/shiny-server/dengue_br && Rscript etl_dengue_br.R && Rscript forecasting/train_models.R && touch restart.txt
```

- ETL: ~2 min, `data/dengue_br.parquet` (~57 MB, 3,4 mi de linhas, só as
  colunas usadas; não versionado).
- Previsão: ~2 min. Pesos do ensemble pelo backtest do **Brasil agregado**;
  modelos pré-ajustados para Brasil + 27 UFs (`forecasting/models`, ~48 MB).
  Município é ajustado ao vivo na primeira visita e fica no cache de disco
  (`app_cache/previsao`). `DENGUE_FC_MUNICIPIOS=1` pré-aquece todos os
  municípios (horas e vários GB — não recomendado).

## O que muda em relação ao dengue_ride

| Tema | Decisão |
|---|---|
| Mapa | No Brasil, as 27 UFs (clique abre o estado); num estado, os municípios dele, com o município em foco destacado. |
| Quadrantes e Epi Scanner | Comparam as unidades do recorte do Painel: UFs no Brasil, municípios da UF num estado. O Epi Scanner passou a seguir o recorte (5.570 municípios num mapa só seria ilegível). |
| Série por município | `SERIE_MUNICIPIOS` (tudo em memória) virou `SERIE_UFS` (no start) + `serie_municipios_uf()` (sob demanda, em cache). |
| Malhas | JSON cru via `htmlwidgets::JS` (serializar a malha de MG como lista R levava 11–14 s por render). |
| Recorte | O filtro só propaga quando o recorte muda (o clique no mapa troca UF e nível em duas mensagens, e o painel recalculava duas vezes). |

Rodar local: `shiny::runApp()` na pasta (preview na porta 8793).
