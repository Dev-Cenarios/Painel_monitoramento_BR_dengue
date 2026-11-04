# ==============================================================================
# Dengue Brasil | etl_dengue_br.R
# Extrai do PostgreSQL ("Info_Dengue".dengue, base do InfoDengue) a serie de
# TODOS os municipios do Brasil e grava data/dengue_br.parquet, a unica fonte
# de dados lida pelo app (via DuckDB, como no dengue_ride).
#
# Diferenca para o etl_ride.R do dengue_ride: sem filtro de municipio e so as
# colunas que o painel e o forecasting usam (o SELECT * nacional passaria de
# 100 MB; o disco do servidor esta quase cheio). O arquivo sai ordenado por
# UF, municipio e semana, em row groups pequenos: o filtro por UF/municipio
# do DuckDB pula os blocos que nao interessam.
#
# Uso (working directory = raiz do projeto):  Rscript etl_dengue_br.R
# ==============================================================================

library(DBI)
library(RPostgres)
library(arrow)

if (file.exists(".Renviron")) readRenviron(".Renviron")

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host     = Sys.getenv("PG_HOST"),
  port     = as.integer(Sys.getenv("PG_PORT", "5432")),
  dbname   = Sys.getenv("PG_DB"),
  user     = Sys.getenv("PG_USER"),
  password = Sys.getenv("PG_PASS"),  # credenciais no .Renviron (ver .Renviron.example)
  bigint   = "numeric"
)

t0 <- Sys.time()
message("[ETL Dengue BR] Lendo \"Info_Dengue\".dengue (Brasil inteiro)...")

# Nome do municipio: o InfoDengue tem alguns em minuscula ("dourados") em
# parte das linhas - fica o nome com inicial maiuscula, igual em todas as
# semanas do municipio.
dados <- DBI::dbGetQuery(con, "
WITH nomes AS (
  SELECT municipio_geocodigo,
         COALESCE(max(municipio_nome) FILTER (WHERE municipio_nome ~ '^[A-ZÀ-Ú]'),
                  max(municipio_nome)) AS municipio_nome
  FROM \"Info_Dengue\".dengue
  GROUP BY 1
)
SELECT d.data_ini_se, d.se::int AS se,
       d.municipio_geocodigo::int AS municipio_geocodigo, n.municipio_nome,
       d.uf, d.uf_ibge::int AS uf_ibge,
       d.casos::int AS casos, d.casos_est::float8 AS casos_est,
       d.casos_est_min::float8 AS casos_est_min, d.casos_est_max::float8 AS casos_est_max,
       d.p_rt1::float8 AS p_rt1, d.rt::float8 AS rt, d.p_inc100k::float8 AS p_inc100k,
       d.nivel::int AS nivel, d.transmissao::int AS transmissao, d.pop::float8 AS pop,
       d.tempmed::float8 AS tempmed, d.umidmed::float8 AS umidmed
FROM \"Info_Dengue\".dengue d
JOIN nomes n USING (municipio_geocodigo)
ORDER BY d.uf_ibge, d.municipio_geocodigo, d.data_ini_se")

DBI::dbDisconnect(con)

dados$data_ini_se <- as.Date(dados$data_ini_se)

dir.create("data", showWarnings = FALSE, recursive = TRUE)
arrow::write_parquet(dados, "data/dengue_br.parquet", chunk_size = 100000)

message(sprintf(
  "[ETL Dengue BR] OK: %s linhas, %d municipios, %s a %s, %.1f MB, em %.0fs.",
  format(nrow(dados), big.mark = ".", decimal.mark = ","),
  length(unique(dados$municipio_geocodigo)),
  format(min(dados$data_ini_se), "%d/%m/%Y"), format(max(dados$data_ini_se), "%d/%m/%Y"),
  file.size("data/dengue_br.parquet") / 1024^2,
  as.numeric(difftime(Sys.time(), t0, units = "secs"))
))
