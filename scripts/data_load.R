santa_maria_2025 <- readr::read_delim(
  "data/2025/INMET_S_RS_A803_SANTA MARIA_01-01-2025_A_31-10-2025.CSV",
  delim = ";",
  escape_double = FALSE,
  locale = readr::locale(decimal_mark = ",", encoding = "ISO-8859-1"),
  trim_ws = TRUE,
  skip = 8
)

df <- santa_maria_2025 |>
  dplyr::mutate(
    mes_num = lubridate::month(Data),
    mes_nome = lubridate::month(Data, label = TRUE, abbr = FALSE),
    date_time = lubridate::hours(as.numeric(stringr::str_remove(`Hora UTC`, "00 UTC")))
  ) |>
  janitor::clean_names()

rm(santa_maria_2025)
