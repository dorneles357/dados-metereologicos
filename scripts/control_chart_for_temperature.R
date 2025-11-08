library(qcc)
library(tidyverse)

# Preparar dados diários
temp_diaria <- df |>
  dplyr::mutate(
    temperatura = temperatura_do_ar_bulbo_seco_horaria_c,
    date = as.Date(data)
  ) |>
  dplyr::group_by(data) |>
  dplyr::summarise(
    temp_media = mean(temperatura, na.rm = TRUE),
    temp_max = max(temperatura, na.rm = TRUE),
    temp_min = min(temperatura, na.rm = TRUE)
  )

# Gráfico X-bar para temperatura média
grafico_controle_temp <- qcc::qcc(
  temp_diaria$temp_tedia, 
  type = "xbar.one",
  title = "Controle de Temperatura Média Diária - Santa Maria"
)
