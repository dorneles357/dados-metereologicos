# Transformar precipitação (cuidado com os zeros)
precip_diaria <- df |>
  dplyr::mutate(
    precipitacao = as.numeric(stringr::str_replace(precipitacao_total_horario_mm, ",", ".")),
    data = as.Date(data)
  ) |>
  dplyr::group_by(data) |>
  dplyr::summarise(
    precip_total = sum(precipitacao, na.rm = TRUE)
  ) |>
  dplyr::filter(precip_total > 0)  # Filtrar dias com chuva

# Gráfico para dias chuvosos
qcc::qcc(precip_diaria$precip_total, 
    type = "xbar.one",
    title = "Controle de Precipitação em Dias Chuvosos")
