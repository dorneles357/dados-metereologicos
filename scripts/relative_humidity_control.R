# Preparar dados de umidade
umidade_diaria <- df |>
  dplyr::mutate(data = as.Date(data)) |>
  dplyr::group_by(data) |>
  dplyr::summarise(
    umidade_media = mean(umidade_relativa_do_ar_horaria_percent, na.rm = TRUE)
  )

# Gráfico de controle
qcc::qcc(umidade_diaria$umidade_media, 
    type = "xbar.one",
    title = "Controle de Umidade Relativa Média")
