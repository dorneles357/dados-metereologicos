# Média móvel de 7 dias para suavizar variações
temp_mm <- temp_diaria |>
  dplyr::mutate(
    media_movel = zoo::rollmean(Temp_Media, 7, fill = NA, align = "right")
  )

# Gráfico de controle com média móvel
qcc::qcc(temp_mm$media_movel[!is.na(temp_mm$media_movel)], 
    type = "xbar.one", 
    title = "Média Móvel 7 dias - Temperatura")
