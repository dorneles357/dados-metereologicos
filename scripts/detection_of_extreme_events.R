# Calcular limites naturais do processo
limites_temp <- temp_diaria |>
  dplyr::summarise(
    media = mean(temp_media),
    sd = sd(temp_media),
    lsc = media + 3 * sd,
    lic = media - 3 * sd
  )

# Identificar pontos fora de controle
temp_diaria |>
  dplyr::mutate(
    fora_controle = temp_media > limites_temp$lsc | temp_media < limites_temp$lic,
    tipo_evento = dplyr::case_when(
      temp_media > limites_temp$lsc ~ "Onda de Calor",
      temp_media < limites_temp$lic ~ "Onda de Frio",
      TRUE ~ "Normal"
    )
  ) |>
  dplyr::filter(fora_controle == 0) |>
  dplyr::select(data, temp_media, tipo_evento) |>
  View()
