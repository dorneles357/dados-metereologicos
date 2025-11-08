df <- santa_maria_2025 |>
  dplyr::mutate(
    mes_num = lubridate::month(Data),
    mes_nome = lubridate::month(Data, label = TRUE, abbr = FALSE),
    date_time = lubridate::hours(as.numeric(stringr::str_remove(`Hora UTC`, "00 UTC")))
  ) |>
  janitor::clean_names()

dplyr::glimpse(df)

# TEMPERATURA -------------------------------------------------------------

#Amplitude da temperatura ao longo dos meses
df |>
  dplyr::select(mes_nome, data, temperatura_maxima_na_hora_ant_aut_c, temperatura_minima_na_hora_ant_aut_c,  hora_utc) |>
  dplyr::mutate(
    variacao_temperatura = temperatura_maxima_na_hora_ant_aut_c - temperatura_minima_na_hora_ant_aut_c,
  ) |>
ggplot2::ggplot(ggplot2::aes(x = variacao_temperatura)) +
  ggplot2::geom_density(ggplot2::aes(fill = mes_nome), alpha = 0.5) +
  ggplot2::labs(
    title = "Variação mensal da temperatura (2025)",
    x = "Taxa de variação (°C)",
    y = "Densidade"
  ) +
  ggplot2::theme_bw()

#Variacao sazonal ao longo do ano
ggplot2::ggplot(df, ggplot2::aes(x = mes_nome, y = temperatura_maxima_na_hora_ant_aut_c)) +
  ggplot2::geom_boxplot() +
  ggplot2::stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "red") +
  ggplot2::theme_minimal() +
  ggplot2::labs(
    title = "Distribuição mensal da temperatura máxima (2025)",
    x = "Mês",
    y = "Temperatura máxima (°C)"
  )

ggplot2::ggplot(df, ggplot2::aes(x = mes_nome, y = temperatura_minima_na_hora_ant_aut_c)) +
  ggplot2::geom_boxplot() +
  ggplot2::stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "blue") +
  ggplot2::theme_minimal() +
  ggplot2::labs(
    title = "Distribuição mensal da temperatura máxima (2025)",
    x = "Mês",
    y = "Temperatura máxima (°C)"
  )

#Temperatura máxima ao longo do dia (por mês)
df |>
  dplyr::mutate(
    hora_num = as.numeric(substr(hora_utc, 1, 2))
  ) |>
  ggplot2::ggplot(ggplot2::aes(x = hora_num, y = temperatura_maxima_na_hora_ant_aut_c)) +
  ggplot2::geom_point(alpha = 0.6, size = 1) +
  ggplot2::facet_wrap(~mes_nome) +
  ggplot2::scale_x_continuous(
    breaks = c(0, 12, 23),
    labels = c("00", "12", "23"),
    limits = c(0, 23)
  ) +
  ggplot2::theme_bw() +
  ggplot2::labs(
    title = "Temperatura máxima ao longo do dia (por mês)",
    x = "Hora (UTC)",
    y = "Temperatura máxima (°C)"
  )

#Temperatura máxima ao longo dos dias (por mes)
df |>
  dplyr::mutate(
    day = lubridate::day(data)
  ) |>
  ggplot2::ggplot(ggplot2::aes(x = day, y = temperatura_maxima_na_hora_ant_aut_c)) +
  ggplot2::geom_point(alpha = 0.6, size = 1) +
  ggplot2::facet_wrap(~mes_nome) +
  ggplot2::theme_bw() +
  ggplot2::labs(
    title = "Temperatura máxima ao longo dos dias (por mês)",
    x = "Dia",
    y = "Temperatura máxima (°C)"
  )

#Variacao da temperatura ao longo do dia
df |>
  dplyr::mutate(
    day = lubridate::day(data),
    amplitude = temperatura_maxima_na_hora_ant_aut_c - temperatura_minima_na_hora_ant_aut_c
  ) |>
  dplyr::filter(mes_nome == "January" & day == 1) |>
  ggplot2::ggplot(ggplot2::aes(x = hora_utc)) +
  ggplot2::geom_segment(
    ggplot2::aes(
      x = hora_utc, xend = hora_utc,
      y = temperatura_minima_na_hora_ant_aut_c,
      yend = temperatura_maxima_na_hora_ant_aut_c
    ),
    color = "gray40", linewidth = 0.6
  ) +
  ggplot2::geom_text(
    ggplot2::aes(
      y = (temperatura_maxima_na_hora_ant_aut_c + temperatura_minima_na_hora_ant_aut_c) / 2,
      label = round(amplitude, 1)
    ),
    color = "gray20", size = 3, vjust = -0.5
  ) +
  ggplot2::geom_point(
    ggplot2::aes(y = temperatura_minima_na_hora_ant_aut_c, color = "Mínima"),
    alpha = 0.8, size = 1.8
  ) +
  ggplot2::geom_point(
    ggplot2::aes(y = temperatura_maxima_na_hora_ant_aut_c, color = "Máxima"),
    alpha = 0.8, size = 1.8
  ) +
  ggplot2::scale_color_manual(
    values = c("Mínima" = "blue", "Máxima" = "red")
  ) +
  ggplot2::theme_bw() +
  ggplot2::labs(
    title = "Temperatura e amplitude térmica (01-01-2025)",
    x = "Hora (UTC)",
    y = "Temperatura (°C)"
  )

# UMIDADE -----------------------------------------------------------------

summary(df$umidade_rel_max_na_hora_ant_aut_percent, na.rm = TRUE)
summary(df$umidade_rel_min_na_hora_ant_aut_percent, na.rm = TRUE)

#Amplitude da umidade relativa ao longo dos meses
df |>
  dplyr::select(mes_nome, data, umidade_rel_max_na_hora_ant_aut_percent, umidade_rel_min_na_hora_ant_aut_percent,  hora_utc) |>
  dplyr::mutate(
    amp_umidade_relativa = umidade_rel_max_na_hora_ant_aut_percent - umidade_rel_min_na_hora_ant_aut_percent,
  ) |>
  ggplot2::ggplot(ggplot2::aes(x = amp_umidade_relativa)) +
  ggplot2::geom_density(ggplot2::aes(fill = mes_nome), alpha = 0.5) +
  ggplot2::labs(
    title = "Variação da umidade relativa (2025)",
    x = "Taxa de variação %",
    y = "Densidade"
  ) +
  ggplot2::theme_bw()

#Variacao ao longo do ano (2025)
ggplot2::ggplot(df, ggplot2::aes(x = mes_nome, y = umidade_rel_max_na_hora_ant_aut_percent)) +
  ggplot2::geom_boxplot() +
  ggplot2::stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "red") +
  ggplot2::theme_minimal() +
  ggplot2::labs(
    title = "Distribuição mensal da umidade relativa máxima (2025)",
    x = "Mês",
    y = "Umidade relativa máxima (%)"
  )
ggplot2::ggplot(df, ggplot2::aes(x = mes_nome, y = umidade_rel_min_na_hora_ant_aut_percent)) +
  ggplot2::geom_boxplot() +
  ggplot2::stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "blue") +
  ggplot2::theme_minimal() +
  ggplot2::labs(
    title = "Distribuição mensal da umidade relativa máxima (2025)",
    x = "Mês",
    y = "Umidade mínima (%)"
  )

#umidade relativa máxima ao longo do dia (por mês)
df |>
  dplyr::mutate(
    hora_num = as.numeric(substr(hora_utc, 1, 2))
  ) |>
  ggplot2::ggplot(ggplot2::aes(x = hora_num, y = umidade_rel_max_na_hora_ant_aut_percent)) +
  ggplot2::geom_point(alpha = 0.6, size = 1) +
  ggplot2::facet_wrap(~mes_nome) +
  ggplot2::scale_x_continuous(
    breaks = c(0, 12, 23),
    labels = c("00", "12", "23"),
    limits = c(0, 23)
  ) +
  ggplot2::theme_bw() +
  ggplot2::labs(
    title = "Umidade relativa máxima ao longo do dia (por mês)",
    x = "Hora (UTC)",
    y = "Umidade relativa (%)"
  )

#Umidade relativa ao longo dos dias (por mes)
df |>
  dplyr::mutate(
    day = lubridate::day(data)
  ) |>
  ggplot2::ggplot(ggplot2::aes(x = day, y = umidade_rel_max_na_hora_ant_aut_percent)) +
  ggplot2::geom_point(alpha = 0.6, size = 1) +
  ggplot2::facet_wrap(~mes_nome) +
  ggplot2::theme_bw() +
  ggplot2::labs(
    title = "Umidade relativa máxima ao longo dos dias (por mês)",
    x = "Dia",
    y = "Umidade relativa (%)"
  )

#Variacao da Umidade relativa ao longo do dia
df |>
  dplyr::mutate(
    day = lubridate::day(data),
    amplitude = umidade_rel_max_na_hora_ant_aut_percent - umidade_rel_min_na_hora_ant_aut_percent
  ) |>
  dplyr::filter(mes_nome == "January" & day == 1) |>
  ggplot2::ggplot(ggplot2::aes(x = hora_utc)) +
  ggplot2::geom_segment(
    ggplot2::aes(
      x = hora_utc, xend = hora_utc,
      y = umidade_rel_min_na_hora_ant_aut_percent,
      yend = umidade_rel_max_na_hora_ant_aut_percent
    ),
    color = "gray40", linewidth = 0.6
  ) +
  ggplot2::geom_text(
    ggplot2::aes(
      y = (umidade_rel_max_na_hora_ant_aut_percent + umidade_rel_min_na_hora_ant_aut_percent) / 2,
      label = round(amplitude, 1)
    ),
    color = "gray20", size = 3, vjust = -0.5
  ) +
  ggplot2::geom_point(
    ggplot2::aes(y = umidade_rel_min_na_hora_ant_aut_percent, color = "Mínima"),
    alpha = 0.8, size = 1.8
  ) +
  ggplot2::geom_point(
    ggplot2::aes(y = umidade_rel_max_na_hora_ant_aut_percent, color = "Máxima"),
    alpha = 0.8, size = 1.8
  ) +
  ggplot2::scale_color_manual(
    values = c("Mínima" = "blue", "Máxima" = "red")
  ) +
  ggplot2::theme_bw() +
  ggplot2::labs(
    title = "Umidade relativa e amplitude (01-01-2025)",
    x = "Hora (UTC)",
    y = "Umidade relativa (%)"
  )
# PRESSAO ATMOSFERICA -----------------------------------------------------

summary(df$pressao_atmosferica_max_na_hora_ant_aut_m_b, na.rm = TRUE)
summary(df$pressao_atmosferica_min_na_hora_ant_aut_m_b, na.rm = TRUE)

ggplot2::ggplot(df, ggplot2::aes(x = mes_nome)) +
  ggplot2::geom_boxplot(ggplot2::aes(y = pressao_atmosferica_max_na_hora_ant_aut_m_b))

ggplot2::ggplot(df, ggplot2::aes(x = mes_nome)) +
  ggplot2::geom_boxplot(ggplot2::aes(y = pressao_atmosferica_min_na_hora_ant_aut_m_b))

# PRECIPTACAO ACUMULADA ---------------------------------------------------

summary(df$precipitacao_total_horario_mm, na.rm = TRUE)

ggplot2::ggplot(df, ggplot2::aes(x = mes_nome)) +
  ggplot2::geom_boxplot(ggplot2::aes(y = precipitacao_total_horario_mm))


df |>
  dplyr::filter(mes_nome == "May") |>
  ggplot2::ggplot(ggplot2::aes(x = data)) +
  ggplot2::geom_point(ggplot2::aes(y = precipitacao_total_horario_mm)) +
  ggplot2::theme_bw()
