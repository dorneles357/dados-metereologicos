df |>
  dplyr::mutate(day = lubridate::day(data)) |>
  #dplyr::filter(mes_nome == "June" & day == 1) |>
  ggplot2::ggplot(ggplot2::aes(x = temperatura_maxima_na_hora_ant_aut_c, y = umidade_rel_max_na_hora_ant_aut_percent)) +
  #ggplot2::geom_point() +
  ggplot2::geom_smooth(ggplot2::aes(fill = mes_nome)) +
  ggplot2::facet_wrap(~mes_nome)

df |>
  dplyr::mutate(day = lubridate::day(data)) |>
  #dplyr::filter(mes_nome == "June" & day == 1) |>
  ggplot2::ggplot(ggplot2::aes(x = temperatura_maxima_na_hora_ant_aut_c, y = pressao_atmosferica_max_na_hora_ant_aut_m_b)) +
  #ggplot2::geom_point() +
  ggplot2::geom_smooth(ggplot2::aes(fill = mes_nome)) +
  ggplot2::facet_wrap(~mes_nome)
