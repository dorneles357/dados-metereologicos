extrair_info_arquivos <- function(arquivos) {
  padrao <- "extracts/(\\d{4})/INMET_(\\w+)_(\\w{2})_([A-Z0-9]+)_([^_]+)_\\d{2}-\\d{2}-\\d{4}_A_\\d{2}-\\d{2}-\\d{4}\\.CSV" #nolint

  info <- stringr::str_match(arquivos, padrao)

  data.frame(
    caminho_completo = arquivos,
    ano = as.integer(info[, 2]),
    tipo_rede = info[, 3],  # CO, NC, etc.
    uf = info[, 4],
    codigo_estacao = info[, 5],
    municipio = info[, 6],
    stringsAsFactors = FALSE
  )
}

# Função para filtrar arquivos
filtrar_arquivos <- function(
  arquivos,
  anos = NULL,
  ufs = NULL,
  municipios = NULL,
  tipos_rede = NULL
) {

  df <- extrair_info_arquivos(arquivos)

  if (!is.null(anos)) {
    df <- df |> dplyr::filter(ano %in% anos)
  }

  if (!is.null(ufs)) {
    df <- df |> dplyr::filter(uf %in% ufs)
  }

  if (!is.null(municipios)) {
    df <- df |> dplyr::filter(municipio %in% municipios)
  }

  if (!is.null(tipos_rede)) {
    df <- df |> dplyr::filter(tipo_rede %in% tipos_rede)
  }

  return(df$caminho_completo)
}

explorar_arquivos <- function(arquivos) {
  df <- extrair_info_arquivos(arquivos)

  cat("=== RESUMO DOS ARQUIVOS DISPONÍVEIS ===\n")
  cat("Total de arquivos:", nrow(df), "\n\n")

  cat("Anos disponíveis:", toString(sort(unique(df$ano))), "\n\n")

  cat("Estados (UF) disponíveis:\n")
  print(table(df$uf))
  cat("\n")

  cat("Tipos de rede disponíveis:\n")
  print(table(df$tipo_rede))
  cat("\n")

  cat("Municípios por UF:\n")
  municipios_por_uf <- df |>
    dplyr::group_by(uf) |>
    dplyr::summarise(
      n_municipios = dplyr::n_distinct(municipio),
      municipios = toString(sort(unique(municipio)))
    )
  print(municipios_por_uf)

  return(df)
}