source("R/utils/file_data_base_filter.R")

if (file.exists("data/raw/extracts/files.txt")) {
  arquivos_csv <- readLines("data/raw/extracts/files.txt")
  arquivos_csv <- arquivos_csv[arquivos_csv != ""]

  cat("Encontrados", length(arquivos_csv), "arquivos CSV\n")
  print(head(arquivos_csv))
} else {
  stop("Arquivo data/raw/extracts/files.txt não encontrado!")
}

# ---------------------------------------------------------
# 1. Explorar os dados disponíveis
# ---------------------------------------------------------
info_completa <- explorar_arquivos(arquivos_csv)

# ---------------------------------------------------------
# 2. Filtrar por ano
# ---------------------------------------------------------
arquivos_2020 <- filtrar_arquivos(arquivos_csv, anos = 2020:2025)

# ---------------------------------------------------------
# 3. Filtrar por UF
# ---------------------------------------------------------
arquivos_rs <- filtrar_arquivos(arquivos_csv, ufs = "RS")
