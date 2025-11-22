# Script de Análise de Consistência de Arquivos Meteorológicos
# Objetivo: Verificar padronização antes do processamento em massa
source("scripts/01_import_data_bases.R")
library(tidyverse)
library(purrr)

## FUNÇÕES DE INSPEÇÃO ---------------------------------------------------------

#' Inspeciona a estrutura básica de um arquivo
#' @param path Caminho do arquivo
#' @return Lista com metadados do arquivo
inspecionar_arquivo <- function(path) {
  cat("=", rep("=", 50), "\n", sep = "")
  cat("Arquivo:", basename(path), "\n")
  
  tryCatch({
    # Lê apenas os metadados (primeiras 10 linhas)
    linhas <- read_lines(file.path("data/raw", path), n_max = 15)
    
    # Lê uma amostra pequena para análise de estrutura
    df_amostra <- readr::read_delim(
      file.path("data/raw", path),
      delim = ";",
      escape_double = FALSE,
      locale = readr::locale(decimal_mark = ",", encoding = "ISO-8859-1"),
      trim_ws = TRUE,
      skip = 8,
      n_max = 5,
      show_col_types = FALSE
    )
    
    # Metadados
    info <- list(
      arquivo = basename(path),
      caminho = path,
      n_linhas_metadados = which(!grepl("^[;]", linhas))[1] - 1,
      n_colunas = ncol(df_amostra),
      nomes_colunas = names(df_amostra),
      tipos_colunas = map_chr(df_amostra, ~class(.x)[1]),
      amostra_dados = df_amostra
    )
    
    # Exibe resumo
    cat("Número de colunas:", info$n_colunas, "\n")
    cat("Nomes das colunas:", paste(info$nomes_colunas, collapse = " | "), "\n")
    cat("Tipos das colunas:", paste(info$tipos_colunas, collapse = " | "), "\n")
    cat("Linhas de metadados:", info$n_linhas_metadados, "\n")
    
    return(info)
    
  }, error = function(e) {
    cat("❌ ERRO na leitura:", e$message, "\n")
    return(list(
      arquivo = basename(path),
      erro = e$message
    ))
  })
}

#' Analisa amostras estratégicas dos arquivos
analisar_amostras_estrategicas <- function(arquivos) {
  cat("ANÁLISE DE AMOSTRAS ESTRATÉGICAS\n")
  cat("=", rep("=", 60), "\n", sep = "")
  
  # Seleciona amostras representativas
  amostras <- c(
    arquivos[1],                           # Primeiro arquivo
    arquivos[length(arquivos)],            # Último arquivo  
    arquivos[round(length(arquivos)/2)],   # Arquivo do meio
    arquivos[grep("2020", arquivos)[1]],   # Um arquivo de 2020
    arquivos[grep("PORTO ALERE", arquivos)[1]]  # Uma estação específica
  )
  
  resultados <- map(amostras, inspecionar_arquivo)
  return(resultados)
}

#' Verifica consistência entre todos os arquivos
verificar_consistencia_global <- function(arquivos) {
  cat("\nVERIFICAÇÃO DE CONSISTÊNCIA GLOBAL\n")
  cat("=", rep("=", 60), "\n", sep = "")
  
  # Amostra menor para análise rápida
  arquivos_amostra <- arquivos[seq(1, length(arquivos), by = 10)]  # A cada 10 arquivos
  
  info_arquivos <- map(arquivos_amostra, function(path) {
    tryCatch({
      df <- readr::read_delim(
        file.path("data/raw", path),
        delim = ";",
        escape_double = FALSE, 
        locale = readr::locale(decimal_mark = ",", encoding = "ISO-8859-1"),
        trim_ws = TRUE,
        skip = 8,
        n_max = 2,
        show_col_types = FALSE
      )
      
      return(list(
        arquivo = basename(path),
        n_colunas = ncol(df),
        colunas = names(df)
      ))
    }, error = function(e) {
      return(list(
        arquivo = basename(path),
        erro = e$message
      ))
    })
  })
  
  # Análise de consistência
  n_colunas <- map_dbl(info_arquivos, ~if(!is.null(.x$n_colunas)) .x$n_colunas else NA)
  colunas_unicas <- unique(map(info_arquivos, "colunas"))
  
  cat("Número de arquivos analisados:", length(arquivos_amostra), "\n")
  cat("Variação no número de colunas:", paste(unique(n_colunas), collapse = ", "), "\n")
  cat("Número de estruturas diferentes:", length(colunas_unicas), "\n")
  
  if(length(colunas_unicas) > 1) {
    cat("❌ ESTRUTURAS DIFERENTES ENCONTRADAS!\n")
    walk(colunas_unicas, ~cat("Estrutura:", paste(.x, collapse = " | "), "\n"))
  } else {
    cat("✅ ESTRUTURA CONSISTENTE!\n")
  }
  
  return(info_arquivos)
}

## EXECUÇÃO DA ANÁLISE ---------------------------------------------------------

cat("INICIANDO ANÁLISE DE CONSISTÊNCIA DOS ARQUIVOS\n")
cat("Total de arquivos:", length(arquivos_rs), "\n\n")

### 1. Análise de amostras estratégicas
cat("1. ANALISANDO AMOSTRAS ESTRATÉGICAS...\n")
resultados_amostras <- analisar_amostras_estrategicas(arquivos_rs)

### 2. Verificação de consistência global  
cat("\n2. VERIFICANDO CONSISTÊNCIA GLOBAL...\n")
consistencia_global <- verificar_consistencia_global(arquivos_rs)

### 3. Análise por período temporal
cat("\n3. ANÁLISE POR PERÍODO TEMPORAL...\n")
anos <- unique(str_extract(arquivos_rs, "\\d{4}"))
cat("Anos encontrados:", paste(anos, collapse = ", "), "\n")

### 4. Análise por estação meteorológica
cat("\n4. ANÁLISE POR ESTAÇÃO METEOROLÓGICA...\n")
estacoes <- unique(str_extract(arquivos_rs, "(?<=_RS_)[A-Z]\\d+_[A-Za-z ]+(?=_)"))
cat("Estações encontradas:\n")
walk(estacoes, ~cat("  -", .x, "\n"))

## RELATÓRIO FINAL DA ANÁLISE --------------------------------------------------

cat("\n", rep("=", 70), "\n", sep = "")
cat("RELATÓRIO FINAL DA ANÁLISE DE CONSISTÊNCIA\n")
cat(rep("=", 70), "\n\n")

# Resumo dos problemas encontrados
problemas <- map(resultados_amostras, ~if(!is.null(.x$erro)) .x$erro else NULL) %>% 
  compact()

if(length(problemas) > 0) {
  cat("❌ PROBLEMAS IDENTIFICADOS:\n")
  walk(problemas, ~cat("  -", .x, "\n"))
} else {
  cat("✅ NENHUM PROBLEMA CRÍTICO IDENTIFICADO\n")
}

# Recomendações
cat("\n📋 RECOMENDAÇÕES PARA PRÓXIMOS PASSOS:\n")

if(length(unique(map_dbl(resultados_amostras, "n_colunas"))) > 1) {
  cat("1. 🔸 CRIAR FUNÇÃO DE PADRONIZAÇÃO DE COLUNAS\n")
} else {
  cat("1. ✅ ESTRUTURA CONSISTENTE - PODE PROCESSAR EM LOTE\n")
}

if(any(grepl("ERRO", map_chr(resultados_amostras, ~if(!is.null(.x$erro)) "ERRO" else "OK")))) {
  cat("2. 🔸 IMPLEMENTAR TRATAMENTO DE ERROS ROBUSTO\n")
} else {
  cat("2. ✅ ARQUIVOS ÍNTEGROS - PODE PROCESSAR TODOS\n")
}

cat("3. 📊 VERIFICAR SE TODAS ESTAções TEM MESMO NÚMERO DE COLUNAS\n")
cat("4. 🗓️ CONFIRMAR SE ESTRUTURA É CONSISTENTE AO LONGO DOS ANOS\n")

cat("\nPróximo passo: Execute este script e compartilhe os resultados!\n")