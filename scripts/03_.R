# Script de Processamento Robusto de Dados Meteorológicos
# Versão Corrigida Baseada na Análise de Consistência

library(tidyverse)
library(lubridate)
library(purrr)

## CONFIGURAÇÃO ----------------------------------------------------------------

ANO_INICIO <- 2015
ANO_FIM <- 2025

# Estações estratégicas para análise
ESTACOES_ESTRATEGICAS <- c(
  "A801", "A803", "A839", "A802"
)

## FUNÇÃO PRINCIPAL CORRIGIDA --------------------------------------------------

processar_arquivo_meteo_robusto <- function(path) {
  cat("Processando:", basename(path), "\n")
  
  tryCatch({
    # Lê o arquivo com encoding específico
    df <- readr::read_delim(
      file.path("data/raw", path),
      delim = ";",
      escape_double = FALSE,
      locale = readr::locale(
        decimal_mark = ",", 
        encoding = "ISO-8859-1"  # ou "Windows-1252"
      ),
      trim_ws = TRUE,
      skip = 8,
      show_col_types = FALSE,
      guess_max = 1000  # Melhora detecção de tipos
    )
    
    # Remove última coluna vazia (problema identificado)
    if (ncol(df) == 20 && names(df)[20] == "...20") {
      df <- df[, 1:19]
    }
    
    # Extrai informações do nome do arquivo
    nome_arquivo <- basename(path)
    info <- strsplit(nome_arquivo, "_")[[1]]
    
    # Processamento básico com correções
    df_processado <- df %>%
      janitor::clean_names() %>%
      mutate(
        # Converte data
        data = as.Date(data, format = "%d/%m/%Y"),
        ano = year(data),
        mes = month(data),
        dia = day(data),
        # Metadados
        estacao_codigo = info[3],
        estacao_nome = info[4],
        estacao_completa = paste(info[3], info[4], sep = "_"),
        ano_arquivo = as.numeric(info[2]),
        .before = 1
      ) %>%
      # Filtra período de interesse
      filter(ano >= ANO_INICIO & ano <= ANO_FIM) %>%
      # Seleciona e renomeia colunas principais
      select(
        data, ano, mes, dia, estacao_codigo, estacao_nome, estacao_completa,
        # Mapeamento das colunas baseado na análise
        precipitacao = matches("precipitacao_total"),
        pressao = matches("pressao_atmosferica_ao_nivel_da_estacao"),
        radiacao = matches("radiacao_global"),
        temperatura = matches("temperatura_do_ar_bulbo_seco"),
        temperatura_orvalho = matches("temperatura_do_ponto_de_orvalho"),
        umidade = matches("umidade_relativa_do_ar_horaria"),
        vento_direcao = matches("vento_direcao_horaria"),
        vento_rajada = matches("vento_rajada_maxima"),
        vento_velocidade = matches("vento_velocidade_horaria")
      ) %>%
      # Converte para numérico (trata casos onde colunas estão vazias)
      mutate(across(
        c(precipitacao, pressao, radiacao, temperatura, temperatura_orvalho,
          umidade, vento_direcao, vento_rajada, vento_velocidade),
        ~ {
          # Converte para caractere, substitui vírgulas, depois para numérico
          num_val <- as.numeric(gsub(",", ".", as.character(.x)))
          # Se todos forem NA, retorna NA, senão retorna os valores convertidos
          if (all(is.na(num_val))) NA_real_ else num_val
        }
      ))
    
    return(df_processado)
    
  }, error = function(e) {
    warning("Falha no arquivo ", basename(path), ": ", e$message)
    return(NULL)
  })
}

## FUNÇÃO DE AGRAGAÇÃO MENSAL -------------------------------------------------

agregar_dados_mensais <- function(dados_meteo) {
  dados_meteo %>%
    group_by(ano, mes, estacao_completa, estacao_nome) %>%
    summarise(
      # Temperatura
      temp_media = mean(temperatura, na.rm = TRUE),
      temp_max_media = mean(temperatura, na.rm = TRUE),  # Ajustar se tiver coluna específica
      temp_min_media = mean(temperatura, na.rm = TRUE),  # Ajustar se tiver coluna específica
      
      # Precipitação
      precipitacao_total = sum(precipitacao, na.rm = TRUE),
      
      # Umidade
      umidade_media = mean(umidade, na.rm = TRUE),
      
      # Vento
      vento_velocidade_media = mean(vento_velocidade, na.rm = TRUE),
      
      # Radiação
      radiacao_media = mean(radiacao, na.rm = TRUE),
      
      # Contagem de observações válidas
      n_observacoes = n(),
      n_temperatura_valida = sum(!is.na(temperatura)),
      
      .groups = 'drop'
    ) %>%
    # Calcula graus-dia para análise energética
    mutate(
      graus_dia_aquecimento = pmax(18 - temp_media, 0),
      graus_dia_resfriamento = pmax(temp_media - 21, 0)
    )
}

## PROCESSAMENTO PRINCIPAL ----------------------------------------------------

cat("INICIANDO PROCESSAMENTO ROBUSTO\n")
cat("Arquivos totais:", length(arquivos_rs), "\n")

### 1. Filtra arquivos válidos
arquivos_validos <- arquivos_rs[!is.na(arquivos_rs) & file.exists(file.path("data/raw", arquivos_rs))]
cat("Arquivos válidos:", length(arquivos_validos), "\n")

### 2. Processa em lote com tratamento de erros
cat("\nProcessando arquivos...\n")
dados_meteo_crus <- map(arquivos_validos, processar_arquivo_meteo_robusto)

### 3. Remove elementos nulos (arquivos com erro)
dados_meteo_filtrados <- keep(dados_meteo_crus, ~!is.null(.x))

### 4. Combina todos os dados
dados_meteo_combinados <- bind_rows(dados_meteo_filtrados)

cat("✅ Dados combinados:", nrow(dados_meteo_combinados), "linhas\n")
cat("✅ Estações encontradas:", n_distinct(dados_meteo_combinados$estacao_completa), "\n")

### 5. Filtra estações estratégicas
dados_estacoes_estrategicas <- dados_meteo_combinados %>%
  filter(estacao_nome %in% ESTACOES_ESTRATEGICAS)

### 6. Agrega para nível mensal
dados_mensais <- agregar_dados_mensais(dados_estacoes_estrategicas)

### 7. Cria média para o RS
dados_rs_mensal <- dados_mensais %>%
  group_by(ano, mes) %>%
  summarise(
    temp_media_rs = mean(temp_media, na.rm = TRUE),
    umidade_media_rs = mean(umidade_media, na.rm = TRUE),
    graus_dia_aquecimento_rs = mean(graus_dia_aquecimento, na.rm = TRUE),
    graus_dia_resfriamento_rs = mean(graus_dia_resfriamento, na.rm = TRUE),
    precipitacao_total_rs = mean(precipitacao_total, na.rm = TRUE),
    n_estacoes = n_distinct(estacao_completa),
    .groups = 'drop'
  )

## SALVAMENTO ------------------------------------------------------------------

cat("\nSalvando resultados...\n")

write_csv(dados_rs_mensal, "data/processed/dados_meteo_rs_mensal.csv")
write_csv(dados_mensais, "data/processed/dados_estacoes_mensais.csv")
saveRDS(dados_rs_mensal, "data/processed/dados_meteo_rs_mensal.rds")

## RELATÓRIO FINAL -------------------------------------------------------------

cat("\n" + rep("=", 60) + "\n")
cat("PROCESSAMENTO CONCLUÍDO\n")
cat(rep("=", 60) + "\n")

cat("Período processado:", ANO_INICIO, "-", ANO_FIM, "\n")
cat("Total de observações horárias:", nrow(dados_meteo_combinados), "\n")
cat("Estações processadas:", n_distinct(dados_meteo_combinados$estacao_completa), "\n")
cat("Estações estratégicas utilizadas:", nrow(dados_mensais), "combinações mês-estacao\n")
cat("Arquivo final salvo: data/processed/dados_meteo_rs_mensal.csv\n")

cat("\nPróximo passo: Integrar com dados de energia para análise CEP!\n")