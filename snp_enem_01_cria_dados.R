# Arquivo: snp_enem_01_cria_dados.R

# Utiliza Microdados ENEM

# Última atualização: 2024-03-24
# Autor: Mateus Silva Figueiredo

# Objetivo geral: identificar quantos pontos cada acerto contribui para a nota do ENEM

# Ações:
# 1 - Carregar dados ENEM
# 2 - Cria colunas NU_ACERTOS (número de acertos) e TX_ACERTOS (padrão de acertos)
# Essa etapa demora horas para preencher as 600 mil linhas.
# 3 - Salva como csv

# Por enquanto, só de Matemática
# Bastante lento, levou aprox. 12 horas para 600 mil linhas de dados.

# ==============================================================================

# Página com todos os microdados: https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/microdados/enem
# Microdados ENEM 2022 para download: https://download.inep.gov.br/microdados/microdados_enem_2022.zip

# ==============================================================================
# Preparação
library(tidyverse)
library(data.table) # carrega fread

setwd("C:/Users/Mateus/Desktop/R/snp_enem")
getwd()
list.files()

# Para manter o Número de Inscrição inalterado
options(scipen = 999, digits = 10)

# ==============================================================================
# Definir colunas de interesse
# separado por disciplinas

colunas <- c("NU_INSCRICAO",
#             "TP_PRESENCA_CN","CO_PROVA_CN","NU_NOTA_CN","TX_RESPOSTAS_CN","TX_GABARITO_CN",
#             "TP_PRESENCA_CH","CO_PROVA_CH","NU_NOTA_CH","TX_RESPOSTAS_CH","TX_GABARITO_CH",
#             "TP_PRESENCA_LC","CO_PROVA_LC","NU_NOTA_LC","TX_RESPOSTAS_LC","TX_GABARITO_LC","TP_LINGUA",
             "TP_PRESENCA_MT","CO_PROVA_MT","NU_NOTA_MT","TX_RESPOSTAS_MT","TX_GABARITO_MT",
#             "NU_NOTA_REDACAO",
             "TP_ST_CONCLUSAO"
             )

# Carregar dados # pode ser pesado
dados_enem_fread<-fread("MICRODADOS_ENEM_2022.csv",
                        #nrow=10, # para carregar poucos dados
                        select = colunas,
                        dec=".",
) # aprox. 10 segundos

# ==============================================================================
# Rodar para Matemática apenas se existir coluna "TP_PRESENCA_MT"
if("TP_PRESENCA_MT" %in% colnames(dados_enem_fread)){

# Apenas presentes em Matemática

dados_enem_fread %>% nrow() # 3476105
dados_enem <- dados_enem_fread[TP_PRESENCA_MT==1] # apenas presentes em MT. 2355395 linhas

dados_enem$TP_PRESENCA_MT %>% table()

dados_enem$CO_PROVA_MT %>% table()
# CO_PROVA_MT mais comum = 1075 = Prova Azul

# Selecionar apenas prova Azul de Matemática
dados <- dados_enem[CO_PROVA_MT == 1075]

# ------------------------------------------------------------------------------
# Limpeza
if(T){rm(dados_enem_fread,dados_enem)}

# ==============================================================================
# Criar coluna NU_ACERTOS em dados_nota
# dados$NU_ACERTOS <- NA

# Criar coluna TX_ACERTOS em dados_nota
# dados$TX_ACERTOS <- NA
# ------------------------------------------------------------------------------
# Cria gabarito_split. Funciona pois gabarito é igual para todas as linhas.
gabarito_split <- unlist(strsplit(dados$TX_GABARITO_MT[i],  split = ""))

# ------------------------------------------------------------------------------
# Carregar dados de matemática, para continuar preenchimento
if(T){
  
  dados_mt<-fread("dados_MT_1075_2024-03-24-14-44.csv",
                  #nrow=10, # para carregar poucos dados
                  #                        select = colunas,
                  dec=".",
  ) # aprox. 10 segundos
  dados <- dados_mt
}

j <- which(is.na(dados$NU_ACERTOS))[1] # Primeira linha com NA


# ------------------------------------------------------------------------------
# i <- 124502 # para linha específica do loop
# i = linha de dados a ser preenchida
# j = linha inicial para o loop

# Começa loop para preencher NU_ACERTOS e TX_ACERTOS  
start_time<-Sys.time(); for (i in j:nrow(dados)) { # começa loop

vetor <- unlist(strsplit(dados$TX_RESPOSTAS_MT[i],  split = "")) == gabarito_split
  
# preenche NU_ACERTOS com número de acertos (0 a 45)
dados$NU_ACERTOS[i] <- sum(vetor) # opção 1: sum vetor

# preenche TX_ACERTOS com padrão de respostas (ex. 000100010000000001000000010000000000000010000)
# 45 caracteres. 0 representa erro, 1 representa acerto.
dados$TX_ACERTOS[i] <- paste0(ifelse(vetor == "TRUE", "1", "0"), collapse = "")
};  end_time<-Sys.time(); end_time-start_time; j<-i
j<-i

# ------------------------------------------------------------------------------
# conferir preenchimento

which(is.na(dados$NU_ACERTOS))[1] # Primeira linha com NA
paste("j =",j)
paste("Existem",sum(rowSums(is.na(dados)) > 0),"linhas com NA")

table(dados$NU_ACERTOS, useNA = "ifany") # Tabela de frequência dos acertos, com NA

# ==============================================================================
start_time<-Sys.time()


# ==============================================================================
# Write DataFrame to a CSV file

tempo_atual <- format(Sys.time(), "%Y-%m-%d-%H-%M");
nome_arquivo <- paste0("dados_MT_1075_",tempo_atual,".csv")

if (T) {write.csv(dados, nome_arquivo, row.names = FALSE)} # salvar com data e hora
if (T) {write.csv(dados, "dados_MT_1075.csv", row.names = FALSE)} # salvar e sobrescrever

# ==============================================================================
} # encerra Matemática
