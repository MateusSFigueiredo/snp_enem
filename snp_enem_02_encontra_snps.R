# Arquivo: snp_enem_02_encontra_snps.R

# Utiliza Microdados ENEM

# Última atualização: 2024-03-24
# Autor: Mateus Silva Figueiredo

# Objetivo geral: identificar quantos pontos cada acerto contribui para a nota do ENEM

# Método:
# 1 - Carregar dados de Matemática, feitos por snp_enem_01

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
# Carregar dados # pode ser pesado
dados_mt<-fread("dados_MT_1075_2024-03-25-10-06.csv",
                        #nrow=10, # para carregar poucos dados
#                        select = colunas,
                        dec=".",
) # aprox. 10 segundos

# Analisar dados carregados
paste("Existem",sum(rowSums(is.na(dados_mt)) > 0),"linhas com NA")

# ==============================================================================
# Escolher candidatos a ter SNP

# ------------------------------
### Por nota
if(F){
nota_max <- 600
candidatos <- dados_mt[NU_NOTA_MT<=nota_max][NU_NOTA_MT>nota_max-100]
}
# ------------------------------
### Por número de acertos
# Segregar alunos que tenham n e n+1 acertos
dados_mt$NU_ACERTOS %>% table()

# Definir n_acertos
n_acertos <- 42

# Definir candidatos a ter SNP
candidatos<-dados_mt[NU_ACERTOS %in% c(n_acertos,n_acertos+1)]

# ------------------------------
### Simulados arbitrários
if(F){
candidatos <- dados_mt[1:10,]
candidatos
candidatos$TX_ACERTOS <- c("000000000000000000000000000000000000000000000",
                           "000000000000000000000000000000000000000000001",
                           "000000000000000000000000000000000000000000011",
                           "000000000000000000000000000000000000000000111",
                           "000000000000000000000000000000000000000001111",
                           "000000000000000000000000000000000000000011111",
                           "000000000000000000000000000000000000000111111",
                           "000000000000000000000000000000000000001111111",
                           "000000000000000000000000000000000000011111111",
                           "000000000000000000000000000000000000111111111")
}
# ==============================================================================
# Criar dataframe candidatos_snp com várias linhas (num_rows)
# Cada linha contém dois estudantes com um snp de diferença nas respostas
num_rows <- 100 # colocar linhas em excesso
candidatos_snp <- data.frame(
                     NU_INSCRICAO_1   =rep(NA, num_rows),
                     NU_NOTA_MT_1     =rep(NA, num_rows),
                     TX_ACERTOS_1     =rep(NA, num_rows),
                     NU_INSCRICAO_2   =rep(NA, num_rows),
                     NU_NOTA_MT_2     =rep(NA, num_rows),
                     TX_ACERTOS_2     =rep(NA, num_rows),
                     NU_QUESTAO_DIF   =rep(NA, num_rows),
                     NU_DIF_NOTA      =rep(NA, num_rows)
                     )

# ------------------------------------------------------------------------------
texts <- candidatos$TX_ACERTOS

# Preencher dataframe candidatos_snp
# Parece que exige numero exato de linhas

n <- length(texts)
k <- 1

# Preencher maioria das colunas
for (i in 1:(n - 1)) {     # i varia de 1 até penúltimo
  for (j in (i + 1):n) {   # j pega todos os textos depois de i
    # Calcula distância entre texto i e texto j
    # Só preencher colunas se distância for 1
    if (adist(texts[i], texts[j]) == 1) {
      candidatos_snp$NU_INSCRICAO_1[k]    <-as.numeric(candidatos$NU_INSCRICAO[i]) # as numeric para não virar integer64
      candidatos_snp$NU_NOTA_MT_1[k]      <-candidatos$NU_NOTA_MT[i]
      candidatos_snp$TX_ACERTOS_1[k]      <-candidatos$TX_ACERTOS[i]
      candidatos_snp$NU_INSCRICAO_2[k]    <-as.numeric(candidatos$NU_INSCRICAO[j]) # as numeric para não virar integer64
      candidatos_snp$NU_NOTA_MT_2[k]      <-candidatos$NU_NOTA_MT[j]
      candidatos_snp$TX_ACERTOS_2[k]      <-candidatos$TX_ACERTOS[j]
      candidatos_snp$NU_QUESTAO_DIF[k]    <-NA
      candidatos_snp$NU_DIF_NOTA[k]       <-abs(candidatos$NU_NOTA_MT[i] - candidatos$NU_NOTA_MT[j]) # valor absoluto, sem negativo
      k <- k+1
    }
  }
}

# ------------------------------------------------------------------------------
# Número total de linhas, incluindo vazias
candidatos_snp %>% nrow()

# Identificar indice da ultima linha preenchida
which(is.na(candidatos_snp$NU_INSCRICAO_1))[1] # Primeira linha com NA
n_linhas <- which(is.na(candidatos_snp$NU_INSCRICAO_1))[1]-1

# Remover linhas vazias
candidatos_snp <- candidatos_snp[1:n_linhas,]

# ------------------------------------------------------------------------------

# Preencher coluna NU_QUESTAO_DIF
for (linha in 1:nrow(candidatos_snp)){
for (car in 1:45) {
  if ((substr(candidatos_snp$TX_ACERTOS_1[linha], car, car)) != 
      (substr(candidatos_snp$TX_ACERTOS_2[linha], car, car))
    )
     {
    candidatos_snp$NU_QUESTAO_DIF[linha]    <-car  # Return the index of the first differing character
    }
 }
}

 
candidatos_snp

# colunas estão preenchidas

# reordenar por numero da questão
candidatos_snp <- candidatos_snp[order(candidatos_snp$NU_QUESTAO_DIF), ]

candidatos_snp

# ==============================================================================
# Salvar como dataframe independente como candidatos_snp_ n_acertos
eval(parse(text=(paste(
"candidatos_snp_", n_acertos, "<- candidatos_snp",
sep=""))))

# Ver dataframe candidatos_snp_ n_acertos que acabou de ser criado
eval(parse(text=(paste(
  "candidatos_snp_", n_acertos,
  sep=""))))

# ==============================================================================
# Analisar resultados específicos
candidatos_snp_42$NU_DIF_NOTA %>% min()
which(candidatos_snp_42$NU_DIF_NOTA == 0)
candidatos_snp_42[3,]

dados_mt$TX_GABARITO_MT[1]
dados_mt[NU_INSCRICAO==210054864156]
dados_mt[NU_INSCRICAO==210054658434]


dados_mt$NU_INSCRICAO <- as.numeric(dados_mt$NU_INSCRICAO)

# ==============================================================================
# Write DataFrame to a CSV file
if (F) {write.csv(candidatos_snp, "nome_do_arquivo.csv", row.names = FALSE)}
# ==============================================================================
