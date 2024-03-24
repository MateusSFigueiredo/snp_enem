# Arquivo: snp_enem_01.R

# Utiliza Microdados ENEM

# Última atualização: 2024-03-17
# Autor: Mateus Silva Figueiredo

# Objetivo geral: identificar quantos pontos cada acerto contribui para a nota do ENEM

# Método:
# 1 - Carregar dados ENEM
# 2 - Comparar respostas com gabarito para cada estudante

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
)
# ==============================================================================
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

# Fazer coluna número de acertos

# ==============================================================================
# Criar coluna NU_ACERTOS em dados_nota
for (i in 1:nrow(dados)){
  sum (unlist(strsplit(dados$TX_GABARITO_MT[i],  split = "")) == 
         unlist(strsplit(dados$TX_RESPOSTAS_MT[i], split = ""))) ->
    dados$NU_ACERTOS[i]
}

dados$NU_ACERTOS %>% table()

# ==============================================================================
# Colocar "TX_RESPOSTAS_MT" em ordem alfabética

dados <- dados[order(dados$TX_RESPOSTAS_MT), ]

# ==============================================================================
# Encontrar pares de estudantes com SNP
# Ideia: usar ordem alfabética

dados$TX_RESPOSTAS_MT

# adist() function calculates the Levenshtein distance between strings

# cria vetor candidatos a ter SNP
candidatos<-c(0)

linha<-1
sucessos<-3

candidatos[sucessos]<-as.numeric(dados$NU_INSCRICAO[linha])
candidatos

if (adist(dados$TX_RESPOSTAS_MT[linha],dados$TX_RESPOSTAS_MT[linha+1]) == 1){
  
}

# ==============================================================================
# Encontrar pares de estudantes com SNP
# Ideia: segregar candidatos por grupos de nota

# 900 < NU_NOTA_MT < 100
dados_nota <- dados[NU_NOTA_MT<=1000][NU_NOTA_MT>900]
dados_nota %>% nrow()
texts <- dados_nota$TX_RESPOSTAS_MT


# ==============================================================================
# ===========================         LIXO       ===============================
# ==============================================================================
# ==============================================================================
texts <- dados$TX_RESPOSTAS_MT[1:10]
texts <- c(texts, "CDADDBCBCDCCBECDBDBBEAEBEACAAEBACBAEADAEEACBC")

# Só usar com poucos textos

# Function to find texts differing by one character
find_one_char_diff <- function(texts) {
  n <- length(texts)
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      dist <- adist(texts[i], texts[j])
 #     cat("Distance between", texts[i], "and", texts[j], ":", dist, "\n")
      if (dist == 1) {
        cat("Texts differing by one character:", texts[i], "and", texts[j], "\n")
      }
    }
  }
}

# Call the function
find_one_char_diff(texts)

# ============

dados_nota[TX_RESPOSTAS_MT=="EAAABBBDCDECDBDCBCCBCECBAACDBAAACEBABDDEDAEBE"]$NU_NOTA_MT
dados_nota[TX_RESPOSTAS_MT=="ECAABBBDCDECDBDCBCCBCECBAACDBAAACEBABDDEDAEBE"]$NU_NOTA_MT

# =======================
# Qual questão tem SNP?

# Cria função compare_texts
compare_texts <- function(text1, text2) {
  # Find the minimum length of the two texts
  min_length <- min(nchar(text1), nchar(text2))
  
  # Iterate through each character and find the first differing character
  for (i in 1:min_length) {
    if (substr(text1, i, i) != substr(text2, i, i)) {
      return(i)  # Return the index of the first differing character
    }
  }
  
  # If no difference is found in the common length, check if one text is longer
  if (nchar(text1) != nchar(text2)) {
    return(min_length + 1)  # Return the index after the end of the shorter text
  }
  
  # Return -1 if the texts are identical
  return(-1)
}

# Roda função
compare_texts("EAAABBBDCDECDBDCBCCBCECBAACDBAAACEBABDDEDAEBE",
              "ECAABBBDCDECDBDCBCCBCECBAACDBAAACEBABDDEDAEBE")



dados_nota$NU_NOTA_MT %>% max()
dados_nota[NU_NOTA_MT==dados_nota$NU_NOTA_MT %>% max()]



i<-100
sum(unlist(strsplit(dados$TX_RESPOSTAS_MT[i],  split = "")) == unlist(strsplit(dados$TX_GABARITO_MT[i],  split = "")))


# ==============================================================================
# Fazer coluna para acertos e erros

vetor <- unlist(strsplit(dados$TX_RESPOSTAS_MT[1000],  split = "")) == unlist(strsplit(dados$TX_GABARITO_MT[1000],  split = ""))
vetor
vetor %>% sum()

result <- paste0(ifelse(vetor == "TRUE", "1", "0"), collapse = "")
result
sum(result)
