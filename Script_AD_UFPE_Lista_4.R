#### QUESTAO 1 ####
#### QUESTAO 2 ####

## 1.

install.packages("readxl")
library(readxl)
install.packages("dplyr")
library(dplyr)

setwd("C:/Users/Pedro/Desktop/Mestrado/Analise de Dados/Material_R/Dados_encontro_2_ufpe/dados_encontro_2_ufpe")
Atlas_2013 <- read_xlsx("atlas2013_dadosbrutos_pt.xlsx")

head(Atlas_2013)
print(Atlas_2013)
summary(Atlas_2013)
dim(Atlas_2013)
str(docentes_pe)

## 2.

load("docentes_pe_censo_escolar_2016.RData")
names(docentes_pe)
docentes_pe_selecao <- docentes_pe%>% filter(NU_IDADE > 18, NU_IDADE < 70)
dim(docentes_pe_selecao)
head(docentes_pe_selecao)

## 3.

load("matricula_pe_censo_escolar_2016.RData")
names(matricula_pe)
matricula_pe_selecao <- matricula_pe%>% filter(NU_IDADE > 1 , NU_IDADE < 25)
dim(matricula_pe_selecao)
head(matricula_pe_selecao)


#### QUESTAO 3 ####