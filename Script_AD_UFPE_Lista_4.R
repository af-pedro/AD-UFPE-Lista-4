#### QUESTAO 1 ####
#### QUESTAO 2 ####

## 1.

if(require(tidyverse) == F) install.packages('tidyverse'); require(tidyverse)
if(require(readxl) == F) install.packages('readxl'); require(readxl)

# definindo diretório e carregando dados
setwd("C:/Users/Pedro/Desktop/Mestrado/Analise de Dados/Material_R/Dados_encontro_2_ufpe/dados_encontro_2_ufpe")
Atlas_2013 <- read_xlsx("atlas2013_dadosbrutos_pt.xlsx", sheet = 2)

head(Atlas_2013)
unique(Atlas_2013$ANO)

# definindo diretório e carregando dados
setwd("C:/Users/Pedro/Desktop/Mestrado/Analise de Dados/Material_R/Dados_encontro_2_ufpe/dados_encontro_2_ufpe")

load("matricula_pe_censo_escolar_2016.RData")
load("docentes_pe_censo_escolar_2016.RData")
load("turmas_pe_censo_escolar_2016.RData")
load("escolas_pe_censo_escolar_2016.RData")

# selecionando dados de 2010 e do Estado de Pernambuco
pnud_pe_2010 <- Atlas_2013 %>% filter(ANO == 2010 & UF == 26)

# removendo base
rm(Atlas_2013) 

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

## 4.


## 5.

## 6.

## 7.

#### QUESTAO 3 ####