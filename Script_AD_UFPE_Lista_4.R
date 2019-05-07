#### QUESTAO 1 ####
## a.

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

## b.

load("docentes_pe_censo_escolar_2016.RData")
names(docentes_pe)
docentes_pe_selecao <- docentes_pe%>% filter(NU_IDADE > 17, NU_IDADE < 71)
dim(docentes_pe_selecao)
head(docentes_pe_selecao)
summary(docentes_pe_selecao$NU_IDADE)

## c.

load("matricula_pe_censo_escolar_2016.RData")
names(matricula_pe)
matricula_pe_selecao <- matricula_pe %>% filter(NU_IDADE > 0 , NU_IDADE < 26)
dim(matricula_pe_selecao)
head(matricula_pe_selecao)
summary(matricula_pe_selecao$NU_IDADE)

## d.

  # Matriculas
matriculas_pe_sel <- matricula_pe_selecao %>% group_by(CO_MUNICIPIO) %>%
  summarise(n_matriculas = n(),
            alunos_media_idade = mean(NU_IDADE),
            alunos_fem_sx = sum(TP_SEXO == 2, na.rm = T),
            alunos_negros = sum(TP_COR_RACA %in% c(2, 3), na.rm = T),
            alunos_indigenas = sum(TP_COR_RACA == 5, na.rm = T),
            alunos_cor_nd = sum(TP_COR_RACA == 0, na.rm = T),
            matriculas_educ_inf = sum(TP_ETAPA_ENSINO %in% c(1, 2), na.rm = T),
            matriculas_educ_fund = sum(TP_ETAPA_ENSINO %in% c(4:21, 41), na.rm = T),
            matriculas_educ_medio = sum(TP_ETAPA_ENSINO %in% c(25:38), na.rm = T)
  )

  # verificacao
dim(matriculas_pe_sel)[1] == length(unique(matricula_pe$CO_MUNICIPIO))
summary(matriculas_pe_sel)

  # Docentes
docentes_pe_sel <- docentes_pe_selecao %>% group_by(CO_MUNICIPIO) %>%
  summarise(n_docentes = n(),
            docentes_media_idade = mean(NU_IDADE),
            docentes_fem_sx = sum(TP_SEXO == 2, na.rm = T),
            docentes_superior = sum(TP_ESCOLARIDADE == 4, na.rm = T),
            docentes_contrato = sum(TP_TIPO_CONTRATACAO %in% c(1, 4), na.rm = T)
  )

  # verificacao
dim(docentes_pe_sel)[1] == length(unique(docentes_pe$CO_MUNICIPIO))
summary(docentes_pe_sel)

  # matriculas
docentes_matriculas_pe_sel <- docentes_pe_sel %>% full_join(matriculas_pe_sel,
                                                by = c("CO_MUNICIPIO" = "CO_MUNICIPIO")
)
dim(pnud_pe_2010)
dim(matriculas_pe_sel)
dim(docentes_matriculas_pe_sel)
names(docentes_matriculas_pe_sel)

  # Média Aritmética / Proporção de docentes 
docentes_matriculas_pe_sel$n_matriculas/docentes_matriculas_pe_sel$n_docentes

  # Grafico
library(ggplot2)
ggplot(docentes_matriculas_pe_sel, aes(n_matriculas, n_docentes)) + geom_point()

  # Mediana
median(docentes_matriculas_pe_sel$n_matriculas/median(docentes_matriculas_pe_sel$n_docentes))

## e.

  # Criando nova tabela de dados com IDHM e Matriculas e Docentes
censo_pnud_pe_sel <- docentes_matriculas_pe_sel %>% full_join(pnud_pe_2010,
                                                by = c( "CO_MUNICIPIO" = "Codmun7")
)

dim(pnud_pe_2010)
dim(matriculas_pe_sel)
dim(censo_pnud_pe_sel)
names(censo_pnud_pe_sel)

  # Juntando base de dados do PNUD e Alunos por docente

censo_pnud_pe_sel <-pnud_pe_2010%>%full_join(matriculas_pe_sel,by = c("IDHM"="CO_MUNICIPIO"))

  # Identificando a maior média
summary(docentes_matriculas_pe_sel$n_matriculas/docentes_matriculas_pe_sel$n_docentes)

  # Analisando o código da maior média
docentes_matriculas_pe_sel$n_matriculas/docentes_matriculas_pe_sel$n_docentes

  # Analisando o que há na linha 177 (de maior média)
censo_pnud_pe_sel["177", ]

  # O código 177 (de maior média) é Tupatininga que tem como IDHM o valor de 0.519

## f.

  # Identificando a maior media
summary(docentes_matriculas_pe_sel$n_matriculas/docentes_matriculas_pe_sel$n_docentes)

  # salvando nova base
DocAlu <- (docentes_matriculas_pe_sel$n_matriculas/docentes_matriculas_pe_sel$n_docentes)
save(DocAlu, file = "DocAlu.RData")
write.csv2(DocAlu, file = "DocAlu.csv",
           row.names = F)

  # Carregando base de dados 
setwd("C:/Users/Pedro/Desktop/Mestrado/Analise de Dados/Material_R/Dados_encontro_2_ufpe/dados_encontro_2_ufpe")
load("DocAlu.RData")

  # Fazendo mutate
censo_pnud_pe_sel_docalu <- censo_pnud_pe_sel %>% mutate(DocAlu)
names(censo_pnud_pe_sel_docalu)

  # Correlacao
cor(censo_pnud_pe_sel_docalu$IDHM, censo_pnud_pe_sel_docalu$DocAlu)

  # teste de correlacao
cor.test(censo_pnud_pe_sel_docalu$IDHM, censo_pnud_pe_sel_docalu$DocAlu)

  # Grafico
ggplot(censo_pnud_pe_sel_docalu, aes(IDHM, DocAlu)) + geom_point()

## g.

  # Salvando nova base
save(censo_pnud_pe_sel_docalu, file = "censo_pnud_pe_sel_docalu.RData")
write.csv2(censo_pnud_pe_sel_docalu, file = "censo_pnud_pe_sel_docalu.csv",
           row.names = F)

#### QUESTAO 3 ####

  # Grafico
ggplot(censo_pnud_pe_sel_docalu, aes(IDHM, DocAlu = "Alunos por docente")) + geom_point()

