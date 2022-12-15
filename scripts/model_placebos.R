library(openxlsx)
library(did)
library(tidyverse)
library(plm)
library(fixest)

##################################################################################################

#### Esse script roda os diff in diffs placebo do impacto do tratamento em laboratório de ciencias,
#### laboratorio de informatica e acesso a internet. Tambem contem a tabela de estatisticas descritivas
#### com os dados do censo.

##################################################################################################

#### Tratando dados

# Lendo Censo e base de escolas

db_censo_vf <- read.xlsx("data/xlsx-csv/db_censo_vf.xlsx")

db_final <- readRDS("data/RDS/db_escolasf_corr.RDS")

# Join com info de tratamento

db_censo_vf <- db_censo_vf %>% rename(COD_INEP = id_escola)

db_placebos <- left_join(db_censo_vf, db_final)

db_placebos <- db_placebos %>% filter(ano<2017)

db_placebos <- db_placebos %>% mutate(ano_instalacao = ifelse(is.na(ano_instalacao),0,ano_instalacao))

#### Placebos ####

# Lab Ciencias

reg_ciencias <- att_gt(yname = "laboratorio_ciencias",
                     tname = "ano",
                     idname = "COD_INEP",
                     gname = "ano_instalacao",
                     data = db_placebos,
                     xformla = ~1,
                     control_group = "notyettreated")

summary(reg_ciencias)

es_ciencias <- aggte(reg_ciencias, type = "dynamic", na.rm = T)

summary(es_ciencias)

ggdid(es_ciencias)

# Lab Informatica

reg_informatica <- att_gt(yname = "laboratorio_informatica",
                       tname = "ano",
                       idname = "COD_INEP",
                       gname = "ano_instalacao",
                       data = db_placebos,
                       xformla = ~1,
                       control_group = "notyettreated")

summary(reg_informatica)

es_informatica <- aggte(reg_informatica, type = "dynamic", na.rm = T)

summary(es_informatica)

ggdid(es_informatica)

# Internet

reg_internet <- att_gt(yname = "internet",
                    tname = "ano",
                    idname = "COD_INEP",
                    gname = "ano_instalacao",
                    data = db_placebos,
                    xformla = ~1,
                    control_group = "notyettreated")

summary(reg_internet)

es_internet <- aggte(reg_internet, type = "dynamic", na.rm = T)

summary(es_internet)

ggdid(es_internet)


#### Tabela Estatisticas Descritivas Censo ####

db_tabela <- db_censo_vf %>% filter(ano<=2016)

table1::label(db_tabela$laboratorio_informatica) <- "Computer Lab"
table1::label(db_tabela$laboratorio_ciencias) <- "Science Lab"
table1::label(db_tabela$internet) <- "Internet"

table1::table1(~laboratorio_informatica+laboratorio_ciencias+internet, data = db_tabela)

#### Tabela Estat?sticas Descritivas Indicadores INEP #### Nao usado no paper, pois contem todas as redes

ind_inep <- read.csv("data/xlsx-csv/ind_inep.csv")

db_tabela2 <- ind_inep %>% filter(ano<=2016)

table1::label(db_tabela2$tdi_ensino_fund_anos_iniciais) <- "Distor??o Idade-S?rie AI EF"
table1::label(db_tabela2$taxa_aprov_ensino_fund_anos_iniciais) <- "Taxa Aprova??o AI EF"
table1::label(db_tabela2$taxa_reprov_ensino_fund_anos_iniciais) <- "Taxa Reprova??o AI EF"
table1::label(db_tabela2$taxa_aband_ensino_fund_anos_iniciais) <- "Taxa Abandono AI EF"

table1::table1(~tdi_ensino_fund_anos_iniciais+taxa_aprov_ensino_fund_anos_iniciais+taxa_reprov_ensino_fund_anos_iniciais
               +taxa_aband_ensino_fund_anos_iniciais, data = db_tabela2)
