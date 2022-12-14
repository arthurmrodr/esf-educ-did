library(schoolmath)
library(stargazer)
library(xtable)
library(plm)
library(tidyverse)
library(did)
library(fixest)

##################################################################################################

#### Esse script roda regressoes para estimar o impacto da implementaco da ESF sobre distorcao idade 
#### serie e aprovacao nas escolas municipais. Contem o modelo principal, a robustez na qual o controle
#### consiste das unidades nunca tratadas e o modelo TWFE

##################################################################################################

############ Indicadores INEP ############

# Lendo indicadores INEP

ind_inep <- read.csv("data/xlsx-csv/ind_inep.csv")
ind_inep <- ind_inep %>% rename(COD_INEP = id_escola)

# Filtrando escolas munici?ais

ind_inep <- ind_inep %>% filter(rede == "municipal")

# Criando base a partir do db_final corrigido

db_final <- readRDS("data/RDS/db_escolasf_corr.RDS")

#db_final <- db_final %>% filter(!is.na(ano_instalacao) | (is.na(NOME_EQUIP) & is.na(ano_instalacao)))

db_inep <- left_join(ind_inep, db_final)

#### Callaway e Sant'Anna ####

# Filtrando apenas ate 2016

db_reg <- db_inep %>% filter(ano<2017) 

# Reprovacao

reg_reprov <- att_gt(yname = "taxa_reprov_ensino_fund_anos_iniciais",
             tname = "ano",
             idname = "COD_INEP",
             gname = "ano_instalacao",
             data = db_reg,
             xformla = ~1,
             control_group = "notyettreated")

summary(reg_reprov)

es_reprov <- aggte(reg_reprov, type = "dynamic", na.rm = T)

summary(es_reprov)

ggdid(es_reprov)

# Distorcao Idade-Serie

reg_tdi <- att_gt(yname = "tdi_ensino_fund_anos_iniciais",
                     tname = "ano",
                     idname = "COD_INEP",
                     gname = "ano_instalacao",
                     data = db_reg,
                     xformla = ~1,
                  control_group = "notyettreated")

summary(reg_tdi)

es_tdi <- aggte(reg_tdi, type = "dynamic", na.rm = T)

summary(es_tdi)

ggdid(es_tdi)

#### Robustez - Not treated ####

# Reprovacao

reg_reprov2 <- att_gt(yname = "taxa_reprov_ensino_fund_anos_iniciais",
                     tname = "ano",
                     idname = "COD_INEP",
                     gname = "ano_instalacao",
                     data = db_reg,
                     xformla = ~1,
                     control_group = "nevertreated")

summary(reg_reprov2)

es_reprov2 <- aggte(reg_reprov2, type = "dynamic", na.rm = T)

summary(es_reprov2)

ggdid(es_reprov2)

# Distorcao Idade-Serie

reg_tdi2 <- att_gt(yname = "tdi_ensino_fund_anos_iniciais",
                  tname = "ano",
                  idname = "COD_INEP",
                  gname = "ano_instalacao",
                  data = db_reg,
                  xformla = ~1,
                  control_group = "nevertreated")

summary(reg_tdi2)

es_tdi2 <- aggte(reg_tdi2, type = "dynamic", na.rm = T)

summary(es_tdi2)

ggdid(es_tdi2)

#### Robustez - TWFE ####

db_reg <- db_reg %>% mutate(tratamento = ifelse(ano_instalacao<=2016,1,0))
db_reg <- db_reg %>% mutate(tratamento = ifelse(is.na(tratamento),0,tratamento))
db_reg <- db_reg %>% mutate(ano_instalacao = ifelse(ano_instalacao>2016,NA,ano_instalacao))
db_reg <- db_reg %>% mutate(anos_pra_tratar = ano - ano_instalacao)
db_reg$anos_pra_tratar <- db_reg$anos_pra_tratar %>% replace_na(0)


db_twfe <- pdata.frame(db_reg, index = c("ano", "COD_INEP"))

#Reprov

regrep <- plm(taxa_reprov_ensino_fund_anos_iniciais ~ tratamento, data = db_twfe, model = "within")

summary(regrep)

# TDI

regtdi <- plm(tdi_ensino_fund_anos_iniciais ~ tratamento, data = db_twfe, model = "within")

summary(regtdi)

# Graficos


twfe_tdi <- feols(tdi_ensino_fund_anos_iniciais ~
               # The time-treatment interaction terms
               i(anos_pra_tratar, tratamento, ref=-1)
             # State and year fixed effects
             | COD_INEP + ano, data=db_reg)

coefplot(twfe_tdi)
esttable(twfe_tdi)
confint(twfe_tdi)

iplot(twfe_tdi, 
      xlab = 'Years to treatment',
      main = '')


twfe_reprov <- feols(taxa_reprov_ensino_fund_anos_iniciais ~
                    # The time-treatment interaction terms
                    i(anos_pra_tratar, tratamento, ref=-1)
                  # State and year fixed effects
                  | COD_INEP + ano, data=db_reg)

coefplot(twfe_reprov)
summary(twfe_reprov)
confint(twfe_reprov)

iplot(twfe_reprov, 
      xlab = 'Years to treatment',
      main = '')
