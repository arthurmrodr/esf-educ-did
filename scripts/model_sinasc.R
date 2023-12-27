library(tidyverse)
library(stargazer)
library(fixest)

##################################################################################################

#### Esse script roda as regressoes iniciais de mecanismo de transmissao, mas esta desatualizado.
#### Para a versao mais recente, ver 'model_sinasc_new.R'.

##################################################################################################


db_reg <- readRDS("data/RDS/db_regnasc.RDS") # Não vai funcionar pq a base é grande demias pro github, precisa fazer manualmente
db_reg <- db_micro

#### Regressoes com a base completa ####

# PESO

#regpeso <- lm( PESO ~ trat_grav + as.character(ANOGRAVIDEZ), data = db_reg)
regpeso <- feols(PESO ~ trat_grav, fixef = c("ANOGRAVIDEZ"), data = db_reg)
summary(regpeso)

#regpesolog <- lm(log(PESO) ~ trat_grav + ESCMAE + IDADEMAE + GESTACAO + RACACOR, data = db_reg)
#summary(regpesolog)

#regpesotrim <- lm( PESO ~ trat_grav + trat_segtri + trat_tertri + as.character(ANOGRAVIDEZ), data = db_reg)
regpesotrim <- feols(PESO ~ trat_grav + trat_segtri + trat_tertri, fixef = c("ANOGRAVIDEZ"), data = db_reg)
summary(regpesotrim)

# ZERO CONSULTAS PRE NATAIS

#regpren <- lm( ZEROCONSULTAS ~ trat_grav + as.character(ANOGRAVIDEZ), data = db_reg)
regpren <- feols( ZEROCONSULTAS ~ trat_grav , fixef = c("ANOGRAVIDEZ"), data = db_reg)
summary(regpren)

#regprentrim <- lm( ZEROCONSULTAS ~ trat_grav + trat_segtri + trat_tertri + as.character(ANOGRAVIDEZ), data = db_reg)
regprentrim <- feols( ZEROCONSULTAS ~ trat_grav + trat_segtri + trat_tertri , fixef = c("ANOGRAVIDEZ"), data = db_reg, vcov = "iid")
summary(regprentrim)

# APGAR 1

regapgar1 <- lm( APGAR1 ~ trat_grav + as.character(ANOGRAVIDEZ), data = db_reg)
summary(regapgar1)

regapgar1trim <- lm( APGAR1 ~ trat_grav + trat_segtri + trat_tertri + as.character(ANOGRAVIDEZ), data = db_reg)
summary(regapgar1trim)

# APGAR 5

regapgar5 <- lm( APGAR5 ~ trat_grav + as.character(ANOGRAVIDEZ), data = db_reg)
summary(regapgar5)

regapgar5trim <- lm( APGAR5 ~ trat_grav + trat_segtri + trat_tertri + as.character(ANOGRAVIDEZ), data = db_reg)
summary(regapgar5trim)

#### Regressoes filtro mães pretas e pardas####

db_microsolo <- db_reg %>% filter(RACACOR != "Branca")

# PESO

regpesosolo <- lm( PESO ~ trat_grav + as.character(ANOGRAVIDEZ), data = db_microsolo)
summary(regpesosolo)


regpesotrimsolo <- lm( PESO ~ trat_grav + trat_segtri + trat_tertri  + as.character(ANOGRAVIDEZ) , data = db_microsolo)
summary(regpesotrimsolo)

# ZERO CONSULTAS PRENATAIS

regprensolo <- lm( ZEROCONSULTAS ~ trat_grav + as.character(ANOGRAVIDEZ), data = db_microsolo)
summary(regprensolo)

regprentrim1solo <- lm( ZEROCONSULTAS ~ trat_grav + trat_segtri + trat_tertri  + as.character(ANOGRAVIDEZ) , data = db_microsolo)
summary(regprentrim1solo)

# APGAR 1

regapgar1solo <- lm( APGAR1 ~ trat_grav + as.character(ANOGRAVIDEZ), data = db_microsolo)
summary(regapgar1solo)

regapgar1trimsolo <- lm( APGAR1 ~ trat_grav + trat_segtri + trat_tertri + as.character(ANOGRAVIDEZ) , data = db_microsolo)
summary(regapgar1trimsolo)

# APGAR 5

regapgar5solo <- lm( APGAR5 ~ trat_grav + as.character(ANOGRAVIDEZ), data = db_microsolo)
summary(regapgar5solo)

regapgar5trimsolo <- lm( APGAR5 ~ trat_grav + trat_segtri + trat_tertri + as.character(ANOGRAVIDEZ) , data = db_microsolo)
summary(regapgar5trimsolo)

#### Tabelas de Resultados

stargazer(regpeso, regpesotrim, regpren, regprentrim)
stargazer(regpesosolo, regpesotrimsolo, regprensolo, regprentrim1solo)

#### Regressoes com efeitos fixos de neighborhood ####

### Base Completa

# Peso

regpeso_fe <- lm( PESO ~ trat_grav + as.character(ANOGRAVIDEZ) + as.character(CODBAIRES), data = db_reg)
regpeso_fe <- feols(PESO ~ trat_grav , fixef = c("ANOGRAVIDEZ", "CODBAIRES"), data = db_reg, vcov = "iid")
summary(regpeso_fe)

regpesotrim_fe <- lm( PESO ~ trat_grav + trat_segtri + trat_tertri + as.character(ANOGRAVIDEZ) + as.character(CODBAIRES), data = db_reg)
regpesotrim_fe <- feols( PESO ~ trat_grav + trat_segtri + trat_tertri , fixef = c("ANOGRAVIDEZ", "CODBAIRES"), data = db_reg, vcov = "iid")
summary(regpesotrim_fe)

# ZERO CONSULTAS PRE NATAIS

regzero_fe <- lm( ZEROCONSULTAS ~ trat_grav + as.character(ANOGRAVIDEZ) + as.character(CODBAIRES), data = db_reg)
regzero_fe <- feols(ZEROCONSULTAS ~ trat_grav , fixef = c("ANOGRAVIDEZ", "CODBAIRES"), data = db_reg, vcov = "iid")
summary(regzero_fe)

regzerotrim_fe <- lm( ZEROCONSULTAS ~ trat_grav + trat_segtri + trat_tertri + as.character(ANOGRAVIDEZ) + as.character(CODBAIRES), data = db_reg)
regzerotrim_fe <- feols(ZEROCONSULTAS ~ trat_grav + trat_segtri + trat_tertri, fixef = c("ANOGRAVIDEZ", "CODBAIRES"), data = db_reg, vcov = "iid")
summary(regzerotrim_fe)

# APGAR 1

regapgar1_fe <- lm( APGAR1 ~ trat_grav + as.character(ANOGRAVIDEZ) + as.character(CODBAIRES), data = db_reg)
regapgar1_fe <- feols(APGAR1 ~ trat_grav , fixef = c("ANOGRAVIDEZ", "CODBAIRES"), data = db_reg, vcov = "iid")
summary(regapgar1_fe)

regapgar1trim_fe <- lm( APGAR1 ~ trat_grav + trat_segtri + trat_tertri + as.character(ANOGRAVIDEZ) + as.character(CODBAIRES), data = db_reg)
regapgar1_fe <- feols( APGAR1 ~ trat_grav + trat_segtri + trat_tertri , fixef = c("ANOGRAVIDEZ", "CODBAIRES"), data = db_reg, vcov = "iid")
summary(regzerotrim_fe)

# APGAR 5

regapgar5_fe <- lm( APGAR5 ~ trat_grav + as.character(ANOGRAVIDEZ) + as.character(CODBAIRES), data = db_reg)
regapgar5_fe <- feols( APGAR5 ~ trat_grav , fixef = c("ANOGRAVIDEZ", "CODBAIRES"), data = db_reg, vcov = "iid")
summary(regapgar5_fe)

regapgar5trim_fe <- lm( APGAR5 ~ trat_grav + trat_segtri + trat_tertri + as.character(ANOGRAVIDEZ) + as.character(CODBAIRES), data = db_reg)
regapgar5trim_fe <- feols( APGAR5 ~ trat_grav + trat_segtri + trat_tertri , fixef = c("ANOGRAVIDEZ", "CODBAIRES"), data = db_reg, vcov = "iid")
summary(regapgar5trim_fe)

### Base Restrita

# PESO

regpesosolo_fe <- lm( PESO ~ trat_grav + as.character(ANOGRAVIDEZ) + as.character(CODBAIRES), data = db_microsolo)
regpesosolo_fe <- feols( PESO ~ trat_grav , fixef = c("ANOGRAVIDEZ", "CODBAIRES"), data = db_microsolo, vcov = "iid")
summary(regpesosolo_fe)


regpesotrimsolo_fe <- lm( PESO ~ trat_grav + trat_segtri + trat_tertri  + as.character(ANOGRAVIDEZ) + as.character(CODBAIRES), data = db_microsolo)
regpesotrimsolo_fe <- feols( PESO ~ trat_grav + trat_segtri + trat_tertri  , fixef = c("ANOGRAVIDEZ", "CODBAIRES"), data = db_microsolo, vcov = "iid")
summary(regpesotrimsolo_fe)

# Zero Consultas

regprensolo_fe <- lm( ZEROCONSULTAS ~ trat_grav + as.character(ANOGRAVIDEZ)+ as.character(CODBAIRES), data = db_microsolo)
regprensolo_fe <- feols( ZEROCONSULTAS ~ trat_grav , fixef = c("ANOGRAVIDEZ", "CODBAIRES"), data = db_microsolo, vcov = "iid")
summary(regprensolo_fe)

regprentrimsolo_fe <- lm( ZEROCONSULTAS ~ trat_grav + trat_segtri + trat_tertri  + as.character(ANOGRAVIDEZ)+ as.character(CODBAIRES) , data = db_microsolo)
regprentrimsolo_fe <- feols( ZEROCONSULTAS ~ trat_grav + trat_segtri + trat_tertri , fixef = c("ANOGRAVIDEZ", "CODBAIRES"), data = db_microsolo, vcov = "iid")
summary(regprentrimsolo_fe)

# APGAR 1

regapgar1solo_fe <- lm( APGAR1 ~ trat_grav + as.character(ANOGRAVIDEZ)+ as.character(CODBAIRES), data = db_microsolo)
regapgar1solo_fe <- feols(APGAR1 ~ trat_grav, fixef = c("ANOGRAVIDEZ", "CODBAIRES"), data = db_microsolo, vcov = "iid")
summary(regapgar1solo_fe)

regapgar1trimsolo_fe <- lm( APGAR1 ~ trat_grav + trat_segtri + trat_tertri + as.character(ANOGRAVIDEZ)+ as.character(CODBAIRES) , data = db_microsolo)
regapgar1trimsolo_fe <- feols(APGAR1 ~ trat_grav + trat_segtri + trat_tertri , fixef = c("ANOGRAVIDEZ", "CODBAIRES"), data = db_microsolo, vcov = "iid")
summary(regapgar1trimsolo_fe)

# APGAR 5

regapgar5solo_fe <- lm( APGAR5 ~ trat_grav + as.character(ANOGRAVIDEZ)+ as.character(CODBAIRES), data = db_microsolo)
regapgar5solo_fe <- feols(APGAR5 ~ trat_grav , fixef = c("ANOGRAVIDEZ", "CODBAIRES"), data = db_microsolo, vcov = "iid")
summary(regapgar5solo_fe)

regapgar5trimsolo_fe <- lm( APGAR5 ~ trat_grav + trat_segtri + trat_tertri + as.character(ANOGRAVIDEZ)+ as.character(CODBAIRES) , data = db_microsolo)
regapgar5trimsolo_fe <- feols(APGAR5 ~ trat_grav + trat_segtri + trat_tertri, fixef = c("ANOGRAVIDEZ", "CODBAIRES"), data = db_microsolo, vcov = "iid")
summary(regapgar5trimsolo_fe)

#### Tabela Resultados ####