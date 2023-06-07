library(tidyverse)
library(stargazer)

##################################################################################################

#### Esse script roda as regressoes 
#### 
#### 
#### 

##################################################################################################


db_reg <- readRDS("data/RDS/db_regnasc.RDS") # Não vai funcionar pq a base é grande demias pro github, precisa fazer manualmente
db_reg <- db_micro

#### Regressoes com a base completa ####

# PESO

regpeso <- lm( PESO ~ trat_grav + as.character(ANOGRAVIDEZ), data = db_reg)
summary(regpeso)

#regpesolog <- lm(log(PESO) ~ trat_grav + ESCMAE + IDADEMAE + GESTACAO + RACACOR, data = db_reg)
#summary(regpesolog)

regpesotrim <- lm( PESO ~ trat_grav + trat_segtri + trat_tertri + as.character(ANOGRAVIDEZ), data = db_reg)
summary(regpesotrim)

# ZERO CONSULTAS PRE NATAIS

regpren <- lm( ZEROCONSULTAS ~ trat_grav + as.character(ANOGRAVIDEZ), data = db_reg)
summary(regpren)

regprentrim <- lm( ZEROCONSULTAS ~ trat_grav + trat_segtri + trat_tertri + as.character(ANOGRAVIDEZ), data = db_reg)
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

#### Regressoes filtro mães solteiras####

db_microsolo <- db_reg %>% filter(ESTCIVMAE == "Solteira")

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
