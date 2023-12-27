

##################################################################################################

#### Esse script roda as regressoes de mecanismo de transmissao do impacto da chegada da ESF no bairro
#### sobre variaveis de peso, zero consultas pre-natais e APGAR, gerando tabelas em LaTeX. Ele requer
#### que o arquivo 'db_reg.RDS' seja importado antes de rodar as regressoes.

##################################################################################################


#### Regressoes com efeitos fixos de neighborhood ####

### Base Completa

# Peso

regpeso_fe <- feols(PESO ~ trat_grav , fixef = c("ANOGRAVIDEZ", "CODBAIRES"), data = db_reg, vcov = "iid")
summary(regpeso_fe)

regpesotrim_fe <- feols( PESO ~ trat_grav + trat_segtri + trat_tertri , fixef = c("ANOGRAVIDEZ", "CODBAIRES"), data = db_reg, vcov = "iid")
summary(regpesotrim_fe)

# ZERO CONSULTAS PRE NATAIS

regzero_fe <- feols(ZEROCONSULTAS ~ trat_grav , fixef = c("ANOGRAVIDEZ", "CODBAIRES"), data = db_reg, vcov = "iid")
summary(regzero_fe)

regzerotrim_fe <- feols(ZEROCONSULTAS ~ trat_grav + trat_segtri + trat_tertri, fixef = c("ANOGRAVIDEZ", "CODBAIRES"), data = db_reg, vcov = "iid")
summary(regzerotrim_fe)

# APGAR 1

regapgar1_fe <- feols(APGAR1 ~ trat_grav , fixef = c("ANOGRAVIDEZ", "CODBAIRES"), data = db_reg, vcov = "iid")
summary(regapgar1_fe)

regapgar1_fe <- feols( APGAR1 ~ trat_grav + trat_segtri + trat_tertri , fixef = c("ANOGRAVIDEZ", "CODBAIRES"), data = db_reg, vcov = "iid")
summary(regzerotrim_fe)

# APGAR 5

regapgar5_fe <- feols( APGAR5 ~ trat_grav , fixef = c("ANOGRAVIDEZ", "CODBAIRES"), data = db_reg, vcov = "iid")
summary(regapgar5_fe)

regapgar5trim_fe <- feols( APGAR5 ~ trat_grav + trat_segtri + trat_tertri , fixef = c("ANOGRAVIDEZ", "CODBAIRES"), data = db_reg, vcov = "iid")
summary(regapgar5trim_fe)

### Base Restrita

db_microsolo <- db_reg %>% filter(RACACOR != "Branca")

# PESO

regpesosolo_fe <- feols( PESO ~ trat_grav , fixef = c("ANOGRAVIDEZ", "CODBAIRES"), data = db_microsolo, vcov = "iid")
summary(regpesosolo_fe)

regpesotrimsolo_fe <- feols( PESO ~ trat_grav + trat_segtri + trat_tertri  , fixef = c("ANOGRAVIDEZ", "CODBAIRES"), data = db_microsolo, vcov = "iid")
summary(regpesotrimsolo_fe)

#  ZERO CONSULTAS PRE NATAIS

regprensolo_fe <- feols( ZEROCONSULTAS ~ trat_grav , fixef = c("ANOGRAVIDEZ", "CODBAIRES"), data = db_microsolo, vcov = "iid")
summary(regprensolo_fe)

regprentrimsolo_fe <- feols( ZEROCONSULTAS ~ trat_grav + trat_segtri + trat_tertri , fixef = c("ANOGRAVIDEZ", "CODBAIRES"), data = db_microsolo, vcov = "iid")
summary(regprentrimsolo_fe)

# APGAR 1

regapgar1solo_fe <- feols(APGAR1 ~ trat_grav, fixef = c("ANOGRAVIDEZ", "CODBAIRES"), data = db_microsolo, vcov = "iid")
summary(regapgar1solo_fe)

regapgar1trimsolo_fe <- feols(APGAR1 ~ trat_grav + trat_segtri + trat_tertri , fixef = c("ANOGRAVIDEZ", "CODBAIRES"), data = db_microsolo, vcov = "iid")
summary(regapgar1trimsolo_fe)

# APGAR 5

regapgar5solo_fe <- feols(APGAR5 ~ trat_grav , fixef = c("ANOGRAVIDEZ", "CODBAIRES"), data = db_microsolo, vcov = "iid")
summary(regapgar5solo_fe)

regapgar5trimsolo_fe <- feols(APGAR5 ~ trat_grav + trat_segtri + trat_tertri, fixef = c("ANOGRAVIDEZ", "CODBAIRES"), data = db_microsolo, vcov = "iid")
summary(regapgar5trimsolo_fe)

#### Tabela Resultados ####

#Base Completa

etable(list(regpeso_fe, regpesotrim_fe, regzero_fe, regzerotrim_fe, regapgar5_fe, regapgar5trim_fe), tex = T)

#Base Restrita para

etable(list(regpesosolo_fe, regpesotrimsolo_fe, regprensolo_fe, regprentrimsolo_fe, regapgar5solo_fe, regapgar5trimsolo_fe), tex = T)

