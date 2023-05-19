

##################################################################################################

#### Esse script roda as regressoes 
#### 
#### 
#### 

##################################################################################################


db_reg <- readRDS("data/RDS/db_regnasc.RDS") # Não vai funcionar pq a base é grande demias pro github, precisa fazer manualmente


#### Regressoes ####

# PESO

regpeso <- lm( PESO ~ trat_grav + ESCMAE + IDADEMAE + GESTACAO + RACACOR, data = db_reg)
summary(regpeso)

regpesolog <- lm(log(PESO) ~ trat_grav + ESCMAE + IDADEMAE + GESTACAO + RACACOR, data = db_reg)
summary(regpesolog)

regpesotrim <- lm( PESO ~ trat_grav + trat_segtri + trat_tertri + ESCMAE + IDADEMAE + GESTACAO + RACACOR + as.character(ANOGRAVIDEZ), data = db_reg)
summary(regpesotrim + ESCMAE + IDADEMAE + GESTACAO + RACACOR)

# MENOS 7 CONSULTAS PRE NATAIS

regpren <- lm( ZEROCONSULTAS ~ trat_grav + as.character(ANOGRAVIDEZ), data = db_reg)
summary(regpren)

regprentrim <- lm( ZEROCONSULTAS ~ trat_grav + trat_segtri + trat_tertri + RACACOR + ESTCIVMAE + ESCMAE + CODOCUPMAE, data = db_reg)
summary(regprentrim)

# APGAR 1

regapgar1 <- lm( APGAR1 ~ trat_grav + as.character(ANOGRAVIDEZ), data = db_reg)
summary(regapgar1)

regapgar1trim <- lm( APGAR1 ~ trat_grav + trat_segtri + trat_tertri + as.character(ANOGRAVIDEZ), data = db_reg)
summary(regapgar1trim)

# APGAR 5

regapgar5 <- lm( APGAR5 ~ trat_grav + as.character(ANOGRAVIDEZ), data = db_reg)
summary(regapgar5)

regapgar5trim <- lm( APGAR5 ~ trat_grav + trat_segtri + trat_tertri + ESCMAE + IDADEMAE + GESTACAO + RACACOR + ESTCIVMAE + QTDFILMORT + IDANOMAL, data = db_reg)
summary(regapgar5trim)

#### Regressoes filtro ####

db_microfilter <- db_micro %>% filter(is.na(tratamento_1pct) | tratamento_1pct >= 2006)

# PESO

regpeso <- lm( PESO ~ trat_grav+ IDADEMAE + RACACOR, data = db_microfilter)
summary(regpeso)


regpesotrim <- lm( PESO ~ trat_grav + trat_segtri + trat_tertri  + IDADEMAE + RACACOR , data = db_microfilter)
summary(regpesotrim)

# MENOS 7 CONSULTAS PR? NATAIS

regpren <- lm( ZEROCONSULTAS ~ trat_grav + as.character(ANOGRAVIDEZ), data = db_microfilter)
summary(regpren)

regprentrim <- lm( ZEROCONSULTAS ~ trat_grav + trat_segtri + trat_tertri + RACACOR + ESTCIVMAE , data = db_micro)
summary(regprentrim)

# APGAR 1

regapgar1 <- lm( APGAR1 ~ trat_grav + as.character(ANOGRAVIDEZ), data = db_microfilter)
summary(regapgar1)

regapgar1trim <- lm( APGAR1 ~ trat_grav + trat_segtri + trat_tertri + RACACOR + ESTCIVMAE , data = db_micro)
summary(regapgar1trim)

# APGAR 5

regapgar5 <- lm( APGAR5 ~ trat_grav + as.character(ANOGRAVIDEZ), data = db_microfilter)
summary(regapgar5)

regapgar5trim <- lm( APGAR5 ~ trat_grav + trat_segtri + trat_tertri + RACACOR + ESTCIVMAE + IDADEMAE + ESCMAE , data = db_micro)
summary(regapgar5trim)

