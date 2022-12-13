rm(list = ls())
library(tidyverse)
library(leaflet)
library(openxlsx)
library(sf)

##################################################################################################

#### Esse script baixa as bases de catchment area, equipes do CNES e localiza??o das escolas municipais.
#### Depois, calcula qual ? a equipe respons?vel por determinada escola, e por fim salva a base de dados
#### 'db_escolasf.RDS', que cont?m o merge correto entre escolas municipais e equipes ESF.

##################################################################################################


# Lendo shapefile catchment area e base CNES equipe

db_equipes <- read.xlsx(xlsxFile = "data/xlsx-csv/db_equipes.xlsx", sheet = 1)

shpcoverage <- st_read("data/shp/health_units_coverage.shp")

db_equipes$COD_Equipe <- as.numeric(db_equipes$COD_Equipe)

db_unificado <-
  left_join(shpcoverage, db_equipes, by = "COD_Equipe")

db_unificado <- db_unificado %>% filter(NOME_EQUIP != "SEM_ESF")

# Lendo base de escolas municipais

db_escmun <- st_read("data/shp/Escolas_Municipais.shp")

# Gerando matriz distancia escolas x equipes

resultado <-
  st_distance(db_escmun, db_unificado) %>% as.data.frame() # Não está funcionando mais por algum motivo. Checar

# Documentando a matriz

names(resultado) <- db_unificado$COD_Equipe
resultado$COD_INEP <- db_escmun$SMEDBOEs_4

# Calculando a Equipe ESF respons?vel pela escola municipal

dist_escolas <-
  resultado %>% gather("OBJECTID", value = "distance", 1:1252)

esf_maisprox <-
  dist_escolas %>% group_by(COD_INEP) %>% slice(which.min(distance))

esf_maisprox$distance <- as.numeric(esf_maisprox$distance)

esf_maisprox <-
  esf_maisprox %>% mutate(COD_Equipe = ifelse(distance == 0, OBJECTID, "0000"))
esf_maisprox$COD_Equipe <- as.numeric(esf_maisprox$COD_Equipe)

# Criando a base de escolas com ano de tratamento e a base das areas para gerar os mapas

db_final <- left_join(esf_maisprox, db_unificado)
db_final$COD_INEP <- as.numeric(as.character(db_final$COD_INEP))
saveRDS(db_final, file = "data/RDS/db_escolasf.RDS")
saveRDS(db_unificado, file = "data/RDS/db_areasmapa.RDS")
