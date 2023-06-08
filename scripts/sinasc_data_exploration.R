theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Times New Roman", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.border = element_blank(),
      ...
    )
}

library(sf)
library(tidyverse)
library(microdatasus)
library(openxlsx)
library(read.dbc)
library(lubridate)
library(did)
library(plm)
library(stargazer)
library(table1)

##################################################################################################

#### Esse script trata os dados do SINASC, gera as informações de tratamento por bairro e gera os mapas
#### e tabelas construídos a partir dos dados do SINASC na seção 3.1. Além disso, gera a base de dados para
#### as regressões de robustez e a salva em 'data/RDS/db_regnasc.RDS'. Nota: nessa versão, determinado bairro
#### é considerado tratado a partir do momento em que 1% de sua área é atendida por uma equipe da ESF.

##################################################################################################

#### Lendo os dados ####

db_microdatasus <- fetch_datasus(year_start = 2006, year_end = 2013, uf = "RJ", information_system = "SINASC")
db_microdatasus <- process_sinasc(db_microdatasus)
db_microdatasus <- db_microdatasus %>% filter(CODMUNRES == "330455")

sf_bairro <- st_read("data/Shp/Limite_Bairro.shp")

#### Criando variaveis de peso, pre natal e APGAR ####

db_microdatasus <- db_microdatasus %>% mutate(PESO = as.numeric(PESO), 
                                              APGAR1 = as.numeric(APGAR1), 
                                              APGAR5 = as.numeric(APGAR5))


db_microdatasus <- db_microdatasus %>% mutate(CONSULTAS_NUM = case_when(CONSULTAS == 'Nenhuma' ~ 0,
                                                          CONSULTAS == '1 a 3 vezes' ~ 2,
                                                          CONSULTAS == '4 a 6 vezes' ~ 5,
                                                          CONSULTAS == "7 ou mais vezes" ~ 7))


desc_sinasc <- db_microdatasus %>% group_by(CODBAIRES) %>% summarise(PESO = mean(PESO, na.rm = T),
                                                                     PRENATAL = mean(CONSULTAS_NUM, na.rm = T),
                                                                     APGAR1 = mean(APGAR1, na.rm = T),
                                                                     APGAR5 = mean(APGAR5, na.rm = T),
                                                                     NUM = n(),
                                                                     PCT_ZEROCONSULTAS = sum(CONSULTAS == "Nenhuma", na.rm = T) / n())

#### Mapas ####

desc_sinasc <- desc_sinasc %>% mutate(CODBAIRES = as.numeric(CODBAIRES)) %>% rename(CODBAIRRO_ = CODBAIRES)


sf_sinasc <- left_join(sf_bairro, desc_sinasc)
sf_sinasc <- sf_sinasc %>% mutate(log_peso = log(PESO))

#Pre natal
ggplot() + geom_sf(data = sf_sinasc, alpha = 0.5, size = 0.1)+
  geom_sf(data = sf_sinasc, aes(fill = PCT_ZEROCONSULTAS), size=0.1, alpha=0.9, color = "white")+
  viridis::scale_fill_viridis(option = "viridis", name = "Share of births with zero prenatal appointments", direction = -1)+
  coord_sf()+
  theme_map()+
  theme(legend.position = "bottom")


# Peso
ggplot() + geom_sf(data = sf_sinasc, alpha = 0.5, size = 0.1)+
  geom_sf(data = sf_sinasc, aes(fill = PESO/1000), size=0.1, alpha=0.9, color = "white")+
  viridis::scale_fill_viridis(option = "viridis", name = "Average weight (kgs)")+
  coord_sf()+
  theme_map()+
  theme(legend.position = "bottom")

#APGAR 1 minutos
#ggplot() + geom_sf(data = sf_sinasc, alpha = 0.5, size = 0.1)+
#  geom_sf(data = sf_sinasc, aes(fill = APGAR1), size=0.1, alpha=0.9, color = "white")+
#  viridis::scale_fill_viridis(option = "viridis", name = "APGAR Score 1 minute after birth")+
#  coord_sf()+
#  theme_map()+
#  theme(legend.position = "bottom")

#APGAR 5 minutos
#ggplot() + geom_sf(data = sf_sinasc, alpha = 0.5, size = 0.1)+
#  geom_sf(data = sf_sinasc, aes(fill = APGAR5), size=0.1, alpha=0.9, color = "white")+
#  viridis::scale_fill_viridis(option = "viridis", name = "APGAR Score 5 minutes after birth")+
#  coord_sf()+
#  theme_map()+
#  theme(legend.position = "bottom")

#### Tabelas ####

table1::label(sf_sinasc$PRENATAL) <- "Number of prenatal appointments"
table1::label(sf_sinasc$APGAR1) <- "APGAR 1 minute"
table1::label(sf_sinasc$APGAR5) <- "APGAR 5 minutes"

table1(~PRENATAL + ~APGAR1 + ~APGAR5, data = sf_sinasc) #tabela bairros

db_tabela <- db_microdatasus %>% filter(!is.na(CODBAIRES))
db_tabela <- db_tabela %>% mutate(ZERO_CONSULTAS = ifelse(CONSULTAS == "Nenhuma", 1, 0))

table1::label(db_tabela$ZERO_CONSULTAS) <- "Zero prenatal appointments"
table1::label(db_tabela$PESO) <- "Weight (grams)"

table1(~ZERO_CONSULTAS + ~PESO, data = db_tabela) #tabela nascimentos

#### Join espacial bairros x equipes ####

# Associando bairros com equipes ESF

db_unificado <- readRDS("data/RDS/db_areasmapa.RDS")
db_unificado <- db_unificado %>% filter(is.na(ano_instalacao) | ano_instalacao <= 2016)


db_unificado <- st_make_valid(db_unificado)
sf_sinasc <- st_make_valid(sf_sinasc)

resultado <- sf::st_intersection(sf_sinasc, db_unificado)

resultado <- resultado %>% mutate(intersect_area = st_area(.)) %>% 
  mutate(percentage = intersect_area / Área) %>% # esse acento pode dar problema por causa do encoding
  mutate(percentage = as.numeric(percentage))


# Mapa percentual de cobertura em 2016

pct_trat_bairro <- resultado %>% filter(ano_instalacao <= 2016) %>% group_by(CODBAIRRO) %>% summarise(pct_trat = sum(percentage))
pct_trat_bairro <- pct_trat_bairro %>% as.data.frame() %>% select(-geometry)

pct_trat_bairro <- left_join(sf_bairro, pct_trat_bairro)
pct_trat_bairro$pct_trat <- pct_trat_bairro$pct_trat %>% replace_na(0)
pct_trat_bairro$pct_trat <- ifelse(pct_trat_bairro$pct_trat > 1, 1, pct_trat_bairro$pct_trat)

ggplot() + geom_sf(data = pct_trat_bairro, alpha = 0.5, size = 0.1)+
  geom_sf(data = pct_trat_bairro, aes(fill = pct_trat), size=0.1, alpha=0.9, color = "grey")+
  scale_fill_fermenter(palette = "RdYlGn", n.breaks = 7, direction = 1, name = "FHS coverage (2016)")+
  coord_sf()+
  theme_map()+
  theme(legend.position = "bottom")

#### Limpando bases para regressoes ####

# Definindo o mes e ano de tratamento

resultado <- resultado %>% mutate(DT_ATIVA = ym(DT_ATIVA))
resultado <- resultado %>% mutate(MES_ATIVA = format(as.Date(DT_ATIVA), "%Y-%m"))
integral <- resultado %>% group_by(CODBAIRRO, MES_ATIVA) %>% summarise(soma_area = sum(percentage))
integral$CODBAIRRO <- as.numeric(integral$CODBAIRRO)
integral <- integral %>% group_by(CODBAIRRO) %>% mutate(soma_final = accumulate(soma_area, sum))
integral <- integral %>% select(-geometry) %>% as.data.frame()

resultado_integral <- integral %>% filter(soma_final >= 0.01) %>% group_by(CODBAIRRO) %>% summarise(tratamento_1pct = min(MES_ATIVA)) #definimos um bairro tratado como 1% da area
resultado_integral <- resultado_integral %>% rename(CODBAIRRO_ = CODBAIRRO)
  
sf_tratamento <- left_join(sf_bairro, resultado_integral)
#sf_tratamento <- sf_tratamento %>% mutate(tratamento = format(as.Date(tratamento_1pct), "%Y-%m"))

# Descobrindo o dia e mes da gravidez

db_microdatasus <- db_microdatasus %>% mutate(CODBAIRES = as.numeric(CODBAIRES))
db_micro <- db_microdatasus %>% mutate(DTNASC = as.Date(DTNASC))

db_micro <- db_micro %>% mutate(DIASGRAVIDEZ = case_when(GESTACAO == 'Menos de 22 semanas' ~ 154,
                                                         GESTACAO == '22 a 27 semanas' ~ 172,
                                                         GESTACAO == '28 a 31 semanas' ~ 207,
                                                         GESTACAO == '32 a 36 semanas' ~ 238,
                                                         GESTACAO == '37 a 41 semanas' ~ 273,
                                                         GESTACAO == '42 semanas ou mais' ~ 294))

db_micro <- db_micro %>% mutate(DTGRAVIDEZ = DTNASC - DIASGRAVIDEZ)
db_micro <- db_micro %>% mutate(MESGRAVIDEZ = format(as.Date(DTGRAVIDEZ), "%Y-%m"))

# Criando variaveis de tratamento pelo bairro

sf_tratamento <- sf_tratamento %>% rename(CODBAIRES = CODBAIRRO_)
db_micro <- left_join(db_micro, sf_tratamento, by = "CODBAIRES")
db_micro <- db_micro %>% filter(!is.na(CODBAIRES))

db_micro <- db_micro %>% mutate(trat_grav = ifelse((tratamento_1pct > MESGRAVIDEZ) | is.na(tratamento_1pct), 0, 1))
db_micro <- db_micro %>% mutate(ANOGRAVIDEZ = year(DTGRAVIDEZ))
db_micro <- db_micro %>% mutate(DTSEGTRI = format(as.Date(DTGRAVIDEZ + 90), "%Y-%m"), DTTERTRI = format(as.Date(DTGRAVIDEZ + 180), "%Y-%m"))
db_micro <- db_micro %>% mutate(trat_segtri = ifelse((tratamento_1pct > DTSEGTRI) | is.na(tratamento_1pct), 0, 1))
db_micro <- db_micro %>% mutate(trat_tertri = ifelse((tratamento_1pct > DTTERTRI) | is.na(tratamento_1pct), 0, 1))

# Criando variaveis de consulta pre-natal

db_micro <- db_micro %>% mutate(ZEROCONSULTAS = ifelse(CONSULTAS_NUM == 0, 1, 0))
db_micro <- db_micro %>% mutate(MENOSQUATROCONSULTAS = ifelse(CONSULTAS_NUM == 0 | CONSULTAS_NUM == 2 , 1, 0))
db_micro <- db_micro %>% mutate(MENOSSETECONSULTAS = ifelse(CONSULTAS_NUM == 0  | CONSULTAS_NUM == 2 | CONSULTAS_NUM == 5, 1, 0))


# Gerando base de dados para regressoes

# saveRDS(db_micro, "data/RDS/db_regnasc.RDS", compress = T) github is stupid and doesn't know how to deal with large files, so don't run this


