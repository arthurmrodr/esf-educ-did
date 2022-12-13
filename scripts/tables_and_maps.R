rm(list = ls())
library(sf)
library(stargazer)
library(xtable)
library(table1)

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

##################################################################################################

#### This script generates descriptive tables, graphs and maps in Section 3. 

##################################################################################################





#Lendo a base de dados da catchment areas

db_unificado <- readRDS("data/RDS/db_areasmapa.RDS")

# Lendo shp municipio RJ
sf_rio <- st_read("data/shp/Limite_do_Munic%C3%ADpio_do_Rio_de_Janeiro.shp")

#### Grafico data instalacao ESF (Figure 2) ####

ggplot() + geom_sf(data = sf_rio, alpha = 0.5, size = 0.1)+
  geom_sf(data = db_unificado, aes(fill = ano_instalacao), size=0.1, alpha=0.9, color = "white")+
  viridis::scale_fill_viridis(option = "magma", name = "Installation year",
                              breaks = c(2000, 2001, 2002, 2003, 2004, 2005,
                                         2006, 2007, 2008, 2009, 2010, 2011, 2012,
                                         2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020),
                              guide = guide_legend(
                                direction = "horizontal",
                                keyheight = unit(2, units = "mm"),
                                keywidth = unit(70 / 8, units = "mm"),
                                title.position = 'top',
                                # I shift the labels around, the should be placed 
                                # exactly at the right end of each legend key
                                title.hjust = 0.5,
                                label.hjust = 1,
                                nrow = 1,
                                byrow = T,
                                label.position = "bottom"))+
  coord_sf()+
  theme_map()+
  theme(legend.position = "bottom")
    


#### Mapa equipes desativadas (Figure 3) ####

db_desativados <- db_unificado %>% filter(ano_fechamento != 9000)

ggplot() + geom_sf(data = sf_rio, alpha = 0.5, size = 0.1)+
  geom_sf(data = db_desativados, fill = "darkslateblue", size=0.1, alpha=0.9, color = "white", show.legend = "Ano Instala??o")+
  coord_sf()+
  theme_map()+
  theme(legend.position = "bottom")

#Lendo dados educacao

ind_inep <- read.csv("data/xlsx-csv/ind_inep.csv")
ind_inep <- ind_inep %>% rename(COD_INEP = id_escola)
ind_inep <- ind_inep %>% filter(rede == "municipal")

#Associando numero de escolas por equipe

db_final_corr <- readRDS("data/RDS/db_areasmapa_corr.RDS")

db_numequipes <- db_final_corr %>% group_by(COD_Equipe) %>% summarise(numequipes = n())

db_unificado <- left_join(db_unificado, db_numequipes)

#### Mapa escolas por equipe (Figure 8) ####

ggplot() + geom_sf(data = sf_rio, alpha = 0.5, size = 0.1)+
  geom_sf(data = db_unificado, aes(fill = numequipes), size=0.1, alpha=0.9, color = "white")+
  viridis::scale_fill_viridis(option = "viridis", breaks = c(0,1,2,3,4,5,6,7,8,9,10), name = "Number of schools",
                              guide = guide_legend(
                                direction = "horizontal",
                                keyheight = unit(2, units = "mm"),
                                keywidth = unit(70 / 8, units = "mm"),
                                title.position = 'top',
                                # I shift the labels around, the should be placed 
                                # exactly at the right end of each legend key
                                title.hjust = 0.5,
                                label.hjust = 1,
                                nrow = 1,
                                byrow = T,
                                label.position = "bottom"))+
  theme_map()+
  theme(legend.position = "bottom")
  
#### Mapa escolas ####

shp_escolas <- st_read("data/shp/Escolas_Municipais.shp")

shp_escolas <- shp_escolas %>% rename(COD_INEP = SMEDBOEs_4)

shp_escolas <- shp_escolas %>% mutate(COD_INEP = as.numeric(COD_INEP))

shp_escolas <- shp_escolas %>% filter(!is.na(COD_INEP))

db_mapaescolas <- left_join(shp_escolas, db_final_corr, by = "COD_INEP")

db_mapaescolas <- db_mapaescolas %>% mutate(tratamento = ifelse(is.na(ano_instalacao),0,1))

ggplot() + geom_sf(data = sf_rio, alpha = 0.5, size = 0.1)+
  geom_sf(data = db_mapaescolas, aes(color = as.character(tratamento)))+
  labs(color = "FHS treatment")+
  theme_map()

db_mapaescolas %>% group_by(tratamento) %>% summarise(n())


#### Tabela Estatistica descritiva resultados ####

#db_saeb <- read.csv("Dados/db_ideb.csv")
#db_saeb <- db_saeb %>% filter(rede == "municipal" & anos_escolares == "iniciais (1-5)")
#db_saeb <- db_saeb %>% filter(id_escola %in% db_final$COD_INEP)

#db_tabela_saeb <- db_saeb %>% filter(ano == 2007) %>% select("ano", "id_escola", "nota_saeb_media_padronizada")

db_inep <- left_join(ind_inep, db_final_corr)
db_inep <- db_inep %>% filter(COD_INEP %in% db_final_corr$COD_INEP)

db_tabela_rend <- db_inep %>% filter(ano == 2007) %>% select("ano", "COD_INEP", "tdi_ensino_fund_anos_iniciais",
                                                             "taxa_aprov_ensino_fund_anos_iniciais",
                                                             "taxa_reprov_ensino_fund_anos_iniciais",
                                                             "taxa_aband_ensino_fund_anos_iniciais")
#db_tabela_saeb <- db_tabela_saeb %>% rename(COD_INEP = id_escola)

#db_tabelaresultados <- full_join(db_tabela_saeb, db_tabela_rend, by = "COD_INEP")

table1::label(db_inep$tdi_ensino_fund_anos_iniciais) <- "Distor??o Idade-S?rie AI EF (2007)"
table1::label(db_inep$taxa_aprov_ensino_fund_anos_iniciais) <- "Taxa Aprova??o AI EF (2007)"
table1::label(db_inep$taxa_reprov_ensino_fund_anos_iniciais) <- "Taxa Reprova??o AI EF (2007)"
table1::label(db_inep$taxa_aband_ensino_fund_anos_iniciais) <- "Taxa Abandono AI EF (2007)"
#table1::label(db_tabelaresultados$nota_saeb_media_padronizada) <- "M?dia SAEB AI EF (2007)"

table1(~tdi_ensino_fund_anos_iniciais+taxa_aprov_ensino_fund_anos_iniciais+taxa_reprov_ensino_fund_anos_iniciais+
         taxa_aband_ensino_fund_anos_iniciais, data = db_inep)
