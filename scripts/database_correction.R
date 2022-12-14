rm(list = ls())

##################################################################################################

#### Esse script corrije a base de dados final de escolas utilizada nos modelos. Ela altera a data
#### de tratamento de escolas de 35 áreas de atuação com inconsistências cadastrais (e, portanto, com
#### NA como valor) para a data de instalação da equipe mais antiga associdada a mesma unidade de saúde.

##################################################################################################


# Lendo RDS com match entre escolas e catchment areas ESF

db_final <- readRDS("data/RDS/db_escolasf.RDS")

db_final <- as.data.frame(db_final) %>% select(-geometry)

db_unificado <- readRDS("data/RDS/db_areasmapa.RDS")

db_unificado <- as.data.frame(db_unificado) %>% select(-geometry)

# Identificando os erros na base

db_erros <- db_final %>% filter(COD_Equipe != 0 & is.na(ano_instalacao))

list_CNES <- unique(db_erros$CNES.x)

# Achando o ano correto (1a equipe ESF da mesma unidade de sa?de)

db_errosfil <- db_unificado %>% filter(CNES.x %in% list_CNES)

anoinicial <- db_errosfil  %>% group_by(CNES.x) %>% summarise(ano_correto = max(ano_instalacao, na.rm = T))

# Corrigindo a base

db_final <- left_join(db_final, anoinicial, by = "CNES.x")

db_final <- db_final %>% mutate(ano_instalacao = ifelse(COD_Equipe != 0 & is.na(ano_instalacao), 
                                                        ano_correto,
                                                        ano_instalacao))

# Criando arquivo RDS com a base nova

saveRDS(db_final, file = "data/RDS/db_escolasf_corr.RDS")
