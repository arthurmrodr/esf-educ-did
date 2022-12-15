All files in this folder are created by the scripts in the 'scripts' folder. 

* 'db_areasmapa' and 'db_escolasf' both come from the 'data_treatment' script. 'db_areasmapa' consists of team characteristics and 'db_escolasf' consists of municipal school characteristics.

* 'db_areasmapa_corr' is created by 'database_correction'. It's nearly identical to 'db_areasmapa', with the exception that the year of treatment for schools covered by teams with missing installation data on the health unit register is no longer NA. It's replaced by the year of installation of the oldest team associated with the same health unit.