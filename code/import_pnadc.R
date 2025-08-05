# Imports e Organização dos Diretórios ------------------------------------
library(tidyverse)
library(this.path)
library(PNADcIBGE)
library(logger)
setwd(this.dir())
saveDir <- '../raw/'

# Importando e Tratando PNADc --------------------------------------------------------
year = 2022
rawData <- get_pnadc(year = year, interview = 5, labels = F, design = F)
log_info("Dados da PNADc extraídos.")
# Deflacionando e fitrando dados
# Selecionamos apenas adultos com renda
rawData <- rawData %>% mutate(VD4046 = VD4046*CO1)
selectedData <- rawData %>% 
  select(c("Ano", "UF", "UPA", "V2003", "V1008", "ID_DOMICILIO", "V1032", "VD4046")) %>%
  rename(rendimento_todasfontes = VD4046,
         peso_dom_comcalib = V1032,
         numero_selecao_domicilio = V1008,
         numero_ordem = V2003) %>%
  unite('id_ind', c('UPA','numero_selecao_domicilio', 'numero_ordem'), remove = FALSE) %>%
  filter(rendimento_todasfontes>0)

# Salvando o Resultado Final ----------------------------------------------
filepath <- paste0(saveDir, "pnad", year, ".rds")
log_info("Salvando PNADc em {filepath}")
saveRDS(selectedData, filepath)