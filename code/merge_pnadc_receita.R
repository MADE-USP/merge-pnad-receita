  # Imports e Organização dos Arquivos --------------------------------------
library(tidyverse)
library(logger)
library(this.path)
library(hutils)
setwd(this.dir())
saveDir <- '../processed/'
dataDir <- '../raw/'
  
# Carregando Dados --------------------------------------------------------
pnad <- readRDS(paste0(dataDir, 'pnad2022.rds'))
receitaCents <- read.csv(paste0(dataDir, 'Dados - IRPF - Centesimos.csv'))
receitaRecorteUm <- read.csv(paste0(dataDir, 'Dados - IRPF - Primeiro Recorte.csv'))
receitaRecorteDois <- read.csv(paste0(dataDir, 'Dados - IRPF - Segundo Recorte.csv'))
column_names <- c(
  "Centésimo",
  "Quantidade de Declarantes",
  "Renda Total",
  "Rendimento Tributável",
  "Base de Cálculo",
  "Aluguel",
  "Previdência Oficial",
  "Previdência RRA",
  "Previdência Privada",
  "Dependentes",
  "Instrução",
  "Despesas Médicas",
  "Pensão Alimentícia",
  "Livro-Caixa",
  "Imóveis",
  "Móveis",
  "Financeiros",
  "Outros Bens e Direitos",
  "13º salário",
  "Rendimentos de Aplicações Financeiras",
  "Ganhos de Capital na Alienação de Bens/Direitos",
  "Participação nos Lucros ou Resultados",
  "Rendimentos Recebidos Acumuladamente",
  "Ganhos Líquidos em Renda Variável",
  "Juros sobre Capital Próprio",
  "Outros",
  "Rendimentos Sujeitos à Tributação Exclusiva",
  "Imposto Devido",
  "Lucros e Dividendos",
  "Rendim. Sócio/Titular ME/EPP Opt SIMPLES",
  "Transf. Patrimoniais doações e heranças",
  "Parcela isenta de aposentadoria etc",
  "Parcela isenta da Atividade Rural",
  "Pensão, proventos de aposentadoria etc",
  "Indenização por Rescisão do Contrato de Trabalho etc",
  "Rendimentos de Caderneta de Poupança etc",
  "Incorporação de Reservas de Capital/Bonificações em Ações",
  "Outros Rendimentos Isentos",
  "Dívida"
)
colnames(receitaCents) <- column_names
colnames(receitaRecorteUm) <- column_names
colnames(receitaRecorteDois) <- column_names
# Juntando PNAD com Dados dos 10000 quantis -------------------------------
pnad <- pnad %>% arrange(desc(rendimento_todasfontes))
v <- pnad$peso_dom_comcalib
  
grupo <- 1 # Primeiro grupo
tg <- 3842 # Tamanho desse recorte
ref <- 1 # Ponto inicial do primeiro grupo: sempre 1
output <- c()
  
# O vetor de pesos sempre tem que estar ordenado
for(i in 1:length(v)){
  if(sum(v[ref:i]) <= tg){
  } else {
    grupo <- grupo + 1
    ref <- i + 1}
  output[i] <- grupo
}
# Aqui, juntamos com a PNADc
pnadMerge <- pnad %>% mutate(grupo = output,
                        populacao_acumulada = cumsum(peso_dom_comcalib),
                        grupo_receita = 11 - grupo)
  
pnadc_receita_recorte1 <- right_join(pnadMerge, receitaRecorteUm,
                           by = c("grupo_receita" = "Centésimo"), keep=TRUE)
# Agora, vamos calibrar os dados da PNADc e individulizar o que é proveniente da receita
mensaliza_e_individualiza <- function(var){
  (var/12)/3842
}
  
pnadc_receita_recorte1 <- pnadc_receita_recorte1 %>%
  group_by(grupo_receita) %>%
  mutate(
    
    renda_anual_todasfontes_centesimo = sum(rendimento_todasfontes*peso_dom_comcalib)*12,
    
    fator_expansão = `Renda Total`/renda_anual_todasfontes_centesimo,
    
    rendimento_todasfontes_calibrado = if_else(fator_expansão >= 1, rendimento_todasfontes*fator_expansão,
                                               rendimento_todasfontes),
    
    across(.cols = c(`Renda Total`:`Dívida`),
           .fns = mensaliza_e_individualiza)
    
  ) %>%
  rename(peso_comcalib = peso_dom_comcalib) %>%
  select(id_ind, peso_comcalib, UF, rendimento_todasfontes, rendimento_todasfontes_calibrado, 
         Centésimo, grupo_receita, fator_expansão,
         `Renda Total`:`Dívida`) %>%
  ungroup()
# Salvando quais são os individuos na receita
individuos_na_receita <- pnadc_receita_recorte1$id_ind
# Juntando com os dados dos milésimos -------------------------------------
pnadc_receita_recorte2 <- pnad %>% filter(!id_ind %in% individuos_na_receita)
pnadc_receita_recorte2 <- pnadc_receita_recorte2 %>% arrange(desc(rendimento_todasfontes))
  
v <- pnadc_receita_recorte2$peso_dom_comcalib
  
grupo <- 1
tg <- 38417 # Novo tamanho
ref <- 1
output <- c()
  
for(i in 1:length(v)){
  if(sum(v[ref:i]) <= tg){
  } else {
    grupo <- grupo + 1
    ref <- i + 1}
    output[i] <- grupo
  }
  
pnadMerge <- pnadc_receita_recorte2 %>% mutate(grupo = output,
                               populacao_acumulada = cumsum(peso_dom_comcalib),
                               grupo_receita = 10 - grupo)
pnadc_receita_recorte2 <- right_join(pnadMerge, receitaRecorteDois,
                                       by = c("grupo_receita" = "Centésimo"), keep=TRUE)
mensaliza_e_individualiza <- function(var){
    (var/12)/38417
  }
pnadc_receita_recorte2 <- pnadc_receita_recorte2 %>%
  group_by(grupo_receita) %>%
  mutate(
    
    renda_anual_todasfontes_centesimo = sum(rendimento_todasfontes*peso_dom_comcalib)*12,
    
    fator_expansão = `Renda Total`/renda_anual_todasfontes_centesimo,
    
    rendimento_todasfontes_calibrado = if_else(fator_expansão >= 1, rendimento_todasfontes*fator_expansão,
                                               rendimento_todasfontes),
    
    across(.cols = c(`Renda Total`:`Dívida`),
           .fns = mensaliza_e_individualiza)
    
  ) %>%
  rename(peso_comcalib = peso_dom_comcalib) %>%
  select(id_ind, peso_comcalib, UF, rendimento_todasfontes, rendimento_todasfontes_calibrado, 
         Centésimo, grupo_receita, fator_expansão,
         `Renda Total`:`Dívida`) %>%
  ungroup()

individuos_na_receita <- append(individuos_na_receita, pnadc_receita_recorte2$id_ind)

# Juntando com outros 99 centis -------------------------------------------
pnadc_receita_recorte3 <- pnad %>% filter(!id_ind %in% individuos_na_receita)
pnadc_receita_recorte3 <- pnadc_receita_recorte3 %>% arrange(desc(rendimento_todasfontes))

v <- pnadc_receita_recorte3$peso_dom_comcalib

grupo <- 1
tg <- 384168 # Novo tamanho
ref <- 1
output <- c()

for(i in 1:length(v)){
  if(sum(v[ref:i]) <= tg){
  } else {
    grupo <- grupo + 1
    ref <- i + 1}
  output[i] <- grupo
}

pnadMerge <- pnadc_receita_recorte3 %>% mutate(grupo = output,
                                               populacao_acumulada = cumsum(peso_dom_comcalib),
                                               grupo_receita = 100 - grupo)
pnadc_receita_recorte3 <- full_join(pnadMerge, receitaCents,
                                     by = c("grupo_receita" = "Centésimo"), keep=TRUE)

mensaliza_e_individualiza <- function(var){
  (var/12)/384168
}
pnadc_receita_recorte3 <- pnadc_receita_recorte3 %>%
  group_by(grupo_receita) %>%
  mutate(
    
    renda_anual_todasfontes_centesimo = sum(rendimento_todasfontes*peso_dom_comcalib)*12,
    
    fator_expansão = `Renda Total`/renda_anual_todasfontes_centesimo,
    
    rendimento_todasfontes_calibrado = if_else(fator_expansão >= 1, rendimento_todasfontes*fator_expansão,
                                               rendimento_todasfontes),
    
    across(.cols = c(`Renda Total`:`Dívida`),
           .fns = mensaliza_e_individualiza)
    
  ) %>%
  rename(peso_comcalib = peso_dom_comcalib) %>%
  select(id_ind, peso_comcalib, UF, rendimento_todasfontes, rendimento_todasfontes_calibrado, 
         Centésimo, grupo_receita, fator_expansão,
         `Renda Total`:`Dívida`) %>%
  ungroup()

# Dataset Final -----------------------------------------------------------

pnadReceita <- bind_rows(pnadc_receita_recorte1, pnadc_receita_recorte2, pnadc_receita_recorte3)
pnadReceita <- pnadReceita %>% mutate(rendimento_todasfontes_calibrado = coalesce(rendimento_todasfontes_calibrado, rendimento_todasfontes))
saveRDS(pnadReceita, paste0(saveDir, "pnadReceita2022.rds"))
# Descrição da Base -------------------------------------------------------
cores_made <- c("#45ff66", "#eb52ff", "#3366ff","#feff41")
pnadReceita <- pnadReceita %>% mutate(centis_no_receita = weighted_ntile(rendimento_todasfontes, peso_comcalib, 100))
pnadReceita$renda_irpfepnad <- coalesce(pnadReceita$rendimento_todasfontes_calibrado, pnadReceita$rendimento_todasfontes)
pnadReceita <- pnadReceita %>% mutate(centis_receita = weighted_ntile(renda_irpfepnad, peso_comcalib, 100))

renda_media_com_receita <- renda_media_com_receita %>% rename(centis = centis_receita)
renda_media_sem_receita <- renda_media_sem_receita %>% rename(centis = centis_no_receita)

df_final <- renda_media_com_receita %>% left_join(renda_media_sem_receita, by="centis")
df_long <- df_final %>%
  pivot_longer(
    cols = -centis,
    names_to = "Base de Dados",
    values_to = "Renda"
  )

p <- ggplot(data = df_long, aes(x=centis, y=Renda, color = `Base de Dados`)) + 
  geom_line() + 
  scale_color_manual(values=c("Renda Média Com Correção"="#eb52ff",
                              "Renda Média Sem Correção"="#3366ff")) +
  theme_classic() + 
  theme(legend.title = element_blank()) + 
  xlab("Centil na Distribuição de Renda")

giniAntes <- acid::weighted.gini(pnadReceita$rendimento_todasfontes, pnadReceita$peso_comcalib)
giniDepois <- acid::weighted.gini(pnadReceita$rendimento_todasfontes_calibrado, pnadReceita$peso_comcalib)
