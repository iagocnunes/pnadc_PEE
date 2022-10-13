## Autor: Iago de Carvalho Nunes
## Contato: github.com/iagocnunes // iagodcn@gmail.com
setwd("C:/XXXX")
rm(list = ls())
options(survey.lonely.psu = "adjust")
options(OutDec=",")
library(tidyverse);library(PNADcIBGE);library(survey);library(srvyr);library(openxlsx)

dados_pnadc <- read_pnadc ("PNADC_2019_trimestre2.txt", "input_PNADC_trimestre2_20220801.txt")
dados_pnadc <- pnadc_labeller (dados_pnadc,
                               "dicionario_PNADC_microdados_trimestre2_20220801.xls")
dados_pnadc$one <- 1

dados_pnadc <- dados_pnadc %>%
  mutate(data_nasc = as.Date(paste0(V2008, "/", V20081, "/", V20082), format = "%d/%m/%Y"),
         idade = as.integer(as.Date("31/03/2019", format = "%d/%m/%Y") - data_nasc) / 365.25,
         idade = ifelse(is.na(idade), V2009, idade),
         idade = as.integer(idade))

pnadc_plano <- pnadc_design(dados_pnadc)
rm(dados_pnadc)
options(scipen=999)

# validar expansão da amostra https://painel.ibge.gov.br/pnadc/
svytotal(~ one, pnadc_plano, na.rm=T)

# META 1A
pop0405_est <- svyby(~V3002=="Sim", ~UF, subset(pnadc_plano, idade >= 4 & idade <= 5), svytotal, na.rm = T)
ftable(pop0405_est)
pop0405_est$perc <- pop0405_est$`V3002 == \"Sim\"TRUE`/(pop0405_est$`V3002 == \"Sim\"TRUE`+pop0405_est$`V3002 == \"Sim\"FALSE`)*100
write.xlsx(pop0405_est,'V1A_2019_est.xlsx')

pop0405_br <- svytotal(~V3002=="Sim", subset(pnadc_plano, idade >= 4 & idade <= 5), na.rm=T)
pop0405_br <- as.data.frame(pop0405_br)
pop0405_br <- as.data.frame(t(pop0405_br))
pop0405_br <- as.data.frame(pop0405_br[1,])
pop0405_br$perc <- pop0405_br$`V3002 == \"Sim\"TRUE`/(pop0405_br$`V3002 == \"Sim\"FALSE`+pop0405_br$`V3002 == \"Sim\"TRUE`)*100
pop0405_br$reg <- "Brasil"
pop0405_br <- pop0405_br %>% rename(V3002_NAO='V3002 == \"Sim\"FALSE', V3002_SIM='V3002 == \"Sim\"TRUE')

pop0405_nort <- svyby(~V3002=="Sim",~idade >= 4 & idade <= 5, subset(pnadc_plano, UF=="Rondônia"|UF=="Acre"|UF=="Amazonas"|UF=="Roraima"|UF=="Pará"|UF=="Amapá"|UF=="Tocantins"), svytotal, na.rm=T)
pop0405_nort <- as.data.frame(pop0405_nort[2,])
pop0405_nort$perc <- pop0405_nort$`V3002 == \"Sim\"TRUE`/(pop0405_nort$`V3002 == \"Sim\"FALSE`+pop0405_nort$`V3002 == \"Sim\"TRUE`)*100
pop0405_nort$reg <- "Norte"
pop0405_nort <- pop0405_nort %>%  
  rename(V3002_NAO='V3002 == \"Sim\"FALSE', V3002_SIM='V3002 == \"Sim\"TRUE') %>% 
  select(V3002_NAO,V3002_SIM, perc, reg)

pop0405_ne <- svyby(~V3002=="Sim",~idade >= 4 & idade <= 5, subset(pnadc_plano, UF=="Maranhão"|UF=="Piauí"|UF=="Ceará"|UF=="Rio Grande do Norte"|UF=="Paraíba"|UF=="Pernambuco"|UF=="Alagoas"|UF=="Sergipe"|UF=="Bahia"), svytotal, na.rm=T)
pop0405_ne <- as.data.frame(pop0405_ne[2,])
pop0405_ne$perc <- pop0405_ne$`V3002 == \"Sim\"TRUE`/(pop0405_ne$`V3002 == \"Sim\"FALSE`+pop0405_ne$`V3002 == \"Sim\"TRUE`)*100
pop0405_ne$reg <- "Nordeste"
pop0405_ne <- pop0405_ne %>%  
  rename(V3002_NAO='V3002 == \"Sim\"FALSE', V3002_SIM='V3002 == \"Sim\"TRUE') %>% 
  select(V3002_NAO,V3002_SIM, perc, reg)

pop0405_se <- svyby(~V3002=="Sim",~idade >= 4 & idade <= 5, subset(pnadc_plano, UF=="Minas Gerais"|UF=="Espírito Santo"|UF=="Rio de Janeiro"|UF=="São Paulo"), svytotal, na.rm=T)
pop0405_se <- as.data.frame(pop0405_se[2,])
pop0405_se$perc <- pop0405_se$`V3002 == \"Sim\"TRUE`/(pop0405_se$`V3002 == \"Sim\"FALSE`+pop0405_se$`V3002 == \"Sim\"TRUE`)*100
pop0405_se$reg <- "Sudeste"
pop0405_se <- pop0405_se %>%  
  rename(V3002_NAO='V3002 == \"Sim\"FALSE', V3002_SIM='V3002 == \"Sim\"TRUE') %>% 
  select(V3002_NAO,V3002_SIM, perc, reg)

pop0405_sul <- svyby(~V3002=="Sim",~idade >= 4 & idade <= 5, subset(pnadc_plano, UF=="Paraná"|UF=="Santa Catarina"|UF=="Rio Grande do Sul"), svytotal, na.rm=T)
pop0405_sul <- as.data.frame(pop0405_sul[2,])
pop0405_sul$perc <- pop0405_sul$`V3002 == \"Sim\"TRUE`/(pop0405_sul$`V3002 == \"Sim\"FALSE`+pop0405_sul$`V3002 == \"Sim\"TRUE`)*100
pop0405_sul$reg <- "Sul"
pop0405_sul <- pop0405_sul %>%  
  rename(V3002_NAO='V3002 == \"Sim\"FALSE', V3002_SIM='V3002 == \"Sim\"TRUE') %>% 
  select(V3002_NAO,V3002_SIM, perc, reg)

pop0405_co <- svyby(~V3002=="Sim",~idade >= 4 & idade <= 5, subset(pnadc_plano, UF=="Mato Grosso do Sul"|UF=="Mato Grosso"|UF=="Goiás"|UF=="Distrito Federal"), svytotal, na.rm=T)
pop0405_co <- as.data.frame(pop0405_co[2,])
pop0405_co$perc <- pop0405_co$`V3002 == \"Sim\"TRUE`/(pop0405_co$`V3002 == \"Sim\"FALSE`+pop0405_co$`V3002 == \"Sim\"TRUE`)*100
pop0405_co$reg <- "Centro-Oeste"
pop0405_co <- pop0405_co %>%  
  rename(V3002_NAO='V3002 == \"Sim\"FALSE', V3002_SIM='V3002 == \"Sim\"TRUE') %>% 
  select(V3002_NAO,V3002_SIM, perc, reg)

pop0405_reg <- rbind(pop0405_br, pop0405_nort, pop0405_ne, pop0405_se, pop0405_sul, pop0405_co)
write.xlsx(pop0405_reg,'V1A_2019_reg.xlsx')
rm (pop0405_br, pop0405_nort, pop0405_ne, pop0405_se, pop0405_sul, pop0405_co, pop0405_reg, pop0405_est)

# META 1B
pop0003_est <- svyby(~V3002=="Sim", ~UF, subset(pnadc_plano, idade >= 0 & idade <= 3), svytotal, na.rm = T)
ftable (pop0003_est)
pop0003_est$perc <- pop0003_est$`V3002 == \"Sim\"TRUE`/(pop0003_est$`V3002 == \"Sim\"TRUE`+pop0003_est$`V3002 == \"Sim\"FALSE`)*100
write.xlsx(pop0003_est,'V1B_2019_est.xlsx')
rm(pop0003_est)

pop0003_br <- svytotal(~V3002=="Sim", subset(pnadc_plano, idade >= 0 & idade <= 3), na.rm=T)
pop0003_br <- as.data.frame(pop0003_br)
pop0003_br <- as.data.frame(t(pop0003_br))
pop0003_br <- as.data.frame(pop0003_br[1,])
pop0003_br$perc <- pop0003_br$`V3002 == \"Sim\"TRUE`/(pop0003_br$`V3002 == \"Sim\"FALSE`+pop0003_br$`V3002 == \"Sim\"TRUE`)*100
pop0003_br$reg <- "Brasil"
pop0003_br <- pop0003_br %>% rename(V3002_NAO='V3002 == \"Sim\"FALSE', V3002_SIM='V3002 == \"Sim\"TRUE')

pop0003_nort <- svyby(~V3002=="Sim",~idade >= 0 & idade <= 3, subset(pnadc_plano, UF=="Rondônia"|UF=="Acre"|UF=="Amazonas"|UF=="Roraima"|UF=="Pará"|UF=="Amapá"|UF=="Tocantins"), svytotal, na.rm=T)
pop0003_nort <- as.data.frame(pop0003_nort[2,])
pop0003_nort$perc <- pop0003_nort$`V3002 == \"Sim\"TRUE`/(pop0003_nort$`V3002 == \"Sim\"FALSE`+pop0003_nort$`V3002 == \"Sim\"TRUE`)*100
pop0003_nort$reg <- "Norte"
pop0003_nort <- pop0003_nort %>%  
  rename(V3002_NAO='V3002 == \"Sim\"FALSE', V3002_SIM='V3002 == \"Sim\"TRUE') %>% 
  select(V3002_NAO,V3002_SIM, perc, reg)

pop0003_ne <- svyby(~V3002=="Sim",~idade >= 0 & idade <= 3, subset(pnadc_plano, UF=="Maranhão"|UF=="Piauí"|UF=="Ceará"|UF=="Rio Grande do Norte"|UF=="Paraíba"|UF=="Pernambuco"|UF=="Alagoas"|UF=="Sergipe"|UF=="Bahia"), svytotal, na.rm=T)
pop0003_ne <- as.data.frame(pop0003_ne[2,])
pop0003_ne$perc <- pop0003_ne$`V3002 == \"Sim\"TRUE`/(pop0003_ne$`V3002 == \"Sim\"FALSE`+pop0003_ne$`V3002 == \"Sim\"TRUE`)*100
pop0003_ne$reg <- "Nordeste"
pop0003_ne <- pop0003_ne %>%  
  rename(V3002_NAO='V3002 == \"Sim\"FALSE', V3002_SIM='V3002 == \"Sim\"TRUE') %>% 
  select(V3002_NAO,V3002_SIM, perc, reg)

pop0003_se <- svyby(~V3002=="Sim",~idade >= 0 & idade <= 3, subset(pnadc_plano, UF=="Minas Gerais"|UF=="Espírito Santo"|UF=="Rio de Janeiro"|UF=="São Paulo"), svytotal, na.rm=T)
pop0003_se <- as.data.frame(pop0003_se[2,])
pop0003_se$perc <- pop0003_se$`V3002 == \"Sim\"TRUE`/(pop0003_se$`V3002 == \"Sim\"FALSE`+pop0003_se$`V3002 == \"Sim\"TRUE`)*100
pop0003_se$reg <- "Sudeste"
pop0003_se <- pop0003_se %>%  
  rename(V3002_NAO='V3002 == \"Sim\"FALSE', V3002_SIM='V3002 == \"Sim\"TRUE') %>% 
  select(V3002_NAO,V3002_SIM, perc, reg)

pop0003_sul <- svyby(~V3002=="Sim",~idade >= 0 & idade <= 3, subset(pnadc_plano, UF=="Paraná"|UF=="Santa Catarina"|UF=="Rio Grande do Sul"), svytotal, na.rm=T)
pop0003_sul <- as.data.frame(pop0003_sul[2,])
pop0003_sul$perc <- pop0003_sul$`V3002 == \"Sim\"TRUE`/(pop0003_sul$`V3002 == \"Sim\"FALSE`+pop0003_sul$`V3002 == \"Sim\"TRUE`)*100
pop0003_sul$reg <- "Sul"
pop0003_sul <- pop0003_sul %>%  
  rename(V3002_NAO='V3002 == \"Sim\"FALSE', V3002_SIM='V3002 == \"Sim\"TRUE') %>% 
  select(V3002_NAO,V3002_SIM, perc, reg)

pop0003_co <- svyby(~V3002=="Sim",~idade >= 0 & idade <= 3, subset(pnadc_plano, UF=="Mato Grosso do Sul"|UF=="Mato Grosso"|UF=="Goiás"|UF=="Distrito Federal"), svytotal, na.rm=T)
pop0003_co <- as.data.frame(pop0003_co[2,])
pop0003_co$perc <- pop0003_co$`V3002 == \"Sim\"TRUE`/(pop0003_co$`V3002 == \"Sim\"FALSE`+pop0003_co$`V3002 == \"Sim\"TRUE`)*100
pop0003_co$reg <- "Centro-Oeste"
pop0003_co <- pop0003_co %>%  
  rename(V3002_NAO='V3002 == \"Sim\"FALSE', V3002_SIM='V3002 == \"Sim\"TRUE') %>% 
  select(V3002_NAO,V3002_SIM, perc, reg)

pop0003_reg <- rbind(pop0003_br, pop0003_nort, pop0003_ne, pop0003_se, pop0003_sul, pop0003_co)
write.xlsx(pop0003_reg,'V1B_2019_reg.xlsx')
rm(pop0003_br, pop0003_nort, pop0003_ne, pop0003_se, pop0003_sul, pop0003_co,pop0003_reg,pnadc_plano)

