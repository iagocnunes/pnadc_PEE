setwd("C:/XXXX")
rm(list = ls())
options(survey.lonely.psu = "adjust")
options(OutDec=",")
library(dplyr);library(PNADcIBGE);library(survey);library(srvyr)

dados_pnadc <- read_pnadc ("PNADC_2012_visita1.txt", "input_PNADC_2012_a_2014_visita1.txt")
dados_pnadc <- pnadc_labeller (dados_pnadc,
                               "dicionario_PNADC_microdados_2012_a_2014_visita1.xls")
dados_pnadc$one <- 1

dados_pnadc %>%
  select(V2008, V20081, V20082, V2009) %>%
  mutate(data_nasc = as.Date(paste0(V2008, "/", V20081, "/", V20082), format = "%d/%m/%Y"),
         idade = as.integer(as.Date("31/03/2012", format = "%d/%m/%Y") - data_nasc) / 365.25) -> idade_
dados_pnadc_pee <- cbind(dados_pnadc,idade_)
rm (dados_pnadc, idade_)
dados_pnadc_pee <- dados_pnadc_pee[, !duplicated(colnames(dados_pnadc_pee))]
dados_pnadc_pee <- as_tibble(dados_pnadc_pee)
dados_pnadc_pee$idade <- ifelse(is.na(dados_pnadc_pee$idade), dados_pnadc_pee$V2009, dados_pnadc_pee$idade)
dados_pnadc_pee$idade <- as.integer(dados_pnadc_pee$idade)
# Dummies ***antes da calibração***
#2016 a 2019:
dados_pnadc_pee$EM_regular1 <- ifelse(dados_pnadc_pee$V3003A=="Regular do ensino médio",1,0)
table(dados_pnadc_pee$EM_regular1)
dados_pnadc_pee$EM_EJA1 <- ifelse(dados_pnadc_pee$V3003A=="Educação de jovens e adultos (EJA) do ensino médio",1,0)
table(dados_pnadc_pee$EM_EJA1)
#2012 a 2014 [2015=3003A e 3003]:
dados_pnadc_pee$EM_regular2 <- ifelse(dados_pnadc_pee$V3003=="Regular do ensino médio",1,0)
table(dados_pnadc_pee$EM_regular2)
dados_pnadc_pee$EM_EJA2 <- ifelse(dados_pnadc_pee$V3003=="Educação de jovens e adultos (EJA) ou supletivo do ensino médio",1,0)
table(dados_pnadc_pee$EM_EJA2)

pnadc_plano <-     
  svydesign(            
    ids = ~ UPA,
    strata = ~ Estrato,
    weights = ~ V1031,
    data = dados_pnadc_pee,
    nest = TRUE
  )

df_pos <- data.frame(posest = unique(dados_pnadc_pee$posest), Freq = unique(dados_pnadc_pee$V1030))
pnadc_calib <- postStratify(pnadc_plano, ~posest, df_pos)
pnadc_fatores = weights(pnadc_calib) / weights(pnadc_plano)
boxplot(pnadc_fatores, horizontal = TRUE, xlab="Fatores de calibração")
saveRDS(pnadc_calib,"pnadc_calib_2012")
rm(list = ls())

pnadc_calib <- readRDS(file="pnadc_calib_2012") 
pnadc_calib <- as_survey_design(pnadc_calib)
options(scipen=999)
svytotal(~ one, pnadc_calib)

# META 3A
pop1517_est <- svyby(~VD3004=="Médio completo ou equivalente"|VD3004=="Superior incompleto ou equivalente"|VD3004=="Superior completo", ~UF, subset(pnadc_calib, idade>=15&idade<=17), svytotal, na.rm=T)
ftable(pop1517_est)
pop1517_est1<- svyby(~V3002=="Sim", ~UF, subset(pnadc_calib,idade>=15&idade<=17), svytotal, na.rm=T)
ftable(pop1517_est1)
rm(pop1517_est, pop1517_est1)

# META 3B
#2016 a 2019:
pop1517_est1<- svyby(~EM_regular1 + EM_EJA1, ~UF, subset(pnadc_calib,idade>=15&idade<=17), svytotal, na.rm=T)
ftable(pop1517_est1)
#2012 a 2014 [2015=3003A e 3003]:
pop1517_est0<- svyby(~EM_regular2 + EM_EJA2, ~UF, subset(pnadc_calib,idade>=15&idade<=17), svytotal, na.rm=T)
ftable(pop1517_est0)

pop1517_est2 <- svyby(~VD3004=="Médio completo ou equivalente"|VD3004=="Superior incompleto ou equivalente"|VD3004=="Superior completo", ~UF, subset(pnadc_calib, idade>=15&idade<=17), svytotal, na.rm=T)
ftable(pop1517_est2)
rm (pop1517_est1, pop1517_est2, pop1517_est0)