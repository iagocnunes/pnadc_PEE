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
#2012 a 2014 [2015= V3003A e V3003]:
dados_pnadc_pee$EF_regular2 <- ifelse(dados_pnadc_pee$V3003=="Regular do ensino fundamental",1,0)
table(dados_pnadc_pee$EF_regular2)
dados_pnadc_pee$EF_EJA2 <- ifelse(dados_pnadc_pee$V3003=="Educação de jovens e adultos (EJA) ou supletivo do ensino fundamental",1,0)
table(dados_pnadc_pee$EF_EJA2)
#2016 a 2019:
dados_pnadc_pee$EF_regular1 <- ifelse(dados_pnadc_pee$V3003A=="Regular do ensino fundamental",1,0)
table(dados_pnadc_pee$EF_regular1)
dados_pnadc_pee$EF_EJA1 <- ifelse(dados_pnadc_pee$V3003A=="Educação de jovens e adultos (EJA) do ensino fundamental",1,0)
table(dados_pnadc_pee$EF_EJA1)

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

# META 2A

#2016 a 2019:
pop0614_est1<- svyby(~EF_regular1 + EF_EJA1, ~UF, subset(pnadc_calib,idade>=6&idade<=14), svytotal, na.rm=T)
ftable(pop0614_est1)
pop0614_est2 <- svyby(~V3003A=="Regular do ensino médio"|V3003A=="Educação de jovens e adultos (EJA) do ensino médio"|V3003A=="Superior - graduação"|V3003A=="Especialização de nível superior"|V3003A=="Mestrado"|V3003A=="Doutorado", ~UF, subset(pnadc_calib, idade>=6&idade<=14), svytotal, na.rm=T)
ftable(pop0614_est2)
#2012 a 2014 [2015=3003A e 3003]:
pop0614_est0<- svyby(~EF_regular2 + EF_EJA2, ~UF, subset(pnadc_calib,idade>=6&idade<=14), svytotal, na.rm=T)
ftable(pop0614_est0)
pop0614_est3 <- svyby(~V3003=="Regular do ensino médio"|V3003=="Educação de jovens e adultos (EJA) do ensino médio"|V3003=="Superior - graduação"|V3003=="Especialização de nível superior"|V3003=="Mestrado"|V3003=="Doutorado", ~UF, subset(pnadc_calib, idade>=6&idade<=14), svytotal, na.rm=T)
ftable(pop0614_est3)

rm (pop0614, pop0614_est0, pop0614_est1, pop0614_est2, pop0614_est3)

# META 2B

# PEE: Percentual de alunos de 6 a 14 anos que concluíram essa etapa na idade recomendada #

#2016 a 2019:
pop16_est <- svyby(~V3003A=="Regular do ensino médio"|V3003A=="Educação de jovens e adultos (EJA) do ensino médio"|V3003A=="Superior - graduação"|V3003A=="Especialização de nível superior"|V3003A=="Mestrado"|V3003A=="Doutorado", ~UF, subset(pnadc_calib, idade>=6&idade<=14), svytotal, na.rm=T)
ftable(pop0614_est)
#2012 a 2014 [2015=3003A e 3003]:
pop16_est0 <- svyby(~V3003=="Regular do ensino médio"|V3003=="Educação de jovens e adultos (EJA) do ensino médio"|V3003=="Superior - graduação"|V3003=="Especialização de nível superior"|V3003=="Mestrado"|V3003=="Doutorado", ~UF, subset(pnadc_calib, idade>=6&idade<=14), svytotal, na.rm=T)
ftable(pop0614_est0)
rm (pop0614_est, pop0614_est0)

# Relatório: percentual de jovens de 16 anos de idade que concluíram o ensino fundamental

#2016 a 2019:
pop16_est <- svyby(~V3003A=="Regular do ensino médio"|V3003A=="Educação de jovens e adultos (EJA) do ensino médio"|V3003A=="Superior - graduação"|V3003A=="Especialização de nível superior"|V3003A=="Mestrado"|V3003A=="Doutorado", ~UF, subset(pnadc_calib, idade==16), svytotal, na.rm=T)
ftable(pop16_est)
#2012 a 2014 [2015=3003A e 3003]:
pop16_est0 <- svyby(~V3003=="Regular do ensino médio"|V3003=="Educação de jovens e adultos (EJA) do ensino médio"|V3003=="Superior - graduação"|V3003=="Especialização de nível superior"|V3003=="Mestrado"|V3003=="Doutorado", ~UF, subset(pnadc_calib, idade==16), svytotal, na.rm=T)
ftable(pop16_est0)
rm (pop16_est, pop16_est0)
