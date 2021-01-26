setwd("C:/XXXX")
rm(list = ls())
options(survey.lonely.psu = "adjust")
options(OutDec=",")
library(dplyr);library(PNADcIBGE);library(survey);library(srvyr)

dados_pnadc <- read_pnadc ("PNADC_2019_trimestre2.txt", "input_pnadc.txt")
dados_pnadc <- pnadc_labeller (dados_pnadc,
                               "dicionario_PNADC_microdados_trimestre2_20200716.xls")
dados_pnadc$one <- 1

dados_pnadc %>%
  select(V2008, V20081, V20082, V2009) %>%
  mutate(data_nasc = as.Date(paste0(V2008, "/", V20081, "/", V20082), format = "%d/%m/%Y"),
         idade = as.integer(as.Date("31/03/2019", format = "%d/%m/%Y") - data_nasc) / 365.25) -> idade_
dados_pnadc_pee <- cbind(dados_pnadc,idade_)
rm (dados_pnadc, idade_)
dados_pnadc_pee <- dados_pnadc_pee[, !duplicated(colnames(dados_pnadc_pee))]
dados_pnadc_pee <- as_tibble(dados_pnadc_pee)
dados_pnadc_pee$idade <- ifelse(is.na(dados_pnadc_pee$idade), dados_pnadc_pee$V2009, dados_pnadc_pee$idade)
dados_pnadc_pee$idade <- as.integer(dados_pnadc_pee$idade)

pnadc_plano <-     
  svydesign(            
    ids = ~ UPA,
    strata = ~ Estrato,
    weights = ~ V1027,
    data = dados_pnadc_pee,
    nest = TRUE
  )

df_pos <- data.frame(posest = unique(dados_pnadc_pee$posest), Freq = unique(dados_pnadc_pee$V1029))
pnadc_calib <- postStratify(pnadc_plano, ~posest, df_pos)
pnadc_fatores = weights(pnadc_calib) / weights(pnadc_plano)
boxplot(pnadc_fatores, horizontal = TRUE, xlab="Fatores de calibração")
saveRDS(pnadc_calib,"pnadc-edu_calib_2019")
rm(list = ls())

pnadc_calib <- readRDS(file="pnadc-edu_calib_2019") 
pnadc_calib <- as_survey_design(pnadc_calib)
options(scipen=999)
svytotal(~ one, pnadc_calib)

# META 1A
pop0405_est <- svyby(~V3002=="Sim", ~UF, subset(pnadc_calib, idade >= 4 & idade <= 5), svytotal, na.rm = T)
ftable(pop0405_est)
svytotal(~V3002=="Sim", subset(pnadc_calib, idade >= 4 & idade <= 5), na.rm=T)
svyby(~V3002=="Sim",~idade >= 4 & idade <= 5, subset(pnadc_calib, UF=="Rondônia"|UF=="Acre"|UF=="Amazonas"|UF=="Roraima"|UF=="Pará"|UF=="Amapá"|UF=="Tocantins"), svymean, na.rm=T)
svyby(~V3002=="Sim",~idade >= 4 & idade <= 5, subset(pnadc_calib, UF=="Maranhão"|UF=="Piauí"|UF=="Ceará"|UF=="Rio Grande do Norte"|UF=="Paraíba"|UF=="Pernambuco"|UF=="Alagoas"|UF=="Sergipe"|UF=="Bahia"), svymean, na.rm=T)
svyby(~V3002=="Sim",~idade >= 4 & idade <= 5, subset(pnadc_calib, UF=="Minas Gerais"|UF=="Espírito Santo"|UF=="Rio de Janeiro"|UF=="São Paulo"), svymean, na.rm=T)
svyby(~V3002=="Sim",~idade >= 4 & idade <= 5, subset(pnadc_calib, UF=="Paraná"|UF=="Santa Catarina"|UF=="Rio Grande do Sul"), svymean, na.rm=T)
svyby(~V3002=="Sim",~idade >= 4 & idade <= 5, subset(pnadc_calib, UF=="Mato Grosso do Sul"|UF=="Mato Grosso"|UF=="Goiás"|UF=="Distrito Federal"), svymean, na.rm=T)

rm (pop0405_est)

# META 1B
pop0003_est <- svyby(~V3002=="Sim", ~UF, subset(pnadc_calib, idade >= 0 & idade <= 3), svytotal, na.rm = T)
ftable (pop0003_est)
svytotal(~V3002=="Sim", subset(pnadc_calib, idade >= 0 & idade <= 3), na.rm=T)
svyby(~V3002=="Sim",~idade >= 0 & idade <= 3, subset(pnadc_calib, UF=="Rondônia"|UF=="Acre"|UF=="Amazonas"|UF=="Roraima"|UF=="Pará"|UF=="Amapá"|UF=="Tocantins"), svymean, na.rm=T)
svyby(~V3002=="Sim",~idade >= 0 & idade <= 3, subset(pnadc_calib, UF=="Maranhão"|UF=="Piauí"|UF=="Ceará"|UF=="Rio Grande do Norte"|UF=="Paraíba"|UF=="Pernambuco"|UF=="Alagoas"|UF=="Sergipe"|UF=="Bahia"), svymean, na.rm=T)
svyby(~V3002=="Sim",~idade >= 0 & idade <= 3, subset(pnadc_calib, UF=="Minas Gerais"|UF=="Espírito Santo"|UF=="Rio de Janeiro"|UF=="São Paulo"), svymean, na.rm=T)
svyby(~V3002=="Sim",~idade >= 0 & idade <= 3, subset(pnadc_calib, UF=="Paraná"|UF=="Santa Catarina"|UF=="Rio Grande do Sul"), svymean, na.rm=T)
svyby(~V3002=="Sim",~idade >= 0 & idade <= 3, subset(pnadc_calib, UF=="Mato Grosso do Sul"|UF=="Mato Grosso"|UF=="Goiás"|UF=="Distrito Federal"), svymean, na.rm=T)

rm(pop0003_est)