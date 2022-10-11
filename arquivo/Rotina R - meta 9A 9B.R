setwd("C:/XXXX")
rm(list = ls())
options(survey.lonely.psu = "adjust")
options(OutDec=",")
library(dplyr);library(PNADcIBGE);library(survey);library(srvyr)

dados_pnadc <- read_pnadc ("PNADC_2019_visita1.txt", "input_PNADC_2019_visita1_20200826.txt")
dados_pnadc <- pnadc_labeller (dados_pnadc,
                               "dicionario_PNADC_microdados_2019_visita1_20200826.xls")
dados_pnadc$one <- 1
pnadc_plano <-     
  svydesign(            
    ids = ~ UPA,
    strata = ~ Estrato,
    weights = ~ V1031,
    data = dados_pnadc,
    nest = TRUE
  )

df_pos <- data.frame(posest = unique(dados_pnadc$posest), Freq = unique(dados_pnadc$V1030))
pnadc_calib <- postStratify(pnadc_plano, ~posest, df_pos)
pnadc_fatores = weights(pnadc_calib) / weights(pnadc_plano)
boxplot(pnadc_fatores, horizontal = TRUE, xlab="Fatores de calibração")
saveRDS(pnadc_calib,"pnadc_calib_2019")
rm(list = ls())
pnadc_calib <- readRDS(file="pnadc_calib_2019") 
pnadc_calib <- as_survey_design(pnadc_calib)
options(scipen=999)
svytotal(~ one, pnadc_calib)

# META 9A
pnadc15 <- subset(pnadc_calib, V2009>=15)
svytotal(~UF, pnadc15, na.rm=T)
svyby(~V3001=="Sim", ~UF, pnadc15, svytotal, na.rm=T)
svytotal(~one, pnadc15, na.rm=T)
svytotal(~V3001=="Sim", pnadc15, na.rm=T)

svytotal(~V2009>=15, subset(pnadc_calib, UF=="Rondônia"|UF=="Acre"|UF=="Amazonas"|UF=="Roraima"|UF=="Pará"|UF=="Amapá"|UF=="Tocantins"), na.rm=T)
svyby(~V3001=="Sim", ~V2009>=15, subset(pnadc_calib, UF=="Rondônia"|UF=="Acre"|UF=="Amazonas"|UF=="Roraima"|UF=="Pará"|UF=="Amapá"|UF=="Tocantins"), svytotal, na.rm=T)
svytotal(~V2009>=15, subset(pnadc_calib, UF=="Maranhão"|UF=="Piauí"|UF=="Ceará"|UF=="Rio Grande do Norte"|UF=="Paraíba"|UF=="Pernambuco"|UF=="Alagoas"|UF=="Sergipe"|UF=="Bahia"), na.rm=T)
svyby(~V3001=="Sim", ~V2009>=15, subset(pnadc_calib, UF=="Maranhão"|UF=="Piauí"|UF=="Ceará"|UF=="Rio Grande do Norte"|UF=="Paraíba"|UF=="Pernambuco"|UF=="Alagoas"|UF=="Sergipe"|UF=="Bahia"), svytotal, na.rm=T)
svytotal(~V2009>=15, subset(pnadc_calib, UF=="Minas Gerais"|UF=="Espírito Santo"|UF=="Rio de Janeiro"|UF=="São Paulo"), na.rm=T)
svyby(~V3001=="Sim", ~V2009>=15, subset(pnadc_calib, UF=="Minas Gerais"|UF=="Espírito Santo"|UF=="Rio de Janeiro"|UF=="São Paulo"), svytotal, na.rm=T)
svytotal(~V2009>=15, subset(pnadc_calib, UF=="Paraná"|UF=="Santa Catarina"|UF=="Rio Grande do Sul"), na.rm=T)
svyby(~V3001=="Sim", ~V2009>=15, subset(pnadc_calib, UF=="Paraná"|UF=="Santa Catarina"|UF=="Rio Grande do Sul"), svytotal, na.rm=T)
svytotal(~V2009>=15, subset(pnadc_calib, UF=="Mato Grosso do Sul"|UF=="Mato Grosso"|UF=="Goiás"|UF=="Distrito Federal"), na.rm=T)
svyby(~V3001=="Sim", ~V2009>=15, subset(pnadc_calib, UF=="Mato Grosso do Sul"|UF=="Mato Grosso"|UF=="Goiás"|UF=="Distrito Federal"), svytotal, na.rm=T)

# META 9B
svytotal(~UF, pnadc15, na.rm=T)
svyby(~V3001=="Não"|VD3005=="Sem instrução e menos de 1 ano de estudo"|VD3005=="1 ano de estudo"|VD3005=="2 anos de estudo"|VD3005=="3 anos de estudo"|VD3005=="4 anos de estudo", ~UF, pnadc15, svytotal, na.rm= T)
svytotal(~one, pnadc15, na.rm=T)
svytotal(~V3001=="Não"|VD3005=="Sem instrução e menos de 1 ano de estudo"|VD3005=="1 ano de estudo"|VD3005=="2 anos de estudo"|VD3005=="3 anos de estudo"|VD3005=="4 anos de estudo", pnadc15, na.rm= T)

svytotal(~one, subset(pnadc15, UF=="Rondônia"|UF=="Acre"|UF=="Amazonas"|UF=="Roraima"|UF=="Pará"|UF=="Amapá"|UF=="Tocantins"), na.rm=T)
svytotal(~V3001=="Não"|VD3005=="Sem instrução e menos de 1 ano de estudo"|VD3005=="1 ano de estudo"|VD3005=="2 anos de estudo"|VD3005=="3 anos de estudo"|VD3005=="4 anos de estudo", subset(pnadc15, UF=="Rondônia"|UF=="Acre"|UF=="Amazonas"|UF=="Roraima"|UF=="Pará"|UF=="Amapá"|UF=="Tocantins"), na.rm=T)
svytotal(~one, subset(pnadc15, UF=="Maranhão"|UF=="Piauí"|UF=="Ceará"|UF=="Rio Grande do Norte"|UF=="Paraíba"|UF=="Pernambuco"|UF=="Alagoas"|UF=="Sergipe"|UF=="Bahia"), na.rm=T)
svytotal(~V3001=="Não"|VD3005=="Sem instrução e menos de 1 ano de estudo"|VD3005=="1 ano de estudo"|VD3005=="2 anos de estudo"|VD3005=="3 anos de estudo"|VD3005=="4 anos de estudo", subset(pnadc15, UF=="Maranhão"|UF=="Piauí"|UF=="Ceará"|UF=="Rio Grande do Norte"|UF=="Paraíba"|UF=="Pernambuco"|UF=="Alagoas"|UF=="Sergipe"|UF=="Bahia"), na.rm=T)
svytotal(~one, subset(pnadc15, UF=="Minas Gerais"|UF=="Espírito Santo"|UF=="Rio de Janeiro"|UF=="São Paulo"), na.rm=T)
svytotal(~V3001=="Não"|VD3005=="Sem instrução e menos de 1 ano de estudo"|VD3005=="1 ano de estudo"|VD3005=="2 anos de estudo"|VD3005=="3 anos de estudo"|VD3005=="4 anos de estudo", subset(pnadc15, UF=="Minas Gerais"|UF=="Espírito Santo"|UF=="Rio de Janeiro"|UF=="São Paulo"), na.rm=T)
svytotal(~one, subset(pnadc15, UF=="Paraná"|UF=="Santa Catarina"|UF=="Rio Grande do Sul"), na.rm=T)
svytotal(~V3001=="Não"|VD3005=="Sem instrução e menos de 1 ano de estudo"|VD3005=="1 ano de estudo"|VD3005=="2 anos de estudo"|VD3005=="3 anos de estudo"|VD3005=="4 anos de estudo", subset(pnadc15, UF=="Paraná"|UF=="Santa Catarina"|UF=="Rio Grande do Sul"), na.rm=T)
svytotal(~one, subset(pnadc15, UF=="Mato Grosso do Sul"|UF=="Mato Grosso"|UF=="Goiás"|UF=="Distrito Federal"), na.rm=T)
svytotal(~V3001=="Não"|VD3005=="Sem instrução e menos de 1 ano de estudo"|VD3005=="1 ano de estudo"|VD3005=="2 anos de estudo"|VD3005=="3 anos de estudo"|VD3005=="4 anos de estudo", subset(pnadc15, UF=="Mato Grosso do Sul"|UF=="Mato Grosso"|UF=="Goiás"|UF=="Distrito Federal"), na.rm=T)

