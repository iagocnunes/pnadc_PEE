setwd("C:/XXXX")
rm(list = ls())
options(survey.lonely.psu = "adjust")
options(OutDec=",")
library(dplyr);library(PNADcIBGE);library(survey);library(srvyr)

dados_pnadc <- read_pnadc ("PNADC_022020.txt", "input_PNADC_trimestral.txt")
dados_pnadc <- pnadc_labeller (dados_pnadc,
                               "dicionario_PNADC_microdados_trimestral.xls")
dados_pnadc$one <- 1
# 2012 a 2015
dados_pnadc$freq12ba <- ifelse(grepl("Superior - gradua��o|Mestrado|Doutorado", dados_pnadc$V3003), 1, 0)
dados_pnadc$freq12bb <- ifelse(grepl("Mestrado|Doutorado", dados_pnadc$V3009), 1, 0)
dados_pnadc$freq12bc <- ifelse(grepl("Superior - gradua��o", dados_pnadc$V3009), ifelse(grepl("Sim", dados_pnadc$V3014), 1, 0), 0)
dados_pnadc$V12B <- ifelse(grepl(1, dados_pnadc$freq12ba) | grepl(1, dados_pnadc$freq12bb) | grepl(1, dados_pnadc$freq12bc), 1, 0)
# 2016 a 2020
dados_pnadc$freq12ba <- ifelse(grepl("Superior - gradua��o|Especializa��o de n�vel superior|Mestrado|Doutorado", dados_pnadc$V3003A), 1, 0)
dados_pnadc$freq12bb <- ifelse(grepl("Especializa��o de n�vel superior|Mestrado|Doutorado", dados_pnadc$V3009A), 1, 0)
dados_pnadc$freq12bc <- ifelse(grepl("Superior - gradua��o", dados_pnadc$V3009A), ifelse(grepl("Sim", dados_pnadc$V3014), 1, 0), 0)
dados_pnadc$V12B <- ifelse(grepl(1, dados_pnadc$freq12ba) | grepl(1, dados_pnadc$freq12bb) | grepl(1, dados_pnadc$freq12bc), 1, 0)

pnadc_plano <-     
  svydesign(            
    ids = ~ UPA,
    strata = ~ Estrato,
    weights = ~ V1027,
    data = dados_pnadc,
    nest = TRUE
  )
df_pos <- data.frame(posest = unique(dados_pnadc$posest), Freq = unique(dados_pnadc$V1029))
pnadc_calib <- postStratify(pnadc_plano, ~posest, df_pos)
pnadc_fatores = weights(pnadc_calib) / weights(pnadc_plano)
boxplot(pnadc_fatores, horizontal = TRUE, xlab="Fatores de calibra��o")
saveRDS(pnadc_calib,"pnadc_calib_2020")

rm(list = ls())

pnadc_calib <- readRDS(file="pnadc_calib_2020") 
pnadc_calib <- as_survey_design(pnadc_calib)
options(scipen=999)
svytotal(~ one, pnadc_calib)

### Meta 12A

svyby(~V2009>=18&V2009<=24, ~UF, pnadc_calib, svytotal, na.rm=T)
svytotal(~V2009>=18&V2009<=24, pnadc_calib, na.rm=T)
#2012 a 2015
svyby(~V3003=="Superior - gradua��o", ~UF, pnadc_calib, svytotal, na.rm=T)
svytotal(~V3003=="Superior - gradua��o", pnadc_calib, na.rm=T)
#2016 a 2019:
svyby(~V3003A=="Superior - gradua��o", ~UF, pnadc_calib, svytotal, na.rm=T)
svytotal(~V3003A=="Superior - gradua��o", pnadc_calib, na.rm=T)

svytotal(~V2009>=18&V2009<=24, subset(pnadc_calib, UF=="Rond�nia"|UF=="Acre"|UF=="Amazonas"|UF=="Roraima"|UF=="Par�"|UF=="Amap�"|UF=="Tocantins"), na.rm=T)
svytotal(~V3003A=="Superior - gradua��o", subset(pnadc_calib, UF=="Rond�nia"|UF=="Acre"|UF=="Amazonas"|UF=="Roraima"|UF=="Par�"|UF=="Amap�"|UF=="Tocantins"), na.rm=T)
svytotal(~V2009>=18&V2009<=24, subset(pnadc_calib, UF=="Maranh�o"|UF=="Piau�"|UF=="Cear�"|UF=="Rio Grande do Norte"|UF=="Para�ba"|UF=="Pernambuco"|UF=="Alagoas"|UF=="Sergipe"|UF=="Bahia"), na.rm=T)
svytotal(~V3003A=="Superior - gradua��o", subset(pnadc_calib, UF=="Maranh�o"|UF=="Piau�"|UF=="Cear�"|UF=="Rio Grande do Norte"|UF=="Para�ba"|UF=="Pernambuco"|UF=="Alagoas"|UF=="Sergipe"|UF=="Bahia"), na.rm=T)
svytotal(~V2009>=18&V2009<=24, subset(pnadc_calib, UF=="Minas Gerais"|UF=="Esp�rito Santo"|UF=="Rio de Janeiro"|UF=="S�o Paulo"), na.rm=T)
svytotal(~V3003A=="Superior - gradua��o", subset(pnadc_calib, UF=="Minas Gerais"|UF=="Esp�rito Santo"|UF=="Rio de Janeiro"|UF=="S�o Paulo"), na.rm=T)
svytotal(~V2009>=18&V2009<=24, subset(pnadc_calib, UF=="Paran�"|UF=="Santa Catarina"|UF=="Rio Grande do Sul"), na.rm=T)
svytotal(~V3003A=="Superior - gradua��o", subset(pnadc_calib, UF=="Paran�"|UF=="Santa Catarina"|UF=="Rio Grande do Sul"), na.rm=T)
svytotal(~V2009>=18&V2009<=24, subset(pnadc_calib, UF=="Mato Grosso do Sul"|UF=="Mato Grosso"|UF=="Goi�s"|UF=="Distrito Federal"), na.rm=T)
svytotal(~V3003A=="Superior - gradua��o", subset(pnadc_calib, UF=="Mato Grosso do Sul"|UF=="Mato Grosso"|UF=="Goi�s"|UF=="Distrito Federal"), na.rm=T)

# 12B
pnadc1824 <- subset(pnadc_calib, V2009>=18&V2009<=24)

svytotal(~UF, pnadc1824, na.rm=T)
svyby(~V12B, ~UF, pnadc1824, svytotal, na.rm=T)
svytotal(~one, pnadc1824, na.rm=T)
svytotal(~V12B, pnadc1824, na.rm=T)

svytotal(~one, subset(pnadc1824, UF=="Rond�nia"|UF=="Acre"|UF=="Amazonas"|UF=="Roraima"|UF=="Par�"|UF=="Amap�"|UF=="Tocantins"), na.rm=T)
svytotal(~V12B, subset(pnadc1824, UF=="Rond�nia"|UF=="Acre"|UF=="Amazonas"|UF=="Roraima"|UF=="Par�"|UF=="Amap�"|UF=="Tocantins"), na.rm=T)
svytotal(~one, subset(pnadc1824, UF=="Maranh�o"|UF=="Piau�"|UF=="Cear�"|UF=="Rio Grande do Norte"|UF=="Para�ba"|UF=="Pernambuco"|UF=="Alagoas"|UF=="Sergipe"|UF=="Bahia"), na.rm=T)
svytotal(~V12B, subset(pnadc1824, UF=="Maranh�o"|UF=="Piau�"|UF=="Cear�"|UF=="Rio Grande do Norte"|UF=="Para�ba"|UF=="Pernambuco"|UF=="Alagoas"|UF=="Sergipe"|UF=="Bahia"), na.rm=T)
svytotal(~one, subset(pnadc1824, UF=="Minas Gerais"|UF=="Esp�rito Santo"|UF=="Rio de Janeiro"|UF=="S�o Paulo"), na.rm=T)
svytotal(~V12B, subset(pnadc1824, UF=="Minas Gerais"|UF=="Esp�rito Santo"|UF=="Rio de Janeiro"|UF=="S�o Paulo"), na.rm=T)
svytotal(~one, subset(pnadc1824, UF=="Paran�"|UF=="Santa Catarina"|UF=="Rio Grande do Sul"), na.rm=T)
svytotal(~V12B, subset(pnadc1824, UF=="Paran�"|UF=="Santa Catarina"|UF=="Rio Grande do Sul"), na.rm=T)
svytotal(~one, subset(pnadc1824, UF=="Mato Grosso do Sul"|UF=="Mato Grosso"|UF=="Goi�s"|UF=="Distrito Federal"), na.rm=T)
svytotal(~V12B, subset(pnadc1824, UF=="Mato Grosso do Sul"|UF=="Mato Grosso"|UF=="Goi�s"|UF=="Distrito Federal"), na.rm=T)
