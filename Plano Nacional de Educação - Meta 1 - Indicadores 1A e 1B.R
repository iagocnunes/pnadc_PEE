## Autor: Iago de Carvalho Nunes
## Contato: github.com/iagocnunes // iagodcn@gmail.com
setwd("C:/XXXX")
rm(list = ls())
options(survey.lonely.psu = "adjust")
options(OutDec=",")
library(tidyverse);library(PNADcIBGE);library(survey);library(srvyr);library(openxlsx);library(data.table)

####----------------------------- CARREGANDO BASES -----------------------------####
####--------- ATENCAO: UNICA PARTE DA ROTINA QUE NAO ESTA AUTOMATIZADA ---------####

# acrescentar dados5_pnadc, dados6_pnadc, etc. quando surgirem dados novos
dados1_pnadc <- read_pnadc ("PNADC_2019_trimestre2.txt", "input_PNADC_trimestre2_20220801.txt")
dados1_pnadc <- pnadc_labeller (dados1_pnadc,
                               "dicionario_PNADC_microdados_trimestre2_20220801.xls")
dados2_pnadc <- read_pnadc ("PNADC_2018_trimestre2.txt", "input_PNADC_trimestre2_20220801.txt")
dados2_pnadc <- pnadc_labeller (dados2_pnadc,
                               "dicionario_PNADC_microdados_trimestre2_20220801.xls")
dados3_pnadc <- read_pnadc ("PNADC_2017_trimestre2.txt", "input_PNADC_trimestre2_20220801.txt")
dados3_pnadc <- pnadc_labeller (dados3_pnadc,
                               "dicionario_PNADC_microdados_trimestre2_20220801.xls")
dados4_pnadc <- read_pnadc ("PNADC_2016_trimestre2.txt", "input_PNADC_trimestre2_20220801.txt")
dados4_pnadc <- pnadc_labeller (dados4_pnadc,
                               "dicionario_PNADC_microdados_trimestre2_20220801.xls")

dados_names <- c("dados1_pnadc","dados2_pnadc","dados3_pnadc","dados4_pnadc")

###----- FIM DA PARTE NAO AUTOMATIZADA; DAQUI PRA BAIXO* EH SO APERTAR PLAY -----###

### * se o nome das variaveis da pnadc mudarem: fazer revisao

for(dados_name in dados_names){
  get(dados_name) %>%
    mutate(one = 1,
           data_nasc = as.Date(paste0(V2008, "/", V20081, "/", V20082), format = "%d/%m/%Y"),
           idade = as.integer(as.Date(paste0("31", "/", "03", "/", Ano), format = "%d/%m/%Y") - data_nasc) / 365.25,
           idade = ifelse(is.na(idade), V2009, idade),
           idade = as.integer(idade),
           regioes = ifelse(UF=="Rondônia"|UF=="Acre"|UF=="Amazonas"|UF=="Roraima"|UF=="Pará"|UF=="Amapá"|UF=="Tocantins", "Norte",
                            ifelse(UF=="Maranhão"|UF=="Piauí"|UF=="Ceará"|UF=="Rio Grande do Norte"|UF=="Paraíba"|UF=="Pernambuco"|UF=="Alagoas"|UF=="Sergipe"|UF=="Bahia", "Nordeste",
                                   ifelse(UF=="Minas Gerais"|UF=="Espírito Santo"|UF=="Rio de Janeiro"|UF=="São Paulo", "Sudeste",
                                          ifelse(UF=="Paraná"|UF=="Santa Catarina"|UF=="Rio Grande do Sul", "Sul",
                                                 ifelse(UF=="Mato Grosso do Sul"|UF=="Mato Grosso"|UF=="Goiás"|UF=="Distrito Federal", "Centro-Oeste", NA))))),
           pais = ifelse(!is.na(regioes), "Brasil", NA)) %>%
    pnadc_design() %>% 
    assign(value = .,
           x = dados_name,
           envir = globalenv())
}


rm(dados_name)
options(scipen=999)

Moda <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

for(dados_name in dados_names){
  get(dados_name) %>%
    as_survey() %>% 
    summarize(ano=Moda(Ano), br_total = survey_total(one, na.rm=T)) %>%
    assign(value = .,
           x = paste0(dados_name,'_out'),
           envir = globalenv())
}

# validar expansão da amostra https://painel.ibge.gov.br/pnadc/
svytotalBR <- do.call(rbind, mget(ls(pattern="_out")))

rm(list = ls(pattern="_out"))
rm(svytotalBR)

####---------------- META 1A ----------------####

##---- Estados
for(dados_name in dados_names){
  get(dados_name) %>%
    as_survey() %>% 
    filter(idade>=4 & idade<=5) %>% 
    group_by(UF, V3002) %>% 
    summarize(ano = Moda(Ano),
              total = survey_total(na.rm=T),
              perc = survey_mean(na.rm=T)*100) %>%
    ungroup() %>% 
    assign(value = .,
           x = paste0(dados_name,'_1A'),
           envir = globalenv())
}

svy1A_Estados <- do.call(rbind, mget(ls(pattern="_1A")))
svy1A_Estados <- setDT(as.data.frame(svy1A_Estados), keep.rownames = F)

rm(list = ls(pattern="_1A"))

##---- Regiões
for(dados_name in dados_names){
  get(dados_name) %>%
    as_survey() %>%  
    filter(idade>=4 & idade<=5) %>% 
    group_by(regioes, V3002) %>% 
    summarize(ano = Moda(Ano),
              total = survey_total(na.rm=T),
              perc = survey_mean(na.rm=T)*100) %>%
    ungroup() %>% 
    assign(value = .,
           x = paste0(dados_name,'_1A'),
           envir = globalenv())
}

svy1A_Regioes <- do.call(rbind, mget(ls(pattern="_1A")))
svy1A_Regioes <- setDT(as.data.frame(svy1A_Regioes), keep.rownames = F)

rm(list = ls(pattern="_1A"))

##---- Brasil
for(dados_name in dados_names){
  get(dados_name) %>%
    as_survey() %>%  
    filter(idade>=4 & idade<=5) %>% 
    group_by(pais, V3002) %>% 
    summarize(ano = Moda(Ano),
              total = survey_total(na.rm=T),
              perc = survey_mean(na.rm=T)*100) %>%
    rename(regioes='pais') %>% 
    ungroup() %>% 
    assign(value = .,
           x = paste0(dados_name,'_1A'),
           envir = globalenv())
}

svy1A_Brasil <- do.call(rbind, mget(ls(pattern="_1A")))
svy1A_Brasil <- setDT(as.data.frame(svy1A_Brasil), keep.rownames = F)
svy1A_Regioes <- rbind(svy1A_Regioes, svy1A_Brasil)

rm(list = ls(pattern="_1A"))

x <- c("svy1A_Regioes","svy1A_Estados")
x <- mget(x, envir=as.environment(-1), mode= "any", inherits=F)
write.xlsx(x, file = "V1A.xlsx")

rm(svy1A_Regioes, svy1A_Brasil, svy1A_Estados, x)

####---------------- META 1B ----------------####

##---- Estados
for(dados_name in dados_names){
  get(dados_name) %>%
    as_survey() %>% 
    filter(idade>=0 & idade<=3) %>% 
    group_by(UF, V3002) %>% 
    summarize(ano = Moda(Ano),
              total = survey_total(na.rm=T),
              perc = survey_mean(na.rm=T)*100) %>%
    ungroup() %>% 
    assign(value = .,
           x = paste0(dados_name,'_1B'),
           envir = globalenv())
}

svy1B_Estados <- do.call(rbind, mget(ls(pattern="_1B")))
svy1B_Estados <- setDT(as.data.frame(svy1B_Estados), keep.rownames = F)

rm(list = ls(pattern="_1B"))

##---- Regiões
for(dados_name in dados_names){
  get(dados_name) %>%
    as_survey() %>%  
    filter(idade>=0 & idade<=3) %>% 
    group_by(regioes, V3002) %>% 
    summarize(ano = Moda(Ano),
              total = survey_total(na.rm=T),
              perc = survey_mean(na.rm=T)*100) %>%
    ungroup() %>% 
    assign(value = .,
           x = paste0(dados_name,'_1B'),
           envir = globalenv())
}

svy1B_Regioes <- do.call(rbind, mget(ls(pattern="_1B")))
svy1B_Regioes <- setDT(as.data.frame(svy1B_Regioes), keep.rownames = F)

rm(list = ls(pattern="_1B"))

##---- Brasil
for(dados_name in dados_names){
  get(dados_name) %>%
    as_survey() %>%  
    filter(idade>=0 & idade<=3) %>% 
    group_by(pais, V3002) %>% 
    summarize(ano = Moda(Ano),
              total = survey_total(na.rm=T),
              perc = survey_mean(na.rm=T)*100) %>%
    rename(regioes='pais') %>% 
    ungroup() %>% 
    assign(value = .,
           x = paste0(dados_name,'_1B'),
           envir = globalenv())
}

svy1B_Brasil <- do.call(rbind, mget(ls(pattern="_1B")))
svy1B_Brasil <- setDT(as.data.frame(svy1B_Brasil), keep.rownames = F)
svy1B_Regioes <- rbind(svy1B_Regioes, svy1B_Brasil)

rm(list = ls(pattern="_1B"))

x <- c("svy1B_Regioes","svy1B_Estados")
x <- mget(x, envir=as.environment(-1), mode= "any", inherits=F)
write.xlsx(x, file = "V1B.xlsx")

rm(list = ls())
