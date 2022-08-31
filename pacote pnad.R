# pacote PNAD

#limpa objetos no environment
rm(list=ls())
#cat("\014") # ctrl + L

library(tidyverse)
library(srvyr)
library(survey)
library(PNADcIBGE)



# Importando --------------------------------------------------------------

faz_desenho_amostral_bootstrap <- function(data) {
      
      # https://rpubs.com/gabriel-assuncao-ibge/pnadc
      dados_x <- survey::svrepdesign(data=data,
                                     weights=~V1028,
                                     type="bootstrap",
                                     repweights= "V1028[0-9]+",
                                     mse=TRUE,
                                     replicates=lenght(sprintf("V1028%03d",seq(1:200))),
                                     df=lenght(sprintf("V1028%03d",seq(1:200))))
      
      # deixa no jeito pra usar dplyr
      dados_x <- srvyr::as_survey_rep(dados_x)
      
      return(dados_x)
      
}


faz_desenho_amostral_antigo <- function(data) {
      
      options(survey.lonely.psu="adjust")
      
      dados_x <- data %>%
            as_survey_design(ids = UPA,
                             strata = Estrato,
                             weights = V1028,
                             nest = TRUE)
      
      return(dados_x)
      
}


importaPnad <- function(trimestre, ano, bootstrap=F, lista_var=F) {
      
      # baixa
      if(lista_var==F){
            
            dados_x <- get_pnadc(year = ano,
                                 quarter = trimestre,
                                 labels = F,
                                 design = F)
      }else{
            
            dados_x <- get_pnadc(year = ano,
                                 quarter = trimestre,
                                 vars = lista_var,
                                 labels = F,
                                 design = F)
      }
      
      
      # se for desenho amostral antigo, tira as replicacoes
      if(bootstrap==F){
            # tira as replicações
            dados_x <- dados_x %>% 
                  select(-c(sprintf("V1028%03d",seq(1:200))))
      }
      
      
      # deixa tudo com classe numeric
      dados_x <- dados_x %>%
            mutate(across(all_of(variaveis), as.numeric))
      
      
      # faz desenho amostral, antigo ou nao
      if(bootstrap==F){
            dados_x <- faz_desenho_amostral_antigo(dados_x)
      }
      
      
      if(bootstrap==T){
            dados_x <- faz_desenho_amostral_bootstrap(dados_x)
      }
      
      
      return(dados_x)
      
}



# funcoes tabelas ---------------------------------------------------------

# tabela de frequencia
faz_tab_freq <- function(data, ...) {
      
      dados_x <- data %>%
          
            group_by(...) %>%
            summarise(qde = survey_total(vartype = "cv"),
                      Ano = first(Ano),
                      Trimestre = first(Trimestre)) %>%
            mutate(periodo = 10*Ano + Trimestre, 
                   .keep = "unused")
      
      return(dados_x)
      
}

# tabela de media
faz_tab_media <- function(data, var_media=rend_H_real, ...) {
      
      dados_x <- data %>%
            
            group_by(...) %>%
            summarise(media = survey_mean({{var_media}}, 
                                          vartype = "cv", 
                                          na.rm = T),
                      Ano = first(Ano),
                      Trimestre = first(Trimestre)) %>%
            mutate(periodo = 10*Ano + Trimestre, 
                   .keep = "unused")
      
      return(dados_x)
      
}






# EXEMPLO -----------------------------------------------------------------

# seleciona variaveis
variaveis <-  c("V1028", "Ano", "Trimestre", "UF",
                
                "V2007", "V2009", "V2010",
                
                "V3002", "V4009", "V4010", "V4013", "V4019", "V4043", 
                "VD2002", "VD2003", "VD2004", 
                
                # jornada de trabalho
                "VD4032", "VD4036", 
                
                # trabalho
                "VD4002", "VD4009", "VD4010", "VD4011",
                "VD4016", "VD4017",
                "VD4005", 
                
                # # pesos replicados
                # sprintf("V1028%03d",seq(1:200)),
                
                # qde empregados - pergunta p/ EMPREGADORES
                "V4016", "V40161", "V40162", "V40163",
                # qde empregados - pergunta p/ EMPREGADOS DO SETOR PRIVADO
                "V4018", "V40181", "V40182", "V40183",
                # qde sócios - pergunta p/ CONTA PRÓPRIA
                "V40171", "V401711")



# importando (default: desenho amostral antigo)
dados_2T22 <- importaPnad(2, 2022, lista_var = variaveis)



# Tabelas

T_1 <- dados_2T22 %>% 
      
      mutate(rend_H_real = VD4016*Habitual,
             fx_1sm = ifelse(rend_H_real<=1212, 1, 0)) %>% 
      faz_tab_freq(., V2007, fx_1sm)

T_1


T_2 <- dados_2T22 %>% 
      mutate(rend_H_real = VD4016*Habitual,
             GR = trunc(UF/10)) %>% 
      faz_tab_media(., var_media=rend_H_real, V2007, GR)

T_2



