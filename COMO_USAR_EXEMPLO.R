# PNAD - PA: faixa etária
# 10/03/23


# 0) INICIO ---------------------------------------------------------------

#limpa objetos no environment
rm(list=ls())
gc() # serve pra apagar a memoria imediatamente (senao, demora ate 10s pra apagar)
# cat("\014")
# .rs.restartR()


# pra instalar o pacote PnadDieese precisa antes instalar o devtools
# credentials::set_github_pat()
install.packages("devtools")
devtools::install_github("gustavoppmonteiro/PnadDieese", force = T)
library(PnadDieese) # esse é pacote do monteiro style


# outros pacotes que vou usar
library(purrr)
library(tidyverse) # um pacote de pacotes. so dplyr ja bastaria
library(writexl)  # serve pra baixar as tabelas no excel
library(srvyr)  # para rodar survey usando dplyr
library(PNADcIBGE)

# as vezes tem que usar esses tb
# library(survey)
# library(dplyr)
# library(tidyr)




# 1) BAIXA PNADS ----------------------------------------------------------

# objeto com as variaveis que vou manter
variaveis <-  c("UF", "V2009", "V4019", "VD4002", "VD4009")


# crio uma funcao para baixar, selecionar variavies e salvar os dados em uma nova pasta
# bootstrap_baixar=F -> pq nao vou baixar os pesos para replicacao (mas deixei a opcao de usar)
baixa_pnad <- function(trimestre_baixar, ano_baixar, bootstrap_baixar=F){

  # importaPnad_sem_desenho_amostral
  dados <- PnadDieese::importaPnad_sem_desenho_amostral(trimestre_baixar,
                                                        ano_baixar,
                                                        lista_var = variaveis,
                                                        bootstrap = bootstrap_baixar,
                                                        sm = T) %>%
    select(-"Efetivo") %>%
    select(-"Habitual") %>%
    filter(UF==15)

  # salva
  dir.create("dados", showWarnings = F)
  save("dados", file = paste0("dados/", trimestre_baixar, "T", ano_baixar, ".RData"))

}


# BAIXA
baixa_pnad(4, 2019)
baixa_pnad(4, 2022)




# 2) FUNCAO PARA ARRUMAR AS BASES -----------------------------------------

arruma_pnad <- function(data, bootstrap_baixar=F){

  # cria variaveis
  dados <- data %>%
    # arrange(UPA, V1008, V1014, V2005) %>%

    mutate(fx_etaria = case_when(V2009<14~0,
                                 (V2009>=14 & V2009<18)~14,
                                 (V2009>=18 & V2009<25)~18,
                                 (V2009>=25 & V2009<40)~25,
                                 (V2009>=40 & V2009<60)~40,
                                 (V2009>=60)~60),

           tx_informal = case_when(VD4009==1~0,
                                   VD4009==2~100,

                                   VD4009==3~0,
                                   VD4009==4~100,

                                   VD4009==5~0,
                                   VD4009==6~100,
                                   VD4009==7~0,

                                   (VD4009==8 & V4019==1)~0, #   Empregador COM cnpj",
                                   (VD4009==8 & V4019==2)~100, # Empregador SEM cnpj",

                                   (VD4009==9 & V4019==1)~0, # "Conta propria COM cnpj",
                                   (VD4009==9 & V4019==2)~100, # "Conta propria SEM cnpj",

                                   VD4009==10~100),

           conta_pp = ifelse(VD4009==9, 1, 0))


  # desenho amostral
  if(bootstrap_baixar==F){
    dados <- dados %>%
      PnadDieese::faz_desenho_amostral_antigo()

  }else{

    dados <- dados %>%
      PnadDieese::faz_desenho_amostral_bootstrap()
  }


  return(dados)

}




# 3) CARREGA E ARRUMA AS BASES --------------------------------------------
file_list <- list.files("dados/", pattern = "\\.RData$", full.names = TRUE)

for(i in seq_along(file_list)){

      load(file_list[i])

      dados <- dados %>%
            arruma_pnad()

      obj_name <- paste0("pnad_", i)
      assign(obj_name, get(ls(pattern = "dados")))

      rm(dados)

}






# 4) FUNCAO TABELAS -------------------------------------------------------

# funcao pra fz frequencia. vai ter fx_etaria nas linhas mas nas colunas posso escolher a variavel (var_cross)
faz_t_qde <- function(data, var_cross){

  tabela <- data %>%
    filter(VD4002==1) %>%
    PnadDieese::freq_1x1(., fx_etaria, {{var_cross}}, cv_max = 0.15)

  return(tabela)

}


# funcao pra fz media
# vai ter fx_etaria nas linhas mas mas posso escolher a coluna (var_cross) e qual vairavel vai fz a media (var_media)
faz_t_media <- function(data, var_cross, var_media){

  tabela <- data %>%
    filter(VD4002==1) %>%
    PnadDieese::media_1x1(., {{var_media}}, fx_etaria, {{var_cross}}, cv_max = 0.15)

    return(tabela)

}




# 5) RODA TABELAS ---------------------------------------------------------

# crio uma lista com as bases que vou rodar
# Lista todos os objetos no ambiente de trabalho
objetos <- ls()

# Filtra apenas os objetos que começam com "pnad_" e são tbl_svy
objetos_pnad <- grep("^pnad_", objetos, value = TRUE)
objetos_tbl_svy <- objetos_pnad[sapply(mget(objetos_pnad, inherits = TRUE), inherits, "tbl_svy")]

# Cria uma lista contendo os objetos tbl_svy filtrados
lista_base <- mget(objetos_tbl_svy, inherits = TRUE)
rm(objetos, objetos_tbl_svy)




# tabelas - vai rodar as funcoes para fz tabelas, para todas as bases na lista acima

# essa faz a frequencia, com fx_etaria nas linhas e tx_informal nas colunas (informal=100, formal=0)
T_1 <- map_df(lista_base, faz_t_qde, var_cross=tx_informal)
T_1

# esse faz tx de informalidade para fx_etaria (linhas) e um indicador de conta propria (coluna)
T_2 <- map_df(lista_base, faz_t_media, var_cross=conta_pp, var_media=tx_informal)
T_2




# 6) EXPORTA --------------------------------------------------------------

# salva em abas os nomes das tabelas
abas <- mget(ls(pattern = "T_"))


# grava as tabeas das abas (uma em cada aba do excel)
write_xlsx(abas, "op_pnad_pa_fx_etaria.xlsx")

# abre o excel
utils::browseURL("op_pnad_pa_fx_etaria.xlsx")



