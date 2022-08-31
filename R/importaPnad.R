#' Funcao para importar dados de um trimestre da pnad continua
#'
#' @param trimestre trimestre da pnad a ser baixada.
#' @param ano ano da pnad a ser baixada.
#' @param bootstrap logica, opcional. Se TRUE, faz o desenho amostral usando os pesos replicados (bootstrap). Se FALSE, faz o desenho amostral usando o metodo antigo (linearizacao de Taylor).
#' @param lista_var objeto opcional contendo lista de variaveis a serem baixadas.
#'
#' @export
#' @examples
#'
#' # NAO RODAR!
#'
#'
#' # instala PnadDieese
#'
#' devtools::install_github("gustavoppmonteiro/PnadDieese")
#' library(PnadDieese)
#'
#' # carrega pacotes
#' library(tidyverse)
#' library(srvyr)
#' library(survey)
#' library(PNADcIBGE)
#'
#'
#' # seleciona variaveis
#' variaveis <-  c("V1028", "Ano", "Trimestre", "UF", "V2007")
#'
#' # importa base
#' dados_2T22 <- importaPnad(2, 2022, lista_var = variaveis)
#'
#'
#' # tabela com frequencia por sexo
#' T_1 <- dados_2T22 %>%
#'       faz_tab_freq(., V2007)
#'
#' T_1



importaPnad <- function(trimestre, ano, bootstrap=F, lista_var=F) {

      # Referência ao pipe
      `%>%` <- magrittr::`%>%`

      # baixa
      if(lista_var==F){

            dados_x <- PNADcIBGE::get_pnadc(year = ano,
                                 quarter = trimestre,
                                 labels = F,
                                 design = F)
      }else{

            dados_x <- PNADcIBGE::get_pnadc(year = ano,
                                 quarter = trimestre,
                                 vars = lista_var,
                                 labels = F,
                                 design = F)
      }


      # se for desenho amostral antigo, tira as replicacoes
      if(bootstrap==F){
            # tira as replicações
            dados_x <- dados_x %>%
                  dplyr::select(-c(sprintf("V1028%03d",seq(1:200))))
      }


      # deixa tudo com classe numeric
      dados_x <- dados_x %>%
            dplyr::mutate(dplyr::across(dplyr::all_of(variaveis), as.numeric))


      # faz desenho amostral, antigo ou nao
      if(bootstrap==F){
            dados_x <- faz_desenho_amostral_antigo(dados_x)
      }


      if(bootstrap==T){
            dados_x <- faz_desenho_amostral_bootstrap(dados_x)
      }


      return(dados_x)

}


# devtools::document()
# ?importaPnad
