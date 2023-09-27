#' Funcao para importar dados de um trimestre da pnad continua
#'
#' @param trimestre trimestre da pnad a ser baixada.
#' @param ano ano da pnad a ser baixada.
#' @param bootstrap logica, opcional. Se TRUE, faz o desenho amostral usando os pesos replicados (bootstrap). Se FALSE, faz o desenho amostral usando o metodo antigo (linearizacao de Taylor).
#' @param lista_var objeto opcional contendo lista de variaveis a serem baixadas.
#' @param sm logica, opcional. Se TRUE, acrescenta duas colunas que baixa do site do DIEESE: salario_minimo e salario_minimo_necessario.
#'
#' @export
#' @examples
#'
#' # NAO RODAR!
#'
#'
#' # instala PnadDieese
#'
#' # devtools::install_github("gustavoppmonteiro/PnadDieese")
#' library(PnadDieese)
#'
#' # precisa desses pacotes:
#' library(tidyverse)
#' library(srvyr)
#' library(survey)
#' library(PNADcIBGE)
#'
#'
#' # seleciona variaveis
#' variaveis <-  c("V1028", "Ano", "Trimestre", "UF", "V2007", "VD4002")
#'
#' # importa dados
#' dados_2T22 <- PnadDieese::importaPnad(2, 2022, lista_var = variaveis)
#'
#'# tabela com frequencia de ocupados por sexo
#' T_1 <- dados_2T22 %>%
#'       filter(VD4002==1) %>%
#'       PnadDieese::faz_tab_freq(., V2007)
#'
#' T_1



importaPnad <- function(trimestre, ano, bootstrap=F, lista_var=NULL, sm=F) {

      # Referência ao pipe
      `%>%` <- magrittr::`%>%`

      # baixa
      if(is.null(lista_var)){

            dados_x <- PNADcIBGE::get_pnadc(year = ano,
                                 quarter = trimestre,
                                 labels = F,
                                 design = F)
      }else{

            var_necessarias <- c("V1028", "Ano", "Trimestre")

            lista_var_final <- unique(lista_var, var_necessarias)

            dados_x <- PNADcIBGE::get_pnadc(year = ano,
                                 quarter = trimestre,
                                 vars = lista_var_final,
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
            dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric))


      # faz desenho amostral, antigo ou nao
      if(bootstrap==F){
            dados_x <- faz_desenho_amostral_antigo(dados_x)
      }


      if(bootstrap==T){
            dados_x <- faz_desenho_amostral_bootstrap(dados_x)
      }


      # acrescenta coluans de sm
      if(sm==T){
            dados_x <- PnadDieese::SM_DIEESE(dados_x)
      }


      return(dados_x)

}


