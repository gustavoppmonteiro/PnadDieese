#' Funcao para importar dados de um trimestre da pnad continua
#'
#' @param trimestre trimestre da pnad a ser baixada.
#' @param ano ano da pnad a ser baixada.
#' @param bootstrap logica, opcional. Se TRUE, mantem os pesos replicados para usar no bootstrap. Se FALSE, descarta essas 200 variaveis.
#' @param lista_var objeto opcional contendo lista de variaveis a serem baixadas.
#' @param sm logica, opcional. Se TRUE, acrescenta duas colunas que baixa do site do DIEESE: salario_minimo e salario_minimo_necessario.
#'
#' @export
#'

importaPnad_sem_desenho_amostral <- function(trimestre, ano, bootstrap=F, lista_var=NULL, sm=F) {

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


      # acrescenta coluans de sm
      if(sm==T){
            dados_x <- PnadDieese::SM_DIEESE(dados_x)
      }


      return(dados_x)

}


