#' Funcao para definir desenho amostral usando pesos replicados (bootstrap).
#'
#' @param data objeto contendo os microdados baixados da pnad.
#'
#' @export

faz_desenho_amostral_bootstrap <- function(data) {

      usethis::use_package("dplyr")
      usethis::use_package("srvyr")
      usethis::use_package("survey")
      usethis::use_package("PNADcIBGE")

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


# devtools::document()
# ?faz_desenho_amostral_bootstrap
