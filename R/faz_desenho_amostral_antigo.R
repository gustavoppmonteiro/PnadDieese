#' Funcao para definir desenho amostral usando metodo antigo (linearizacao de Taylor).
#'
#' @param data objeto contendo os microdados baixados da pnad.
#'
#' @export

faz_desenho_amostral_antigo <- function(data) {

      # Referência ao pipe
      `%>%` <- magrittr::`%>%`

      options(survey.lonely.psu="adjust")

      dados_x <- data %>%
            srvyr::as_survey_design(ids = UPA,
                             strata = Estrato,
                             weights = V1028,
                             nest = TRUE)

      return(dados_x)

}

# devtools::document()
# ?faz_desenho_amostral_antigo
