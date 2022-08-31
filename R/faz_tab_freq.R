#' Funcao para fazer crosstabs de frequencia a partir da amostra complexa da Pnad
#'
#' @param data objeto contendo os microdados da pnad, ja com o desenho da amostra commplexa configurado.
#' @param ... nomes das variaveis usadas para recorte dos dados. Cria ainda uma variava "periodo" uma variavel com a informacao do trimestre. Ex.: 20201 = 1o trimestre de 2020.
#'
#' @export


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



# devtools::document()
# ?faz_tab_freq
