#' Funcao para fazer crosstabs tendo como conteudo uma media, a partir da amostra complexa da Pnad
#'
#' @param data objeto contendo os microdados da pnad, ja com o desenho da amostra commplexa configurado.
#' @param var_media nome da variavel da qual sera extraida a media (conteudo da tabela).
#' @param ... nomes das variaveis usadas para recorte dos dados. Cria ainda uma variava "periodo" uma variavel com a informacao do trimestre. Ex.: 20201 = 1o trimestre de 2020.
#'
#' @export


# tabela de media
faz_tab_media <- function(data, var_media, ...) {

      dados_x <- data %>%

            dplyr::group_by(...) %>%
            summarise(media = srvyr::survey_mean({{var_media}},
                                          vartype = "cv",
                                          na.rm = T),
                      Ano = first(Ano),
                      Trimestre = first(Trimestre)) %>%
            dplyr::mutate(periodo = 10*Ano + Trimestre,
                   .keep = "unused")

      return(dados_x)

}


# devtools::document()
# ?faz_tab_media
