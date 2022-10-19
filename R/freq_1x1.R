#' Funcao para fazer um crosstab 1x1 - com uma variavel na coluna e outra na linha
#'
#' @param data objeto contendo os microdados da pnad, ja com o desenho da amostra commplexa configurado.
#' @param linha nome da variavel das linhas.
#' @param coluna nome da variavel das colunas.
#' @param cv_max valor entre 0 e 1 para o coeficiente de variacao maximo aceito nas estimativas
#'
#' @export

freq_1x1 <- function(data, linha, coluna, cv_max=0.3) {

      options(survey.lonely.psu="adjust")

      # Referência ao pipe
      `%>%` <- magrittr::`%>%`

      data <- data %>%
            dplyr::mutate(linha = {{linha}},
                          coluna = {{coluna}})

      # linha x coluna
      T1 <- data %>%
            dplyr::group_by(linha, coluna) %>%
            dplyr::summarise(freq = srvyr::survey_total(vartype = "cv")) %>%

            dplyr::select(linha, coluna, freq, freq_cv)

      # linha
      T2 <- data %>%

            dplyr::group_by(linha) %>%
            dplyr::summarise(freq = srvyr::survey_total(vartype = "cv"),
                             coluna = 999999) %>%

            dplyr::select(linha, coluna, freq, freq_cv)

      # coluna
      T3 <- data %>%

            dplyr::group_by(coluna) %>%
            dplyr::summarise(freq = srvyr::survey_total(vartype = "cv"),
                             linha = 999999) %>%

            dplyr::select(linha, coluna, freq, freq_cv)

      # total
      T4 <- data %>%

            dplyr::summarise(freq = srvyr::survey_total(vartype = "cv"),
                             linha = 999999,
                             coluna = 999999) %>%

            dplyr::select(linha, coluna, freq, freq_cv)


      T5 <- rbind(T1, T2, T3, T4)

      T6 <- T5 %>%
            dplyr::mutate(freq_tabela = ifelse((freq_cv<=cv_max | freq_cv==0),
                                        freq,
                                        999999999999)) %>%
            tidyr::pivot_wider(id_cols = linha,
                        names_from = coluna,
                        names_prefix = "col_",
                        values_from = freq_tabela)

      T7 <- T6 %>%
            dplyr::mutate(Total = col_999999,
                          .keep="unused") %>%

            dplyr::mutate(linha = as.character(ifelse(linha==999999,
                                                      "Total",
                                                      linha)))


            T7 <- T7 %>%
                  dplyr::relocate("Total", .after = last_col())


      return(T7)

}

#TODO: fz o readme.


