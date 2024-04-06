
#' Funcao para fazer um crosstab de uma media, de 1x1 - com uma variavel na coluna e outra na linha
#'
#' @param data objeto contendo os microdados da pnad, ja com o desenho da amostra commplexa configurado.
#' @param var_media nome da variavel cuja media deve aparecer no conteudo da tabela.
#' @param linha nome da variavel das linhas.
#' @param coluna nome da variavel das colunas.
#' @param cv_max valor entre 0 e 1 para o coeficiente de variacao maximo aceito nas estimativas
#'
#' @export

media_1x1 <- function(data, var_media, linha, coluna, cv_max=0.3) {

      options(survey.lonely.psu="adjust")

      # Referência ao pipe
      `%>%` <- magrittr::`%>%`

      data <- data %>%
            dplyr::mutate(var_media = {{var_media}},
                          linha = {{linha}},
                          coluna = {{coluna}})

      # pra fazer coluna de periodo no final
      ddd <- data %>%
            dplyr::summarise(Ano = first(Ano),
                             Trimestre = first(Trimestre)) %>%
            dplyr::mutate(periodo = 10*Ano + Trimestre,
                          .keep = "unused")


      # linha x coluna
      T1 <- data %>%
            dplyr::group_by(linha, coluna) %>%
            dplyr::summarise(freq = srvyr::survey_mean(var_media, vartype = "cv", na.rm = T)) %>%

            dplyr::select(linha, coluna, freq, freq_cv)

      # linha
      T2 <- data %>%

            dplyr::group_by(linha) %>%
            dplyr::summarise(freq = srvyr::survey_mean(var_media, vartype = "cv", na.rm = T),
                             coluna = 999999) %>%

            dplyr::select(linha, coluna, freq, freq_cv)

      # coluna
      T3 <- data %>%

            dplyr::group_by(coluna) %>%
            dplyr::summarise(freq = srvyr::survey_mean(var_media, vartype = "cv", na.rm = T),
                             linha = 999999) %>%

            dplyr::select(linha, coluna, freq, freq_cv)

      # total
      T4 <- data %>%

            dplyr::summarise(freq = srvyr::survey_mean(var_media, vartype = "cv", na.rm = T),
                             linha = 999999,
                             coluna = 999999) %>%

            dplyr::select(linha, coluna, freq, freq_cv)


      T5 <- rbind(T1, T2, T3, T4)

      T6 <- T5 %>%
            dplyr::mutate(freq_tabela = ifelse((freq_cv<=cv_max & freq_cv>0),
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
            dplyr::relocate("Total", .after = last_col()) %>%
            dplyr::mutate(periodo = ddd$periodo[1])

      return(T7)

}



