#' Pivot the data frame so each measurement is it's own column.
#'
#' This requires specifying the acceptable quality (品質情報) level. All measurements
#' that fall below this threshold will be removed.
#'
#' @param data A dataframe read by `read_amedas_csv`
#' @param min_quality The minimum acceptable level for 品質情報 (quality)
#' @param ignore_device_changes Silence warnings about changes in the "均質番号" field
#'
#' @return A pivoted [tibble][tibble::tibble-package]
#' @export
#' @importFrom magrittr %>%
pivot_measurements <- function (data, min_quality, ignore_device_changes = FALSE) {
  measures <- levels(data$項目名)

  if ("品質情報" %in% colnames(data)) {
    data <- data[data$品質情報 >= min_quality,]
  }

  if ("均質番号" %in% colnames(data) & ignore_device_changes == FALSE) {
    for (m in measures) {
      unique_stns <- unique(data$均質番号[data$項目名 == m])
      if (length(unique_stns) > 1) {
        warning(
          paste0("The measuring device for \"", m, "\" has changes during the data period.\n  ",
                 "Ignore this warning by setting ignore_device_changes = TRUE.")
        )
      }
    }
  }

  pivoted <- data %>%
    dplyr::select(-tidyselect::any_of(c("品質情報", "均質番号", "現象なし情報"))) %>%
    tidyr::pivot_wider(names_from = 項目名, values_from = データ)

  if (is.character(data$データ)) {
    # Readr chokes if it's asked to parse a non-char column so this needs to be
    # fenced in a conditional.
    return(
      pivoted %>%
        dplyr::mutate(dplyr::across(tidyselect::all_of(measures), readr::parse_guess))
    )
  }

  return(pivoted)
}
