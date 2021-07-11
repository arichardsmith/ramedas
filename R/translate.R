#' Translate column names to English
#'
#' Convert column names to English. The measurement translations are currently incomplete.
#'
#' @param data AMEDAS dataframe to translate
#'
#' @return The same dataframe, but with the columns named in English
#' @export
translate_columns <- function (data) {
  data %>%
    dplyr::rename_with(~dict_translate(.x, lookup_table, if.missing = "skip")) %>%
    dplyr::rename_with(translate_timestamp, .cols = tidyselect::matches("[年月]{2,}"))
}

lookup_table <- list(
  admin_area = "都府県振興局",
  station = "観測所名",
  measure = "項目名",
  data = "データ",
  quality = "品質情報",
  continuity = "均質番号",
  no_ob = "現象なし情報",
  solar_hours = "日照時間(時間)",
  temp = "気温(℃)",
  relative_humidity = "相対湿度(％)",
  precip = "降水量(mm)",
  temp_avg = "平均気温(℃)",
  temp_max = "最高気温(℃)",
  temp_min = "最低気温(℃)",
  pcp_total = "降水量の合計(mm)",
  air_pressure = "現地気圧(hPa)",
  max_hs = "最深積雪(cm)"
)

#' Translate (rename) character vector based on a dictionary
#' @param .vec character vector to rename
#' @param dictionary lookup dictionary
#' @param if.missing what to do for values with no dictionary entry. One of "skip" | "na" | "error".
#' @noRd
dict_translate <- function (.vec, dictionary, if.missing = "na") {
  lt_search <- unname(unlist(dictionary))
  lt_replace <- names(dictionary)

  if (if.missing == "error" & !all(.vec %in% lt_search)) {
    stop("Some values are missing from dictionary. Set if.missing to \"skip\" or \"na\" to suppress this error")
  }

  for (i in seq_along(lt_search)) {
    s <- lt_search[i]
    r <- lt_replace[i]

    .vec <- replace(.vec, which(.vec == s), r)
  }

  if (if.missing == "na" & any(!(.vec %in% lt_replace))) {
    warning("NAs generated as some values are missing from dictionary.")
    .vec[!(.vec %in% lt_replace)] <- NA_character_
  }

  return(.vec)
}

translate_timestamp <- function (ts) {
  ts %>%
    stringr::str_replace("[年月日時]+", "timestamp") %>%
    stringr::str_replace("集計開始", "start") %>%
    stringr::str_replace("集計終了", "end")
}
