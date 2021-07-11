#' AMEDAS station information
#'
#' Name and supplementary information for all AMEDAS stations
#'
#' @source <https://www.jma.go.jp/jma/kishou/know/amedas/kaisetsu.html>
#' @format Tibble with the following columns
#' \describe{
#' \item{都府県振興局}{Prefecture/Administrative district}
#' \item{観測所番号}{Station ID Number}
#' \item{種類}{Station type.
#'     (See [地域気象観測所一覧](https://www.jma.go.jp/jma/kishou/know/amedas/ame_master_20210707.pdf))}
#' \item{観測所名}{Station name}
#' \item{ｶﾀｶﾅ名}{Station name in Katakana}
#' \item{所在地}{Station address}
#' \item{緯度(度)}{Station Latitude (degrees)}
#' \item{緯度(分)}{Station Latitude (minutes)}
#' \item{経度(度))}{Station Longitude (degrees)}
#' \item{経度(分))}{Station Longitude (minutes)}
#' \item{海面上の高さ(ｍ)}{Station elevation}
#' \item{風速計の高さ(ｍ)}{Wind gauge height}
#' \item{温度計の高さ(ｍ)}{Thermometer height}
#' \item{観測開始年月日}{Date station was installed}
#' \item{備考1}{Notes 1}
#' \item{備考2}{Notes 2}
#' }
"amedas_stations"

#' Add station metadata to AMEDAS readings
#'
#' @param target AMADAS data frame to join to
#'
#' @return The data frame with station metadata added
#' @export
left_join_stations <- function (target) {
  to_join <- stations

  if ("都府県振興局" %in% colnames(target)) {
    # Avoid duplicating the 都府県振興局 data
    to_join <- dplyr::select(to_join, -都府県振興局)
  }

  dplyr::left_join(target, to_join, by = "観測所名")
}
