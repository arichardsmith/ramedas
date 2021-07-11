#' Visualise reading quality over time
#'
#' Plot the reading quality values over time making it easy to asses the data
#' quality and decide `min_quality` values when pivoting
#'
#' @param data AMEDAS dataframe to visualise
#'
#' @return ggplot graphic
#' @export
#'
#' @examples
#' raw_data <- read_amedas_csv("data.csv")
#' visualize_quality_over_time(raw_data)
#' # All < 8 readings are out of season readings, ok to drop them
#' data <- pivot_measurements(raw_data, min_quality = 8)
visualize_quality_over_time <- function (data) {
  if (!requireNamespace("ggplot2")) {
    stop("ggplot is required to generate visualizations")
  }

  if (!("品質情報" %in% colnames(data))) {
    stop("No 品質情報 column in dataframe")
  }

  date_col <- colnames(data)[grepl("年月", colnames(data))][1]

  ggplot2::ggplot(data, aes(!!sym(date_col), 品質情報)) +
    ggplot2::geom_point() +
    ggplot2::facet_grid(vars(観測所名), vars(項目名))
}

#' Summarise record quality
#'
#' @param data Raw AMEDAS dataframe to summarise
#'
#' @return Tibble with count and proportion for each measurement
#' @export
summarise_quality <- function (data) {
  if (!("品質情報" %in% colnames(data))) {
    stop("No 品質情報 column in dataframe")
  }

  data %>%
    dplyr::group_by(観測所名, 項目名, 品質情報) %>%
    dplyr::summarise(項目数 = n(), .groups = "drop_last") %>%
    dplyr::mutate(割合 = 項目数 / sum(項目数)) %>%
    dplyr::ungroup()
}
