#' Read an AMEDAS CSV data file
#'
#' AMEDAS files downloaded from the JMA website can't be read with `read_csv`
#' due to multiple headers. `read_amedas_csv` attempts to parse the csv and
#' convert it into a tidy dataframe.
#'
#' @param filename File to read
#' @return A [tibble][tibble::tibble-package] with one row per time, station and observation type and
#'   includes the measurement metadata (quality etc)
#'
#' @importFrom magrittr %>%
#' @export
read_amedas_csv <- function (filename) {
  timestamp <- extract_timestamp(filename)

  # If there is no timestamp, start reading from the top of the file
  skip_n <- ifelse(is.na(timestamp), 0, 2)

  src_frame <- readr::read_csv(
    filename,
    col_names=FALSE, # We reconstruct these later
    locale = readr::locale(encoding = 'SHIFT-JIS'),
    skip = skip_n,
    col_types = readr::cols(.default = readr::col_character())
  )

  # The number of header rows changes (!!) but non-header rows will start with
  # a timestamp
  is_header <- !grepl("^\\d{2}", src_frame[[1]])
  headers <- src_frame[which(is_header),]
  df <- src_frame[which(!is_header),]

  if (all(is.na(headers[[1]]))) {
    # The first column should contain the timestamp format.
    # If it doesn't then the file is likely in "web format", so we insert
    # a timestamp format on the last row.

    x <- nrow(headers)

    headers[x, 1] <- "年月日時"
    headers[x + 1,] <- NA
  }

  is_timestamp <- function (x) grepl("^[年月日時分曜]+$", x)
  search_cols <- min(c(ncol(headers), 7)) # Up to 6 columns should be part of the timestamp

  measure_row <- which(is_timestamp(headers[[1]]))

  date_cols <- headers[,1:search_cols] %>%
    sapply(function (x) any(is_timestamp(x))) %>%
    which() %>%
    unname()

  # Compute header row types
  header_types <- character(nrow(headers))
  header_types[measure_row] <- "項目名"
  header_types[measure_row - 1] <- "観測所名"

  if (measure_row > 2) {
    # Pref should also be present
    header_types[measure_row - 2] <- "都府県振興局"
  }

  data_row <- measure_row + 1

  # Format date columns to help with pivoting later
  # headers[,date_cols][is.na(headers[,date_cols])] <- ""
  date_headers <- headers[,date_cols] %>%
    sapply(function (col) paste(col[!is.na(col)], collapse = "_"))

  headers[nrow(headers), date_cols] <- matrix(date_headers, nrow = 1)
  headers[1:(nrow(headers)-1),date_cols] <- "date"

  if (nrow(headers) - measure_row > 1) {
    # Extra measurement info
    header_types[measure_row + 1] <- "sub_measure"
    data_row <- measure_row + 2
  }

  header_types[data_row] <- "reading"

  extract_exp <- paste(rep("([^_]*)", length(header_types)), collapse = "_")

  headers[is.na(headers)] <- "" # Remove NAs before flattening
  flat_headers <- sapply(headers, paste, collapse = "_")
  colnames(df) <- flat_headers

  df <- df %>%
    tidyr::pivot_longer(col = -tidyselect::contains("date")) %>%
    tidyr::extract(name, into = header_types, regex = extract_exp) %>%
    dplyr::mutate(reading = ifelse(reading == "", "データ", reading)) %>%
    tidyr::pivot_wider(names_from = reading, values_from = value)

  # Extract name for date columns
  cols <- colnames(df)

  date_cols <- which(grepl("date", cols))
  cols[date_cols] <- cols[date_cols] %>%
    strsplit("_") %>%
    lapply(function (y) {
      matches <- y[y != "" & y != "date"]
      return (paste(matches, collapse = "_"))
    }) %>%
    unlist()

  colnames(df) <- cols

  # Set types on columns
  df$データ <- readr::parse_guess(df$データ) # There could still be strings, so may return chr vector
  df$観測所名 <- factor(df$観測所名)

  if ("都府県振興局" %in% cols) {
    df$都府県振興局 <- factor(df$都府県振興局)
  }

  if ("sub_measure" %in% cols) {
    df$項目名 <- ifelse(df$sub_measure != "", paste(df$項目名, df$sub_measure, sep = "_"), df$項目名)
    df <- subset(df, select = -c(sub_measure))
  }
  df$項目名 <- factor(df$項目名)

  if ("品質情報" %in% cols) {
    df$品質情報 <- readr::parse_integer(df$品質情報)
  }

  if ("均質番号" %in% cols) {
    df$均質番号 <- readr::parse_integer(df$均質番号)
  }

  if ("現象なし情報" %in% cols) {
    df$現象なし情報 <- readr::parse_logical(df$現象なし情報)
  }

  attr(df, "accessed") <- timestamp

  return(df)
}

#' Extracts download timestamp from file header
#'
#' @param filename
#'
#' @return timestamp as text
extract_timestamp <- function (filename) {
  # Read from the file first to get timestamp and verify shape
  f <- file(filename, open = "r", encoding = "SHIFT-JIS")
  timestamp <- scan(f, nlines = 1, what = character(), quiet = TRUE, sep = "\n") # Note: if sep is not defined it splits vector on whitespace
  close(f)

  if (length(timestamp) < 1) {
    # Return early on blank first lines
    return(NA_character_)
  }

  ptn <- "ダウンロードした時刻：(\\d{4}/\\d{2}/\\d{2}\\s?\\d{2}:\\d{2}:\\d{2})"
  if (!grepl(ptn, timestamp)) {
    return(NA_character_)
  }

  return(regmatches(timestamp, regexec(ptn, timestamp))[[1]][2])
}
