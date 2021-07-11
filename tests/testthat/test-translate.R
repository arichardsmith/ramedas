# This doesn't actually get Japanese values, it just filters out alphanumeric ones
get_japanese_values <- function (x) {
  x[!grepl("^[a-z0-9_]+$", x)]
}

test_that("translates all columns in standard.csv", {
  df <- read_amedas_csv("fixtures/standard.csv") %>%
    translate_columns()

  jp_cols <- get_japanese_values(colnames(df))
  expect_equal(jp_cols, character(0))
})

test_that("translates start and end timestamps", {
  df <- read_amedas_csv("fixtures/han-jun.csv") %>%
    translate_columns()

  expect_equal(colnames(df)[1], "start_timestamp")
  expect_equal(colnames(df)[2], "end_timestamp")
})
