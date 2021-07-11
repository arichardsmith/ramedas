test_that("it sets the correct column names", {
  df <- read_amedas_csv("fixtures/standard.csv")

  post_pivot <- pivot_measurements(df, 0)

  expect_equal(colnames(post_pivot), c("年月日時", "観測所名", "気温(℃)", "積雪(cm)"))
})

test_that("it drops values below the given quality threshold", {
  df <- read_amedas_csv("fixtures/daily.csv")

  no_filter <- pivot_measurements(df, min_quality = 0)
  with_filter <- pivot_measurements(df, min_quality = 5)

  subpar_value_1 <- no_filter[no_filter$年月日 == "2020/10/3","最深積雪(cm)"][[1]]
  subpar_value_2 <- with_filter[with_filter$年月日 == "2020/10/3","最深積雪(cm)"][[1]]
  expect_equal(subpar_value_1, 5)
  expect_equal(subpar_value_2, NA_real_)
})

test_that("it warns when station continuity changes", {
  df <- read_amedas_csv("fixtures/discontinuous.csv")

  expect_warning(pivot_measurements(df, min_quality = 0), "\"積雪\\(cm\\)\" has changes during the data period")
})

test_that("continuity warning can be ignored", {
  df <- read_amedas_csv("fixtures/discontinuous.csv")

  expect_silent(pivot_measurements(df, min_quality = 0, ignore_device_changes = TRUE))
})

test_that("it can pivot 'web formated' data", {
  df <- read_amedas_csv("fixtures/web-format.csv")

  post_pivot <- pivot_measurements(df, 0)
  expect_equal(colnames(post_pivot), c("年月日時", "観測所名", "気温(℃)", "積雪(cm)"))
})

test_that("it guesses column types post pivot", {
  df <- read_amedas_csv("fixtures/wind.csv")

  post_pivot <- pivot_measurements(df, 0)
  expect_true(is.numeric(post_pivot[["風速(m/s)"]]))
  expect_true(is.character(post_pivot[["風速(m/s)_風向"]]))
})
