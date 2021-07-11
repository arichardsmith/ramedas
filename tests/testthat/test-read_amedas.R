suppressMessages(library(magrittr))

test_that("can read a basic file", {
  df <- read_amedas_csv("fixtures/standard.csv")

  expect_equal(
    colnames(df),
    c("年月日時", "観測所名", "項目名", "データ", "品質情報", "均質番号", "現象なし情報")
  )

  expect_true(is.factor(df$観測所名))
  expect_true(is.factor(df$項目名))
  expect_true(is.numeric(df$品質情報))
  expect_true(is.numeric(df$均質番号))


  expect_equal(df$データ[1], 12.1)

  expect_equal(df$品質情報[1], 8)
  expect_equal(df$品質情報[2], 1)

  expect_equal(df$均質番号[1], 1)
})

test_that("correctly parses 現象なし情報", {
  df <- read_amedas_csv("fixtures/standard.csv")

  expect_equal(
    df[df$年月日時 == "2020/10/30 16:00:00" & df$項目名 == "積雪(cm)",]$現象なし情報,
    NA
  )

  expect_equal(
    df[df$年月日時 == "2020/10/30 17:00:00" & df$項目名 == "積雪(cm)",]$現象なし情報,
    TRUE
  )

  expect_equal(
    df[df$年月日時 == "2020/11/9 19:00:00" & df$項目名 == "積雪(cm)",]$現象なし情報,
    FALSE
  )
})

test_that("attaches timestamp to dataframe", {
  df <- read_amedas_csv("fixtures/standard.csv")

  expect_equal(attr(df, "accessed"), "2021/07/08 17:16:38")
})

test_that("can read a file with split date component columns", {
  df <- read_amedas_csv("fixtures/split-date.csv")

  expect_true("年" %in% colnames(df))
  expect_true("月" %in% colnames(df))
  expect_true("日" %in% colnames(df))
  expect_true("時" %in% colnames(df))

  date <- df %>%
    dplyr::mutate(date = paste(年, 月, 日, 時, sep = "/")) %>%
    .$date

  expect_equal(date[1], "2020/10/1/1")
})

test_that("can read a file with daily readings", {
  df <- read_amedas_csv("fixtures/daily.csv")

  expect_equal(df[[1,1]], "2020/10/1")
  expect_equal(df$データ[1], 13.4)
})

test_that("can read a file with period readings", {
  df <- read_amedas_csv("fixtures/han-jun.csv")

  expect_equal(df$集計開始_年月日[1], "2020/10/1")
  expect_equal(df$集計終了_年月日[1], "2020/10/5")
  expect_equal(df$データ[1],　14.8)
})

test_that("can read a file with montly values", {
  df <- read_amedas_csv("fixtures/month.csv")

  expect_equal(df$年月[1], "2020/10")
})

test_that("can read a file with more than one station", {
  df <- read_amedas_csv("fixtures/multi-stn.csv")

  expect_equal(levels(df$観測所名), c("旭川", "札幌"))
  expect_equal(as.character(df$観測所名[1:4]), c("旭川", "旭川", "札幌", "札幌"))
})

test_that("can read a file that includes admin district/prefecture", {
  df <- read_amedas_csv("fixtures/with-pref.csv")

  expect_equal(levels(df$都府県振興局), c("上川"))
})

test_that("can read a file that has wind direct/speed columns", {
  df <- read_amedas_csv("fixtures/wind.csv")

  expect_true("風速(m/s)" %in% levels(df$項目名))
  expect_true("風速(m/s)_風向" %in% levels(df$項目名))
})

test_that("can read a file with formating like that on the webpage", {
  df <- read_amedas_csv("fixtures/web-format.csv")

  expect_equal(df[[1, 1]], "2020年10月1日1時")
  expect_equal(levels(df$項目名), c("気温(℃)", "積雪(cm)"))
  expect_equal(df$データ[1], "12.1") # The data column will always be a character vector
  expect_equal(df$データ[2], "///")
})
