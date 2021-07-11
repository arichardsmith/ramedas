library(readr)

amedas_stations <- read_csv(here::here("data-raw/ame_master_20210707.csv"), locale = locale(encoding = "CP932")) %>%
  dplyr::mutate(ローマ字名 = zipangu::str_conv_romanhira(ｶﾀｶﾅ名,"roman"), .after = ｶﾀｶﾅ名)

save(amedas_stations, file = "data/amedas_stations.rda")
