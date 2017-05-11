
library(readr)
library(devtools)

radon_mn <- read_csv("https://raw.githubusercontent.com/hafen/d3m-radon/master/data/r_26/raw_data/radon.csv", col_types = cols(
  .default = col_double(),
  radonFile_index = col_character(),
  state = col_character(),
  state2 = col_character(),
  fips = col_character(),
  stfips = col_number(),
  zip = col_character(),
  region = col_number(),
  typebldg = col_number(),
  basement = col_character(),
  windoor = col_character(),
  rep = col_character(),
  wave = col_character(),
  starttm = col_character(),
  stoptm = col_character(),
  startdt = col_character(),
  stopdt = col_character(),
  dupflag = col_number(),
  zipflag = col_number(),
  cntyfips = col_number(),
  county = col_character(),
  county_code = col_character()
))




radon_mn <- as.data.frame(radon_mn)
attr(radon_mn, "spec") <- NULL

devtools::use_data(radon_mn, overwrite = TRUE)
