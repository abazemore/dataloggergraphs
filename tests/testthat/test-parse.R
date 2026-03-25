# # parse_datalogger
# Central parsing function ----
test_that(
  "Central parsing function has expected columns and rows for Hanwell, discards invalid files",
  {
    envdata <- parse_brand(test_path("fixtures", "hanwell"), brand = "hanwell")
    # 3 warnings expected for invalid file
    # expect_warning(parse_brand(test_path("fixtures", "hanwell"), brand = "hanwell"))
    expect_equal(nrow(envdata), 10)
    expect_equal(names(envdata),
    c('site', 'location', 'datetime', 'temp', 'RH', 'model', 'serial'))
    expect_true(sum(is.na(envdata)) == 0)

  }
)
test_that(
  "Central parsing function has expected columns and rows for Meaco, discards invalid files",
  {
    # 3 warnings expected for invalid file
    expect_warning(parse_brand(test_path(
      "fixtures", "meaco"
    ), brand = "meaco"))
    envdata <- parse_brand(test_path(
      "fixtures", "meaco"
    ), brand = "meaco")
    expect_equal(nrow(envdata), 40)
    expect_equal(names(envdata),
    c('site', 'location', 'datetime', 'temp', 'RH', 'lux', 'UV', 'model', 'serial'))
    expect_equal(sum(is.na(
      envdata
    )), 80)
  }
)

test_that(
  "Central parsing function has expected columns and rows for Meaco single csv",
  {
    envdata <- parse_brand(test_path(
      "fixtures", "meaco", 'Meaco_1.csv'
    ), brand = "meaco")
    expect_equal(nrow(envdata), 10)
    expect_equal(names(envdata),
    c('site', 'location', 'datetime', 'temp', 'RH', 'model', 'serial'))
    expect_equal(sum(is.na(
      envdata)
    ), 0)
  }
)
test_that(
  "Central parsing function has expected columns and rows for previous, discards invalid files",
  {
    envdata <- parse_brand(test_path(
      "fixtures", "previous"
    ), brand = "previous")
    # 3 warnings expected for invalid file
    expect_warning(parse_brand(test_path(
      "fixtures", "previous"
    ), brand = "previous"))
    expect_equal(nrow(envdata), 16000)
    expect_equal(names(envdata),
    c("site", "location", "datetime", "temp", "RH", "lux", "UV", "model", "serial"))
    expect_true(sum(is.na(
      envdata
    )) == 16000)

  }
)
test_that(
  "Central parsing function has expected columns and rows for Rotronic, discards invalid files",
  {
    envdata <- parse_brand(test_path(
      "fixtures", "rotronic"
    ), brand = "rotronic")
    # 3 warnings expected for invalid file
    expect_warning(parse_brand(test_path(
      "fixtures", "rotronic"
    ), brand = "rotronic"))
    expect_equal(nrow(envdata), 20)
    expect_equal(names(envdata),
    c('site', 'location', 'datetime', 'temp', 'RH', 'model', 'serial'))
    expect_true(sum(is.na(
      envdata
    )) == 0)

  }
)
test_that(
  "Central parsing function has expected columns and rows for T&D, discards invalid files",
  {
    envdata <- parse_brand(test_path(
      "fixtures", "tandd"
    ), brand = "tandd")
    # 3 warnings expected for invalid file
    expect_warning(parse_brand(test_path(
      "fixtures", "tandd"
    ), brand = "tandd"))
    expect_equal(nrow(envdata), 30)
    expect_equal(names(envdata),
    c('site', 'location', 'datetime', 'temp', 'RH', 'lux', 'UV', 'model', 'serial'))
    expect_true(sum(is.na(
      envdata)) == 0)

  }
)
test_that(
  "Central parsing function has expected columns and rows for TinyTag, discards invalid files",
  {
    envdata <- parse_brand(test_path(
            "fixtures", "tinytag"
          ), brand = "tinytag")
    # 3 warnings expected for invalid files
    expect_warning(parse_brand(test_path(
      "fixtures", "tinytag"
    ), brand = "tinytag"))
    expect_equal(nrow(envdata), 20)
    expect_equal(names(envdata),
    c('site', 'location', 'datetime', 'temp', 'RH', 'model', 'serial'))
    expect_true(sum(is.na(
      envdata
    )) == 0)

  }
)
test_that(
  "Central parsing function has expected columns and rows for Trend, discards invalid files",
  {
    envdata <- parse_brand(test_path(
      "fixtures", "trend"
    ), brand = "trend")
    # 3 warnings expected for invalid files
    expect_warning(parse_brand(test_path(
      "fixtures", "trend"
    ), brand = "trend"))
    expect_equal(nrow(envdata), 30)
    expect_equal(names(envdata),
    c('site', 'location', 'datetime', 'temp', 'RH', 'model', 'serial'))
    expect_equal(sum(is.na(
      envdata
    )), 30)

  }
)

# parse_hanwell ----
test_that("Hanwell file has correct number of columns and rows",
          {
            envdata <- parse_hanwell(test_path("fixtures", "hanwell", "Hanwell_1.csv"))
            expect_equal(nrow(envdata), 10)
            expect_equal(names(envdata),
                         c('site', 'location', 'datetime', 'temp', 'RH', 'model', 'serial'))
          })

test_that("Hanwell file has no NA values", {
  expect_equal(sum(is.na((
    parse_hanwell(test_path("fixtures", "hanwell", "Hanwell_1.csv"))
  ))), 0)
})

test_that("Hanwell file has correct date format", {
  expect_equal(head(parse_hanwell(
    test_path("fixtures", "hanwell", "Hanwell_1.csv")
  )[3], 1),
  tidyr::tibble(
    "datetime" = lubridate::parse_date_time("2025-11-21 00:00:00",
                                            tz = "UTC", orders = "Ymd HMS")
  ))
})

# # parse_meaco ----
test_that("Pre-Gingerbread Meaco file has correct columns and rows with no NA values",
          {
            envdata <- parse_meaco(test_path("fixtures", "meaco", "Meaco_1.csv"))
            expect_equal(nrow(envdata), 10)
            if("temp" %in% names(envdata)) {
              expect_equal(names(envdata),
                           c('site', 'location', 'datetime', 'temp', 'RH', 'model', 'serial'))
            } else {
              expect_equal(names(envdata),
                           c('site', 'location', 'datetime', 'lux', 'UV', 'lux', 'UV', 'model', 'serial'))
            }
          })

test_that("Pre-Gingerbread Meaco file has no NA values", {
  expect_equal(sum(is.na(parse_meaco(test_path("fixtures", "meaco", "Meaco_1.csv")))), 0)
})

test_that("Pre-Gingerbread Meaco file has correct date format", {
  expect_equal(head(parse_meaco(
    test_path("fixtures", "meaco", "Meaco_1.csv")
  )[3], 1),
  tidyr::tibble(
    "datetime" = lubridate::parse_date_time("2025-11-06 15:59:00",
                                            tz = "UTC", orders = "Ymd HMS")
  ))
})

test_that("Gingerbread Meaco file has correct columns and rows",
          {
          envdata <- parse_meaco(test_path("fixtures", "meaco", "Meaco_3.csv"))
          expect_equal(nrow(envdata), 10)
            expect_equal(names(envdata), c('site', 'location', 'datetime', 'temp', 'RH', 'model', 'serial'))
          })

test_that("Gingerbread Meaco file has no NA values", {
  expect_equal(sum(is.na(parse_meaco(test_path("fixtures", "meaco", "Meaco_3.csv")))), 0)
})

test_that("Gingerbread Meaco file has correct date format", {
  expect_equal(head(parse_meaco(
    test_path("fixtures", "meaco", "Meaco_3.csv")
  )[3], 1), tidyr::tibble("datetime" =
                            lubridate::parse_date_time("2025-12-31 00:04:00",
                                                       tz = "UTC", orders = "Ymd HMS")))
})

# # parse_miniclima ----
test_that("miniClima file has correct number of columns and rows with no NA values",
          {
            envdata <- parse_miniclima(
              test_path("fixtures", "miniclima", "miniClima_1.csv"))
            expect_equal(nrow(envdata), 10)
            expect_equal(names(envdata),
                         c('site', 'location', 'datetime', 'temp', 'RH', 'model', 'serial'))
            expect_equal(sum(is.na(envdata)), 0)
          })
test_that("miniClima file has correct date format", {
  expect_equal(head(envdata <- parse_miniclima(
    test_path("fixtures", "miniclima", "miniClima_1.csv"))
  [3], 1),
  tidyr::tibble(
    "datetime" = lubridate::parse_date_time("2025-08-07 00:00:00",
                                            tz = "UTC", orders = 'Ymd HMS')
  ))
})

# parse_previous ----
test_that("Previous csv file has correct number of columns and rows with no NA values",
          {
            envdata <- parse_previous(test_path("fixtures", "previous", "sample_data.csv"))
            expect_equal(nrow(envdata), 16000)
            expect_equal(names(envdata),
                         c('site', 'location', 'datetime', 'temp', 'RH', 'lux', 'UV', 'model', 'serial'))
            expect_equal(sum(is.na(envdata)), 16000)
          })
test_that("Previous csv file has correct date format", {
  expect_equal(head(parse_previous(test_path("fixtures", "previous", "sample_data.csv"))[3], 1),
  tidyr::tibble(
    "datetime" = lubridate::parse_date_time("2021-09-01 01:00:00",
                                            tz = "UTC", orders = 'Ymd HMS')
  ))
})

# # parse_rotronic ----
test_that("Rotronic csv file has correct number of columns and rows with no NA values",
{
  envdata <- parse_meaco(test_path("fixtures", "meaco", "Meaco_3.csv"))
            expect_equal(nrow(envdata), 10)
            expect_equal(names(envdata),
            c('site', 'location', 'datetime', 'temp', 'RH', 'model', 'serial'))
            expect_equal(sum(is.na(envdata)), 0)
          })

test_that("Rotronic tsv file has correct number of columns and rows with no NA values",
          {
            expect_equal(nrow(parse_rotronic(
              test_path("fixtures", "rotronic", "Rotronic_2.xls")
            )), 10)
            expect_equal(names(parse_rotronic(
              test_path("fixtures", "rotronic", "Rotronic_2.xls")
            )),
            c('site', 'location', 'datetime', 'temp', 'RH', 'model', 'serial'))
            expect_equal(sum(is.na(parse_rotronic(
              test_path("fixtures", "rotronic", "Rotronic_2.xls")
            ))), 0)
          })

test_that("Rotronic csv file has correct date format", {
  expect_equal(head(parse_rotronic(
    test_path("fixtures", "rotronic", "Rotronic_1.csv")
  )[3], 1),
  tidyr::tibble(
    "datetime" = lubridate::parse_date_time("2020-01-24 13:37:22", tz = "UTC", orders = 'Ymd HMS')
  ))
})

test_that("Rotronic tsv file has correct date format", {
  expect_equal(head(parse_rotronic(
    test_path("fixtures", "rotronic", "Rotronic_2.xls")
  )[3], 1),
  tidyr::tibble(
    "datetime" = lubridate::parse_date_time("2020-01-24 13:08:35",
                                            tz = "UTC", orders = "Ymd HMS")
  ))
})

# parse_tandd ----
test_that("T&D file has correct number of columns and rows with no NA values", {
  expect_equal(nrow(parse_tandd(
    test_path("fixtures", "tandd", "TandD_1.csv")
  )), 10)
  expect_equal(names(parse_tandd(
    test_path("fixtures", "tandd", "TandD_1.csv")
  )),
  c('site', 'location', 'datetime', 'temp', 'RH', 'lux', 'UV', 'model', 'serial'))
  expect_equal(sum(is.na(parse_tandd(
    test_path("fixtures", "tandd", "TandD_1.csv")
  ))), 0)
})

test_that("T&D file has correct date format", {
  expect_equal(head(parse_tandd(
    test_path("fixtures", "tandd", "TandD_1.csv")
  )[3], 1),
  tidyr::tibble(
    "datetime" = lubridate::parse_date_time("2021-08-21 08:06:00", orders = "Ymd HMS")
  ))
})
#
# # # parse_tinytag ----
test_that("TinyTag file has correct number of columns and rows with no NA values",
          {
            expect_equal(nrow(parse_tinytag(
              test_path("fixtures", "tinytag", "TinyTag_1.csv")
            )), 10)
            expect_equal(names(parse_tinytag(
              test_path("fixtures", "tinytag", "TinyTag_1.csv")
            )),
            c('site', 'location', 'datetime', 'temp', 'RH', 'model', 'serial'))
          })

test_that("TinyTag file has no NA values", {
  expect_equal(sum(is.na(parse_tinytag(
    test_path("fixtures", "tinytag", "TinyTag_1.csv")
  ))), 0)
})

test_that("TinyTag file has correct date format", {
  expect_equal(head(parse_tinytag(
    test_path("fixtures", "tinytag", "TinyTag_1.csv")
  )[3], 1),
  tidyr::tibble(
    "datetime" = lubridate::parse_date_time("2021-07-26 14:26:00",
                                            tz = "UTC", orders = "Ymd HMS")
  ))
})

# parse_trend ----
test_that("TrendBMS file has correct number of columns and rows (T or RH only)",
          {
            envdata <- parse_trend(
              test_path("fixtures", "trend", "Trend_1.csv")
            )
            expect_equal(nrow(envdata), 10)
            expect_equal(ncol(envdata), 6)
            expect_equal(names(envdata),
                         c('site', 'location', 'datetime', 'temp', 'model', 'serial'))
          })

test_that("TrendBMS file has expected NA values", {
  expect_equal(sum(is.na((
    parse_trend(test_path("fixtures", "trend", "Trend_1.csv"))
  ))), 0)
})

test_that("TrendBMS file has correct date format", {
  expect_equal(head(parse_trend(
    test_path("fixtures", "trend", "Trend_1.csv")
  )[3], 1),
  tidyr::tibble(
    "datetime" = lubridate::parse_date_time("2021-09-01 00:30:00",
                                            tz = "UTC", orders = "Ymd HMS")
  ))
})
