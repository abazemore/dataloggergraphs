# combine_data
test_that("combine_data has expected rows and columns for TRH data",
         { envdata_mc <- parse_meaco(test_path("fixtures", "meaco", "Meaco_1.csv"))
          envdata_tt <- parse_tinytag(test_path("fixtures", "tinytag", "TinyTag_1.csv"))
          envdata_mc_tt <- combine_data(list(envdata_mc, envdata_tt))
          expect_equal(nrow(envdata_mc_tt), 20)
          expect_equal(names(envdata_mc_tt), c('site', 'location', 'datetime', 'temp', 'RH', 'model', 'serial'))
          expect_equal(sum(is.na(envdata_mc_tt)), 0)}
)
test_that("combine_data has expected rows and columns for TRH and light data",
         { envdata_mc <- parse_meaco(test_path("fixtures", "meaco", "Meaco_1.csv"))
          envdata_td <- parse_tandd(test_path("fixtures", "tandd", "TandD_1.csv"))
          envdata_mc_td <- combine_data(list(envdata_mc, envdata_td))
          expect_equal(nrow(envdata_mc_td), 20)
          expect_equal(names(envdata_mc_td), c('site', 'location', 'datetime', 'temp', 'RH', 'lux', 'UV', 'model', 'serial'))
          expect_equal(sum(is.na(envdata_mc_td)), 20)
})

# remove_faulty

# subset_readings
