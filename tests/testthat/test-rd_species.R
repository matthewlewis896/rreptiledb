test_that("rd_species returns a tibble", {
  expect_s3_class(rd_species(), 'tbl_df')
})
