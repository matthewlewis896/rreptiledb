test_that("rd_fetch rejects single word inputs", {
  expect_error(rd_fetch('test'))
})

test_that("rd_fetch rejects subspecies", {
  expect_error(rd_fetch('Boa constrictor constrictor'))
})

test_that("rd_fetch rejects non-species", {
  expect_error(rd_fetch('test test'))
})

test_that("rd_fetch returns a list of length 11", {
  expect_equal(length(rd_fetch('Boa constrictor')), 11)
})

test_that("rd_fetch list items are character", {
  expect_type(rd_fetch('Boa constrictor')[[1]], 'character')
})
