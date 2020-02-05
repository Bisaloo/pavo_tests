context("colour volume")

data(sicalis)

test_that("voloverlap", {

  tcs.sicalis.C <- subset(colspace(vismodel(sicalis)), "C")
  tcs.sicalis.T <- subset(colspace(vismodel(sicalis)), "T")
  expect_doppelganger("voloverlap", voloverlap(tcs.sicalis.T, tcs.sicalis.C, plot = TRUE))

})
