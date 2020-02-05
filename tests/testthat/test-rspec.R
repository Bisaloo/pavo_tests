context("rspec")
data(sicalis)
data(teal)

test_that("plot.rspec", {

  expect_doppelganger("plot.rspec-overlay", plot(sicalis, col = spec2rgb(sicalis)))
  expect_identical({plot(sicalis, type = "overlay")}, {plot(sicalis)})
  expect_doppelganger("plot.rspec-stack", plot(sicalis, type = "stack"))
  expect_doppelganger("plot.rspec-select", plot(sicalis, select = 2))

  # svg doesn't work well with gradients
  # expect_doppelganger("plot.rspec-heatmap", plot(sicalis, type = "heatmap"))

})

test_that("explorespec", {

  expect_doppelganger("explorespec", {par(mfrow = c(3,3)); explorespec(sicalis[,1:10])})

})

test_that("aggplot", {

  expect_doppelganger("aggplot", aggplot(sicalis, by = 3))
  expect_doppelganger("aggplot-quantile", aggplot(sicalis, by = 3, FUN.error = function(x) quantile(x, c(0.0275, 0.975))) )
  expect_doppelganger("aggplot-shadecol", aggplot(sicalis, by = 3, shadecol = spec2rgb(sicalis), lcol = 1))

})

test_that("plotsmooth", {

  expect_doppelganger("plotsmooth", plotsmooth(sicalis[1:7], ask = FALSE))

})

test_that("peakshape", {

  expect_doppelganger("peakshape", {par(mfrow = c(3,3)); peakshape(sicalis[,1:10])})
  expect_doppelganger("peakshape-lims", {par(mfrow = c(3,3)); peakshape(sicalis[,1:10], lim = c(300,400))})

})
