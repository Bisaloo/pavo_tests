context("colspace")

data(sicalis)

test_that("tcs", {

  tcs_sicalis <- colspace(vismodel(sicalis, relative = TRUE))

  expect_doppelganger("tetraplot", plot(tcs_sicalis, col = spec2rgb(sicalis)))
  expect_identical(plot(tcs_sicalis), tetraplot(tcs_sicalis))

  skip_if_not_installed("mapproj")

  expect_doppelganger("projplot", projplot(tcs_sicalis))

})

test_that("tri", {

  tri_sicalis <- colspace(vismodel(sicalis, visual = "apis"))

  expect_doppelganger("triplot", plot(tri_sicalis, col = spec2rgb(sicalis)))
  expect_identical(plot(tri_sicalis), triplot(tri_sicalis))

})

test_that("di", {

  di_sicalis <- colspace(vismodel(sicalis, visual = "canis"))

  expect_doppelganger("diplot", plot(di_sicalis, col = spec2rgb(sicalis)))
  expect_identical(plot(di_sicalis), diplot(di_sicalis))

})

test_that("cie", {

  ciexyz_sicalis <- colspace(vismodel(sicalis, visual = "cie10"))
  cielch_sicalis <- colspace(vismodel(sicalis, visual = "cie10"), space = "cielch")
  cielab_sicalis <- colspace(vismodel(sicalis, visual = "cie10"), space = "cielab")

#  expect_doppelganger("ciexyz", plot(ciexyz_sicalis, ciebg = FALSE))
  expect_doppelganger("cielch", plot(cielch_sicalis))
  expect_doppelganger("cielab", plot(cielab_sicalis))

  expect_identical(plot(cielab_sicalis), cieplot(cielab_sicalis))

})

test_that("cat", {

  cat_sicalis <- colspace(vismodel(sicalis, visual = "musca"), space = "categorical")

  expect_doppelganger("catplot", plot(cat_sicalis))

  expect_identical(plot(cat_sicalis), catplot(cat_sicalis))

})

test_that("seg", {

  seg_sicalis <- colspace(vismodel(sicalis, visual = "segment"))

  expect_doppelganger("segplot", plot(seg_sicalis))

  expect_identical(plot(seg_sicalis), segplot(seg_sicalis))

})

test_that("hexagon", {

  hex_sicalis <- colspace(vismodel(sicalis, visual = "apis", relative = FALSE), space = "hexagon")

  expect_doppelganger("hexagon", plot(hex_sicalis))

  expect_identical(plot(hex_sicalis), hexplot(hex_sicalis))

})

test_that("coc", {

  coc_sicalis <- colspace(vismodel(sicalis, visual = "apis", relative = FALSE, qcatch = "Ei", vonkries = TRUE), space = "coc")

  expect_doppelganger("cocplot", plot(coc_sicalis))

  expect_identical(plot(coc_sicalis), cocplot(coc_sicalis))

})
