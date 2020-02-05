context("misc")

test_that("vignette-plot", {

  data(flowers)
  flowers <- flowers[, 1:7]

  vis_flowers <- vismodel(flowers)

  flowerscol <- spec2rgb(flowers)

  expect_doppelganger("vignette-plot", {
    par(mfrow = c(2, 6), oma = c(3, 3, 0, 0))
    layout(rbind(c(2, 1, 4 , 3, 6 , 5),
                 c(1, 1, 3 , 3, 5 , 5),
                 c(8, 7, 10, 9, 12, 11),
                 c(7, 7, 9 , 9, 11, 11)))
    for (i in 1:6) {
      par(mar = c(2, 2, 2, 2))
      plot(flowers, select = i+1, col = flowerscol, lwd = 3, ylim = c(0, 100))
      par(mar = c(4.1, 2.5, 2.5, 2))
      barplot(as.matrix(vis_flowers[i, 1:4]), yaxt = "n", col = "black")
    }

    mtext("Wavelength (nm)", side = 1, outer = TRUE, line = 1)
    mtext("Reflectance (%)", side = 2, outer = TRUE, line = 1)
  })

})

