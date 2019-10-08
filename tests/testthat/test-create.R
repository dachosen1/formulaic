test_that("multiplication works", {
  n <- 10
  dd <- data.table::data.table(w = rnorm(n= n), x = rnorm(n = n), pixel_1 = rnorm(n = n))
  dd[, 'pixel 2' := 0.3 * pixel_1 + rnorm(n)]
  dd[, pixel_3 := 0.3 * pixel_1 + rnorm(n)]
  dd[, item_1 := 0.3 * pixel_3 + rnorm(n)]
  dd[, item_2 := 0.3 * pixel_3 + rnorm(n)]
  dd[, y := 5 * x + 3 * pixel_1 + 2 * pixel_3 + rnorm(n)]
})
