test_that("my sq work", {
  l <- mysq(2:4)
  expect_equal(l[1], 4)
  a <- myncurve(mu=10,sigma=3,a=5)
  expect_equal(a[1], 0.0478)
})


