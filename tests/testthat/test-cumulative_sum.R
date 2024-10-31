

test_that("cumulative sum with reset works", {
  set.seed(1)
  x <- sample(1000)
  cut <- sort(sample(1000, 5))
  
  expect_identical(
    cumsum_cut      (x, cut),
    cumsum_cut_naive(x, cut)
  )
})
