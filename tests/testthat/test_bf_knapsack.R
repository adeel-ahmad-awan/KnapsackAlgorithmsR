context("brute_force_knapsack")

suppressWarnings(RNGversion(min(as.character(getRversion()),"3.5.3")))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <- data.frame(
  w=sample(1:4000, size = n, replace = TRUE),
  v=runif(n = n, 0, 10000)
)

test_that("Correct object is returned", {
  expect_silent(gk <- brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500))
  expect_named(gk, c("value", "elements"))
})

test_that("functions rejects errounous input.", {
  expect_error(brute_force_knapsack("hej", 3500))
  expect_error(brute_force_knapsack(x = knapsack_objects[1:8,], W = -3500))
})

test_that("Function return correct results.", {
  gk <- brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
  expect_equal(round(gk$value), 16770)
  expect_equal(as.numeric(gk$elements[1]),5)
  expect_equal(as.numeric(gk$elements[2]),8)

  gk <- brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500)
  expect_equal(round(gk$value), 16770)
  expect_equal(as.numeric(gk$elements[1]),5)
  expect_equal(as.numeric(gk$elements[2]),8)

  gk <- brute_force_knapsack(x = knapsack_objects[1:8,], W = 2000)
  expect_equal(round(gk$value), 15428)

  gk <- brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000)
  expect_equal(round(gk$value), 15428)

  st <- system.time(gk <- brute_force_knapsack(x = knapsack_objects[1:16,], W = 2000))
  expect_true(as.numeric(st)[2] <= 0.01)

  gk <- brute_force_knapsack(x = knapsack_objects[1:800,], W = 3500)
  expect_equal(round(gk$value), 192647)

  gk <- brute_force_knapsack(x = knapsack_objects[1:1200,], W = 3500)
  expect_equal(round(gk$value), 270290)
})
