# Tests for the formula helpers in R/reformulas.R.

test_that("reformulas_addints adds a treatment-by-control interaction", {
  f <- reformulas_addints(mpg ~ cyl + gear, "cyl", "gear")
  expect_true("cyl:gear" %in% attr(terms(f), "term.labels"))
})

test_that("reformulas_addints adds one interaction per control", {
  f <- reformulas_addints(mpg ~ cyl + gear + disp, "cyl", c("gear", "disp"))
  labs <- attr(terms(f), "term.labels")
  expect_true(all(c("cyl:gear", "cyl:disp") %in% labs))
})

test_that("reformulas_addints errors when a control is absent from the formula", {
  expect_error(reformulas_addints(mpg ~ cyl + gear, "cyl", "gears"))
})

test_that("reformulas_randint keeps random intercepts and drops random slopes", {
  f <- ~ 1 + a + b + (a | g) + (1 + a | h)
  res <- reformulas_randint(f)
  bars <- reformulas::findbars(res)
  expect_length(bars, 2)
  # every surviving random term is intercept-only: its LHS is the constant 1
  expect_true(all(vapply(bars, function(b) b[[2]] == 1, logical(1))))
})
