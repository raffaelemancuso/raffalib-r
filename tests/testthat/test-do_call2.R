# Tests for do_call2() in R/do_call2.R.
#
# The contract is exact object identity: do_call2(f, list(...)) must return an
# object identical() to a direct f(...) call, for every model class we care
# about and every way of naming the function. The failure mode it guards
# against is silent -- the fit is numerically correct either way, it just drags
# a copy of the data (or of the function body) around inside $call.

# A fitter's $call is recorded in the frame the call is evaluated in, so the
# comparison has to be made in one place. Each helper below builds `direct` and
# the do_call2() variants side by side in the same environment.

test_that("do_call2() reproduces a direct lm() call exactly", {
  direct <- lm(mpg ~ wt, data = mtcars)

  expect_identical(do_call2(lm, list(mpg ~ wt, data = mtcars)), direct)
  expect_identical(do_call2("lm", list(mpg ~ wt, data = mtcars)), direct)
  # already-quoted input must not be quoted twice
  expect_identical(
    do_call2(lm, list(quote(mpg ~ wt), data = quote(mtcars))),
    direct
  )
  expect_identical(
    do_call2("lm", list(quote(mpg ~ wt), data = quote(mtcars))),
    direct
  )
})

test_that("base do.call() does NOT reproduce it (the reason this exists)", {
  direct <- lm(mpg ~ wt, data = mtcars)

  # function object -> the whole body of lm() lands in $call
  expect_false(identical(do.call(lm, list(mpg ~ wt, data = mtcars)), direct))
  # string, unquoted args -> the data frame lands in $call
  expect_false(identical(do.call("lm", list(mpg ~ wt, data = mtcars)), direct))
  # the manual fix does work, and do_call2() must agree with it
  expect_identical(
    do.call("lm", list(quote(mpg ~ wt), data = quote(mtcars))),
    direct
  )
})

test_that("do_call2() reproduces a direct glm() call exactly", {
  df <- data.frame(y = c(rep(0L, 15), rep(1L, 17)), x = mtcars$wt)
  direct <- glm(y ~ x, data = df, family = binomial())

  expect_identical(do_call2(glm, list(y ~ x, data = df, family = binomial())), direct)
  expect_identical(do_call2("glm", list(y ~ x, data = df, family = binomial())), direct)
  expect_identical(
    do_call2(glm, list(quote(y ~ x), data = quote(df), family = quote(binomial()))),
    direct
  )
})

test_that("do_call2() reproduces a direct MASS::glm.nb() call exactly", {
  skip_if_not_installed("MASS")

  set.seed(1)
  df <- data.frame(x = rnorm(200))
  df$y <- MASS::rnegbin(200, mu = exp(1 + 0.5 * df$x), theta = 2)

  direct <- MASS::glm.nb(y ~ x, data = df)
  # glm.nb() rebuilds and re-evaluates its own call internally, so this is a
  # sharper test than lm(): the recorded call has to survive that round trip
  expect_identical(do_call2(MASS::glm.nb, list(y ~ x, data = df)), direct)

  # spelled as a bare name after attaching the namespace-qualified function
  glm.nb <- MASS::glm.nb
  direct_bare <- glm.nb(y ~ x, data = df)
  expect_identical(do_call2(glm.nb, list(y ~ x, data = df)), direct_bare)
  expect_identical(do_call2("glm.nb", list(y ~ x, data = df)), direct_bare)
})

test_that("identical() is unattainable for glmmTMB, by construction", {
  skip_if_not_installed("glmmTMB")

  # Documents why the glmmTMB test below asserts on $call rather than on the
  # whole object: a glmmTMB fit holds external pointers to its TMB AD objects
  # and records its own wall-clock optimisation time, so two fits are never
  # identical() even when both come from the same direct call. Weakening the
  # assertion there is therefore not hiding a do_call2() defect.
  set.seed(2)
  df <- data.frame(x = rnorm(200))
  df$y <- rpois(200, lambda = exp(1 + 0.4 * df$x))

  a <- glmmTMB::glmmTMB(y ~ x, data = df, family = stats::poisson)
  b <- glmmTMB::glmmTMB(y ~ x, data = df, family = stats::poisson)

  expect_false(identical(a, b))
  expect_identical(a$call, b$call) # ...but the call is stable
})

test_that("do_call2() reproduces a direct glmmTMB() call", {
  skip_if_not_installed("glmmTMB")

  set.seed(2)
  df <- data.frame(x = rnorm(300), g = factor(rep(1:15, each = 20)))
  df$y <- rpois(300, lambda = exp(1 + 0.4 * df$x))

  # the namespaced head records itself as written, so compare against a
  # namespaced direct call
  direct <- glmmTMB::glmmTMB(y ~ x, data = df, family = glmmTMB::nbinom2)
  fitted <- do_call2(glmmTMB::glmmTMB, list(y ~ x, data = df, family = glmmTMB::nbinom2))
  expect_identical(fitted$call, direct$call)
  expect_equal(glmmTMB::fixef(fitted), glmmTMB::fixef(direct))

  glmmTMB <- glmmTMB::glmmTMB
  nbinom2 <- glmmTMB::nbinom2
  direct_bare <- glmmTMB(y ~ x, data = df, family = nbinom2)

  for (fitted in list(
    do_call2(glmmTMB, list(y ~ x, data = df, family = nbinom2)),
    do_call2("glmmTMB", list(y ~ x, data = df, family = nbinom2)),
    do_call2(glmmTMB, list(quote(y ~ x), data = quote(df), family = quote(nbinom2)))
  )) {
    expect_identical(fitted$call, direct_bare$call)
    expect_identical(fitted$call$data, as.name("df"))
    expect_equal(glmmTMB::fixef(fitted), glmmTMB::fixef(direct_bare))
  }

  # with a random effect, and already quoted
  direct_re <- glmmTMB(y ~ x + (1 | g), data = df, family = nbinom2)
  fitted_re <- do_call2(
    glmmTMB,
    list(quote(y ~ x + (1 | g)), data = quote(df), family = quote(nbinom2))
  )
  expect_identical(fitted_re$call, direct_re$call)
  expect_equal(glmmTMB::fixef(fitted_re), glmmTMB::fixef(direct_re))

  # the payoff: glmmTMB already keeps $frame and the TMB data, so an inlined
  # copy on top of that is pure waste.
  #
  # Compare the size of $call, not of the whole fit: every fit here also
  # captures this test frame through its formula environment, and that frame
  # holds the other model objects, which swamps the difference. $call is what
  # do_call2() changes and is not environment-dependent.
  inlined <- do.call("glmmTMB", list(y ~ x, data = df, family = nbinom2))
  by_name <- do_call2(glmmTMB, list(y ~ x, data = df, family = nbinom2))
  expect_true(is.data.frame(inlined$call$data))
  expect_lt(
    length(serialize(by_name$call, NULL)),
    length(serialize(inlined$call, NULL))
  )
})

test_that("the inlined copy is not deduplicated away on serialization", {
  # Why the saving is real rather than an illusion of object.size(): R shares
  # references when serializing ENVIRONMENTS, but not ordinary vectors and
  # lists. A data frame reachable twice is written twice. So the copy do.call()
  # leaves in $call costs its full size on disk, on every saveRDS(), every
  # future export and every cluster send -- it is never folded into the copy
  # the fit already holds.
  ser <- function(x) length(serialize(x, NULL))
  big <- data.frame(y = rnorm(4000), x = rnorm(4000))
  for (k in 1:10) big[[paste0("unused", k)]] <- rnorm(4000)

  expect_equal(ser(list(big, big)), 2 * ser(list(big)), tolerance = 0.01)

  # ...whereas an environment reachable twice is written once
  e <- new.env()
  assign("d", big, envir = e)
  expect_equal(ser(list(e, e)), ser(list(e)), tolerance = 0.01)

  # hence the difference between the two $call objects is the whole data frame
  inlined <- do.call("lm", list(y ~ x, data = big))
  by_name <- do_call2(lm, list(y ~ x, data = big))
  expect_gt(ser(inlined$call) - ser(by_name$call), ser(big) * 0.9)
})

test_that("the data frame is kept out of $call", {
  big <- data.frame(y = rnorm(500), x = rnorm(500))
  for (k in 1:10) big[[paste0("unused", k)]] <- rnorm(500)

  inlined <- do.call("lm", list(y ~ x, data = big))
  by_name <- do_call2(lm, list(y ~ x, data = big))

  # the symptom: what is stored in $call
  expect_true(is.data.frame(inlined$call$data))
  expect_identical(by_name$call$data, as.name("big"))

  # and the consequence
  expect_lt(
    as.numeric(utils::object.size(by_name)),
    as.numeric(utils::object.size(inlined))
  )
  expect_equal(coef(by_name), coef(inlined))
})

test_that("the function body is kept out of $call", {
  fitted <- do_call2(lm, list(mpg ~ wt, data = mtcars))
  expect_identical(fitted$call[[1L]], as.name("lm"))

  inlined <- do.call(lm, list(mpg ~ wt, data = mtcars))
  expect_true(is.function(inlined$call[[1L]]))
})

test_that("a pkg::fun head is recorded as written", {
  direct <- stats::lm(mpg ~ wt, data = mtcars)
  fitted <- do_call2(stats::lm, list(mpg ~ wt, data = mtcars))

  expect_identical(fitted, direct)
  expect_identical(fitted$call[[1L]], quote(stats::lm))
  expect_equal(coef(fitted), coef(lm(mpg ~ wt, data = mtcars)))
})

test_that("quote = TRUE falls through to base do.call() semantics", {
  a <- do_call2("lm", list(mpg ~ wt, data = mtcars), quote = TRUE)
  b <- do.call("lm", list(mpg ~ wt, data = mtcars), quote = TRUE)
  expect_identical(a, b)
})

test_that("envir is honoured", {
  f <- function() {
    hidden <- mtcars[1:10, ]
    do_call2(lm, list(mpg ~ wt, data = hidden))
  }
  m <- f()
  expect_identical(m$call$data, as.name("hidden"))
  expect_equal(nobs(m), 10L)
})

test_that("a formula carrying its own environment still resolves", {
  # a formula is a call to `~`; splicing it into a call unevaluated would
  # rebuild it against the wrong environment and lose `z`
  mk <- function() {
    z <- mtcars$wt * 2
    mpg ~ z
  }
  fml <- mk()

  base_fit <- do.call("lm", list(fml, data = mtcars))
  expect_equal(
    coef(do_call2(lm, list(fml, data = mtcars))),
    coef(base_fit)
  )

  args <- list(fml, data = mtcars) # pre-built: the fallback path
  expect_equal(coef(do_call2(lm, args)), coef(base_fit))
})

test_that("documented fallbacks behave like base do.call()", {
  # pre-built arg list: the caller has already discarded the expressions
  args <- list(mpg ~ wt, data = mtcars)
  expect_identical(do_call2(lm, args), do.call("lm", args))

  # anonymous function: no name to record
  fitted <- do_call2(function(f, d) lm(f, data = d), list(mpg ~ wt, mtcars))
  expect_equal(coef(fitted), coef(lm(mpg ~ wt, data = mtcars)))
})

test_that("argument names and positions are preserved", {
  direct <- lm(mpg ~ wt, data = mtcars, weights = mtcars$am + 1)
  expect_identical(
    do_call2(lm, list(mpg ~ wt, data = mtcars, weights = mtcars$am + 1)),
    direct
  )

  # partial matching and out-of-order named arguments
  direct2 <- lm(data = mtcars, formula = mpg ~ wt)
  expect_identical(do_call2(lm, list(data = mtcars, formula = mpg ~ wt)), direct2)
})

test_that("it works with no arguments and with a single argument", {
  expect_identical(do_call2(list, list()), list())
  expect_identical(do_call2("sum", list(1:10)), 55L)
  expect_identical(do_call2(sum, list(1:10)), 55L)
})
