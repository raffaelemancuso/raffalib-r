# Tests for the gtsummary table helpers (gtsummary_add_mean_diff and the
# gtsummary_mean_diff worker).

toydf_two_groups <- function(n = 500) {
  set.seed(42)
  data.frame(
    height = stats::rnorm(n, 170, 10),
    age = stats::rpois(n, 30),
    high_income = sample(c(TRUE, FALSE), n, replace = TRUE),
    education = factor(sample(c("HS", "BSC", "MSC"), n, replace = TRUE)),
    gender = factor(sample(c("F", "M"), n, replace = TRUE))
  )
}

diff_col <- function(tbl) {
  dplyr::select(
    tbl$table_body,
    dplyr::all_of(c("variable", "var_type", "label", "diff_in_means"))
  )
}

diff_footnote <- function(tbl) {
  fns <- tbl$table_styling$footnote_header
  dplyr::last(fns$footnote[fns$column == "diff_in_means"])
}

mean_diff <- function(df, var, fun = mean) {
  unname(diff(tapply(df[[var]], df$gender, fun, na.rm = TRUE)))
}

col_prop_diff <- function(df, var, level) {
  prop <- prop.table(table(df[[var]], df$gender), margin = 2)
  unname(prop[level, "M"] - prop[level, "F"])
}

test_that("gtsummary_add_mean_diff defaults match manual mean/col_pct diffs", {
  toydf <- toydf_two_groups()
  tbl <- gtsummary::tbl_summary(toydf, by = "gender", missing = "no") |>
    gtsummary_add_mean_diff()
  body <- diff_col(tbl)

  expect_equal(
    body$diff_in_means[body$variable == "height"],
    gtsummary::style_sigfig(mean_diff(toydf, "height"))
  )
  expect_equal(
    body$diff_in_means[body$variable == "education" & body$label == "BSC"],
    sprintf("%.2f%%", 100 * col_prop_diff(toydf, "education", "BSC"))
  )

  fn <- diff_footnote(tbl)
  expect_match(fn, "M minus F", fixed = TRUE)
  expect_match(fn, "means for continuous variables", fixed = TRUE)
  expect_match(
    fn,
    "column percentages (in percentage points) for categorical and dichotomous variables",
    fixed = TRUE
  )
})

test_that("glue templates combine several statistics per cell", {
  toydf <- toydf_two_groups()
  tbl <- gtsummary::tbl_summary(toydf, by = "gender", missing = "no") |>
    gtsummary_add_mean_diff(
      statistic = list(
        height ~ "{mean}\n{median}",
        education ~ "{row_pct}",
        high_income ~ "{cell_pct}"
      )
    )
  body <- diff_col(tbl)

  expect_equal(
    body$diff_in_means[body$variable == "height"],
    paste0(
      gtsummary::style_sigfig(mean_diff(toydf, "height")),
      "\n",
      gtsummary::style_sigfig(mean_diff(toydf, "height", stats::median))
    )
  )

  # age not selected -> falls back to the continuous default "{mean}"
  expect_equal(
    body$diff_in_means[body$variable == "age"],
    gtsummary::style_sigfig(mean_diff(toydf, "age"))
  )

  prop_row <- prop.table(table(toydf$education, toydf$gender), margin = 1)
  expect_equal(
    body$diff_in_means[body$variable == "education" & body$label == "BSC"],
    sprintf("%.2f%%", 100 * unname(prop_row["BSC", "M"] - prop_row["BSC", "F"]))
  )

  prop_cell <- prop.table(table(toydf$high_income, toydf$gender))
  expect_equal(
    body$diff_in_means[body$variable == "high_income"],
    sprintf("%.2f%%", 100 * unname(prop_cell["TRUE", "M"] - prop_cell["TRUE", "F"]))
  )

  fn <- diff_footnote(tbl)
  # mean covers all continuous variables (height + the age fallback), so it is
  # phrased generically; median covers only height, so it is listed
  expect_match(fn, "means for continuous variables", fixed = TRUE)
  expect_match(fn, "medians for height", fixed = TRUE)
  expect_match(
    fn, "row percentages (in percentage points) for education",
    fixed = TRUE
  )
  expect_match(
    fn, "cell percentages (in percentage points) for high_income",
    fixed = TRUE
  )
})

test_that("statistics sharing a scope are merged in the footnote", {
  toydf <- toydf_two_groups()
  tbl <- gtsummary::tbl_summary(toydf, by = "gender", missing = "no") |>
    gtsummary_add_mean_diff(
      statistic = list(
        gtsummary::all_continuous() ~ "{mean}\n{median}",
        gtsummary::all_categorical() ~ "{row_pct} / {col_pct}"
      )
    )
  fn <- diff_footnote(tbl)
  expect_match(fn, "means and medians for continuous variables", fixed = TRUE)
  expect_match(
    fn,
    "column and row percentages (in percentage points) for categorical and dichotomous variables",
    fixed = TRUE
  )
})

test_that("a type-wide statistic keeps the generic footnote wording", {
  toydf <- toydf_two_groups()
  tbl <- gtsummary::tbl_summary(toydf, by = "gender", missing = "no") |>
    gtsummary_add_mean_diff(
      statistic = gtsummary::all_continuous() ~ "{median}"
    )
  expect_match(
    diff_footnote(tbl),
    "medians for continuous variables",
    fixed = TRUE
  )
})

test_that("the digits formula-list-selector overrides rounding", {
  toydf <- toydf_two_groups()
  tbl <- gtsummary::tbl_summary(toydf, by = "gender", missing = "no") |>
    gtsummary_add_mean_diff(
      digits = list(
        height ~ 3,
        high_income ~ function(x) sprintf("%.4f", x)
      )
    )
  body <- diff_col(tbl)

  # continuous with integer digits: fixed decimals instead of sigfig
  expect_match(
    body$diff_in_means[body$variable == "height"],
    "^-?[0-9,]+\\.[0-9]{3}$"
  )
  # function digits format the raw 0-1 value; their output is used verbatim,
  # so no % sign is appended
  expect_match(
    body$diff_in_means[body$variable == "high_income"],
    "^-?0\\.[0-9]{4}$"
  )
  # variables not covered keep the default (education: 2-decimal percentage)
  expect_match(
    body$diff_in_means[body$variable == "education" & body$label == "BSC"],
    "^-?[0-9]+\\.[0-9]{2}%$"
  )

  # categorical with integer digits: decimal places of the percentage,
  # applied to every level row of the variable
  tbl2 <- gtsummary::tbl_summary(toydf, by = "gender", missing = "no") |>
    gtsummary_add_mean_diff(digits = education ~ 0)
  body2 <- diff_col(tbl2)
  expect_match(
    body2$diff_in_means[body2$variable == "education" & body2$label == "BSC"],
    "^-?[0-9]+%$"
  )
  expect_match(
    body2$diff_in_means[body2$variable == "education" & body2$label == "HS"],
    "^-?[0-9]+%$"
  )
})

test_that("invalid digits abort", {
  toydf <- toydf_two_groups()
  tbl <- gtsummary::tbl_summary(toydf, by = "gender", missing = "no")

  expect_error(
    gtsummary_add_mean_diff(tbl, digits = height ~ -1),
    "digits"
  )
  expect_error(
    gtsummary_add_mean_diff(tbl, digits = height ~ c(1, 2)),
    "digits"
  )
})

test_that("invalid or type-mismatched statistics abort", {
  toydf <- toydf_two_groups()
  tbl <- gtsummary::tbl_summary(toydf, by = "gender", missing = "no")

  # bare keywords are no longer accepted: no placeholder in the template
  expect_error(
    gtsummary_add_mean_diff(tbl, statistic = age ~ "mean"),
    "placeholder"
  )
  expect_error(
    gtsummary_add_mean_diff(tbl, statistic = education ~ "col_pct"),
    "placeholder"
  )
  expect_error(
    gtsummary_add_mean_diff(tbl, statistic = age ~ "{geometric_mean}"),
    "Unknown statistic"
  )
  expect_error(
    gtsummary_add_mean_diff(tbl, statistic = age ~ "{col_pct}"),
    "not valid for continuous"
  )
  expect_error(
    gtsummary_add_mean_diff(tbl, statistic = education ~ "{mean}\n{median}"),
    "not valid for categorical"
  )
})
