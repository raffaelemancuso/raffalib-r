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

# decimal places of the first number in the group cells of a variable
# (mean / median) or of their percentage
shown_decimals <- function(tbl, var, pct = FALSE) {
  cells <- unlist(tbl$table_body[tbl$table_body$variable == var, c("stat_1", "stat_2")])
  cells <- cells[!is.na(cells)]
  pattern <- if (pct) "[0-9.]+(?=%)" else "^-?[0-9,]+(\\.[0-9]+)?"
  nums <- regmatches(cells, regexpr(pattern, cells, perl = TRUE))
  max(nchar(sub("^[^.]*\\.?", "", nums)))
}

# the difference formatted with the decimals shown by the group columns
expected_mean_diff <- function(tbl, toydf, var, fun = mean) {
  gtsummary::style_number(
    mean_diff(toydf, var, fun),
    digits = shown_decimals(tbl, var), big.mark = ","
  )
}
expected_pct_diff <- function(tbl, var, d) {
  sprintf("%.*f%%", shown_decimals(tbl, var, pct = TRUE), 100 * d)
}

test_that("gtsummary_add_mean_diff defaults match manual mean/col_pct diffs", {
  toydf <- toydf_two_groups()
  tbl <- gtsummary::tbl_summary(toydf, by = "gender", missing = "no") |>
    gtsummary_add_mean_diff()
  body <- diff_col(tbl)

  expect_equal(
    body$diff_in_means[body$variable == "height"],
    expected_mean_diff(tbl, toydf, "height")
  )
  expect_equal(
    body$diff_in_means[body$variable == "education" & body$label == "BSC"],
    expected_pct_diff(tbl, "education", col_prop_diff(toydf, "education", "BSC"))
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
      expected_mean_diff(tbl, toydf, "height"),
      "\n",
      expected_mean_diff(tbl, toydf, "height", stats::median)
    )
  )

  # age not selected -> falls back to the continuous default "{mean}"
  expect_equal(
    body$diff_in_means[body$variable == "age"],
    expected_mean_diff(tbl, toydf, "age")
  )

  prop_row <- prop.table(table(toydf$education, toydf$gender), margin = 1)
  expect_equal(
    body$diff_in_means[body$variable == "education" & body$label == "BSC"],
    expected_pct_diff(
      tbl, "education", unname(prop_row["BSC", "M"] - prop_row["BSC", "F"])
    )
  )

  prop_cell <- prop.table(table(toydf$high_income, toydf$gender))
  expect_equal(
    body$diff_in_means[body$variable == "high_income"],
    expected_pct_diff(
      tbl, "high_income", unname(prop_cell["TRUE", "M"] - prop_cell["TRUE", "F"])
    )
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
  # variables not covered follow the decimals of the group columns
  # (education: gtsummary's default whole-number percentages)
  expect_equal(shown_decimals(tbl, "education", pct = TRUE), 0)
  expect_match(
    body$diff_in_means[body$variable == "education" & body$label == "BSC"],
    "^-?[0-9]+%$"
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

test_that("the difference copies the decimals shown by the group columns", {
  toydf <- toydf_two_groups()
  tbl <- gtsummary::tbl_summary(
    toydf,
    by = "gender",
    missing = "no",
    statistic = list(
      gtsummary::all_continuous() ~ "{mean} ({sd})",
      gtsummary::all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(height ~ 3, age ~ 0, education ~ c(0, 1), high_income ~ c(0, 2))
  ) |>
    gtsummary_add_mean_diff()
  body <- diff_col(tbl)

  # continuous: as many decimals as the mean of the group columns
  expect_match(body$diff_in_means[body$variable == "height"], "^-?[0-9,]+\\.[0-9]{3}$")
  expect_match(body$diff_in_means[body$variable == "age"], "^-?[0-9,]+$")
  expect_equal(
    body$diff_in_means[body$variable == "height"],
    gtsummary::style_number(mean_diff(toydf, "height"), digits = 3, big.mark = ",")
  )
  # percentages: as many decimals as the percentage of the group columns
  expect_match(
    body$diff_in_means[body$variable == "education" & body$label == "BSC"],
    "^-?[0-9]+\\.[0-9]{1}%$"
  )
  expect_match(body$diff_in_means[body$variable == "high_income"], "^-?[0-9]+\\.[0-9]{2}%$")

  # the mean on one line and the SD on the next: still the first number
  tbl2 <- gtsummary::tbl_summary(
    toydf,
    by = "gender",
    missing = "no",
    statistic = gtsummary::all_continuous() ~ "{mean}\n({sd})",
    digits = gtsummary::all_continuous() ~ 2
  ) |>
    gtsummary_add_mean_diff()
  body2 <- diff_col(tbl2)
  expect_match(body2$diff_in_means[body2$variable == "height"], "^-?[0-9,]+\\.[0-9]{2}$")

  # nothing to read (no percentage shown): the fallbacks
  tbl3 <- gtsummary::tbl_summary(
    toydf,
    by = "gender",
    missing = "no",
    statistic = gtsummary::all_categorical() ~ "{n}"
  ) |>
    gtsummary_add_mean_diff()
  body3 <- diff_col(tbl3)
  expect_match(
    body3$diff_in_means[body3$variable == "education" & body3$label == "BSC"],
    "^-?[0-9]+\\.[0-9]{2}%$"
  )
})

test_that("gtsummary_set_theme puts the SD on a new line by default", {
  toydf <- toydf_two_groups()
  gtsummary_set_theme()
  withr::defer(gtsummary::reset_gtsummary_theme())

  tbl <- gtsummary::tbl_summary(toydf, by = "gender", missing = "no")
  body <- tbl$table_body
  expect_match(body$stat_1[body$variable == "height"], "^[0-9,.]+\n\\([0-9,.]+\\)$")
  expect_match(body$stat_1[body$variable == "education" & body$label == "BSC"], "^[0-9,]+ \\([0-9.]+%\\)$")

  # an explicit statistic still wins
  tbl2 <- gtsummary::tbl_summary(
    toydf,
    by = "gender",
    missing = "no",
    statistic = gtsummary::all_continuous() ~ "{median}"
  )
  expect_match(tbl2$table_body$stat_1[tbl2$table_body$variable == "height"], "^[0-9,.]+$")

  # the difference column reads its digits from the first line
  tbl3 <- gtsummary_add_mean_diff(tbl)
  body3 <- diff_col(tbl3)
  expect_equal(
    body3$diff_in_means[body3$variable == "height"],
    expected_mean_diff(tbl, toydf, "height")
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
