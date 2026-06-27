# raffalib

<!-- badges: start -->
<!-- badges: end -->

A personal R package collecting helper functions the author reaches for again
and again in empirical research: building publication-ready tables and exporting
them to Word, quick descriptive statistics and data-wrangling shortcuts,
list/vector/data-frame utilities, formula manipulation, and a set of helpers
around [`glmmTMB`](https://github.com/glmmTMB/glmmTMB) (alternative optimizers
and control-function 2SLS).

It is a grab-bag of conveniences rather than a single-purpose package; pick the
pieces you need. Everything is documented — see `?raffalib` and the individual
help pages.

## Installation

```r
# install.packages("remotes")
remotes::install_github("raffaelemancuso/raffalib-r")
```

The package leans on a fairly wide set of CRAN packages (it `Imports` modelsummary,
gtsummary, flextable, officer, glmmTMB, optimx, calibrar, and others); installing
from GitHub will pull them in.

## What's inside

### Tables and Word export

| Function | Purpose |
| --- | --- |
| `correlation_table()` | Correlation matrix as a `flextable`, with a numbered variable legend in the footer. |
| `flextable2docx()`, `ggplot2docx()`, `plot2docx()` | Write a `flextable`, a `ggplot`, or a base R plot to a `.docx` file, with caption and page-geometry control. |
| `modelsummary_build_labelled_coef_map()` | Turn variable labels (including factor levels) into a `coef_map` for `modelsummary()`. |
| `modelsummary_common_coefs_at_bottom()` | Re-order coefficients so terms shared across models print last. |
| `modelsummary_getgofmap()`, `modelsummary_missing_variables_in_coef_map()` | Goodness-of-fit map helper and coef-map diagnostics. |
| `gtsummary_add_mean_diff()`, `gtsummary_add_significance_stars()`, `gtsummary_format_statistic_column()` | Extensions for `gtsummary` tables: between-group differences, custom significance stars, statistic-column formatting. |

### Descriptive statistics

`myfreq()` (frequency table that always shows `NA`s), `descquant()` (evenly
spaced quantiles), `descstrange()` (counts of `NA`/`NaN`/`Inf`), `na_per_group()`,
`dupsa()` (assert that grouping variables uniquely identify rows), and
`get_dropped_obs()` (the rows a model dropped through listwise deletion).

### Data wrangling

`nan2na()` (replace `NaN` with `NA` in numeric columns), `sort_columns_by_label()`
/ `sort_columns_by_name()`, `catcols()` (print matching column names), and
`startlog()` / `endlog()` to bracket a pipeline and report how many rows,
columns or cells it changed.

### Lists and vectors

`list_rename_names()` / `list_rename_values()`, `sort_named_list_by_names()` /
`sort_named_list_by_values()`, `catvec()`, `as_named_list()`, `vec_relocate()`.

### Formulas

`reformulas_addints()` (add treatment-by-control interactions) and
`reformulas_randint()` (strip random slopes, keep random intercepts).

### `glmmTMB` helpers

- **`glmmTMB_2sls()`** — control-function 2SLS with a (possibly non-Gaussian,
  possibly mixed) `glmmTMB` second stage. For a Gaussian second stage it
  reproduces textbook 2SLS; for count/binary outcomes it is the consistent
  alternative to the "forbidden regression". Returns an object with
  `print`/`tidy`/`glance`/`nobs` methods, so it flows straight into
  `modelsummary()`, and offers cluster-bootstrap standard errors that correct
  for the generated regressor.
- **Alternative optimizers** — three families of `glmmTMBControl()` constructors
  that swap in optimizers from `optimx` and `calibrar` when the default
  `nlminb()` struggles to converge:
  `glmmTMB_control_optimx_*()` (e.g. `_bfgs`, `_nlminb`, `_bobyqa`),
  `glmmTMB_control_calibrar_*()` (local methods),
  `glmmTMB_control_optimh_*()` (global/heuristic methods such as CMA-ES,
  differential evolution, particle swarm).
- **`glmmTMB_get_optimum()`**, **`glmmTMB_get_hessian_1()`** /
  **`glmmTMB_get_hessian_2()`** — extract the current estimates (to warm-start a
  refit) and the Hessian at the optimum.

### Miscellaneous

`find_method()` (which S3 method a generic would dispatch), `gen_batches()`
(split an index range into chunks), and `save_backup()` / `read_backup()`
(time-stamped `.rds` snapshots).

## Examples

Save a regression table to Word:

```r
library(raffalib)
library(modelsummary)

mod <- lm(mpg ~ wt + hp, data = mtcars)
tbl <- modelsummary(mod, output = "flextable")
flextable2docx(tbl, "regression.docx", word_prop = list(caption_text = "Table 1"))
```

Fit an IV model whose second stage is a Poisson `glmmTMB`:

```r
m <- glmmTMB_2sls(
  first_stage  = x ~ z + w,   # endogenous x, instrument z, control w
  second_stage = y ~ x + w,
  data = d, family = poisson(),
  instruments = "z", n_boot = 200
)
m                 # weak-instrument and Wu-Hausman diagnostics
generics::tidy(m) # ready for modelsummary()
```

Refit a stubborn model with a different optimizer:

```r
glmmTMB::glmmTMB(
  count ~ mined + (1 | site),
  family = poisson, data = glmmTMB::Salamanders,
  control = glmmTMB_control_optimx_bfgs()
)
```

## License

GPL-3. See `LICENSE.txt`.
