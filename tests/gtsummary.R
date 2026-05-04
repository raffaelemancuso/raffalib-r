library(gtsummary)

N <- 1000
toydf <- data.frame(
  height = rnorm(N, mean = 170, sd = 10),
  weight = rnorm(N, mean = 70, sd = 15),
  male = sample(c(TRUE, FALSE), N, replace = TRUE),
  gender = sample(c("M", "F"), N, replace = TRUE),
  high_income = sample(c(TRUE, FALSE), N, replace = TRUE),
  children = rbinom(N, size = 3, prob = 0.5),
  age = rpois(N, lambda = 30),
  education = sample(c("HS", "BSC", "MSC", "PHD"), N, replace = TRUE)
)
toydf$gender %<>% as.factor()
toydf$age %>% summary()

my_mean_diff <- function(data, variable, by, tbl, ...) {
  x <- data[[variable]]
  g <- data[[by]]
  lvls <- levels(g)
  vartype <- class(x)[1]
  
  if(
    (vartype=="character" | vartype=="factor" | vartype=="numeric" | vartype=="integer") &
    length(unique(x)) <= 10
  ) 
  {
    vartype <- "categorical"
  }
  
  print(paste0("Variable: ", variable, ", Type: ", vartype))
  
  switch(
    vartype,
    factor = {
      prop <- prop.table(table(x, g), margin = 2)
      return((prop[, lvls[2]] - prop[, lvls[1]]) * 100)
    },
    categorical = {
      prop <- prop.table(table(x, g), margin = 2)
      return((prop[, lvls[2]] - prop[, lvls[1]]) * 100)
    },
    character = {
      prop <- prop.table(table(x, g), margin = 2)
      return((prop[, lvls[2]] - prop[, lvls[1]]) * 100)
    },
    numeric = {
      return(diff(tapply(x, g, mean, na.rm = TRUE)))
    },
    integer = {
      return(diff(tapply(x, g, mean, na.rm = TRUE)))
    },
    logical = {
      prop <- prop.table(table(x, g), margin = 2)
      diffs <- (prop[, lvls[2]] - prop[, lvls[1]]) * 100
      return(diffs["TRUE"])
    },
    {
      stop(glue("ERROR: Unrecognized type {vartype}"))
    }
  )
}

toydf %>% 
  tbl_summary(
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "no",
    by = "gender",
    percent = "column"
  ) %>% gtsummary::add_stat(
    fns = gtsummary::everything() ~ my_mean_diff,
    location = list(
      gtsummary::all_continuous() ~ "label",
      gtsummary::all_categorical() ~ "level",
      gtsummary::all_dichotomous() ~ "label"
    )
  ) %>%
  raffalib::gtsummary_add_mean_diff() %>%
  add_p() %>%
  raffalib::gtsummary_add_significance_stars() %>%
  add_overall() %>%
  modify_header(
    all_stat_cols() ~
      "**{level}**\nN = {style_number(n, big.mark=',')}\n({style_percent(p, digits=0)}%)"
  )
