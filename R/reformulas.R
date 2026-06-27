# raffalib-r misc helper functions
# Copyright (C) 2026 Raffaele Mancuso
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

#' Add interaction terms in a formula.
#' See: https://stackoverflow.com/q/79750252/1719931.
#' 
#' @param form A formula.
#' @param treat Treatment variable (string).
#' @param controls A character vector of control variables.
#' @return A formula with interaction terms added between `treat` and each variable in `controls`.
#' @export
#' @examples
#' reformulas_addints(mpg ~ cyl + gear, "cyl", c("gear"))
#' reformulas_addints(mpg ~ cyl + gear + disp, "cyl", c("gear", "disp"))
#' reformulas_addints(mpg ~ cyl + gear + disp + hp, "cyl", c("gear", "disp"))
#' reformulas_addints(mpg ~ cyl + cyl * gear, "cyl", c("gear"))
#' # A control that is not in the formula is an error:
#' try(reformulas_addints(mpg ~ cyl + gear, "cyl", c("gears")))
reformulas_addints <- function(form, treat, controls) {
  stopifnot (inherits(form, "formula"))
  terms <- terms(form)
  
  variables <- attr(terms, "term.labels")
  
  stopifnot(controls %in% variables, treat %in% variables)
  
  for (control in controls) {
    new <- as.formula(paste("~ . + ", control, ":", treat))
    form <- update(form, new)
  }
  
  form
}

#' Remove random slopes from a formula, while retaining random intercepts.
#' See: https://github.com/bbolker/reformulas/issues/11#issuecomment-3221120343
#'
#' @param form A formula
#' @return The new formula
#' @export
#' @examples
#' f <- ~ 1 + a  + b + (a | f) + (1 + a | g) + (a + b | h ) + (1 + a + b | i)
#' reformulas_randint(f)
reformulas_randint <- function(form) {
   fixed <- reformulas::nobars(form)
   bars <- reformulas::findbars(form) 
   for (i in seq_along(bars)) {
        bars[[i]][[2]] <- 1
   }
   reformulas::addForm(fixed, Reduce(reformulas::addForm0, bars))
}
