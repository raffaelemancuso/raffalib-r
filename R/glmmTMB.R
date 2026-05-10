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

#' @export
glmmTMB_get_hessian_1 <- function(mod) {
  # See: https://github.com/glmmTMB/glmmTMB/issues/1226#issuecomment-3181703715
  # 250% faster than method 2
  bestpar <- with(mod$obj$env, last.par.best[-random])
  H1 <- with(mod$obj, optimHess(bestpar, fn, gr))
  return(H1)
}

#' @export
glmmTMB_get_hessian_2 <- function(mod) {
  # See: https://github.com/glmmTMB/glmmTMB/issues/1226#issuecomment-3181703715
  bestpar <- with(mod$obj$env, last.par.best[-random])
  H2 <- numDeriv::jacobian(mod$obj$gr, bestpar)
  return(H2)
}

#' Get current estimated coefficients from a glmmTMB model
#'
#' @return Object to be passed to the `start` argument of `glmmTMB`
#' @export
glmmTMB_get_optimum <- function(mod) {
  # Fixed effects
  starting_point <- list(
    beta = glmmTMB::fixef(mod)$cond,
    betazi = glmmTMB::fixef(mod)$zi,
    betadisp = glmmTMB::fixef(mod)$disp
  )
  # Random effects
  # ref <- glmmTMB::ranef(mod)
  # if(length(ref$cond) > 0) {
  #   starting_point$theta = ref$cond
  # }
  # if(length(ref$zi) > 0) {
  #   starting_point$thetazi = ref$zi
  # }
  # if(length(ref$disp) > 0) {
  #   starting_point$thetadisp = ref$disp
  # }
  return(starting_point)
}
