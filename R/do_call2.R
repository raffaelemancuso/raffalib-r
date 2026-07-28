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

# --- do.call() THAT DOES NOT INLINE ITS ARGUMENTS INTO $call --- #

#' Call a function by name without inlining the arguments into `$call`
#'
#' A drop-in replacement for [base::do.call()] whose result is `identical()` to
#' the object a direct call would have produced. `do.call()` builds the call it
#' evaluates out of the *values* in `args`, so a model fitted through it stores
#' a full copy of the data (and, if `what` is passed as a function object rather
#' than a name, the function's entire body) inside `$call`. That copy travels
#' with the object through [saveRDS()], `future` exports and cluster workers.
#'
#' The fix is due to G. Grothendieck: pass the function as a *string* and the
#' arguments *quoted*, so the recorded call refers to things by name.
#'
#' ```r
#' out1 <- lm(mpg ~ wt, data = mtcars)
#' out2 <- do.call("lm", list(quote(mpg ~ wt), data = quote(mtcars)))
#' identical(out1, out2)
#' #> TRUE
#' ```
#'
#' `do_call2()` applies both transformations automatically. Neither can be done
#' from the values in `args` — by the time `do.call()` sees them the names are
#' gone — so both read the caller's unevaluated expressions via
#' [base::substitute()].
#'
#' @section What it does:
#' \enumerate{
#'   \item **Stringifies the function name.** `do_call2(lm, ...)` records
#'     `lm(...)` rather than inlining the body of `lm`. A `pkg::fun` head is
#'     kept as written.
#'   \item **Quotes the arguments.** The expressions the caller wrote inside
#'     `list(...)` are spliced into the call, so `data = big` is recorded as the
#'     symbol `big` instead of the data frame.
#' }
#' Both steps are idempotent: input already in the fixed form (an explicit
#' `quote()` around an argument) is left alone rather than quoted twice.
#'
#' @section Limitations:
#' The names have to still exist at the point of call. Two cases cannot be
#' fixed, and both silently fall back to [base::do.call()]'s behaviour:
#' \itemize{
#'   \item `args` supplied as a pre-built list (`do_call2(lm, my_args)`) rather
#'     than an inline `list(...)`. The caller has already discarded the
#'     expressions; nothing can recover them. Build the call with an inline
#'     `list(...)`, or bind the data under a name yourself — see
#'     [fit_glmmTMB_parallel()], which does exactly that.
#'   \item `what` given as an anonymous or computed function, which has no name
#'     to record.
#' }
#' A `pkg::fun` head, or a function held in a variable, records itself as
#' written (`stats::lm(...)`, `f(...)`), so the result is `identical()` to a
#' direct call *spelled that way*, not to `lm(...)`.
#'
#' Because the recorded call refers to the data by name, whatever later
#' re-evaluates it (`update()`, `predict()` with new data, `boot`) needs that
#' name to still resolve. That is the trade-off of the compact object: a fit you
#' will save and reload wants the name; a fit you will hand to an unrelated
#' session wants to be self-contained.
#'
#' @param what Either a function or a non-empty character string naming the
#'   function to be called, exactly as in [base::do.call()].
#' @param args A list of arguments. Pass it as an inline `list(...)` so the
#'   caller's expressions can be recovered.
#' @param quote Logical; should the arguments be quoted? As in
#'   [base::do.call()]. When `TRUE` the caller is asking for the values *not* to
#'   be re-evaluated, so the argument rewriting stands aside and the result
#'   matches [base::do.call()] exactly.
#' @param envir The environment in which the call is evaluated. Defaults to the
#'   caller's frame, as in [base::do.call()].
#'
#' @return The value of the call — `identical()` to a direct invocation
#'   whenever the names could be recovered.
#'
#' @seealso [base::do.call()], [fit_glmmTMB_parallel()]
#'
#' @examples
#' direct <- lm(mpg ~ wt, data = mtcars)
#'
#' # do.call() inlines: the function body, or the data frame
#' identical(direct, do.call(lm, list(mpg ~ wt, data = mtcars)))
#' identical(direct, do.call("lm", list(mpg ~ wt, data = mtcars)))
#'
#' # do_call2() records names instead
#' identical(direct, do_call2(lm, list(mpg ~ wt, data = mtcars)))
#' identical(direct, do_call2("lm", list(mpg ~ wt, data = mtcars)))
#'
#' # already-quoted input is not quoted twice
#' identical(direct, do_call2(lm, list(quote(mpg ~ wt), data = quote(mtcars))))
#'
#' @export
do_call2 <- function(what, args, quote = FALSE, envir = parent.frame()) {
  what_expr <- substitute(what)
  args_expr <- substitute(args)

  # Both steps must be no-ops on input that is already in the fixed form,
  # otherwise quote(mtcars) becomes quote(quote(mtcars)) and evaluates to a
  # symbol rather than the data frame.
  unquote <- function(e) {
    while (is.call(e) && length(e) == 2L && identical(e[[1L]], quote(quote))) {
      e <- e[[2L]]
    }
    e
  }

  # 1. Stringify the function name if it is not already a string.
  head_expr <- NULL
  if (!(is.character(what) && length(what) == 1L)) {
    w <- unquote(what_expr)
    if (is.name(w)) {
      what <- as.character(w) # lm        -> "lm"
    } else if (is.call(w) && as.character(w[[1L]]) %in% c("::", ":::")) {
      head_expr <- w # stats::lm -> kept as written
    } else if (is.name(what)) {
      what <- as.character(what) # a symbol arriving by value
    }
    # Anything else (an anonymous or computed function) has no name to record;
    # `what` stays the function object, exactly as do.call() would leave it.
  }

  # 2. Quote the arguments that are not already quoted.
  if (!quote && is.call(args_expr) && identical(args_expr[[1L]], quote(list))) {
    items <- as.list(args_expr)[-1L]
    nms <- names(items)
    args <- lapply(items, unquote)
    names(args) <- nms
  }

  # do.call() cannot resolve "stats::lm" as a string, so that one case is
  # assembled by hand -- the same call do.call() would have built internally.
  if (is.null(head_expr)) {
    do.call(what, args, quote = quote, envir = envir)
  } else {
    eval(as.call(c(list(head_expr), args)), envir)
  }
}
