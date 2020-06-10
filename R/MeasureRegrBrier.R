#' @template surv_measure
#' @templateVar title Integrated Brier Score
#' @templateVar fullname MeasureRegrBrier
#'
#' @aliases MeasureRegrBrier mlr_measures_surv.brier
#'
#' @description
#' Calculates the Integrated Brier score.
#'
#' For an individual with outcome at \eqn{y}, and predicted CDF, \eqn{F}, the
#' IBS is given by
#' \deqn{L(F, y) = \int (I(Y \le y) - F(y))^2}
#'
#' Note: If comparing the integrated graf score to other packages, e.g. [pec::pec()], then
#' `method = 2` should be used, however the results may still be very slightly different as
#' this package uses `survfit` to estimate the censoring distribution, in line with the Graf 1999 paper.
#' Whereas some other packages use `prodlim` with `reverse = TRUE` (meaning Kaplan-Meier is not used).
#'
#' @template measure_integrated
#' @template param_integrated
#' @template param_points
#' @template param_method
#'
#' @family Probabilistic survival measures
#' @export
MeasureRegrBrier = R6::R6Class("MeasureRegrBrier",
  inherit = MeasureRegrIntegrated,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(integrated = TRUE, points, method = 2) {
      super$initialize(
        integrated = integrated,
        points = points,
        method = method,
        id = "surv.brier",
        range = c(0, 1),
        minimize = TRUE,
        packages = "distr6",
        predict_type = "distr",
        properties = character()
      )
    }
  ),

  private = list(
    .score = function(prediction, ...) {
      integrated_score(
        score = regr_ibs(
          truth = prediction$truth,
          distribution = prediction$distr,
          times = self$points),
        integrated = self$integrated,
        method = self$method)
    }
  )
)
