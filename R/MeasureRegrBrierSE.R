#' @template regr_measure
#' @templateVar title Standard Error of Integrated Brier Score
#' @templateVar fullname MeasureRegrBrierSE
#'
#' @description
#' Calculates the standard error of [MeasureRegrBrier].
#'
#' @template learner_integratedSE
#' @template param_integrated
#' @template param_points
#'
#' @family Probabilistic regression measures
#' @export
MeasureRegrBrierSE = R6::R6Class("MeasureRegrBrierSE",
  inherit = MeasureRegrIntegrated,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(integrated = TRUE, points) {
      super$initialize(
        integrated = integrated,
        points = points,
        id = "regr.brier_se",
        range = c(0, Inf),
        minimize = TRUE,
        packages = "distr6",
        predict_type = "distr",
        properties = character()
      )
    }
  ),

  private = list(
    .score = function(prediction, ...) {
      integrated_se(
        score = regr_ibs(
          truth = prediction$truth,
          distribution = prediction$distr,
          times = self$points),
        integrated = self$integrated)
    }
  )
)
