#' @template regr_measure
#' @templateVar title Standard Error of Integrated Regression Log loss
#' @templateVar fullname MeasureRegrIntLoglossSE
#'
#' @description
#' Calculates the standard error of [MeasureRegrLogloss].
#'
#' @template learner_integratedSE
#' @template param_integrated
#' @template param_points
#' @template param_eps
#' @template field_eps
#'
#' @family Probabilistic regression measures
#' @export
MeasureRegrIntLoglossSE = R6::R6Class("MeasureRegrIntLoglossSE",
  inherit = MeasureRegrIntegrated,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(integrated = TRUE, points, eps = 1e-15) {
      super$initialize(
        integrated = integrated,
        points = points,
        id = "regr.intlogloss_se",
        range = c(0, Inf),
        minimize = TRUE,
        packages = "distr6",
        predict_type = "distr",
        properties = character()
      )

      assertNumeric(eps)
      private$.eps = eps
      invisible(self)
    }
  ),

  active = list(
    eps = function(eps) {
      if (missing(eps)) {
        return(private$.eps)
      } else {
        assertNumeric(eps)
        private$.eps = eps
      }
    }
  ),

  private = list(
    .eps = numeric(0),
    .score = function(prediction, ...) {
      integrated_se(
        score = regr_logloss(
          truth = prediction$truth,
          distribution = prediction$distr,
          times = self$points,
          eps = self$eps),
        integrated = self$integrated)
    }
  )
)
