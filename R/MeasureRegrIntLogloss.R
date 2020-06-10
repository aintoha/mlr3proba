#' @template regr_measure
#' @templateVar title Integrated Regression Log loss
#' @templateVar inherit [MeasureRegr]
#' @templateVar fullname MeasureRegrIntLogloss
#' @templateVar pars eps = 1e-15
#' @templateVar eps_par TRUE
#'
#' @template measure_integrated
#' @template param_integrated
#' @template param_points
#' @template param_eps
#' @template param_method
#'
#' @description
#' Calculates the integrated logarithmic (log), loss, aka integrated cross entropy.
#'
#' For an individual with outcome \eqn{Y} and predicted CDF, \eqn{F}, the
#' integrated log loss is given by
#' \deqn{L(F, y) = - \int I(Y \le y)log(F(y)) + I(Y > y)log(1-F(y)) dy}
#'
#' @family Probabilistic regression measures
#' @export
MeasureRegrIntLogloss = R6::R6Class("MeasureRegrIntLogloss",
  inherit = MeasureRegrIntegrated,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(integrated = TRUE, points, eps = 1e-15, method = 2) {
      super$initialize(
        integrated = integrated,
        points = points,
        method = method,
        id = "regr.intlogloss",
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
    #' @field eps
    #' Returns `eps` parameter, see `initialize`.
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

    .score = function(prediction, task, train_set) {
      integrated_score(
        score = regr_logloss(
          truth = prediction$truth,
          distribution = prediction$distr,
          times = self$points,
          eps = self$eps),
        integrated = self$integrated,
        method = self$method
      )
    }
  )
)
