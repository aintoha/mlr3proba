#' @template surv_measure
#' @templateVar title Standard Error of Regression Log loss
#' @templateVar fullname MeasureRegrLoglossSE
#'
#' @template param_eps
#'
#' @description
#' Calculates the standard error of [MeasureRegrLogloss].
#'
#' The standard error of the Logloss, L, is approximated via,
#' \deqn{se(L) = sd(L)/\sqrt{N}}{se(L) = sd(L)/\sqrt N}
#' where \eqn{N} are the number of observations in the test set, and \eqn{sd} is the standard deviation.
#'
#' @family Probabilistic survival measures
#' @export
MeasureRegrLoglossSE = R6::R6Class("MeasureRegrLoglossSE",
  inherit = MeasureRegrLogloss,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(eps = 1e-15) {
      super$initialize(eps, id = "regr.logloss_se")
    }
  ),

  private = list(
    .score = function(prediction, ...) {
      ll = as.numeric(prediction$distr$pdf(data = matrix(prediction$truth, nrow = 1)))
      ll[ll == 0] = self$eps
      sd(ll) / sqrt(length(ll))
    }
  )
)
