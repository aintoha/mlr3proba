#' @template regr_measure
#' @templateVar title Log loss
#' @templateVar inherit [MeasureRegr]
#' @templateVar fullname MeasureRegrLogloss
#' @templateVar pars eps = 1e-15
#' @templateVar eps_par TRUE
#'
#' @template param_eps
#' @template param_id
#'
#' @description
#' Calculates the cross-entropy, or logarithmic (log), loss.
#'
#' The logloss, in the context of probabilistic predictions, is defined as the negative log probability
#' density function, \eqn{f}, evaluated at the observed value, \eqn{y},
#' \deqn{L(f, y) = -log(f(y))}
#'
#' @family Probabilistic regression measures
#' @export
MeasureRegrLogloss = R6::R6Class("MeasureRegrLogloss",
  inherit = MeasureRegr,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(eps = 1e-15, id = "regr.logloss") {
      super$initialize(
        id = id,
        range = c(0, Inf),
        minimize = TRUE,
        predict_type = "distr",
        packages = "distr6"
      )

      assertNumeric(eps)
      private$.eps = eps
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
    .score = function(prediction, ...) {
      ll = as.numeric(prediction$distr$pdf(data = matrix(prediction$truth, nrow = 1)))
      ll[ll == 0] = self$eps
      return(mean(-log(ll)))
    }
  )
)
