#' @title Abstract Class for Integrated Survival Measures
#' @description This class is abstract and should not be constructed directly.
#' @format [R6::R6Class()] inheriting from [MeasureSurv].
#' @section Fields:
#' See [MeasureSurv] as well as
#' * integrated :: logical(1) \cr Should the integrated score be returned?
#' * times :: numeric() \cr Either the times over which to integrate the score or a *single* time-point for which the (non-integrated) score should be returned.
#' @export
MeasureSurvIntegrated = R6Class("MeasureSurvIntegrated",
  inherit = MeasureSurv,
  public = list(
    initialize = function(integrated = TRUE, times, id, range, minimize, packages, predict_type, properties) {
      if(class(self)[[1]] == "MeasureSurvIntegrated")
        stop("This is an abstract class that should not be constructed directly.")

      super$initialize(
        id = id,
        range = range,
        minimize = minimize,
        packages = packages,
        predict_type = predict_type,
        properties = properties
      )

      assertFlag(integrated)
      private$.integrated = integrated

      if (!missing(times)) {
        if (!integrated) {
          assertNumeric(times, len = 1,
                        .var.name = "For the non-integrated score, only a single time-point can
                        be returned.")
        } else {
          assertNumeric(times)
        }

        private$.times = times
      }
    }
  ),

  active = list(
    integrated = function(integrated){
      if(missing(integrated)){
        return(private$.integrated)
      } else {
        assertFlag(integrated)
        private$.integrated <- integrated
      }
    },

    times = function(times){
      if (!missing(times)) {
        if (!self$integrated) {
          assertNumeric(times, len = 1,
                        .var.name = "For the non-integrated score, only a single time-point can
                        be returned.")
        } else {
          assertNumeric(times)
        }
        private$.times = times
      } else {
        return(private$.times)
      }
    }
  ),

  private = list(
    .integrated = logical(),
    .times = numeric()
  )
)