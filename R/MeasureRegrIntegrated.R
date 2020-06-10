#' @title Abstract Class for Integrated Regression Measures
#' @description This is an abstract class that should not be constructed directly.
#' @template param_integrated
#' @template param_points
#' @template param_method
#' @template param_id
#' @template param_range
#' @template param_minimize
#' @template param_packages
#' @template param_predict_type
#' @template param_measure_properties
#' @export
MeasureRegrIntegrated = R6Class("MeasureRegrIntegrated",
  inherit = MeasureRegr,
  public = list(
    #' @description This is an abstract class that should not be constructed directly.
    initialize = function(integrated = TRUE, points, method = 2, id, range, minimize, packages, predict_type, properties) {
      if (class(self)[[1]] == "MeasureRegrIntegrated") {
        stop("This is an abstract class that should not be constructed directly.")
      }

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

      if (!integrated) {
        if (missing(points)) {
          stop("For the non-integrated score, only a single point can be returned.")
        } else {
          assertNumeric(points,
            len = 1,
            .var.name = "For the non-integrated score, only a single point can be returned."
          )
        }
        private$.points = points
      } else {
        assertNumeric(method, 1, 2, any.missing = FALSE, all.missing = FALSE)
        private$.method = method
        if (!missing(points)) {
          assertNumeric(points)
          private$.points = points
          if (length(points) == 1) {
            private$.integrated = FALSE
          }
        }
      }
    }
  ),

  active = list(
    #' @field integrated `(logical(1))`
    #' Returns if the measure should be integrated or not.
    #' Settable.
    integrated = function(integrated) {
      if (missing(integrated)) {
        return(private$.integrated)
      } else {
        assertFlag(integrated)
        if (!integrated & length(self$points) > 1) {
          stop(sprintf(
            "For the non-integrated score, only a single point can be returned.
            Currently self$points = %s",
            paste0("c(", paste0(self$points, collapse = ", "), ").")))
        }
        private$.integrated = integrated
      }
    },

    #' @field points `(numeric())`
    #' Returns the points at which the measure should be evaluated at, or integrated over.
    #' Settable.
    points = function(points) {
      if (!missing(points)) {
        if (!self$integrated) {
          assertNumeric(points,
            len = 1,
            .var.name = "For the non-integrated score, only a single point can
                        be returned.")
        } else {
          assertNumeric(points)
        }
        private$.points = points
      } else {
        return(private$.points)
      }
    },

    #' @field method `(integer(1))`
    #' Returns which method is used for approximating integration.
    #' Settable.
    method = function(method) {
      if (missing(method)) {
        return(private$.method)
      } else {
        assertNumeric(method, 1, 2, any.missing = FALSE, all.missing = FALSE)
        private$.method = as.integer(method)
      }
    }
  ),

  private = list(
    .integrated = logical(),
    .points = numeric(),
    .method = integer()
  )
)
