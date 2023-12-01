#' @title LPJmL meta data class
#'
#' @description
#' A meta data container for the LPJmLDataCalc class that extends the
#' \link[lpjmlkit]{LPJmLMetaData} such that aggregation can be tracked.
#'

LPJmLMetaDataCalc <- R6::R6Class( # nolint

  classname = "LPJmLMetaDataCalc",

  lock_objects = FALSE,

  inherit = lpjmlkit:::LPJmLMetaData,

  public = list( # TODO: Must be possible to do this better

    #' @description
    #' Initialize the LPJmLMetaDataCalc object by copying all
    #' private attributes from an LPJmLMetaData object to private attributes
    #' of this object.
    #' !Internal method only to be used for package development!
    #' @param lpjml_meta an LPJmLMetaData object.
    initialize = function(lpjml_meta) {
      # get private attribute names from lpjml_meta
      private_attr_names <- names(lpjml_meta$.__enclos_env__$private)

      # iterate over private attributes and assign them to private env
      for (attr_name in private_attr_names) {
        attr_data <- lpjml_meta$.__enclos_env__$private[[attr_name]]
        private_env <- parent.env(environment())[["private"]]

        # only assign if attribute contains data but is not a function
        if (!is.function(attr_data)) {
          assign(attr_name, attr_data,
                 envir = private_env)
        }
      }
    },

    #' @description
    #' Save in metadata that data is in space_aggregation format
    #' !Internal method only to be used for package development!
    #' @param agg_method string indicating the aggregation method
    .__set_space_aggregation__ = function(agg_method) {
      private$.space_aggregation <- agg_method
    },

    #' @description
    #' Save in metadata that data is in time_aggregation format
    #' !Internal method only to be used for package development!
    #' @param agg_method string indicating the aggregation method
    .__set_time_aggregation__ = function(agg_method) {
      private$.time_aggregation <- agg_method
    },



    #' @description
    #' Wrapper for [`LPJmLMetaData`] print method.
    #' @param spaces string of spaces to be printed as prefix
    #' @param ... additional arguments passed to [`LPJmLMetaData`] print method
    print = function(spaces = "", ...) {
      super$print(spaces = spaces, ...)
      # print space_aggregation attribute
      cat(
        paste0(
          spaces,
          lpjmlkit:::col_var("$space_aggregation"),
          " ",
          # Color red if aggregated
          ifelse(!is.null(self$space_aggregation),
                 lpjmlkit:::col_warn(self$space_aggregation),
                 "FALSE"),
          "\n"
        )
      )

      # print time_aggregation attribute
      cat(
        paste0(
          spaces,
          lpjmlkit:::col_var("$time_aggregation"),
          " ",
          # Color red if aggregated
          ifelse(!is.null(self$time_aggregation),
                 lpjmlkit:::col_warn(self$time_aggregation),
                 "FALSE"),
          "\n"
        )
      )
    }
  ),

  active = list(
    #' @field space_aggregation Indication weather the data has been
    #' subject to space aggregation.
    space_aggregation = function() {
      return(private$.space_aggregation)
    },

    #' @field time_aggregation Indication weather the data has been
    #' subject to time aggregation.
    time_aggregation = function() {
      return(private$.time_aggregation)
    }
  ),

  private = list(
    .space_aggregation = NULL,
    .time_aggregation = NULL
  )
)
