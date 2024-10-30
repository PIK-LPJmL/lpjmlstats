#' @title LPJmL meta data class
#'
#' @description
#' A meta data container for the LPJmLDataCalc class that extends the
#' \link[lpjmlkit]{LPJmLMetaData} such that aggregation can be tracked.
#'

LPJmLMetaDataCalc <- R6::R6Class( # nolint

  classname = "LPJmLMetaDataCalc",

  lock_objects = FALSE,

  inherit = lpjmlkit::LPJmLMetaData,

  public = list(

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
          cli::col_blue("$space_aggregation"),
          " ",
          # Color red if aggregated
          ifelse(!is.null(self$space_aggregation),
                 cli::col_red(self$space_aggregation),
                 "FALSE"),
          "\n"
        )
      )

      # print time_aggregation attribute
      cat(
        paste0(
          spaces,
          cli::col_blue("$time_aggregation"),
          " ",
          # Color red if aggregated
          ifelse(!is.null(self$time_aggregation),
                 cli::col_red(self$time_aggregation),
                 "FALSE"),
          "\n"
        )
      )

      # print sim path abbreviation
      cat(
        paste0(
          spaces,
          cli::col_blue("$sim_ident"),
          " ",
          self$sim_ident,
          "\n"
        )
      )

      # print sim path abbreviation
      cat(
        paste0(
          spaces,
          cli::col_blue("$pos_in_var_grp"),
          " ",
          self$pos_in_var_grp,
          "\n"
        )
      )
    },

    #' @description
    #' Set the simulation identifier
    #' !Internal method only to be used for package development!
    #' @param sim_ident string, simulation identifier
    .__set_sim_ident__ = function(sim_ident) {
      private$.sim_ident <- sim_ident
    },

    #' @description
    #' Set the position of the lpjml_calc inside of its var_grp.
    #' !Internal method only to be used for package development!
    #' @param pos_in_var_grp A list with the position of the lpjml_calc
    #' inside of the var_grp. The first entry is the type; can be
    #' "baseline", "under_test" or "compare".
    #' The second entry is the compare item if
    #' of type "compare", e.g. "diff".
    #' E.g. list("under_test") or list("compare", "diff").
    .__set_pos_in_var_grp__ = function(pos_in_var_grp) {
      private$.pos_in_var_grp <- pos_in_var_grp
    }
  ),

  active = list(
    #' @field space_aggregation boolean, Indication weather the data has been
    #' subject to space aggregation.
    space_aggregation = function() {
      return(private$.space_aggregation)
    },

    #' @field time_aggregation boolean, Indication weather the data has been
    #' subject to time aggregation.
    time_aggregation = function() {
      return(private$.time_aggregation)
    },

    #' @field band_names_disp
    #' named vector, versions of band names used for display, usually shorter
    band_names_disp = function() {
      if (!is.null(private$.band_names))
        return(private$.band_names)  # abbreviation of band names could be added here if needed
      else
        return(NULL)
    },

    #' @field pos_in_var_grp
    #' list, position of the lpjml_calc inside of its var_grp.
    pos_in_var_grp = function() {
      return(private$.pos_in_var_grp)
    },

    #' @field sim_ident
    #' string, simulation identifier
    sim_ident = function() {
      return(private$.sim_ident)
    },

    #' @field name
    #' string, hopefully model version invariant name of the variable
    name = function() {
      if (!is.null(private$.name))
        return(tolower(private$.name))
      else
        return(tolower(private$.variable))
    },

    #' @field var_and_band_disp
    #' string, variable name together with name of first band, e.g. `soiln$200`
    var_and_band_disp = function() {
      paste0(self$name,
             ifelse(is.null(self$band_names_disp),
                    "", "$"),
             # below vanishes if band_names_disp is NULL
             self$band_names_disp[[1]])
    }
  ),

  private = list(
    .space_aggregation = NULL,
    .time_aggregation = NULL,
    .sim_ident = "undefined simulation",
    .pos_in_var_grp = list("undefined position in var_grp")
  )
)
