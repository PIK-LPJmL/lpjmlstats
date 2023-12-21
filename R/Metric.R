#' @title Metric
#' @description
#' A Metric is a class defines a procedure to evaluate and compare
#' one or several under test outputs against a baseline output.
#' It is also a container for the results of all evaluations, that have been
#' performed with this metric.
#' @export
Metric <- # nolint:object_linter_name
  R6::R6Class(
    "Metric",
    public = list(
      #' @description
      #' Compute and store an individual summary of the raw data presented.
      #' Uses the internal summary function.
      #' @param data Raw data to be summarized
      #' @param var Variable name
      #' @param type Type of data ("bsl" or "ut")
      capture_summary = function(data, var, type) {
        summary <- self$summarize(data)
        self$store_summary(summary, var, type)
      },

      #' @description
      #' Store a summary in the metric object.
      #' @param summary Summary to be stored
      #' @param var Variable name
      #' @param type Type of data ("bsl" or "ut")
      store_summary = function(summary, var, type) {
        # create new evaluation group if it doesn't exists
        if (is.null(self$evalList[[var]])) {
          eval_grp <- EvalGrp$new(var)
          self$evalList[[var]] <- eval_grp
        }

        # store data
        if (type == "bsl") {
          self$evalList[[var]]$bsl <- summary
        } else if (type == "ut") {
          sim_name <- summary$meta$sim_name
          self$evalList[[var]]$ut[[sim_name]] <- summary
        } else {
          stop("type must be either 'bsl' or 'ut'")
        }
      },

      #' @description
      #' Compare the baseline summary with the under test summaries and stores
      #' the results in the metric object.
      #' Uses the internal comparison function.
      #' @param var Variable name
      add_comparison = function(var) {
        # get the evalulation group for this variable
        eval_grp <- self$evalList[[var]]
        # add comparison to evaluation group
        self$compare(eval_grp)
      },

      #' @description
      #' Pipeline to summarize the raw data.
      #' Will be overwritten by the individual metric classes.
      #' @param data Raw data to be summarized
      summarize = function(data) {
        NULL
      },

      #' @description
      #' Pipeline to compare the baseline summary with the under test summaries.
      #' Will be overwritten by the individual metric classes.
      #' @param eval_grp Evaluation group
      compare = function(eval_grp) {
        NULL
      },

      #' @field evalList
      #' List of evaluation groups. Each evaluation group contains the summaries
      #' and the comparisons for one variable.
      evalList = list()
    )
  )


#' @title GlobalSumTimeAvg
#' @description
#' GlobalSumTimeAvg metric.
#' @export
GlobalSumTimeAvg <- # nolint:object_linter_name
  R6::R6Class(
    "GlobalSumTimeAvg",
    inherit = Metric,
    public = list(
      #' @description
      #' Compute a global area weighted sum of a variable and then average
      #' all timesteps of the simulation period.
      #' @param data Raw data to be summarized
      summarize = function(data) {
        data %>%
          aggregate(cell = list(to = "global", stat = "weighted_sum")) %>%
          aggregate(time = list(to = "sim_period", stat = "mean"))
      },

      #' @description
      #' Compare the baseline summary with the under test summaries by
      #' subtracting the baseline from the under test.
      #' @param eval_grp Evaluation group
      compare = function(eval_grp) {
        eval_grp$cmp <- lapply(eval_grp$ut, function(x) x - eval_grp$bsl)
      }
    )
  )

#' @title GlobalSumTimeSer
#' @description
#' GlobalSumTimeSer metric.
#' @export
GlobalSumTimeSer <- # nolint:object_linter_name
  R6::R6Class("GlobalSumTimeSer",
              inherit = Metric,
              public = list(
                #' @description
                #' Compute a global area weighted sum of a variable
                #' for each timestep.
                #' @param data Raw data to be summarized
                summarize = function(data) {
                  data %>%
                    aggregate(cell = list(to = "global", stat = "weighted_sum"))
                }
              ))

#' @title TimeAvg
#' @description
#' TimeAvg metric.
#' @export
TimeAvg <- # nolint:object_linter_name
  R6::R6Class(
    "TimeAvg",
    inherit = Metric,
    public = list(
      #' @description
      #' Average all timesteps of the simulation period.
      #' @param data Raw data to be summarized
      summarize = function(data) {
        data %>%
          aggregate(time = list(to = "sim_period", stat = "mean"))
      },

      #' @description
      #' Compare the baseline summary with the under test summaries by
      #' subtracting the baseline from the under test.
      #' Delete the baseline summaries.
      #' @param eval_grp Evaluation group
      compare = function(eval_grp) {
        eval_grp$cmp <- lapply(eval_grp$ut, function(x) x - eval_grp$bsl)
        eval_grp$bsl <- NULL
      }
    )
  )

# An evaluation group contains all under test and baseline summaries and
# comparisons for one variable.
EvalGrp <- # nolint:object_linter_name
  R6::R6Class(
    "EvalGrp",
    public = list(
      initialize = function(var_name) {
        self$var_name <- var_name
      },

      var_name = NULL,

      ut = NULL,
      # list of under test summaries
      bsl = NULL,
      # baseline summary
      cmp = NULL  # list of comparisons under test against baseline
    )
  )
