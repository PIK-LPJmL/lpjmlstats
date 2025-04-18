#' @importFrom memoise memoise forget
#' @importFrom rlang hash
#' @importFrom utils capture.output head tail
#'

# Function creates a custom hash for the arguments of the aggregate function
# that hashed the contents of the LPJmLDataCalc object rather than
# the environment itself.
# This is necessary because the LPJmLDataCalc environment changes
# when doing an exact copy with $clone(deep = TRUE).
# This causes memoise to not find the cached result even when the
# function is called with identical data.
hash_custom <- function(arg_list) {
  get_content <- function(obj) {
    if (inherits(obj, "LPJmLDataCalc")) {
      # extract content of the LPJmLDataCalc object
      # to avoid hashing the environment

      obj <- list(utils::head(obj$data), utils::tail(obj$data),
                  utils::capture.output(obj$meta$print()),
                  utils::head(obj$grid$data), utils::tail(obj$grid$data),
                  utils::head(obj$grid$region_matrix), utils::tail(obj$grid$region_matrix))
    }
    return(obj)
  }

  for (el in names(arg_list)) {
    arg_list[[el]] <- get_content(arg_list[[el]])
  }

  return(rlang::hash(arg_list))
}

aggregate <- memoise::memoise(aggregate, hash = hash_custom)

read_file <- memoise::memoise(read_file, hash = hash_custom)
