#' Create parameter set from a template and random numbers'
#'
#' From random uniform samples to paramaters
#'  - recursivley iterate over a list of lists (or data frames)
#'  - each element is an atomic vector with possible attributes
#'  - if a vector has attributes, on every NA in that vector the rescale function is called. The arguments for that function are
#'    - a random number [0,1] that is fetched from a vector of random numbers with length nvar which is the number of NAs in the list of lists vector of random numbers
#'     - the attributes belonging to the atomic vector
#'
#' @param sample
#' A numeric vector with length n, where n is the number of \code{NA}s in the parameters list
#' @param parameters
#' A list of lists (or data.frames) with \code{NA}s where a parameter should be sampled. Each atomic element can have an arbitratry numer of \code{NA} values and attributes.
#' @param .fun
#' Optional. A function to be applied to each \code{NA} in the atomic vector. Its first argument is the nth sample correspoding to the nth \code{NA} value in th whole list. Attributes stored on that vector will be passed as further arguments to the function.
#'
#' @return
#' A list of lists of the properties length as template
#' @import iterators
#' @export
create_parset <- function(sample, parameters, .fun = NULL) {
  rapply(
    parameters,
    function(vec, iter) {
      # rapply recursivley iterates over the list until it encounters an atomic type,
      # a vector or matrix. sapply then iterates over that object (it should be a vector...)
      new_vec <- sapply(vec, function(el) {
        # if the current element of the vector vec is NA ...
        if (is.na(el)) {
          # check if a function was supplied.
          # If not, return the nth element of the sample vector to replace the missing value
          if (is.null(.fun)) return(sample[iter$nextElem()])
          # if a function was supplied, call it with arguments stored as attributes on vector vec
          do.call(.fun, c(list(sample[iter$nextElem()]), attributes(vec)))
        } else el # ... else return the current element of vec unchanged.
      })
      as.vector(new_vec)
    },
    iter = iterators::icount(length(sample)),
    how = "replace"
  )

  # LOOP DA SHOOP!
  # unpar <- unlist(parameters, recursive = F, use.names = T)
  # lapply(unpar, attributes)
  # parnames <- names(parameters)
  #
  # for (i in seq_along(parameters)) {
  #   if (is.list(parameters[[i]]))
  #
  # }
}
