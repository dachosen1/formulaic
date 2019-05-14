#' Add backticks if needed
#'
#' Add backticks to make a appropriate variable.
#'
#' @param  x  variables
#' @param  include.backtick  logical. If TRUE the corresponding component will remove any white space.
add.backtick <- function(x, include.backtick = "as.needed"){
  if(include.backtick == "all"){
    w <- 1:length(x)
  }
  if(include.backtick == "as.needed"){
    w <- grep(pattern = " ", x = x, fixed = TRUE)
  }
  if(length(w) > 0){
    x[w] <- sprintf("`%s`", x[w])
  }
  return(x)
}

#' Create Formula
#'
#' Automatically creates a formula from the following parameters:
#'
#' @param  outcome.name  The name of the variable serving as the outcome.
#' @param  input.names The names of the variables with the full names delineated.
#' @param  input.patterns  Includes additional input variables.  The user may enter patterns -- e.g. to include every variable with a name that includes the pattern.  Multiple patterns may be included as a character vector.  However, each pattern may not contain spaces and is otherwise subject to the same limits on patterns as used in the grep function.
#' @param  all.data.names  The names of the data within which to search for patterns.
#' @param  include.backtick  add backticks if needed
#' @param  return.as the data type of the output.  If not set as "formula", then a character vector will be returned.
#'
#' @details  Return as the data type of the output.  If not set as "formula", then a character vector will be returned.
#' The input.names and names of variables matching the input.patterns will be concatenated to form the full list of input variables.
#' Note:  Does not account for interactions (a*b)
#'
#' @export
#' @examples
#'  n <- 10
#'  dd <- data.table::data.table(w = rnorm(n= n), x = rnorm(n = n), pixel_1 = rnorm(n = n))
#'  dd[, pixel_2 := 0.3 * pixel_1 + rnorm(n)]
#'  dd[, y := 5 * x + 3 * pixel_1 + 2 * pixel_2 + rnorm(n)]
#'
#'  create.formula(outcome.name = "y", input.names = "x", input.patterns = c("pi", "xel"),
#'    all.data.names = names(dd), return.as = "character")
#' @import stats
#' @export
create.formula <- function(outcome.name, input.names, input.patterns = NA, all.data.names = NA,
                           include.backtick = "as.needed", return.as = "formula"){
  variable.names.from.patterns <- c()
  if(!is.na(input.patterns[1]) & !is.na(all.data.names[1])){
    pattern <- paste(input.patterns, collapse = "|")
    variable.names.from.patterns <- all.data.names[grep(pattern = pattern, x = all.data.names)]
  }
  all.input.names <- unique(c(input.names, variable.names.from.patterns))
  all.input.names <- all.input.names[all.input.names != outcome.name]
  if(!is.na(all.data.names[1])){
    all.input.names <- all.input.names[all.input.names %in% all.data.names]
  }
  input.names.delineated <- add.backtick(x =  all.input.names, include.backtick = include.backtick)
  outcome.name.delineated <- add.backtick(x = outcome.name, include.backtick = include.backtick)
  the.formula <- sprintf("%s ~ %s", outcome.name.delineated, paste(input.names.delineated, collapse = "+"))

  if(return.as == "formula"){
    return(stats::as.formula(the.formula))
  }
  if(return.as != "formula"){
    return(the.formula)
  }
}
