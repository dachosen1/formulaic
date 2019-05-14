#' Reduce Formula
#'
#' TBD
#'
#' @param  dt TBD
#' @param  outcome.name TBD
#' @param  input.names TBD
#' @param  input.patterns TBD
#' @param  max.input.categories TBD
#' @param  max.outcome.categories.to.search TBD
#' @param  return.as TBD
#'
#'@import data.table
#'
#'@export
reduce.formula <- function(dt, outcome.name, input.names, input.patterns = NA, max.input.categories = 20,
                           max.outcome.categories.to.search = 4, return.as = "formula"){

  dt <- data.table::setDT(dt)

  if(!(outcome.name %in% names(dt))){
    return("Error:  outcome.name is not in names(dt).")
  }

  pattern.names <- list()
  if(!is.na(input.patterns[1])){
    for(i in 1:length(input.patterns)){
      pattern.names[[i]] <- names(dt)[grep(pattern = input.patterns[i], x = names(dt))]
    }
  }
  all.input.names <- c(input.names, as.character(pattern.names))

  num.outcome.categories <- dt[!is.na(get(outcome.name)), length(unique(get(outcome.name)))]

  if(num.outcome.categories <= max.outcome.categories.to.search){
    num.unique.tab <- dt[, lapply(X = .SD, FUN = function(x){return(length(unique(x[!is.na(x)])))}),
                         .SDcols = input.names, by = outcome.name]

    min.categories.tab <- num.unique.tab[, lapply(X = .SD, FUN = "min"), .SDcols = input.names]

    reduced.inputs <- names(min.categories.tab)[min.categories.tab >= 2]
  }
  if(num.outcome.categories > max.outcome.categories.to.search){
    reduced.inputs <- all.input.names
  }

  the.formula <- formulaic::create.formula(outcome.name = outcome.name, input.names = reduced.inputs,
                                all.data.names = names(dt), input.patterns = NA, return.as = return.as)
  return(the.formula)
}


#' reduce existing formula
#'
#' @param  dt tBU
#' @param  the.initial.formula TBU
#' @param  max.input.categories TBU
#' @param  max.outcome.categories.to.search TBU
#' @param  return.as TBU
#'
#'
reduce.existing.formula <- function(dt, the.initial.formula, max.input.categories = 20,
                                    max.outcome.categories.to.search = 4, return.as = "formula"){

  dt <- data.table::setDT(dt)

  the.sides <- strsplit(x = the.initial.formula, split = "~")[[1]]
  lhs <- trimws(x = the.sides[1], which = "both")
  lhs.original <- gsub(pattern = "`", replacement = "", x = lhs)
  if(!(lhs.original %in% names(dt))){
    return("Error:  Outcome variable is not in names(dt).")
  }

  the.pieces.untrimmed <- strsplit(x = the.sides[2], split = "+", fixed = TRUE)[[1]]
  the.pieces.untrimmed.2 <- gsub(pattern = "`", replacement = "", x = the.pieces.untrimmed, fixed = TRUE)
  the.pieces.in.names <- trimws(x = the.pieces.untrimmed.2, which = "both")

  return(reduce.formula(dt = dt, outcome.name = lhs.original, input.names = the.pieces.in.names,
                        input.patterns = NA, return.as = return.as))

}
