#' Add backtick
#'
#' Function that add backticks to the input variables.
#'
#' @param  x  Character value specifying the name of input parameters.
#' @param  include.backtick specifies whether a backtick should be added. Parameter values should be either 'all' or 'as.needed'
#' @param dat Data
#' @import data.table
#' @export

add.backtick <- function(x, include.backtick = "as.needed", dat = NULL){

  original.format.dt <- FALSE
  len.x <- length(x)
  if(include.backtick == "all"){
    w <- 1:len.x
  }
  if (include.backtick == "as.needed") {
    if(is.null(dat)){
      w <- which(x != make.names(names = x))
    }
    if(!is.null(dat)){
      #require(data.table)

      if(is.data.frame(dat)){
        original.format.dt <- is.data.table(x = dat)
      }
      data.table::setDT(dat)

      requires.backtick <- logical(length = len.x)

      for(i in 1:len.x){
        value.exists <- !is.null(tryCatch(expr = unique(eval(parse(text = sprintf("dat[, `%s`]", eval((eval(x[i]))))))), error = function(e) return(NULL)))

        if(value.exists == TRUE & x[i] != make.names(x[i])){
          requires.backtick[i] <- TRUE
        }
      }
      w <- which(requires.backtick == TRUE)
    }

  }
  if(is.data.frame(x = dat)){
    if(original.format.dt == F){
      setDF(x = dat)
    }
  }

  if (length(w) > 0) {
    x[w] <- sprintf("`%s`", x[w])
  }
  return(x)
}
