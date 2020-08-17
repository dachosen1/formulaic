#' Add backtick
#'
#' Function that add backticks to the input variables.
#'
#' @param  x  Character value specifying the name of input parameters.
#' @param  include.backtick specifies whether a backtick should be added. Parameter values should be either 'all' or 'as.needed'
#' @import data.table
#' @export

add.backtick <- function(x, include.backtick = "as.needed", dat = NULL){

  len.x <- length(x)
  if(include.backtick == "all"){
    w <- 1:len.x
  }
  if (include.backtick == "as.needed") {
    if(is.null(dat)){
      w <- which(x != make.names(names = x))
    }
    if(!is.null(dat)){
      require(data.table)
      data.table::setDT(dat)
      requires.backtick <- logical(length = len.x)

      for(i in 1:len.x){
        value.exists <- is.null(tryCatch(expr = dat[, unique(eval(parse(text = x[i])))], error = function(e) return(NULL)))

        if(value.exists == TRUE & x[i] %in% names(dat) & x[i] != make.names(x[i])){
          requires.backtick[i] <- TRUE
        }
      }
      w <- which(requires.backtick == TRUE)
    }

  }
  if (length(w) > 0) {
    x[w] <- sprintf("`%s`", x[w])
  }
  return(x)
}
