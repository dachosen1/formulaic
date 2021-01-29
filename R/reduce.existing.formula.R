#' Reduce Existing Formula
#'
#' The reduce.existing.formula function was designed to perform quality checks and automatic removal of impractical variables can also be accessed when an existing formula has been previously constructed. This method uses natural language processing techniques to deconstruct the components of a formula.
#'
#' @param  the.initial.formula is an object of class "formula" or "character" that states the inputs and output in the form y ~ x1 + x2.
#' @param  dat Data frame, list or environment (or object coercible by as.data.frame to a data frame) containing the variables in the model.
#' @param  max.input.categories Limits the maximum number of variables that will be employed in the formula.As default it is set at 20, but users can still change at his/her convenience.
#' @param  max.outcome.categories.to.search A numeric value. The create.formula function es a feature that identifies input variables exhibiting a lack of contrast. When reduce = TRUE, these variables are automatically excluded from the resulting formula. This search may be expanded to subsets of the outcome when the number of unique measured values of the outcome is no greater than max.outcome.categories.to.search. In this case, each subset of the outcome will be separately examined, and any inputs builthat exhibit a lack of contrast within at least one subset will be excluded.
#' @param  force.main.effects This is a logical value.  When TRUE, the intent is that any term ed as an interaction (of multiple variables) must also be listed individually as a main effect.
#' @param  order.as  rearranges its first argument into ascending or descending order.
#' @param  include.backtick Add backticks to make a appropriate variable
#' @param  format.as The data type of the output.  If not set as "formula", then a character vector will be returned.
#' @param  envir The path to search. Global environment is default value
#'
#' @import data.table
#'
#' @export
#' @examples
#' data('snack.dat')
#' the.initial.formula <- 'Income ~ .'
#'
#' reduce.existing.formula(the.initial.formula = the.initial.formula,dat = snack.dat,
#'   max.input.categories = 30)$formula
#'
#' @export
reduce.existing.formula <-
  function(the.initial.formula,
           dat,
           max.input.categories = 20,
           max.outcome.categories.to.search = 4,
           force.main.effects = TRUE,
           order.as = "as.specified",
           include.backtick = "as.needed",
           format.as = "formula", envir = .GlobalEnv) {

    status.formula.object <- methods::is(object = the.initial.formula, class2 = "formula")
    status.character.object <- base::is.character(the.initial.formula)
    if(status.formula.object == FALSE & status.character.object == FALSE){
      stop("the.initial.formula must be a formula or character object.")

    }

    if (status.formula.object == TRUE) {
      the.sides <- as.character(the.initial.formula)[2:3]
    }

    if (status.character.object == TRUE) {
      the.sides <- strsplit(x = the.initial.formula, split = "~")[[1]]
    }

    outcome.name <- trimws(x = the.sides[1], which = "both")

    the.pieces.untrimmed <-
      strsplit(x = the.sides[2],
               split = "+",
               fixed = TRUE)[[1]]

    the.pieces.untrimmed.2 <-
      gsub(
        pattern = "`",
        replacement = "",
        x = the.pieces.untrimmed,
        fixed = TRUE
      )

    the.pieces.trimmed <-
      trimws(x = the.pieces.untrimmed.2, which = "both")

    w.int <- grep(pattern = "\\*", x = the.pieces.trimmed)
    w.input <-
      (1:length(the.pieces.trimmed))[!((1:length(the.pieces.trimmed)) %in% w.int)]

    if (length(w.input) == 0) {
      input.names <- NULL
    }
    if (length(w.input) > 0) {
      input.names <- the.pieces.trimmed[w.input]
    }

    if (length(w.int) == 0) {
      interactions <- NULL
    }

    if (length(w.int) > 0) {
      untrimmed.interactions <-
        strsplit(x = the.pieces.trimmed[w.int],
                 split = "*",
                 fixed = TRUE)
      interactions <-
        lapply(X = untrimmed.interactions, FUN = "trimws", which = "both")
    }

    return(
      create.formula(
        outcome.name = outcome.name,
        input.names = input.names,
        input.patterns = NULL,
        dat = dat,
        interactions = interactions,
        force.main.effects = force.main.effects,
        reduce = TRUE,
        max.input.categories = max.input.categories,
        max.outcome.categories.to.search = max.outcome.categories.to.search,
        order.as = order.as,
        include.backtick = include.backtick,
        format.as = format.as
      )
    )
  }
