#' Add backticks
#'
#' add.backtick is subsidiary function to create.formula that add backticks to the names of the variables.
#'
#' @param  x  variables
#' @param  include.backtick specifies whether a backtick should be added. Parameter values should be either 'all' or 'as.needed'
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
#' Create formula is a tool to automatically create a formula from a provided variable and output names. Reduces the time required to manually input variables for modeling. Output can be used in linear regression, random forest, neural network etc. Create formula becomes useful when modeling data with multiple features. Reduces the time required for modeling and implementation :

#' @param outcome.name The name of the variables serving as the outcome.
#' @param input.names The names of the variables with the full names delineated.
#' @param input.patterns Includes additional input variables.  The user may enter patterns -- e.g. to include every variable with a name that includes the pattern.  Multiple patterns may be included as a character vector. However, each pattern may not contain spaces and is otherwise subject to the same limits on patterns as used in the grep function.
#' @param dat User can specify a data.frame object that will be used to remove any variables that are not listed in names(dat. As default it is set as NULL. In this case, the formula is created simply from the outcome.name and input.names.
#' @param interactions Each entry of the list is meant to specify all of the terms that will form a single interaction.
#' @param force.main.effects This is a logical value.  When TRUE, the intent is that any term included as an interaction (of multiple variables) must also be listed individually as a main effect.
#' @param reduce  As default it is set as FALSE. When the user set it as TRUE, it will go through the logic of checking for too few contrasts or too many contrasts.
#' @param max.input.categories Limits the maximum number of variables that will be employeed in the formula. As default it is set at 20, but users can still change at his/her convinence.
#' @param max.outcome.categories.to.search Limits the maximum number of outcome categories will be investigated in the formula. As default it is set at 4.
#' @param order.as User can specify the order the input variables in the formula in a variety of ways for patterns: increasing for increasing alphabet order, decreasing for decreasing alphabet order, column.order for as they appear in data, and as.specified for maintaining the user's specified order.
#' @param include.backtick Add backticks if needed. As default it is set as 'as.needed', which add backticks when only it is needed. The other option is 'all'. The use of include.backtick = "all" is limited to cases in which the output is generated as a character variable. When the output is generated as a formula object, then R automatically removes all unnecessary backticks. That is, it is only compatible when format.as != formula.
#' @param format.as The data type of the output.  If not set as "formula", then a character vector will be returned.
#' @param variables.to.exclude This will automatically drop the variable from the regression. It also supersede any variables metioned in input.names.
#' @param include.intercept As a default it is set as TRUE; however, if user changes it to 'FALSE', then intercept will be removed from the result, the formula.
#'
#' @details  Return as the data type of the output.  If not set as "formula", then a character vector will be returned.
#' The input.names and names of variables matching the input.patterns will be concatenated to form the full list of input variables.
#' Note:  Does not account for interactions (a*b)
#' @import data.table
#' @export
#' @examples
#'  n <- 10
#'  dd <- data.table::data.table(w = rnorm(n= n), x = rnorm(n = n), pixel_1 = rnorm(n = n))
#'  dd[, pixel_2 := 0.3 * pixel_1 + rnorm(n)]
#'  dd[, y := 5 * x + 3 * pixel_1 + 2 * pixel_2 + rnorm(n)]
#'
#'  create.formula(outcome.name = "y", input.names = "x", input.patterns = c("pi", "xel"), dat = dd)
#' @import stats
#' @export
create.formula <-
  function(outcome.name,
           input.names = NULL,
           input.patterns = NULL,
           dat = NULL,
           interactions = NULL,
           force.main.effects = TRUE,
           reduce = FALSE,
           max.input.categories = 20,
           max.outcome.categories.to.search = 4,
           order.as = "as.specified",
           include.backtick = "as.needed",
           format.as = "formula",
           variables.to.exclude = NULL,
           include.intercept = TRUE) {

    specified.from <-
      exclude.not.in.names.dat <-
      exclude.matches.outcome.name <-
      exclude.lack.contrast <-
      min.categories <-
      exclude.numerous.categories <-
      include.variable <-
      variable <- . <- exclude.user.specified <-  NULL

    if (!is.null(input.names)) {
      if (is.na(input.names[1])) {
        input.names <- NULL
      }
      input.names <- unique(input.names)
    }
    if (!is.null(interactions)) {
      if (is.na(interactions[1])) {
        interactions <- NULL
      }
      interactions <- unique(interactions)
    }
    if (!is.null(input.patterns)) {
      if (is.na(input.patterns[1])) {
        input.patterns <- NULL
      }
      input.patterns <- unique(input.patterns)
    }

    if (!is.null(variables.to.exclude)) {
      if (is.na(variables.to.exclude[1])) {
        variables.to.exclude <- NULL
      }
      variables.to.exclude <- unique(variables.to.exclude)
    }
    if (is.data.frame(dat)) {
      data.table::setDT(dat)

      if (!is.null(input.names)) {
        if ("." %in% input.names) {
          input.names <- unique(c(input.names[input.names != "."], names(dat)))
        }
      }


      if (length(names(dat)) == 0) {
        return("Error:  dat must be an object with specified names.")
      }
      if (!(outcome.name %in% names(dat))) {
        return(
          "Error:  To create a formula, the outcome.name must match one of the values in names(dat)."
        )
      }


      variable.names.from.exclude <- c()
      if (!is.null(variables.to.exclude)) {
        variable.names.from.exclude <- unique(variables.to.exclude)
      }


      variable.names.from.patterns <- c()

      if (!is.null(input.patterns)) {
        pattern <- paste(input.patterns, collapse = "|")
        variable.names.from.patterns <-
          names(dat)[grep(pattern = pattern, x = names(dat))]
      }

      unlisted.interactions <- NULL
      if (!is.null(interactions)) {
        unlisted.interactions <- unlist(interactions)
      }

      unique.names <-
        unique(c(
          input.names,
          variable.names.from.patterns,
          unlisted.interactions
        ))

      if (length(unique.names) == 0) {
        unique.names <- NA
      }

      #Compute inclusion.table

      inclusion.table <-
        data.table(variable = unique.names)[!is.na(variable)]

      if (is.null(variables.to.exclude)) {
        num.from.variables.to.exclude <- 0
      }
      if (!is.null(variables.to.exclude)) {
        if (is.na(variables.to.exclude[1])) {
          num.from.variables.to.exclude <- 0
        }
        num.from.variables.to.exclude <-
          length(variables.to.exclude[!is.na(variables.to.exclude)])
      }
      if (is.null(input.names)) {
        num.from.input.names <- 0
      }
      if (!is.null(input.names)) {
        if (is.na(input.names[1])) {
          num.from.input.names <- 0
        }
        num.from.input.names <-
          length(input.names[!is.na(input.names)])
      }
      if (is.null(input.patterns)) {
        num.from.input.patterns <- 0
      }
      if (!is.null(input.patterns)) {
        if (is.na(input.patterns[1])) {
          num.from.input.patterns <- 0
        }
        num.from.input.patterns <-
          length(variable.names.from.patterns[!(variable.names.from.patterns %in% c(input.names))])
      }
      if (is.null(interactions)) {
        num.from.interactions <- 0
      }
      if (!is.null(interactions)) {
        if (is.na(interactions[1])) {
          num.from.interactions <- 0
        }
        num.from.interactions <-
          length(unique(unlisted.interactions[!(
            unlisted.interactions %in% c(
              input.names,
              variable.names.from.patterns,
              variables.to.exclude
            )
          )]))
      }

      inclusion.table[variable %in% names(dat), class := as.character(dat[, as.character(lapply(X = .SD, FUN = "class")), .SDcols = variable]), by = variable]
      inclusion.table[, order := 1:.N]
      inclusion.table[, specified.from := c(
        rep.int(x = "input.names", times = num.from.input.names),
        rep.int(x = "input.patterns", times = num.from.input.patterns),
        rep.int(x = "interactions", times = num.from.interactions)
      )]

      inclusion.table[, exclude.user.specified := variable %in% variable.names.from.exclude]
      inclusion.table[, exclude.not.in.names.dat := !(variable %in% names(dat))]
      inclusion.table[, exclude.matches.outcome.name := (variable == outcome.name)]

      if (reduce == TRUE) {
        num.outcome.categories <-
          dat[!is.na(get(outcome.name)), length(unique(get(outcome.name)))]

        the.inputs <-
          inclusion.table[variable %in% names(dat), variable]

        if (num.outcome.categories <= max.outcome.categories.to.search) {
          num.unique.tab <-
            dat[, lapply(
              X = .SD,
              FUN = function(x) {
                return(length(unique(x[!is.na(x)])))
              }
            ), .SDcols = the.inputs, by = outcome.name]
        }
        if (num.outcome.categories > max.outcome.categories.to.search) {
          num.unique.tab <-
            dat[, lapply(
              X = .SD,
              FUN = function(x) {
                return(length(unique(x[!is.na(x)])))
              }
            ), .SDcols = the.inputs]
        }

        min.categories.tab <-
          num.unique.tab[, .(variable = the.inputs,
                             min.categories = as.numeric(lapply(X = .SD, FUN = "min"))), .SDcols = the.inputs]

        min.categories.tab[, exclude.lack.contrast := min.categories < 2]

        inclusion.table <-
          merge(
            x = inclusion.table,
            y = min.categories.tab,
            by = "variable",
            all.x = TRUE,
            all.y = TRUE
          )

        inclusion.table[, exclude.numerous.categories := min.categories > max.input.categories &
                          class %in% c("character", "factor")]
      }
      setorderv(x = inclusion.table,
                cols = "order",
                order = 1L)
      exclusion.columns <-
        grep(pattern = "exclude", x = names(inclusion.table))

      inclusion.table[, include.variable := rowMeans(.SD, na.rm = TRUE) == 0, .SDcols = exclusion.columns]

      if (force.main.effects == TRUE) {
        all.input.names <-
          inclusion.table[include.variable == TRUE, variable]
      }
      if (force.main.effects == FALSE) {
        all.input.names <-
          inclusion.table[include.variable == TRUE &
                            specified.from != "interactions", variable]
      }

      if (order.as == "column.order") {
        all.input.names <- names(dat)[names(dat) %in% all.input.names]
      }


      # Compute included.interactions

      include.interaction <-
        as.logical(lapply(
          X = interactions,
          FUN = function(x) {
            return(inclusion.table[variable %in% x, min(include.variable) == 1])
          }
        ))

      interactions.with.backtick <-
        lapply(X = interactions,
               FUN = "add.backtick",
               include.backtick = include.backtick)

      all.interaction.terms <-
        as.character(lapply(
          X = interactions.with.backtick,
          FUN = "paste",
          collapse = " * "
        ))

      interaction.terms <-
        all.interaction.terms[include.interaction == TRUE]

      interactions.table <-
        data.table(interactions = all.interaction.terms, include.interaction = include.interaction)

    }

    if (!is.data.frame(x = dat)) {
      if (!is.null(interactions)) {
        interactions.with.backtick <-
          lapply(X = interactions,
                 FUN = "add.backtick",
                 include.backtick = include.backtick)

        interaction.terms <-
          as.character(lapply(
            X = interactions.with.backtick,
            FUN = "paste",
            collapse = " * "
          ))
      }
      if (is.null(interactions)) {
        interaction.terms <- NULL
      }

      all.input.names <- input.names
      inclusion.table <- data.table()
      interactions.table <- data.table()

      if (is.null(dat)) {
        inclusion.table.statement <-
          "dat was not provided (NA); no inclusion table was computed."
        interactions.table.statement <-
          "dat was not provided (NA); no interactions.table object was computed."
      }
      if (!is.null(dat)) {
        inclusion.table.statement <-
          "dat was not a data.frame; no inclusion table was computed."
        interactions.table.statement <-
          "dat was not a data.frame; no interactions.table object was computed."
      }
    }

    if (length(c(all.input.names[!is.null(all.input.names)], interaction.terms[!is.null(interaction.terms)])) == 0) {
      all.input.names <- "1"
    }

    if (order.as == "increasing") {
      all.input.names <- sort(x = all.input.names, decreasing = FALSE)
    }
    if (order.as == "decreasing") {
      all.input.names <- sort(x = all.input.names, decreasing = TRUE)
    }

    input.names.delineated <-
      add.backtick(x =  all.input.names, include.backtick = include.backtick)
    outcome.name.delineated <-
      add.backtick(x = outcome.name, include.backtick = include.backtick)

    rhs.with.missing <- c(input.names.delineated, interaction.terms)
    rhs <- rhs.with.missing[!is.na(rhs.with.missing)]

    if (length(rhs) == 0) {
      rhs <- "1"
    }

    the.formula <-
      sprintf("%s ~ %s", outcome.name.delineated, paste(rhs, collapse = " + "))

    if(include.intercept == FALSE){
      the.formula <- sprintf("%s - 1", the.formula)
    }

    if (format.as == "formula") {
      the.formula <- stats::as.formula(the.formula)
    }

    res <-
      list(
        formula = the.formula,
        inclusion.table = inclusion.table,
        interactions.table = interactions.table
      )

    return(res)
  }

#' reduce existing formula
#'
#' @param  the.initial.formula  object of class "lm" or for multiple responses of class c("mlm", "lm").
#' @param  dat Data frame, list or environment (or object coercible by as.data.frame to a data frame) containing the variables in the model.
#' @param  max.input.categories This limits the maximum number of variables that will be employeed in the formula. As default it is set at 20, but users can still change at his/her convinence.
#' @param  max.outcome.categories.to.search This limits the maximum number of outcome categories will be investigated in the formula. As default it is set at 4, but users can still change at his/her convinence.
#' @param  force.main.effects This is a logical value.  When TRUE, the intent is that any term included as an interaction (of multiple variables) must also be listed individually as a main effect.
#' @param  order.as  rearranges its first argument into ascending or descending order.
#' @param  include.backtick Add backticks to make a appropriate variable
#' @param  format.as The data type of the output.  If not set as "formula", then a character vector will be returned.
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
           format.as = "formula") {

    if (class(the.initial.formula) == "formula") {
      the.sides <- as.character(the.initial.formula)[2:3]
    }
    if (is.character(the.initial.formula)) {
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
