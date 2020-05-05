#' Add backtick
#'
#' Function that add backticks to the input variables.
#'
#' @param  x  Character value specifying the name of input parameters.
#' @param  include.backtick specifies whether a backtick should be added. Parameter values should be either 'all' or 'as.needed'
#' @export

add.backtick <- function(x, include.backtick = "as.needed") {
  if (include.backtick == "all") {
    w <- seq_along(x)

  }
  if (include.backtick == "as.needed") {
    w <- grep(pattern = " ",
              x = x,
              fixed = TRUE)
  }
  if (length(w) > 0) {
    x[w] <- sprintf("`%s`", x[w])
  }
  return(x)
}

#' Create Formula
#'
#' Create formula is a tool to automatically create a formula object from a provided variable and output names. Reduces the time required to manually input variables for modeling. Output can be used in linear regression, random forest, neural network etc. Create formula becomes useful when modeling data with multiple features. Reduces the time required for modeling and implementation :

#' @param outcome.name A character value specifying the name of the formula's outcome variable. In this version, only a single outcome may be ed. The first entry of outcome.name will be used to build the formula.
#' @param input.names The names of the variables with the full names delineated. User can specify '.' or 'all' to e all the column variables.
#' @param input.patterns es additional input variables. The user may enter patterns -- e.g. to e every variable with a name that es the pattern. Multiple patterns may be ed as a character vector. However, each pattern may not contain spaces and is otherwise subject to the same limits on patterns as used in the grep function.
#' @param dat User can specify a data.frame object that will be used to remove any variables that are not listed in names(dat. As default it is set as NULL. In this case, the formula is created simply from the outcome.name and input.names.
#' @param interactions A list of character vectors. Each character vector es the names of the variables that form a single interaction. Specifying interactions = list(c("x", "y"), c("x", "z"), c("y", "z"), c("x", "y", "z")) would lead to the interactions x*y + x*z + y*z + x*y*z.
#' @param force.main.effects This is a logical value. When TRUE, the intent is that any term ed as an interaction (of multiple variables) must also be listed individually as a main effect.
#' @param reduce A logical value. When dat is not NULL and reduce is TRUE, additional quality checks are performed to examine the input variables. Any input variables that exhibit a lack of contrast will be excluded from the model. This search is global by default but may be conducted separately in subsets of the outcome variables by specifying max.outcome.categories.to.search. Additionally, any input variables that exhibit too many contrasts, as defined by max.input.categories, will also be excluded.
#' @param max.input.categories Limits the maximum number of variables that will be employed in the formula. As default it is set at 20, but users can still change at his/her convenience.
#' @param max.outcome.categories.to.search A numeric value. The create.formula function es a feature that identifies input variables exhibiting a lack of contrast. When reduce = TRUE, these variables are automatically excluded from the resulting formula. This search may be expanded to subsets of the outcome when the number of unique measured values of the outcome is no greater than max.outcome.categories.to.search. In this case, each subset of the outcome will be separately examined, and any inputs that exhibit a lack of contrast within at least one subset will be excluded.
#' @param order.as User can specify the order the input variables in the formula in a variety of ways for patterns: increasing for increasing alphabet order, decreasing for decreasing alphabet order, column.order for as they appear in data, and as.specified for maintaining the user's specified order.
#' @param include.backtick Add backticks if needed. As default it is set as 'as.needed', which add backticks when only it is needed. The other option is 'all'. The use of include.backtick = "all" is limited to cases in which the output is generated as a character variable. When the output is generated as a formula object, then R automatically removes all unnecessary backticks. That is, it is only compatible when format.as != formula.
#' @param format.as The data type of the output. If not set as "formula", then a character vector will be returned.
#' @param variables.to.exclude A character vector. Any variable specified in variables.to.exclude will be dropped from the formula, both in the individual inputs and in any associated interactions. This step supersedes the inclusion of any variables specified for inclusion in the other parameters.
#' @param include.intercept A logical value. When FALSE, the intercept will be removed from the formula.
#'
#' @details  Return as the data type of the output.  If not set as "formula", then a character vector will be returned.
#' The input.names and names of variables matching the input.patterns will be concatenated to form the full list of input variables.
#'
#' @import data.table stats
#'
#' @export
#' @examples
#'  n <- 10
#'  dd <- data.table::data.table(w = rnorm(n= n), x = rnorm(n = n), pixel_1 = rnorm(n = n))
#'  dd[, pixel_2 := 0.3 * pixel_1 + rnorm(n)]
#'  dd[, y := 5 * x + 3 * pixel_1 + 2 * pixel_2 + rnorm(n)]
#'
#'  create.formula(outcome.name = "y", input.names = "x", input.patterns = c("pi", "xel"), dat = dd)
#'
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

    input.names <- unique(input.names)
    interactions <- unique(interactions)
    input.patterns <- unique(input.patterns)

    if (is.data.frame(dat)) {
      data.table::setDT(dat)

      if (!is.null(input.names)) {
        if ("." %in% input.names | "all" %in% input.names) {
          input.names <-
            unique(c(input.names[!input.names %in% c(".", 'all')], names(dat)))
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
      variable.names.from.exclude <- NULL
      if (!is.null(variables.to.exclude)) {
        variable.names.from.exclude <- unique(variables.to.exclude)
      }

      variable.names.from.patterns <- NULL

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

      if (length(unique.names) == 0 |
          is.null(length(unique.names))) {
        unique.names <- NA
      }

      #Compute inclusion.table

      inclusion.table <-
        data.table(variable = unique.names)[!is.na(variable)]


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
        input.names <-
          inclusion.table[include.variable == TRUE, variable]
      }

      if (force.main.effects == FALSE) {
        input.names <-
          inclusion.table[include.variable == TRUE &
                            specified.from != "interactions", variable]
      }

      if (order.as == "column.order") {
        input.names <- names(dat)[names(dat) %in% input.names]
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

      inclusion.table <- data.table()
      interactions.table <- data.table()

    }

    if (length(c(input.names[!is.null(input.names)], interaction.terms[!is.null(interaction.terms)])) == 0) {
      input.names <- "1"
    }

    if (order.as == "increasing") {
      input.names <- sort(x = input.names, decreasing = FALSE)
    }

    if (order.as == "decreasing") {
      input.names <- sort(x = input.names, decreasing = TRUE)
    }

    input.names.delineated <-
      add.backtick(x =  input.names, include.backtick = include.backtick)

    outcome.name.delineated <-
      add.backtick(x = outcome.name, include.backtick = include.backtick)

    rhs.with.missing <- c(input.names.delineated, interaction.terms)
    rhs <- rhs.with.missing[!is.na(rhs.with.missing)]

    if (length(rhs) == 0) {
      rhs <- "1"
    }

    the.formula <-
      sprintf("%s ~ %s", outcome.name.delineated, paste(rhs, collapse = " + "))

    if (include.intercept == FALSE) {
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
#'
#' @export
#' @examples
#' data('snack.dat')
#' the.initial.formula <- Awareness ~ .
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
      (seq_along(the.pieces.trimmed))[!((seq_along(the.pieces.trimmed)) %in% w.int)]

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
