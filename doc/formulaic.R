## ----setup---------------------------------------------------------------
library(formulaic)
library(data.table)
library(knitr)

## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  message = FALSE,
  warning = FALSE,
  error = FALSE,
  comment = "#>"
)

tools::compactPDF(paths = "../vignettes/",
                  qpdf = Sys.which(Sys.getenv("R_QPDF", "qpdf")),
                  gs_quality = 'ebook')

## ------------------------------------------------------------------------
data("snack.dat")

## ----constant------------------------------------------------------------
outcome.name.awareness <- "Awareness"
outcome.name.satisfaction <- "Satisfaction"

input.names <- c("Age", "Gender", "Income", "Region", "Persona", "Typo")
input.patterns <- c("User", "BP_", outcome.name.awareness, outcome.name.satisfaction)


input.names <- c("Age", "Gender", "Income", "Region", "Persona", "Typo")
input.patterns <- c("User", "BP_", outcome.name.awareness, outcome.name.satisfaction)

reduce <- FALSE
max.input.categories = 20
max.outcome.categories.to.search = 4
order.as = "as.specified" 
include.backtick = "as.needed"
format.as = "formula"

## ----snack.dat-----------------------------------------------------------
dim(snack.dat)
names(snack.dat)

## ----add.backtick--------------------------------------------------------
formulaic:::add.backtick(x = names(snack.dat),include.backtick = 'all')

formulaic:::add.backtick(x = names(snack.dat),include.backtick = 'as.needed')

## ----create.formula examples 1-------------------------------------------
form.1 <-
  create.formula(
    outcome.name = outcome.name.awareness,
    input.names = NA,
    input.patterns = NA,
    dat = snack.dat
  )

form.1$formula
form.1$inclusion.table

## ----create.formula examples 2-------------------------------------------
form.2 <-
  create.formula(
    outcome.name = outcome.name.awareness,
    input.names = ".",
    input.patterns = NA,
    dat = snack.dat
  )

form.2$formula
form.2$inclusion.table

## ----create.formula examples 3-------------------------------------------
form.3 <-
  create.formula(
    outcome.name = outcome.name.awareness,
    input.names = input.names,
    input.patterns = NA,
    dat = NA,
    reduce = reduce,
    max.input.categories = max.input.categories,
    max.outcome.categories.to.search = max.outcome.categories.to.search,
    order.as = order.as,
    include.backtick = include.backtick,
    format.as = "character"
  )

form.3$formula
form.3$inclusion.table

## ----create.formula examples 4-------------------------------------------
form.4 <-
  create.formula(
    outcome.name = outcome.name.awareness,
    input.names = input.names,
    input.patterns = input.patterns,
    dat = snack.dat,
    reduce = reduce,
    max.input.categories = max.input.categories,
    max.outcome.categories.to.search = max.outcome.categories.to.search,
    order.as = order.as,
    include.backtick = include.backtick,
    format.as = format.as
  )

form.4$formula
form.4$inclusion.table

## ----create.formula examples 5-------------------------------------------
form.5 <-
  create.formula(
    outcome.name = outcome.name.awareness,
    input.names = input.names,
    input.patterns = input.patterns,
    dat = snack.dat,
    reduce = TRUE,
    max.input.categories = max.input.categories,
    max.outcome.categories.to.search = max.outcome.categories.to.search,
    order.as = order.as,
    include.backtick = include.backtick,
    format.as = format.as
  )

form.5$formula
form.5$inclusion.table

## ----create.formula examples 6-------------------------------------------
form.6 <-
  create.formula(
    outcome.name = outcome.name.satisfaction,
    input.names = input.names,
    input.patterns = input.patterns,
    dat = snack.dat,
    reduce = TRUE,
    max.input.categories = max.input.categories,
    max.outcome.categories.to.search = max.outcome.categories.to.search,
    order.as = order.as,
    include.backtick = include.backtick,
    format.as = format.as
  )

form.6$formula
form.6$inclusion.table

## ----reduce.existing.formula examples 7----------------------------------
form.7 <-
  reduce.existing.formula(
    the.initial.formula = form.3$formula,
    dat = snack.dat,
    format.as = "character"
  )

form.7$formula
form.7$inclusion.table

## ----create.formula examples 8-------------------------------------------
form.8 <-
  reduce.existing.formula(
    the.initial.formula = form.4$formula,
    dat = snack.dat,
    format.as = "formula"
  )

form.8$formula
form.8$inclusion.table

