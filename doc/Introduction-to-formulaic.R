## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=FALSE---------------------------------------------------
library(formulaic)
library(data.table)
library(knitr)
data("snack.dat", package = "formulaic")

## ----constant,echo=FALSE-------------------------------------------------
id.name <- "User ID"
awareness.name <- "Awareness"
satisfaction.name <- "Satisfaction"
age.name <- "Age"
gender.name <- "Gender"
income.name <- "Income"
region.name <- "Region"
persona.name <- "Persona"
bp.patterns <- "BP_"
consumption.name <- "Consumption"
consideration.name <- "Consideration"
advocacy.name <- "Advocacy"
satisfaction.name <- "Satisfaction"
age.group.name <- "Age Group"
income.group.name <- "Income Group"

max.input.categories <- 20
max.outcome.categories.to.search <- 4
order.as <- "as.specified"
include.backtick <- "as.needed"
format.as <- "formula"
force.main.effects <- TRUE

## ----example-------------------------------------------------------------
awareness.name = "Awareness"
variable.names = c("Age", "Gender", "Income Group", "Region", "Persona", "Typo")

ex.form <-
  create.formula(outcome.name = awareness.name,
                 input.names = variable.names,
                 dat = snack.dat)

ex.form$formula
lm_example <- lm(formula = ex.form, data = snack.dat)
summary(lm_example)

## ------------------------------------------------------------------------
user.outcome.name <- "Satisfaction"
user.input.names <- c('Age Group', 'Gender', 'Region')

create.formula(outcome.name = user.outcome.name, input.names = user.input.names)$formula

## ----dataset-------------------------------------------------------------
list(dim(snack.dat), names(snack.dat))

## ----add.backtics example------------------------------------------------
as.needed = formulaic:::add.backtick(x = names(snack.dat), include.backtick = 'as.needed')
all = formulaic:::add.backtick(x = names(snack.dat), include.backtick = 'all')

library(DT)
datatable(data = cbind(as.needed, all))

## ----add.backtics example 2----------------------------------------------
create.formula(outcome.name = awareness.name, input.names = variable.names)$formula

## ----add.backtics example 3----------------------------------------------
create.formula(
  outcome.name = awareness.name,
  input.names = variable.names,
  format.as = "character",
  include.backtick = "all"
)$formula

## ----add.backtics example 4----------------------------------------------
create.formula(
  outcome.name = awareness.name,
  input.names = variable.names,
  format.as = "character",
  include.backtick = "as.needed"
)$formula

## ----create.formula examples basic---------------------------------------
outcome.name.awareness <- "Awareness"
input.names <-
  c("Age", "Gender", "Income", "Region", "Persona", "Typo")

basic.form <-
  create.formula(outcome.name = outcome.name.awareness,
                 input.names = input.names,
                 dat = snack.dat)

print(basic.form)

## ----create.formula example interactions---------------------------------
interactions = list(c("Age Group", "Gender"),
                    c("Age Group", "Region"),
                    c("Age Group", "Gender", "Region"))

interaction.form <-
  create.formula(
    outcome.name = outcome.name.awareness,
    input.names = input.names,
    dat = snack.dat,
    interactions = interactions
  )

print(interaction.form)

## ----create.formula example input.patterns-------------------------------
bp.pattern = "BP_"
input.patterns = c("Gend", bp.pattern)

pattern.form <-
  create.formula(
    outcome.name = outcome.name.awareness,
    input.names = input.names,
    dat = snack.dat,
    input.patterns = input.patterns
  )

print(pattern.form)

## ----create.formula example dot.1----------------------------------------
dot.form.1 <-
  create.formula(outcome.name = outcome.name.awareness,
                 input.names = ".",
                 dat = snack.dat)

print(dot.form.1)

## ----create.formula example dot.2----------------------------------------

input.names = c("Gender", ".")

dot.form.2 <- create.formula(outcome.name = outcome.name.awareness, input.names = input.names, dat = snack.dat)

print(dot.form.2)

## ----create.formula example dot.3----------------------------------------

input.names = c("Typo", ".")

dot.form.2 <- create.formula(outcome.name = outcome.name.awareness, input.names = input.names, dat = snack.dat)

print(dot.form.2)

## ----create.formual example variables.to.exclude.form--------------------
input.names <-
  c("Age",
    "Gender",
    "Income",
    "Region",
    "Persona",
    "Typo",
    "Age Group")
interactions <-
  list(
    c("Age", "Gender"),
    c("Age", "Income"),
    c("Age", "Gender", "Income"),
    c("Gender", "Inco"),
    c("Age", "Reg ion")
  )
bp.pattern = "BP_"
variables.to.exclude = c("BP_Delicious_0_10", "Gender")

variables.to.exclude.form <-
  create.formula(
    outcome.name = outcome.name.awareness,
    input.names = input.names,
    interactions = interactions,
    input.patterns = bp.pattern,
    variables.to.exclude = variables.to.exclude,
    dat = snack.dat
  )


print(variables.to.exclude.form)

## ----create.formula outcomes as inputs-----------------------------------
input.names <- c("Income", "Age", "Income")
income.name = "Income"

outcomes.as.inputs.form <-
  create.formula(outcome.name = income.name,
                 input.names = input.names,
                 dat = snack.dat)

print(outcomes.as.inputs.form)

## ----create.formula example duplicated.inputs and interactions-----------
duplicated.inputs <- c(rep.int(x = "Age", times = 2), "Income")
duplicated.interactions <-
  list(c("Age", "Income"), c("Age", "Income"))

duplicated.form <-
  create.formula(
    outcome.name = outcome.name.awareness,
    input.names = duplicated.inputs,
    interactions = duplicated.interactions,
    dat = snack.dat
  )

print(duplicated.form)

## ----create.formula example with typo------------------------------------
input.names <- c("Age", "Typo")
income.name <- "Income"

formula.with.typo <-
  create.formula(outcome.name = income.name, input.names = input.names)
print(formula.with.typo)

## ----create.formula example without typo---------------------------------
formula.without.typo <-
  create.formula(outcome.name = income.name,
                 input.names = input.names,
                 dat = snack.dat)
print(formula.without.typo)

## ----Numeric variable with no variation----------------------------------
snack.dat[, .N, keyby = c("Awareness", "Consideration")]

## ----lack of contrast example numerical variables------------------------
formula.consideration <-
  create.formula(outcome.name = consideration.name,
                 input.names = c(age.name, awareness.name))

print(formula.consideration$formula)

glm(formula = formula.consideration,
    data = snack.dat,
    family = "binomial")$coefficients

## ----create.formula with lack of contrast categorical 1------------------
formula.consideration <-
  create.formula(
    outcome.name = consideration.name,
    input.names = c(age.name, awareness.name),
    dat = snack.dat,
    reduce = TRUE
  )

print(formula.consideration)

## ----lack of contrast example categorical variables 0--------------------
formula.awareness <-
  create.formula(outcome.name = awareness.name,
                 input.names = c(age.group.name, gender.name))

print(formula.awareness$formula)

## ----create.formula with lack of contrast 2, eval=FALSE, include=TRUE----
#  
#  
#  formula.awareness <-
#    create.formula(
#      outcome.name = awareness.name,
#      input.names = c(age.group.name, gender.name),
#      dat = snack.dat[get(age.group.name) == "[ 18, 35)", ],
#      reduce = TRUE
#    )
#  
#  print(formula.awareness)

## ----create.fomula lack of contrast 3------------------------------------
formula.consideration.1 <-
  create.formula(
    outcome.name = consideration.name,
    input.names = c(age.group.name, gender.name, awareness.name),
    dat = snack.dat,
    reduce = TRUE,
    max.outcome.categories.to.search = 1
  )

print(formula.consideration.1)

## ----create.formula lack of contrast 4-----------------------------------
formula.consideration.2 <-
  create.formula(
    outcome.name = consideration.name,
    input.names = c(age.group.name, gender.name, awareness.name),
    dat = snack.dat,
    reduce = TRUE,
    max.outcome.categories.to.search = 2
  )

print(formula.consideration.2)

## ----create.formula large volume of categorical variables 01-------------
create.formula(
  outcome.name = satisfaction.name,
  input.names = c(age.name, income.name, region.name, id.name),
  dat = snack.dat,
  reduce = TRUE,
  max.input.categories = 30
)$formula

## ----create.formula large volume of categorical variables----------------
create.formula(
  outcome.name = awareness.name,
  input.names = ".",
  reduce = TRUE,
  dat = snack.dat,
  max.input.categories = 30
)$formula


## ----reduce.existing.formula example-------------------------------------
the.initial.formula <- Awareness ~ .

reduce.existing.formula(
  the.initial.formula = the.initial.formula,
  dat = snack.dat,
  max.input.categories = 30
)$formula

