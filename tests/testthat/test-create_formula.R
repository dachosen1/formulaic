context("Create Formula")

n <- 10
dd <-
  data.table::data.table(w = rnorm(n = n),
                         x = rnorm(n = n),
                         pixel_1 = rnorm(n = n))
dd[, 'pixel 2' := 0.3 * pixel_1 + rnorm(n)]
dd[, pixel_3 := 0.3 * pixel_1 + rnorm(n)]
dd[, item_1 := 0.3 * pixel_3 + rnorm(n)]
dd[, item_2 := 0.3 * pixel_3 + rnorm(n)]
dd[, y := 5 * x + 3 * pixel_1 + 2 * pixel_3 + rnorm(n)]


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

input.names <- "Age"
interactions = list(c("Age Group", "Gender"))

test = data.frame()


#--------------------------------------------------------

formula.1 <- formulaic::create.formula(
  outcome.name = "y",
  input.names = c("x", "Random error", "y"),
  input.patterns = c("pix"),
  dat = dd
)

formula.2 <- formulaic::create.formula(
  outcome.name = "y",
  input.names = '.',
  input.patterns = "pix",
  dat = dd,
  include.backtick = 'all'
)

all.inputs <-
  `y` ~ `w` + `x` + `pixel_1` + `pixel 2` + `pixel_3` + `item_1` + `item_2`

test_that('Inclusion table: input names',
          {
            expect_equal(formula.1$formula, y ~ x + pixel_1 + `pixel 2` + pixel_3)
            expect_false(formula.1$inclusion.table[formula.1$inclusion.table$variable == "Random error"]$include.variable)
            expect_false(formula.1$inclusion.table[formula.1$inclusion.table$variable == "y"]$include.variable)
          })

test_that('Formula output',
          expect_that(formula.1$formula, is_a('formula')))

test_that('input names all',
          expect_equal(formula.2$formula, all.inputs))

# ---------------------------------

interaction.form <-
  formulaic::create.formula(
    outcome.name = awareness.name,
    input.names = input.names,
    dat = snack.dat,
    interactions = interactions
  )

test_that('outcome error',
          {
            expect_equal(
              formulaic::create.formula(
                input.names = 'y',
                outcome.name = 'x',
                dat = snack.dat
              ),
              "Error:  To create a formula, the outcome.name must match one of the values in names(dat)."
            )
            expect_equal(
              formulaic::create.formula(
                input.names = 'y',
                outcome.name = 'x',
                dat = test
              ),
              "Error:  dat must be an object with specified names."
            )
          })



test_that(
  'Interaction outcome',
  expect_equal(
    interaction.form$formula,
    Awareness ~ Age + `Age Group` + Gender + `Age Group` *
      Gender
  )
)
