# Compare formula structure only, ignoring the attached environment.
# covr instruments the package in a temp library whose namespace differs
# from the inline test formula's environment, so direct expect_equal on
# formula objects can fail when testthat edition 3 / waldo is active.
expect_formula_equal <- function(actual, expected) {
  expect_equal(deparse(actual), deparse(expected))
}

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
  input.patterns = "pix",
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
            expect_formula_equal(formula.1$formula, y ~ x + pixel_1 + `pixel 2` + pixel_3)
            expect_false(formula.1$inclusion.table[formula.1$inclusion.table$variable == "Random error"]$include.variable)
            expect_false(formula.1$inclusion.table[formula.1$inclusion.table$variable == "y"]$include.variable)
          })

test_that('Formula output',
          expect_s3_class(formula.1$formula, 'formula'))

test_that('input names all',
          expect_formula_equal(formula.2$formula, all.inputs))

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
            expect_error(
              formulaic::create.formula(
                input.names = 'y',
                outcome.name = 'x',
                dat = snack.dat
              )
            )
            expect_error(
              formulaic::create.formula(
                input.names = 'y',
                outcome.name = 'x',
                dat = test
              )
            )
          })

test_that(
  'Interaction outcome',
  expect_formula_equal(
    interaction.form$formula,
    Awareness ~ Age + `Age Group` + Gender + `Age Group` *
      Gender
  )
)

# ---------------------------------
# One-sided formula tests (outcome.name = NULL)

test_that('one-sided formula with input.names', {
  one.sided.1 <- formulaic::create.formula(
    outcome.name = NULL,
    input.names = c("x", "w"),
    dat = dd
  )
  expect_formula_equal(one.sided.1$formula, ~ x + w)
  expect_s3_class(one.sided.1$formula, "formula")
})

test_that('one-sided formula with input.patterns', {
  one.sided.2 <- formulaic::create.formula(
    outcome.name = NULL,
    input.patterns = "pix",
    dat = dd
  )
  expect_formula_equal(one.sided.2$formula, ~ pixel_1 + `pixel 2` + pixel_3)
})

test_that('one-sided formula without dat', {
  one.sided.3 <- formulaic::create.formula(
    outcome.name = NULL,
    input.names = c("a", "b", "c")
  )
  expect_formula_equal(one.sided.3$formula, ~ a + b + c)
})

test_that('one-sided formula with interactions', {
  one.sided.4 <- formulaic::create.formula(
    outcome.name = NULL,
    input.names = c("x", "w"),
    interactions = list(c("x", "w")),
    dat = dd
  )
  expect_formula_equal(one.sided.4$formula, ~ x + w + x * w)
})

test_that('one-sided formula with include.intercept = FALSE', {
  one.sided.5 <- formulaic::create.formula(
    outcome.name = NULL,
    input.names = c("x", "w"),
    dat = dd,
    include.intercept = FALSE
  )
  expect_formula_equal(one.sided.5$formula, ~ x + w - 1)
})

test_that('one-sided formula with reduce = TRUE', {
  one.sided.6 <- formulaic::create.formula(
    outcome.name = NULL,
    input.names = c("x", "w", "pixel_1"),
    dat = dd,
    reduce = TRUE
  )
  expect_s3_class(one.sided.6$formula, "formula")
  # All variables should be included since they have contrast
  expect_true("x" %in% all.vars(one.sided.6$formula))
})

test_that('one-sided formula with . for all columns', {
  one.sided.7 <- formulaic::create.formula(
    outcome.name = NULL,
    input.names = '.',
    dat = dd
  )
  # Should include all columns from dd
  expect_s3_class(one.sided.7$formula, "formula")
  expect_true(length(all.vars(one.sided.7$formula)) == ncol(dd))
})
