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

formula.1 <- create.formula(
  outcome.name = "y",
  input.names = c("x", "Random error", "y"),
  input.patterns = c("pix"),
  dat = dd
)

formula.2 <- create.formula(
  outcome.name = "y",
  input.names = '.',
  input.patterns = c("pix"),
  dat = dd,
  include.backtick = 'all'
)

all.inputs <- `y`~ `w` + `x` + `pixel_1` + `pixel 2` + `pixel_3` + `item_1` + `item_2`

test_that('Inclusion table: input names',
          {
            expect_equal(formula.1$formula, y ~ x + pixel_1 + `pixel 2` + pixel_3)
            expect_false(formula.1$inclusion.table[formula.1$inclusion.table$variable == "Random error"]$include.variable)
            expect_false(formula.1$inclusion.table[formula.1$inclusion.table$variable == "y"]$include.variable)
          })

test_that('Formula output',
          expect_that(formula.1$formula, is_a('formula')))

test_that(
  'input names all',
  expect_equal(
    formula.2$formula,all.inputs
  )
)


