context("Reduce Formula")


the.initial.formula <- Awareness ~ .

formula_1 <- formulaic::reduce.existing.formula(
  the.initial.formula = the.initial.formula,
  dat = snack.dat,
  max.input.categories = 30
)

test_that(
  'Reduce formula',
  expect_equal(
    formula_1$formula,
    Awareness ~ Age + Gender + Income + Region + Persona + Product +
      `Age Group` + `Income Group`
  )
)
