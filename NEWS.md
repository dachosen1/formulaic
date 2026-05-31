# formulaic 1.0.0

### R compatibility:
* Minimum R version bumped to 4.0.0, aligning with modern CRAN standards.
* `testthat` minimum version bumped to 3.0.0; deprecated `context()` and `expect_that()` calls replaced with testthat 3 equivalents (`expect_s3_class()`).

### Code quality:
* Replaced all uses of `T`/`F` abbreviations with `TRUE`/`FALSE` throughout source files (`create.formula`, `add.backtick`).
* Replaced explicit `== TRUE` / `== FALSE` comparisons with idiomatic R (`if (x)`, `if (!x)`) in `create.formula` and `reduce.existing.formula`.

### CI/CD:
* Updated GitHub Actions from `actions/checkout@v2` / `actions/upload-artifact@main` to `@v4` to comply with GitHub's deprecation of v2/v3 runners.

### Documentation:
* Fixed bug in vignette: `lm(formula = ex.form, ...)` corrected to `lm(formula = ex.form$formula, ...)`.
* Removed duplicate `add.backtick` vignette example chunk.
* Fixed stray Roxygen tag (`#' @param`) that leaked into the vignette's parameter description section; replaced with proper markdown.
* Corrected `reduce.existing.formula` parameter description (was incorrectly listed as class `"lm"`; now correctly describes `"formula"` or character input).
* Fixed `:::` (private accessor) to `::` for the exported `add.backtick` function in the vignette.
* Made the `transformation 2` vignette example self-contained rather than relying on ambient variable state.

# formulaic 0.0.9

### New features:
* create.formula: Added support for one-sided formulas by setting `outcome.name = NULL`. One-sided formulas (e.g., `~ x1 + x2`) are used by many R functions including `xtabs()`, `model.matrix()`, `matchit()`, `weightit()`, `cor.test()`, `pairs()`, and others.

### Documentation:
* Added one-sided formula examples to README
* Added "One-Sided Formulas" section to package vignette
* Updated `outcome.name` parameter documentation

# formulaic 0.0.8


### Minor updates: 
* reduce.existing.formula: A new parameter envir was added to store the environment of the formula, with the global environment as the default.

* add.backtick: A new parameter dat was addedBackticks can be added based on the relationship of the object to the data dat. The data can be formatted as either a data.frame or data.table object. 

### Major updates:
Improvement to accommodate variable transformations.  This allows the user to specify more complex quantities, e.g. sqrt(Age^2 * log(Income)).  In create.formula, this may be specified in the input.names or the interactions.  The calculated inclusion.table of quality checks has been updated to evaluate the transformations and then assess the inclusion and exclusion criteria.  Likewise, in add.backtick, a similar evaluation is used to determine whether encompass the object within backticks for use in a formula object.  This improves upon the previous version of the formulaic package.  Previously the main effects and interactions could only be based on the variables within a data.frame.  With this change, the package should better accommodate most transformations of these variables. 


# formulaic 0.0.7
* Implemented additional test coverage for create formula
* Improved code base quality 
* Addressed NA variables in SnackDat Cookies Product 
* Minor updates to the package vignette
* Removed dependency on stats on and DT 

# formulaic 0.0.6
* modified package citations
* added code coverage 

# formulaic 0.0.5
* Create formula now supports 'all' in input names 
* Added coverage test files
* Update the Vignette
* Refactor add.backtick function in Create formula to i.backtick  to address situtaion where  situal where include backtick  == 'all' outcome didn't properly compute. 

# formulaic 0.0.4
* Export function add.backtick 
* Modified Formulaic Vignette

# formulaic 0.0.3
* now depends on R (>= 3.1.0). #feedback from reddit user 

# formulaic 0.0.2
* Added a `NEWS.md` file to track changes to the package.
* Exported function add.backtick() as standalone function
* Modified the wording of the package overview and function description
* Addressed spelling and grammatical errors
* Linked to new package website

