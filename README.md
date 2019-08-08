# formulaic
[![Build Status](https://travis-ci.com/dachosen1/formulaic.svg?token=Vccc85T4kcPx9zLDqfTx&branch=master)](https://travis-ci.com/dachosen1/formulaic) 
[![codecov](https://codecov.io/gh/dachosen1/formulaic/branch/master/graph/badge.svg)](https://codecov.io/gh/dachosen1/formulaic)
[![license](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
[![CRAN status](https://www.r-pkg.org/badges/version/formulaic)](https://CRAN.R-project.org/package=formulaic)
[![](https://cranlogs.r-pkg.org/badges/formulaic)](https://cran.r-project.org/package=formulaic)


## Overview 

**Formulaic** is useful to create a dynamic formula with multiple features. It not only diminishes the time required for modeling and implementing, but also enriches the quality of the result.

Many statistical models and analyses in R are implemented through formula objects. The formulaic package creates a unified approach for programmatically and dynamically generating formula objects in R. Users may specify the inputs and outcomes of a model directly, search for variables to include based upon naming patterns, and identify variables to exclude. A wide range of quality checks are implemented to identify issues such as misspecified variables, duplication, a lack of contrast in the inputs, and a large number of levels in categorical data. These issues are documented and reported in a manner that provides greater accountability and useful information to guide an investigators choices in selecting features.

Dynamically generated formula objects can enable the development of applications that incorporate a user's inputs for selecting the variables of a model and the subsets of data to include. The quality checks enable a programmatic reduction of the inputs to those that are appropriate for inclusion in the model. By automatically limiting these selections, many models that would lead to errors or intractable computations may be proactively avoided. By performing this process in the creation of the formula, all statistical models and analyses relying on formula objects can easily make use of these features. Furthermore, the list of quality checks can be easily expanded to incorporate additional concerns.

## Install the current release from CRAN:
```r
install.packages('formulaic')
```

## Install the development version from GitHub:
```r
devtools::install_github('dachosen1/formulaic')
```

## Usage 

Formulaic package has two main functions – **create.formula** and **reduce.existing.formula** – and one subsidiary function, **add.backticks**. The main purpose of developing the package is to help users to build a robust model faster and more convenient.
 
**create.formula** automatically creates a formula from a provided list of input variables and the output variable. The variables will undergo a series of qualification tests such as automatic variable/categories reduction, typo, duplication, and lack of contrasted features elimination, etc. to make sure that a given feature is usable for modeling. This will reduce the time to build a model and set the users free from the trivial maneuver: manually inputting variables for modeling. The outcome of this formula can be used in a wide range from simple linear regression to any machine learning techniques such as random forest, neural network, etc. 
 
The principal advantages of using create.formula are followed:
 
1) Being able to dynamically generate a formula from a vector of inputs, without necessarily having to spell them all out by name. 

2) Adding variables by searching for patterns.
 
3) Simple integration of interactions.

4) Easy removal of specific variables. 
 
5) Quality checks that resolve a variety of issues -- typos, duplication, lack of contrast, etc. -- while providing a transparent explanation.
 
**reduce.existing.formula** trims an existing formula down. Users plug an existing formula into the function, then it will undergo the same test as create.formula.
 
**add.backticks** applies backticks the variables needs backticks to be employed in a formula as default. Users can also add backticks to all the variables; however, it is not necessary.   
 
### Example 
 
``` r 
n <- 10
dd <- data.table::data.table(w = rnorm(n= n), x = rnorm(n = n), pixel_1 = rnorm(n = n))
dd[, pixel_2 := 0.3 * pixel_1 + rnorm(n)]
dd[, y := 5 * x + 3 * pixel_1 + 2 * pixel_2 + rnorm(n)]

# create formula object 
formula1 <- create.formula(outcome.name = "y", input.names = "x", input.patterns = c("pi", "xel"), dat = dd)

# implement formula object
model <- lm(formula = formula1, data = dd)
```



