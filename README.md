# formulaic
![R-CMD-check](https://github.com/dachosen1/formulaic/workflows/R-CMD-check/badge.svg)
[![Codacy Badge](https://api.codacy.com/project/badge/Grade/0c8859e97a934d0daf112469a7b7279d)](https://app.codacy.com/manual/anderson.nelson1/formulaic?utm_source=github.com&utm_medium=referral&utm_content=dachosen1/formulaic&utm_campaign=Badge_Grade_Dashboard)
[![Build Status](https://travis-ci.com/dachosen1/formulaic.svg?token=Vccc85T4kcPx9zLDqfTx&branch=master)](https://travis-ci.com/dachosen1/formulaic) 
[![codecov](https://codecov.io/gh/dachosen1/formulaic/branch/master/graph/badge.svg)](https://codecov.io/gh/dachosen1/formulaic)
[![license](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
[![CRAN status](https://www.r-pkg.org/badges/version/formulaic)](https://CRAN.R-project.org/package=formulaic)
[![](http://cranlogs.r-pkg.org/badges/grand-total/formulaic?color=blue)](https://cran.r-project.org/package=formulaic)
[![](https://cranlogs.r-pkg.org/badges/formulaic)](https://cran.r-project.org/package=formulaic)


## Overview 

**Formulaic** is useful in creating a dynamic formula with multiple features. It not only diminishes the time required for modeling and implementing, but also enriches the quality of the result.
Many statistical models and analyses in R are implemented through formula objects. The formulaic package creates a unified approach for programmatically and dynamically generating formula objects in R. Users may specify the inputs and outcomes of a model directly, search for variables to include based upon naming patterns, and identify variables to exclude. A wide range of quality checks is implemented to identify issues such as misspecified variables, duplication, a lack of contrast in the inputs, and a large number of levels in categorical data. These issues are documented and reported in a manner that provides greater accountability and useful information to guide the investigator's choices in selecting features.
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
 
**create.formula** automatically creates a formula from a provided list of input variables and the output variable. The variables will undergo a series of qualification tests such as automatic variable/categories reduction, typo, duplication, and lack of contrasted features elimination, etc. to make sure that a given feature is used for modeling. This will reduce the time to build a model and set the users free from the trivial maneuver: manually inputting variables for modeling. The outcome of this formula can be used in a wide range from simple linear regression to any machine learning techniques such as random forest, neural network, etc. 

The principal advantages of using create.formula are as followed:
 
1) Being able to dynamically generate a formula from a vector of inputs, without necessarily having to spell them all out by name. 

2) Adding variables by searching for patterns.
 
3) Simple integration of interactions.

4) Easy removal of specific variables. 
 
5) Quality checks that resolve a variety of issues -- typos, duplication, lack of contrast, etc. -- while providing a transparent explanation.
 
**reduce.existing.formula** Trims existing formula . Users plug an existing formula into the function; then, it will undergo the same test as create.formula.
 
**add.backticks** applies backticks to the variables that require backticks to be employed in a formula as default. Users can also add backticks to all the variables; however, it is not necessary.   

 
### Example 
 
``` r 
n <- 10
dd <- data.table::data.table(w = rnorm(n= n), x = rnorm(n = n), pixel_1 = rnorm(n = n))
dd[, 'pixel 2' := 0.3 * pixel_1 + rnorm(n)]
dd[, pixel_3 := 0.3 * pixel_1 + rnorm(n)]
dd[, item_1 := 0.3 * pixel_3 + rnorm(n)]
dd[, item_2 := 0.3 * pixel_3 + rnorm(n)]
dd[, y := 5 * x + 3 * pixel_1 + 2 * pixel_3 + rnorm(n)]
```
The resulting script create a data.table with 8 unique features.

```r 
names(dd)
"w"       "x"       "pixel_1" "pixel 2" "pixel_3" "item_1"  "item_2"  "y"  
```
Traditionally, creating a formula in R required the user to select the desired variables or use the y~ . notation to select all the features. **`formulaic`** is useful to choose variables programmatically to include and perform a quality check against the input.

**use case:** Creating a model with all pixel patterns 

``` r 
# create formula object 
formula1 <- create.formula(outcome.name = "y", input.names = c("x","Random error", "y"), input.patterns = c("pix"), dat = dd)

formula1$formula
y ~ x + pixel_1 + `pixel 2` + pixel_3
```
The result is a formula object of all the pixel variables and the input x. Notice that the 'Random error' variable was automatically excluded from the output, and a backtick was automatically added to the variable pixel 2. Since the independent variable y was included as a feature, it was excluded from the output formula. 

Details of the variables included are provided in the inclusion table 

```r 
# inclustion table
> formula1$inclusion.table
       variable   class order specified.from exclude.user.specified exclude.not.in.names.dat exclude.matches.outcome.name include.variable
1:            x numeric     1    input.names                  FALSE                    FALSE                        FALSE             TRUE
2: Random error    <NA>     2    input.names                  FALSE                     TRUE                        FALSE            FALSE
3:            y numeric     3    input.names                  FALSE                    FALSE                         TRUE            FALSE
4:      pixel_1 numeric     4 input.patterns                  FALSE                    FALSE                        FALSE             TRUE
5:      pixel 2 numeric     5 input.patterns                  FALSE                    FALSE                        FALSE             TRUE
6:      pixel_3 numeric     6 input.patterns                  FALSE                    FALSE                        FALSE             TRUE

# implement formula object
model <- lm(formula = formula1, data = dd)
```



