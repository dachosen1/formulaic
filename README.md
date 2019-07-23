# formulaic

[![Build Status](https://travis-ci.com/dachosen1/formulaic.svg?token=Vccc85T4kcPx9zLDqfTx&branch=master)](https://travis-ci.com/dachosen1/formulaic) 
[![codecov](https://codecov.io/gh/dachosen1/formulaic/branch/master/graph/badge.svg)](https://codecov.io/gh/dachosen1/formulaic)

## Overview 
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









