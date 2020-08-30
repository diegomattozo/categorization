## Installation

> Install this package following the tutorial for installing R packages from github in: https://cran.r-project.org/web/packages/githubinstall/vignettes/githubinstall.html

## Introduction

In this R package there are two supervised categorization methods of continuous predictor variables for binary response models. The first is a class of univariate methods that seeks to keep the association between the response variable and the categorized covariate using qualitative association metrics like Information Statistics and Kendalls Tau-C. The second is a multivariate method that seeks to maintain the dependency structure between the predictor variables by searching, for each step of the algorithm, the best cutpoint in the space of all possible cutpoints for all the predictor variables.  

## Usage

> IMPORTANT: The R functions for the two categorization methods expect that the last column of your table is the response variable. For example:

|  X1 |  X2 |  Y  |
| --- | ----| ----|
| X11 | X12 | Y13 |
| X21 | X22 | Y23 |
| X31 | X32 | Y23 |


### Univariate

`multdiscretization::discretize(table, meth = 4)`

In which table is the dataset to be categorized and meth is the chosen association metric. There are five association metrics: 
  1. Caim 
  2. Cacc 
  3. Ameva 
  4. Information Value 
  5. Kendall's Tau-C. 

The return value is a R list (associative array) with the transformed dataset and the cutpoints for each predictor variable.

### Multivariate

`multdiscretization::multdiscretization(train, validation)`

In this method the best cutpoints are selected from a validation dataset to avoid overfitting so you must pass a training and a validation dataset. The return value is a R list with the two categorized datasets and the vector of cutpoints for all variables. 

#### Helper methods

`apply_univariate_cutpoints(db, quantile_cutpoints, cutpoints)`

This function applies the cutpoints returned from the univariate method in a new dataset.

`train_test_split(db, test_percentual, seed)`

Helper function to randomly split your training dataset.

`cutpoint_discretization(db, cutpoints)`

This function applies the cutpoints from the multivariate method in a new dataset.

For more  detailed information of each method type `??multdiscretization` in R.


