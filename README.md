A knitr engine for maxima
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

The R-package knitr already supports *many* programming languages to be
used within code chunks. It also provides the possibility to write your
own engine for processing code that is not supported.

> Maxima is a system for the manipulation of symbolic and numerical
> expressions, including differentiation, integration, Taylor series,
> Laplace transforms, ordinary differential equations, systems of linear
> equations, polynomials, sets, lists, vectors, matrices and tensors.
> Maxima yields high precision numerical results by using exact
> fractions, arbitrary-precision integers and variable-precision
> floating-point numbers. Maxima can plot functions and data in two and
> three dimensions.

This engine can be used to process maxima code within Markdown
documents.

To use it, one needs to register the function `maxima` in the file
maxima.R as a knitr engine at the beginning of ones \*.Rmd file

``` r
source("maxima.R")
knit_engines$set(maxima = maxima)
```
