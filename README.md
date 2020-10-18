A knitr engine for maxima
================

# Description

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

# Features

  - knitr.maxima sends maxima commands to and retrieves respective
    output from the maxima interpreter
  - each maxima command is automatically wrapped in a tex(…) command and
    is appropriatley formatted for output as 
  - “%” can be used as if working within maxima nonetheless, i.e. it is
    replaced by the previous command
  - each chunk is executed in a separate maxima session, i.e. variables
    and functions defined in a preceeding code chunk are not carried
    over to the next chunk

# Limitations

  - “%” only work one step back

# Usage

To use it, one needs to register the function `maxima` in the file
maxima.R as a knitr engine at the beginning of ones \*.Rmd file

``` r
source("maxima.R")
knit_engines$set(maxima = maxima)
```

# Requirements

# To Do

  - chunk option eval doesn’t have any effect
  - check this out:
    <https://rdrr.io/github/skranz/LyxMaxima/man/convert.maxima.output.html>
  - “%” should work the same way as in maxima (chaining arbitraily many
    commands)
  - persistent
    sessions

# Links

  - <http://maxima.sourceforge.net/docs/manual/maxima_174.html#Introduction-to-alt_002ddisplay>
  - <https://en.wikipedia.org/wiki/List_of_computer_algebra_systems>
  - <https://github.com/yihui/runr>
