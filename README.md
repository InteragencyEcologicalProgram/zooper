Zooplankton Synthesis in the Sacramento San Joaquin Delta
================
12/05/2019

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

# Introduction

An R package to download and integrate zooplankton data from the
Sacramento San Joaquin Delta.

Install the package with

``` r
#Requires devtools
#install.packages("devtools")

devtools::install_github("https://github.com/InteragencyEcologicalProgram/zooper")
```

The `Zoopsynther` function takes zooplankton data from different surveys
and integrates the data according to user-specified parameter choices.
The shiny app is a GUI (graphical user interface) that allows folks
without R experience to run the zooplankton synthesizer funciton and
download the resulting data. The shiny app (coming soon) also allows
folks with all experience levels to easily visualize the data.

The `Zoopdownloader` function downloads the zooplankton datasets from
their respective online sources and converts them to a consistent
format.

# Community or taxon-specific analyses?

The biggest problem with integrating zooplankton datasets is variability
in taxonomic resolution. To resolve this, we have developed 2 approaches
to consolidating inconsistent data to “least common denominator taxa.”
Depending on what type of analysis you wish to run, you may wish for
different types of synthesized data.

## For community data analyzers

*I want to analyze the community composition at whatever taxonomic level
lets me use all these datasets.*

  - Consistent taxonomic categories
  - No plankters counted more than once
  - Sacrifices some taxonomic resolution
  - Removes taxa with no relatives in all datasets (eg., Annelida)

## For specific taxa analyzers

*I want all possible data on these specific taxa.*

  - Calculates total CPUE for higher taxonomic levels
  - Some plankters appear in multiple nested taxa (e.g., Calanoida,
    Copepoda)
  - Perserves taxonomic resolution and creates taxonomic categories that
    are comparable across all datasets
  - Labels taxa that are comparable across all datasets, warns about
    those that are not.

# Size classes

We have integrated zooplankton data from 3 net size classes:

1.  Macro (500-505
    ![mu](https://latex.codecogs.com/gif.latex?%24%5Cmu%24)m): Amphipods
    and mysids
2.  Meso (150 - 160
    ![mu](https://latex.codecogs.com/gif.latex?%24%5Cmu%24)m): Copepods,
    cladocera
3.  Micro (43 ![mu](https://latex.codecogs.com/gif.latex?%24%5Cmu%24)m):
    Copepods, rotifers

Nets accurately sample zooplankton larger than the mesh size.
Zooplankton smaller than the mesh size are still captured and often
recorded in datasets, but the resulting CPUEs are not accurate. To
account for this we:

1.  Resolve taxonomic resolution separately within each net size class.
2.  If `Data = 'Taxa'`, we mark “summed groups” with the net size class
    from which they were derived.
3.  All potentially undersampled data are marked with a flag
    `Undersampled == TRUE`
4.  For the plots in the shiny app, all data with `Undersampled == TRUE`
    are removed. However, data downloaded from the app do contain
    undersampled data.

# Unresolved issues

For many studies, taxonomic resolution has changed over time. This could
confound analyses of zooplankton communities and abundances over time.

# Code of conduct

Please note that the ‘zooper’ project is released with a [Contributor
Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project,
you agree to abide by its terms.
