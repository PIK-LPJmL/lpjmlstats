# Statistical tools for LPJmL data analysis

R package **lpjmlstats**, version **0.5.0**

[![CRAN status](https://www.r-pkg.org/badges/version/lpjmlstats)](https://cran.r-project.org/package=lpjmlstats)  [![R build status](https://github.com/PIK-LPJmL/lpjmlstats/workflows/check/badge.svg)](https://github.com/PIK-LPJmL/lpjmlstats/actions) [![codecov](https://codecov.io/gh/PIK-LPJmL/lpjmlstats/branch/master/graph/badge.svg)](https://app.codecov.io/gh/PIK-LPJmL/lpjmlstats) [![r-universe](https://pik-piam.r-universe.dev/badges/lpjmlstats)](https://pik-piam.r-universe.dev/builds)

## Purpose and Functionality

This package provides statistical tools for LPJmL data analysis
    to be used for benchmarking LPJmL outputs.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("lpjmlstats")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Tutorial

The package comes with vignettes describing the basic functionality of the package and how to use it. You can load them with the following command (the package needs to be installed):

```r
vignette("benchmark-change-settings") # benchmark-change-settings
vignette("benchmark")                 # benchmark
```

## Questions / Problems

In case of questions / problems please contact David Hötten <davidho@pik-potsdam.de>.

## Citation

To cite package **lpjmlstats** in publications use:

Hötten D, Breier J (2024). _lpjmlstats: Statistical tools for LPJmL data analysis_. R package version 0.5.0, <https://github.com/PIK-LPJmL/lpjmlstats>.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {lpjmlstats: Statistical tools for LPJmL data analysis},
  author = {David Hötten and Jannes Breier},
  year = {2024},
  note = {R package version 0.5.0},
  url = {https://github.com/PIK-LPJmL/lpjmlstats},
}
```
