# Reporting package for edgeTransport

R package **reporttransport**, version **0.0.2**

[![CRAN status](https://www.r-pkg.org/badges/version/reporttransport)](https://cran.r-project.org/package=reporttransport)  [![R build status](https://github.com/pik-piam/reporttransport/workflows/check/badge.svg)](https://github.com/pik-piam/reporttransport/actions) [![codecov](https://codecov.io/gh/pik-piam/reporttransport/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/reporttransport) 

## Purpose and Functionality

This package contains edgeTransport-specific routines to report model results. The main functionality is to generate transport reporting variables in MIF format from a given edgeTransport model run folder or REMIND input data.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("reporttransport")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Questions / Problems

In case of questions / problems please contact Johanna Hoppe Your family name <johanna.hoppe@pik-potsdam.de>.

## Citation

To cite package **reporttransport** in publications use:

Your family name J (2024). _reporttransport: Reporting package for edgeTransport_. R package version 0.0.2, <https://github.com/pik-piam/reporttransport>.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {reporttransport: Reporting package for edgeTransport},
  author = {Johanna Hoppe {Your family name}},
  year = {2024},
  note = {R package version 0.0.2},
  url = {https://github.com/pik-piam/reporttransport},
}
```
