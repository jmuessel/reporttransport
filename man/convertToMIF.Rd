% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convertToMIF.R
\name{convertToMIF}
\alias{convertToMIF}
\title{Convert variables into model intercomparison format MIF}
\usage{
convertToMIF(
  vars,
  GDPMER,
  helpers,
  scenario,
  model,
  gdx,
  isTransportExtendedReported = FALSE
)
}
\arguments{
\item{vars}{Variables to be aggregated and converted into MIF format}

\item{GDPMER}{GDP on market exchange rate basis to be used as weight for regional aggregation}

\item{helpers}{List of helpers}

\item{scenario}{Scenario name in MIF entry}

\item{model}{Model name in MIF entry}

\item{gdx}{GDX file containing further regional aggregation levels}

\item{isTransportExtendedReported}{Switch to enable the extended transport variable set}
}
\value{
Variables provided in different aggregation levels in MIF format
}
\description{
Convert variables into model intercomparison format MIF
}
\author{
Johanna Hoppe
}
