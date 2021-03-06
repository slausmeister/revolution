% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/population_density.R
\name{get_pop_density_with_sti}
\alias{get_pop_density_with_sti}
\title{Correlation Between Population Density and STI}
\usage{
get_pop_density_with_sti(regions = "all")
}
\arguments{
\item{regions}{A vector of district IDs or district names, which will be analyzed.}
}
\value{
A tibble which contains the average STI over the pandemic with the population density of the district
}
\description{
This function can be used to analyze the relation between the population density
and average incidence over the pandemic. The user can specify, which districts
to analyze.
}
\section{Warning}{

When calling this function with the paramater "all" (the default paramater),
the results may take a while, because this is a very expensive computation.
}

\examples{
get_pop_density_with_sti(regions=c(8221, "Mannheim"))
}
\seealso{
Other population density and STI: 
\code{\link{plot_pop_density_with_linear_model}()}
}
\concept{population density and STI}
