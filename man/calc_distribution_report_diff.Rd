% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/healthcare_system.R
\name{calc_distribution_report_diff}
\alias{calc_distribution_report_diff}
\title{Time Difference Between Infection and Report of Cases}
\usage{
calc_distribution_report_diff(
  ages = "all",
  regions = "Germany",
  from = "2020-01-01",
  to = Sys.Date(),
  cut = Inf
)
}
\arguments{
\item{ages}{A vector of numbers specifying the desired age groups. The available age groups are "A00-A04","A05-A14","A15-A34","A35-A59","A60-A79" and "A80+".
The numbers in \code{ages} are automatically assigned to the belonging age group and afterwards, the cases and deaths of of these age groups are added up.}

\item{regions}{A vector that either consists of strings (either the names of German districts or the names of German states) or district ID's.
If this vector has more than one entry, the tibble contains the data of these regions together.}

\item{from}{A date that specifies the beginning of the time series}

\item{to}{A date that specifies the end of the time series}

\item{cut}{A integer that determines at which number the distribution is cut
off, because some of the results are nonsensical}
}
\value{
A plot that displays the distribution of differences between infection and report date
}
\description{
This function returns a tibble, which summarises the distribution of the
difference between a case being reported and a person being infected over the
whole pandemic. It only uses the cases for which the actual time of infection is know.
}
\section{Warning}{

When specifying region \strong{and} agegroup, the data will not be accurate because
there is no population data for the age groups in each district and it will be estimated
by the age distribution of Germany.
Therefore, it is recommended to specify only one or the other
}

\seealso{
Other time difference infection vs. report: 
\code{\link{plot_distribution_report_diff}()}
}
\concept{time difference infection vs. report}
