% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/auto_mpg.R
\docType{data}
\name{auto_mpg}
\alias{auto_mpg}
\title{Auto MPG Data Set}
\format{
A data frame with 388 rows and 9 variables:
\describe{
\item{\code{mpg}}{numeric; miles per gallon.}
\item{\code{cyl}}{numeric; number of cylinders.}
\item{\code{disp}}{numeric; engine displacement.}
\item{\code{hp}}{numeric; horse power.}
\item{\code{wt}}{numeric; weight.}
\item{\code{accel}}{numeric; acceleration.}
\item{\code{year}}{numeric; make year.}
\item{\code{origin}}{factor; origin.}
}
}
\source{
\href{https://archive.ics.uci.edu/ml/datasets/auto+mpg}{ICU Machine Learning Repository}. This dataset was taken from the
StatLib library which is maintained at Carnegie Mellon University. The dataset was used in the
1983 American Statistical Association Exposition.
}
\usage{
auto_mpg
}
\description{
Predicting city-cycle fuel consumption (miles per gallon) from
automobile characteristics.
}
\note{
Row names are the automobile names with appended year. Four cases with
duplicate row names were deleted. Six cases with missing values
were also deleted.
}
\keyword{datasets}
