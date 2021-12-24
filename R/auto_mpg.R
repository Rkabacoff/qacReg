#' @title Auto MPG Data Set
#'
#' @description
#' City-cyle fuel consumption in miles per gallon, predicted from
#' 3 multivalued discrete variables and 5 continuous variables.
#'
#' @note
#' Missing values have been removed from the original data set.
#'
#' @source
#' \href{https://archive.ics.uci.edu/ml/datasets/auto+mpg}{ICU Machine Learning Repository}. This dataset was taken from the
#' StatLib library which is maintained at Carnegie Mellon University. The dataset was used in the
#' 1983 American Statistical Association Exposition.
#'
#' @format A data frame with 392 rows and 9 variables:
#' \describe{
#'   \item{\code{carname}}{character; car name.}
#'   \item{\code{mpg}}{numeric; miles per gallon.}
#'   \item{\code{cyl}}{factor; number of cylinders.}
#'   \item{\code{disp}}{numeric; engine displacement.}
#'   \item{\code{hp}}{numeric; horse power.}
#'   \item{\code{wt}}{numeric; weight.}
#'   \item{\code{accel}}{numeric; acceleration.}
#'   \item{\code{year}}{factor; make year.}
#'   \item{\code{origin}}{factor; origin.}
#' }
"auto_mpg"
