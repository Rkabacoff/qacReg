#' @title bikes
#'
#' @description
#' Seoul bike sharing demand data set
#'
#' @format A data frame with 8760 rows and 17 variables:
#' \describe{
#'   \item{\code{date}}{double. Date of rental.}
#'   \item{\code{count}}{integer. Count of bikes rented at that hour.}
#'   \item{\code{hour}}{integer. Hour of the day.}
#'   \item{\code{temp}}{double. Temperature in Celsius.}
#'   \item{\code{humidity}}{integer. Humidity in %.}
#'   \item{\code{wind}}{double. Windspeed m/s.}
#'   \item{\code{visibility}}{integer. Visibility 10m.}
#'   \item{\code{dew}}{double. Dew point tempurature in Celsius.}
#'   \item{\code{solar}}{double. Solar radiation in MJ/m2.}
#'   \item{\code{rainfall}}{double. Rainfall in mm.}
#'   \item{\code{snowfall}}{double. Snowfall in cm.}
#'   \item{\code{seasons}}{character. Seasons - Winter, Spring Summer, Autumn.}
#'   \item{\code{holiday}}{character. Holiday/No Holiday.}
#'   \item{\code{funcday}}{character. Functional Day - Yes/No.}
#'   \item{\code{weekday}}{integer. Weekday (Mon, Tue, ...).}
#'   \item{\code{year}}{double. Year.}
#'   \item{\code{month}}{integer. Month (Jan, Feb, ...).}
#' }
"bikes"
