#' Calculate age
#'
#' By default, calculates the typical "age in years", with a
#' floor applied so that you are, e.g., 5 years old from
#' 5th birthday through the day before your 6th birthday. Set
#' \code{floor = FALSE} to return decimal ages, and change units
#' for units other than years.
#' @param dob date-of-birth, the day to start calculating age.
#' @param ageDay the date on which age is to be calculated.
#' @param units unit to measure age in. Defaults to "years".
#'  Passed to duration.
#' @param floor boolean for whether or not to floor the result.
#' Defaults to TRUE.
#' @return Age in units. Will be an integer if floor = TRUE.
#' @examples
#' mydob <- as.Date('1983-10-20')
#' age(mydob)
#' age(mydob, units = "minutes")
#' age(mydob, floor = FALSE)
#'
#' @export
#'
age <- function(
  dob,  ageDay = Sys.Date(),  units = "years",  floor = TRUE
) {

  calcAge <-  lubridate::interval(dob, ageDay) /
    lubridate::duration(num = 1, units = units)
  if (floor) return(as.integer(floor(calcAge)))
  return(calcAge)
}
