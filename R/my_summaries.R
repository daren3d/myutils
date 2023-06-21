#' Summaries for numeric vectors
#'
#' @param x vector.
#' @param na.rm passed into other functions.
#'
#' @return a numeric vector with min, max, mean, sd, length and number of
#'  missing values.
#' @export
#'
#' @examples
#' nvec <- c(2022, 2021, NA, 2021, 2021, NA, 2022)
#' numeric_summary(nvec, na.rm = TRUE)
numeric_summary <- function(x, na.rm = FALSE){

  min <- min(x, na.rm = na.rm)
  max <- max(x, na.rm = na.rm)
  mean <- mean(x, na.rm = na.rm)
  sd <- sd(x, na.rm = na.rm)
  length <- length(x)
  Nmiss <- sum(is.na(x))

  c(min = min, max = max, mean = mean, sd = sd, length = length, Nmiss = Nmiss)
}

#' Summaries for character vectors
#'
#' @param x vector.
#'
#' @return a numeric vector of length, number of missing values and number of
#'  unique values.
#' @export
#'
#' @examples
#' cvec <- c("Boston", NA, "Brookline", "Brighton", NA, "Boston")
#' char_summary(cvec)
char_summary <- function(x){

  length <- length(x)
  Nmiss <- sum(is.na(x))
  Nunique <- length(unique(x))

  c(length = length, Nmiss = Nmiss, Nunique = Nunique)
}
