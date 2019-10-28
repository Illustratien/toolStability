#' @title Wheat APSIM model simulated database
#'
#' @keywords datasets
#'
#' @docType data
#'
#' @usage data(Data)
#'
#' @description Multi-environment trail evaluating 5 genotypes in 4 locations for 4 years, with 2 nitrogen application rates, 2 sowing dates, and 2 CO2 levels of treatments (Casadebaig et al., 2016).
#'
#' @format A dataframe with 640 observations on the following 8 variables.
#' \itemize{
#' \item{\code{Yield}}{       unit: kg*ha^-1.}
#' \item{\code{Genotype}}{    genotypes, 5 varieties.}
#' \item{\code{Environment}}{ 128 unique combination of environments for each genotype.}
#' \item{\code{Year}}{        4 years.}
#' \item{\code{Sites}}{      4 locations.}
#' \item{\code{Nitrogen}}{    2 nitrogen application levels.}
#' \item{\code{CO2}}{         2 CO2 concentration levels.}
#' \item{\code{Sowing}}{      2 sowing dates.}
#'  }
#'
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{casadebaig2016}{toolStability}
#'
#'
#' @examples
#' data(Data)
#' \donttest{
#' boxplot(Yield ~ Genotype,
#'   data = Data,
#'   col = "lightblue", border = "darkblue"
#' )
#' }
"Data"
