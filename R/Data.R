#' @title Wheat APSIM model simulated database
#'
#' @keywords datasets
#'
#' @docType data
#'
#' @usage data(Data)
#'
#' @description Multi-environment trail evaluating 5 genotypes in 4 locations of 11 years (Casadebaig et al., 2016).
#'
#' @format A dataframe with 220 observations on the following 3 variables.
#' \itemize{
#' \item{\code{Yield}}{kg ha^{-1} }
#' \item{\code{Genotype}}{genotype, 5 varieties}
#' \item{\code{Environment}}{environment, 44 environments}
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
