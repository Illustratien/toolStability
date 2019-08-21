utils::globalVariables(c("Bi", "Bi1", "Bi2", "E", "Environment", "Genotype", "Mean.Yield", "Mj", "X", "Xi.bar", "Xj.bar", "Xj.max", "corrected.X", "corrected.rank", "dev", "deviation", "mean.rank", "s2d1", "s2d2", "s2di", "s2xi", "sqr", "sqr1", "wi"))
#' @title Adjusted coefficient of variaiton
#'
#' @description
#' \code{adjusted_coefficient_of_variation} calculate variance of a genotype across environments.
#'
#' @keywords regression approach
#'
#' @details
#' Adjusted coefficient of variaiton (Döring and Reckling, 2018) is calculatd based on regression function.
#' Variety with low adjusted coefficient of variation is considered as stable.
#' Under the linear model
#'
#' \deqn{v_{i} = a + bm_{i}}
#' where \eqn{v_{i}} is the \eqn{log_{10}} of penotypic variance and
#' \eqn{m_{i}} is the \eqn{log_{10}} of penotypic mean.
#' \deqn{\widetilde{c_{i}} = \frac{1}{\widetilde{\mu_{i}}}  \left [
#' 10^{(2-b)\cdot
#' m_{i} + (b-2)\cdot
#' \bar{m} + v_{i}}
#' \right ]
#' ^{0.5} \cdot
#' 100 \%}
#'
#' @param data a dataframe containing trait, genotype and environment.
#' @param trait colname of a column containing a numeric vector of interested trait to be analysized.
#' @param genotype colname of a column containing a character or factor vector labeling different genotypic varieties
#' @param environment colname of a column containing a character or factor vector labeling different environments
#'
#' @return a data table with adjusted coefficient of variation
#'
#' @author Tien Cheng Wang
#'
#' @references
#' \insertRef{doering2018}{toolStability}
#'
#' @importFrom dplyr group_by summarise mutate
#' @importFrom data.table data.table
#' @importFrom Rdpack reprompt
#' @importFrom stats var
#'
#' @export
#'
#' @examples
#' data(Data)
#' res <- adjusted_coefficient_of_variation(Data, "Yield", "Genotype", "Environment")
adjusted_coefficient_of_variation <- function(data, trait, genotype, environment) {
  # combine vectors into data table
  Data <- data.table(X = data[[trait]], Genotype = data[[genotype]], Environment = data[[environment]])
  res <- summarise(
    group_by(
      mutate(
        group_by(Data, Environment), # for each environment
        Xj.bar = mean(X, na.rm = TRUE)
      ), # first calculate environmental mean
      Genotype
    ), # for each genotype
    Xi.bar = mean(X, na.rm = TRUE), # then calculate genotypic mean
    Xi.logvar = log10(var(X, na.rm = TRUE)),
    Xi.logmean = log10(mean(X, na.rm = TRUE))
  )
  b <- sum((res$Xi.logmean - mean(res$Xi.logmean)) * (res$Xi.logvar - mean(res$Xi.logvar))) / sum((res$Xi.logmean - mean(res$Xi.logmean))^2)

  res$adjusted.coefficient.of.variation <- 100 * (1 / res$Xi.bar) * sqrt(10^(((2 - b) * res$Xi.logmean) + ((b - 2) * (mean(res$Xi.logmean))) + res$Xi.logvar))

  return(res[, c("Genotype", "adjusted.coefficient.of.variation")])
}
