utils::globalVariables(c("Bi", "Bi1", "Bi2", "E", "Environment", "Genotype", "Mean.Yield", "Mj", "X", "Xi.bar", "Xj.bar", "Xj.max", "corrected.X", "corrected.rank", "dev", "deviation", "mean.rank", "s2d1", "s2d2", "s2di", "s2xi", "sqr", "sqr1", "wi"))
#' @title Coefficient of regression
#'
#' @description
#' \code{coefficient_of_regression} calculate variance of a genotype across environments.
#'
#' @keywords regression approach
#'
#' @details
#' Coefficient of regression (Finlay and Wilkinson, 1963) is calculatd based on regression function.
#' Variety with low coefficient of regression is considered as stable.
#' Under the linear model
#' \deqn{Y =\mu + b_{i}e_{j} + g_{i} + d_{ij}}
#' where Y is the predicted phenotypic values, \eqn{g_{i}}, \eqn{e_{j}} and \eqn{\mu} denoting
#' genotypic, environmental and overall population mean,respectively.
#'
#' The effect of GE-interaction may be expressed as:
#' \deqn{(ge)_{ij} = b_{i}e_{j} + d_{ij}}
#' where \eqn{b_{i}} is the coefficient of regression and \eqn{d_{ij}} a deviation.
#'
#' Coefficient of regression may be expressed as:
#' \deqn{ b_{i}=1 + \frac{\sum_{j} (X_{ij} -\bar{X_{i.}}-\bar{X_{.j}}+\bar{X_{..}})\cdot
#' (\bar{X_{.j}}- \bar{X_{..}})}{\sum_{j}(\bar{X_{.j}}-\bar{X_{..}})^{2}}}
#'
#' where \eqn{X_{ij}} is the observed phenotypic mean value of genotype i(i=1,..., G)
#' in environment j(j=1,...,E), with \eqn{\bar{X_{i.}}} and  \eqn{\bar{X_{.j}}} \cr
#' denoting marginal means of genotype i and environment j,respectively. \cr
#' \eqn{\bar{X_{..}}} denote the overall mean of X.
#'
#'
#' @param data a dataframe containing trait, genotype and environment.
#' @param trait colname of a column containing a numeric vector of interested trait to be analysized.
#' @param genotype colname of a column containing a character or factor vector labeling different genotypic varieties
#' @param environment colname of a column containing a character or factor vector labeling different environments
#'
#' @return a data table with coefficient of regression
#'
#' @author Tien Cheng Wang
#'
#' @references
#' \insertRef{finlay1963}{toolStability}
#'
#' @importFrom dplyr group_by summarise mutate
#' @importFrom data.table data.table
#' @importFrom Rdpack reprompt
#'
#' @export
#'
#' @examples
#' data(Data)
#' coefficient.of.regression <- coefficient_of_regression(Data, "Yield", "Genotype", "Environment")
coefficient_of_regression <- function(data, trait, genotype, environment) {
  # combine vectors into data table
  if (length(environment) == 1){
    Data <- data.table(X =  data[[trait]] ,
                       Genotype = data[[genotype]],
                       Environment = data[[environment]])

  }else { # if input is the vector containing the name that are going to combine in one column
    data$Environment <- interaction(data[,environment],sep = '_')

    Data <- data.table(X = data[[trait]] ,
                       Genotype = data[[genotype]],
                       Environment = data[['Environment']])
  }
  varnam <- paste0("Mean.",trait)
  X..bar <- mean(Data$X) # overall mean of X
  res <- summarise(
    mutate(
      group_by(
        mutate(
          group_by(Data, Environment), # for each environment
          Xj.bar = mean(X)
        ), # first calculate environmental mean
        Genotype
      ), # for each genotype
      Xi.bar = mean(X), # then calculate genotypic mean
      Bi1 = (X - Xi.bar - Xj.bar + X..bar) * (Xj.bar - X..bar),
      Bi2 = (Xj.bar - X..bar)^2
    ),
    coefficient.of.regression = 1 + sum(Bi1, na.rm = TRUE) / sum(Bi2, na.rm = TRUE),
    !!varnam := mean(X)
  )

  return(res[, c("Genotype", varnam,"coefficient.of.regression")])
}
