#' @title Safety-first Index
#'
#' @description
#' \code{safety_first_index} calculate variance of a genotype across environments.
#'
#' @keywords probabilistic approach
#'
#' @details
#' Safety-first index (Eskridge, 1990) is calculated based on the assumption of
#' that the trait from each genotype follows normal distribution over enviornments.
#' Among different environments, trait below a given cirtical level \eqn{\lambda}
#' is defined as failure of trait. The probability of trait failure can be obtained
#' by entering mean and variance of trait and \eqn{\lambda}
#' into the cumulated density function of normal distribution.
#' Variety with low safety first index is considered as stable.
#' Equation of adjusted coefficient of variation can be found in vignette file.
#'
#' @return a data table with coefficient of determination
#'
#' @param data a dataframe containing trait, genotype and environment.
#' @param trait colname of a column containing a numeric vector of interested trait to be analyzed.
#' @param genotype colname of a column containing a character or factor vector labeling different genotypic varieties.
#' @param environment colname of a column containing a character or factor vector labeling different environments.
#' @param lambda the minimal acceptable value of trait that the user expected from crop across environments. Lambda should between the ranges of trait vlaue.
#'
#' @author Tien-Cheng Wang
#'
#' @references
#' \insertRef{eskridge1990}{toolStability}
#'
#' @importFrom dplyr group_by summarise mutate
#' @importFrom data.table data.table
#' @importFrom Rdpack reprompt
#' @importFrom stats pnorm sd median shapiro.test
#' @importFrom nortest ad.test
#'
#' @export
#'
#' @examples
#' data(Data)
#' safety.first.index <- safety_first_index(Data, "Yield", "Genotype", "Environment", median(Data$Yield))
safety_first_index <- function(data, trait, genotype, environment, lambda) {
  if (!is.numeric(data[[trait]])) {
    stop("Trait must be a numeric vector")
  }
  if (!is.numeric(lambda)) {
    stop("Lambda must be numeric!")
  }
  if (lambda > range(data[[trait]])[2] | lambda < range(data[[trait]])[1]) {
    stop("Lambda must in the range of trait")
  }
  if (missing(lambda)) {
    lambda <- median(data$trait)
    message(sprintf("lambda = %d (medain of %s) is used for Safty first index!", lambda, trait))
  }
  sample_number <-length(unique(data[[environment]]))
  if (sample_number<3) {
    stop("Environment number must above 3")
  }
  if (sample_number <= 5000 & sample_number >= 3) {
    normtest <- function(x){
      return(shapiro.test(x)$p.value > 0.05)
    }
  } else if (sample_number > 5000) {
    normtest <- function(x){
      return(ad.test(x)$p.value > 0.05)
    }
  }

  # combine vectors into data table
  Data <- data.table(X = data[[trait]], Genotype = data[[genotype]], Environment = data[[environment]])
  # calculate doefficient determination
  res <- summarise(
    group_by(Data, Genotype), # for each environment
    Normality = normtest(X), # test normality for each genotype
    safety.first.index = pnorm((lambda - mean(X)) / sd(X))
  )
  if (any(!res$Normality)) {
    warning("Input trait is not completely follow normality assumption !
 please see Normality column for more information.")
  }

  return(res[, c("Genotype", "Normality", "safety.first.index")])
}
