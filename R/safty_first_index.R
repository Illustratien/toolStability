#' @title Safty-first Index
#'
#' @description
#' \code{safty_first_index} calculate variance of a genotype across environments.
#'
#' @keywords probabilistic approach
#'
#' @details
#' Safty-first index (Eskridge, 1990) is calculatd based on the normality assumption of trait over the enviornments.
#' Among different environments, trait below a given cirtical level \eqn{\lambda} is defined as failure of trait.
#' Safty-first index calculating the probability of trait failure over the environment.Variety with low safty first index is considered as stable.
#'
#' \deqn{Pr(Y_{ij} < \lambda) = \Phi \left[
#' (\lambda - \mu_{i})/ \sqrt \Sigma_{ii}
#' \right]}
#'
#' where \eqn{\lambda} is the critical subsistence level of trait.
#' \eqn{\Phi} is the cumulative distribution function of the standard normal distribution. \eqn{\mu_{i}} and \eqn{\sigma_{ii}}
#' is the mean and varance of the system i.
#'
#' @param data a dataframe containing trait, genotype and environment.
#' @param trait colname of a column containing a numeric vector of interested trait to be analysized.
#' @param genotype colname of a column containing a character or factor vector labeling different genotypic varieties
#' @param environment colname of a column containing a character or factor vector labeling different environments
#' @param lambda threshold value of trait that define stability.
#' @return a data table with coefficient of determination
#'
#' @author Tien Cheng Wang
#'
#' @references
#' \insertRef{eskridge1990}{toolStability}
#'
#' @importFrom dplyr group_by summarise mutate
#' @importFrom data.table data.table
#' @importFrom Rdpack reprompt
#' @importFrom stats pnorm sd shapiro.test median
#'
#' @export
#'
#' @examples
#' data(Data)
#' safty.first.index <- safty_first_index(Data,'Yield','Genotype','Environment',median(Data$Yield))
#'
safty_first_index <- function(data,trait,genotype,environment,lambda){
  if(!is.numeric(data[[trait]])){stop('Trait must be a numeric vector')}
  if(!is.numeric(lambda)){stop('Lambda must be numeric!')}
  if(lambda>range(data[[trait]])[2] | lambda<range(data[[trait]])[1]){stop('Lambda must in the range of trait')}
  if(missing(lambda)){
    lambda=median(data$trait)
    message(sprintf('lambda = %d (medain of %s) is used for Safty first index!',lambda,trait))
  }

  # combine vectors into data table
  Data <- data.table(X=data[[trait]],Genotype=data[[genotype]],Environment=data[[environment]])
  # calculate doefficient determination
  res <- summarise(
    group_by(Data,Genotype),                             # for each environment
    Normality=shapiro.test(X)$p.value>=0.05,             # test normality for each genotype
    safty.first.index= pnorm((lambda-mean(X))/sd(X)))
  if(any(!res$Normality)){warning('Input trait is not completely follow normality assumption ! \n please see Normality column for more information.')}

  return(res[ ,c("Genotype",'Normality',"safty.first.index")] )

}
