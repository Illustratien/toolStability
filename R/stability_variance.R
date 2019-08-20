utils::globalVariables(c('Bi','Bi1','Bi2','E','Environment','Genotype','Mean.Yield','Mj','X','Xi.bar','Xj.bar','Xj.max','corrected.X','corrected.rank','dev','deviation','mean.rank','s2d1','s2d2','s2di','s2xi','sqr','sqr1','wi'))
#' @title Stability variance
#'
#' @description
#' \code{stability_variance} calculate variance of a genotype across environments.
#'
#' @keywords dynamic stability
#'
#' @details
#' Stability variance (Shukla, 1972) is calculatd based on lindear combination of ecovalence and mean square of genotype-environment interaction.
#' Variety with low stability variance is considered as stable.
#'
#' \deqn{
#' \Sigma^{2}_{i} = \frac{1}{(G-1)\cdot(G-2)\cdot(E-1)} \left [
#' G \cdot(G-1)\cdot \sum_{j} (X_{ij} - \bar{X_{i.}}_\bar{X_{.j}} + \bar{X_{..}})^{2}- \sum_{i} \sum_{j} (X_{ij} - \bar{X_{i.}}_\bar{X_{.j}} + \bar{X_{..}})^{2}
#' \right ]
#' }
#' where \eqn{X_{ij}} is the observed phenotypic mean value of genotype i(i=1,..., G)
#' in environment j(j=1,...,E), with \eqn{\bar{X_{i.}}} and  \eqn{\bar{X_{.j}}} \cr
#' denoting marginal means of genotype i and environemnt j,respectively. \cr
#' \eqn{\bar{X_{..}}} denote the overall mean of X.
#'
#' Negative values of stability variance is replaced with 0.
#'
#' @param data a dataframe containing trait, genotype and environment.
#' @param trait colname of a column containing a numeric vector of interested trait to be analysized.
#' @param genotype colname of a column containing a character or factor vector labeling different genotypic varieties
#' @param environment colname of a column containing a character or factor vector labeling different environments
#'
#' @return a data table with stability variance
#'
#' @author Tien Cheng Wang
#'
#' @references
#' \insertRef{shukla1972}{toolStability}
#'
#' @importFrom dplyr group_by summarise mutate
#' @importFrom data.table data.table
#' @importFrom Rdpack reprompt
#'
#' @export
#'
#' @examples
#' data(Data)
#' stability.variance <- stability_variance(Data,'Yield','Genotype','Environment')
#'
stability_variance <- function(data,trait,genotype,environment){
  if(!is.numeric(data[[trait]])){stop('Trait must be a numeric vector')}
  # combine vectors into data table
  Data <- data.table(X=data[[trait]],Genotype=data[[genotype]],Environment=data[[environment]])

  X..bar <- mean(data[[trait]])
  G <- length(unique(data[[genotype]]))

  res <- mutate(
    group_by(
      mutate(
        group_by(Data,Environment),            # for each environment
        Xj.bar=mean(X)),                       # first calculate environmental mean
      Genotype),                               # for each genotype
    Xi.bar=mean(X),                            # then calculate genotypic mean
    sqr=((X-Xi.bar-Xj.bar+X..bar)^2)/(length(X)-1))
  wisum <- sum(res$sqr)
  res <- summarise(res,
                   wi= sum(sqr, na.rm=TRUE),
                   stability.variance=(G*(G-1)*wi-wisum)/((G-1)*(G-2)))
  # replace negative value to zero as stated by Shukla, 1972.
  res$stability.variance[res$stability.variance<0] <- 0

  return(res[ ,c("Genotype","stability.variance")] )
}
