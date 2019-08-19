utils::globalVariables(c('Bi','Bi1','Bi2','E','Environment','Genotype','Mean.Yield','Mj','X','Xi.bar','Xj.bar','Xj.max','corrected.X','corrected.rank','dev','deviation','mean.rank','s2d1','s2d2','s2di','s2xi','sqr','sqr1','wi'))
#' @title Deviation mean squares
#'
#' @description
#' \code{deviation_mean_squares} calculate variance of a genotype across environments.
#'
#' @keywords regression approach
#'
#' @details
#' deviation mean squares is calculatd based on regression function.
#' Variety with low stability variance is considered as stable.
#' Under the linear model
#' \deqn{Y =\mu + b_{i}e_{j} + g_{i} + d_{ij}}
#' where Y is the predicted phenotypic values, \eqn{g_{i}}, \eqn{e_{j}} and \eqn{\mu} denoting
#' genotypic, environmental and overall popluation mean,respectively.
#'
#' The effect of GE-interaction may be expressed as:
#' \deqn{(ge)_{ij} = b_{i}e_{j} + d_{ij}}
#' where \eqn{b_{i}} is the coefficient of regression and \eqn{d_{ij}} a deviation.
#'
#' Deviation mean squares(\link(\code(deviation_mean_squares))):
#' \deqn{ s^{2}_{di} = \frac{1}{E-2} \big[
#'  sum_{j}(X_{ij} -bar(X_{i.})- bar(X_{.j})+bar(X_{..})^{2} - (b_{i} - 1)^{2})
#'  (bar(X_{.j}-bar(X_{..})))^{2}} \big]
#'
#' where \eqn{X_{ij}} is the observed phenotypic mean value of genotype i(i=1,..., G)
#' in environment j(j=1,...,E), with \eqn{bar(X)_{i.}} and  \eqn{bar(X)_{.j}} \cr
#' denoting marginal means of genotype i and environemnt j,respectively. \cr
#' \eqn{bar(X)_{..}} denote the overall mean of X.
#'
#'
#' @param data a dataframe containing trait, genotype and environment.
#' @param trait colname of a column containing a numeric vector of interested trait to be analysized.
#' @param genotype colname of a column containing a character or factor vector labeling different genotypic varieties
#' @param environment colname of a column containing a character or factor vector labeling different environments
#'
#' @return a data table with deviation mean squares
#'
#' @author Tien Cheng Wang
#'
#' @references
#' \insertRef{eberhart1966}{toolStability}
#'
#' @importFrom dplyr group_by summarise mutate
#' @importFrom data.table data.table
#' @importFrom Rdpack reprompt
#'
#' @export
#'
#' @examples
#' data(Data)
#' deviation.mean.squares <- deviation_mean_squares(Data,'Yield','Genotype','Environment')
#'
deviation_mean_squares <- function(data,trait,genotype,environment){
  if(!is.numeric(data[[trait]])){stop('Trait must be a numeric vector')}
  # combine vectors into data table
  Data <- data.table(X=data[[trait]],Genotype=data[[genotype]],Environment=data[[environment]])
  X..bar=mean(Data$X)               # overall mean of X

  X..bar=mean(trait)               # overall mean of X
  res <- summarise(
    mutate(
      group_by(
        ungroup(
          mutate(
            group_by(Data,Environment),          # for each environment
            Xj.bar=mean(X))),                    # first calculate environmental mean
        Genotype),                               # for each genotype
      Xi.bar=mean(X),                            # then calculate genotypic mean
      E=length(X),                               # number of environment
      s2d1=((X-Xi.bar-Xj.bar+X..bar)^2)/(E-2),
      s2d2=((Xj.bar-X..bar)^2)/(E-2),
      Bi1=(X-Xi.bar-Xj.bar+X..bar)*(Xj.bar-X..bar),
      Bi2=(Xj.bar-X..bar)^2),
    Bi=1+(sum(Bi1,na.rm=TRUE)/sum(Bi2,na.rm=TRUE)),
    deviation.mean.squares=sum(s2d1,na.rm=TRUE)-((Bi-1)^2)*sum(s2d2,na.rm=TRUE))

  return(res[ ,c("Genotype","deviation.mean.squares")] )
}
