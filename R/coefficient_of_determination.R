utils::globalVariables(c('Bi','Bi1','Bi2','E','Environment','Genotype','Mean.Yield','Mj','X','Xi.bar','Xj.bar','Xj.max','corrected.X','corrected.rank','dev','deviation','mean.rank','s2d1','s2d2','s2di','s2xi','sqr','sqr1','wi'))
#' @title Coefficient of determination
#'
#' @description
#' \code{coefficient_of_determination} calculate variance of a genotype across environments.
#'
#' @keywords regression approach
#'
#' @details
#' Coefficient of determination (Pinthus, 1976) is calculatd based on regression function.
#' Variety with low coefficient of determination is considered as stable.
#' Under the linear model
#' \deqn{Y =\mu + b_{i}\cdot e_{j} + g_{i} + d_{ij}}
#' where Y is the predicted phenotypic values, \eqn{g_{i}}, \eqn{e_{j}} and \eqn{\mu} denoting
#' genotypic, environmental and overall popluation mean,respectively. \eqn{b_{i}} is the coefficient of regression and \eqn{d_{ij}} denotes deviation.
#'
#' The effect of GE-interaction may be expressed as:
#' \deqn{(ge)_{ij} = b_{i}e_{j} + d_{ij}}
#'
#' Deviation mean squares(\code{\link{deviation_mean_squares}}):
#' \deqn{
#' s^{2}_{di} = \frac{1}{E-2} \left [
#' \sum_{j}(X_{ij} -\bar{X_{i.}}- \bar{X_{.j}}+\bar{X_{..}}^{2} - (b_{i} - 1)^{2}) \cdot
#' \sum_{j}(\bar{X_{.j}}-\bar{X_{..}})^{2} \right ]
#' }
#'
#' Environmental variance (\code{\link{environmental_variance}})can be expressed as :
#' \deqn{S^{2}_{xi} = \frac{\sum_{j} (X_{ij} - \bar{X_{i.}})^{2}}{E-1} }
#'
#' Coefficient of determination may be expressed as:
#' \deqn{r^{2}_{i} = 1 - \frac{s_{di}^{2}}{s_{xi}^{2}} }
#'
#' where \eqn{X_{ij}} is the observed phenotypic mean value of genotype i(i=1,..., G)
#' in environment j(j=1,...,E), with \eqn{\bar{X_{i.}}} and  \eqn{\bar{X_{.j}}} \cr
#' denoting marginal means of genotype i and environemnt j,respectively. \cr
#' \eqn{\bar{X_{..}}} denote the overall mean of X.
#'
#' @param data a dataframe containing trait, genotype and environment.
#' @param trait colname of a column containing a numeric vector of interested trait to be analysized.
#' @param genotype colname of a column containing a character or factor vector labeling different genotypic varieties
#' @param environment colname of a column containing a character or factor vector labeling different environments
#'
#' @return a data table with coefficient of determination
#'
#' @author Tien Cheng Wang
#'
#' @references
#' \insertRef{pinthus1973}{toolStability}
#'
#' @importFrom dplyr group_by summarise mutate
#' @importFrom data.table data.table
#' @importFrom Rdpack reprompt
#'
#' @export
#'
#' @examples
#' data(Data)
#' coef.of.determination <- coefficient_of_determination(Data,'Yield','Genotype','Environment')
#'
coefficient_of_determination <- function(data,trait,genotype,environment){
  # combine vectors into data table
  Data <- data.table(X=data[[trait]],Genotype=data[[genotype]],Environment=data[[environment]])
  # overall mean of X
  X..bar=mean(Data$X)
  # calculate doefficient determination
  res <- summarise(
    mutate(
      group_by(
        mutate(
          group_by(Data,Environment),          # for each environment
          Xj.bar=mean(X)),                    # first calculate environmental mean
        Genotype),                               # for each genotype
      Xi.bar=mean(X),                            # then calculate genotypic mean
      E=length(X),                               # number of environment
      s2d1=((X-Xi.bar-Xj.bar+X..bar)^2)/(E-2),
      s2d2=((Xj.bar-X..bar)^2)/(E-2),
      Bi1=(X-Xi.bar-Xj.bar+X..bar)*(Xj.bar-X..bar),
      Bi2=(Xj.bar-X..bar)^2,
      dev=((X-Xi.bar)^2)/(E-1)),
    Bi=1+(sum(Bi1,na.rm=TRUE)/sum(Bi2,na.rm=TRUE)),
    s2di=sum(s2d1,na.rm=TRUE)-((Bi-1)^2)*sum(s2d2,na.rm=TRUE),
    s2xi=sum(dev,na.rm=TRUE),
    coefficient.of.determination=1-(s2di/s2xi))    # final product

  return(res[ ,c("Genotype","coefficient.of.determination")] )
}
