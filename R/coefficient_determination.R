#' @title Coefficient of determination
#'
#' @description
#' \code{coefficient_determination} calculate variance of a genotype across environments.
#'
#' @keywords regression approach
#'
#' @details
#' coefficient of determination is calculatd based on regression function.
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
#' Environmental variance (\link(\code(environmental_variance)))can be expressed as :
#' \deqn{S^{2}_{xi} = \frac{\sum_{j}(X_{ij} - \bar(X)_{i.})^2}{E-1} }
#' 
#' Coefficient of determination may be expressed as:
#' \deqn{ r^{2}_{i} = 1 - \frac{s^{2}_{di}}{s^{2}_{xi}}}
#' 
#' where \eqn{X_{ij}} is the observed phenotypic mean value of genotype i(i=1,..., G)
#' in environment j(j=1,...,E), with \eqn{bar(X)_{i.}} and  \eqn{bar(X)_{.j}} \cr
#' denoting marginal means of genotype i and environemnt j,respectively. \cr
#' \eqn{bar(X)_{..}} denote the overall mean of X.
#'
#'
#' @param trait numeric vector of interested trait to be analysized.
#' @param genotype a character or factor vector labeling different genotypic varieties
#' @param environment a character or factor vector labeling different environments
#'
#' @return a data table with environmental variance indices
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
#' coef.determination <- coefficient_determination(Data$Yield,Data$Genotype,Data$Environment)
#'
coefficient_determination <- function(trait,genotype,environment){
  if(!is.numeric(trait)){stop('Trait must be a numeric vector')}
  
  # combine vectors into data table
  Data <- data.table(X=trait,Genotype=genotype,Environment=environment)
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
      Bi2=(Xj.bar-X..bar)^2,
      dev=((X-Xi.bar)^2)/(E-1)),
    Bi=1+(sum(Bi1,na.rm=TRUE)/sum(Bi2,na.rm=TRUE)),
    s2di=sum(s2d1,na.rm=TRUE)-((Bi-1)^2)*sum(s2d2,na.rm=TRUE),
    s2xi=sum(dev,na.rm=TRUE),
    coefficient.determination=1-(s2di/s2xi))
  
  return(res[ ,c("Genotype","coefficient.determination")] )
}
