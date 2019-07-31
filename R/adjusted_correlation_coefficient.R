#' @title Adjusted correlation coefficient
#'
#' @description
#' \code{adjusted_correlation_coefficient} calculate variance of a genotype across environments.
#'
#' @keywords regression approach
#'
#' @details
#' Adjusted correlation coefficient is calculatd based on regression function.
#' Variety with low adjusted correlation coefficient is considered as stable.
#' Under the linear model
#'
#' \deqn{\Sigma^{2} = A\mu^{b}}
#' \deqn{v_{i} = a + bm_{i}}
#' where \eqn{v_{i}} is the \eqn{log_{10}} of penotypic variance and \eqn{m_{i}} is the \eqn{log_{10}}  of penotypic mean.
#' \deqn{\~{c_{i} = \frac{1}{\~{\mu_{i}}} \big[ 10^{(2-b)m_{i} + (b-2)bar(m) + v_{i}}\big]^{0.5}*100%}
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
#' \insertRef{doering2018}{toolStability}
#'
#' @importFrom dplyr group_by summarise mutate
#' @importFrom data.table data.table
#' @importFrom Rdpack reprompt
#'
#' @export
#'
#' @examples
#' data(Data)
#' adjusted.correlation.coefficient <- adjusted_correlation_coefficient(Data$Yield,Data$Genotype,Data$Environment)
#'
adjusted_correlation_coefficient <- function(trait,genotype,environment){
  if(!is.numeric(trait)){stop('Trait must be a numeric vector')}

  # combine vectors into data table
  Data <- data.table(X=trait,Genotype=genotype,Environment=environment)
    res <- summarise(
      group_by(
        ungroup(
          mutate(
            group_by(x,Environment),                # for each environment
            Xj.bar=mean(X,na.rm = TRUE))),          # first calculate environmental mean
        Genotype),                                  # for each genotype
      Xi.bar=mean(X,na.rm=TRUE),                    # then calculate genotypic mean
      Xi.logvar=log10(var(X,na.rm = TRUE)),
      Xi.logmean=log10(mean(X,na.rm=TRUE)))
    b= sum((res$Xi.logmean-mean(res$Xi.logmean))*(res$Xi.logvar-mean(res$Xi.logvar)))/sum((res$Xi.logmean-mean(res$Xi.logmean))^2)

res$adjusted.coefficient.correlation=100*(1/res$Xi.bar)*sqrt(10^(((2-b)*res$Xi.logmean)+((b-2)*(mean(res$Xi.logmean)))+res$Xi.logvar))

  return(res[ ,c("Genotype","adjusted.coefficient.correlation")] )
}
