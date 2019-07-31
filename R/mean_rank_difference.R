#' @title Mean rank difference
#'
#' @description
#' \code{mean_rank_difference} calculate variance of a genotype across environments.
#'
#' @keywords nonparametric approach
#'
#' @details
#' Mean rank difference is calculatd based on regression function.
#' Variety with low mean rank difference is considered as stable.
#'
#' \deqn{S_{i}1 = \frac{\sum_{j} |r_{ij}-bar(r)_{i.}|}{E*(E-1)/2}}
#' where \eqn{r_{ij}} is the rank of genotype i in environment j , based on the corrected \eqn{X_{ij}} values.
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
#' \insertRef{nassar1987}{toolStability}
#'
#' @importFrom dplyr group_by summarise mutate
#' @importFrom data.table data.table
#' @importFrom gtools combinations
#' @importFrom Rdpack reprompt
#'
#' @export
#'
#' @examples
#' data(Data)
#' mean.rank.difference<- mean_rank_difference(Data$Yield,Data$Genotype,Data$Environment)
#'
mean_rank_difference <- function(trait,genotype,environment){
  if(!is.numeric(trait)){stop('Trait must be a numeric vector')}
  abs.dev.sum <- function(x){
    res <- c()
    n <- length(x)
    for (i in seq(1,n-1)){
      res <- sum(res,abs(x[(i+1):n]-x[i]))
    }
    res <- res*2/(n*(n-1))
    return(res)
  }
  # combine vectors into data table
  Data <- data.table(X=trait,Genotype=genotype,Environment=environment)
  res <- summarise(
    group_by(
      ungroup(
        mutate(
          group_by(
            ungroup(
              mutate(
                group_by(Data,Genotype),
                corrected.X=X-mean(X))),
            Environment),
          corrected.rank=rank(-corrected.X,na.last="keep", ties.method="min"))),
      Genotype),
    mean.rank.difference= abs.dev.sum(corrected.rank))
  return(res[ ,c("Genotype","mean.rank.difference")] )
}
