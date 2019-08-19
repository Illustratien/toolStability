utils::globalVariables(c('Bi','Bi1','Bi2','E','Environment','Genotype','Mean.Yield','Mj','X','Xi.bar','Xj.bar','Xj.max','corrected.X','corrected.rank','dev','deviation','mean.rank','s2d1','s2d2','s2di','s2xi','sqr','sqr1','wi'))
#' @title Mean rank difference
#'
#' @description
#' \code{mean_rank_difference} calculate variance of a genotype across environments.
#'
#' @keywords nonparametric approach
#'
#' @details
#' Mean rank difference (Nassar and Hühn, 1987) is calculatd based on regression function.
#' Variety with low mean rank difference is considered as stable.
#'
#' Correction for each genotype i was done by subtraction of marginal genotypic mean \eqn{\bar{X_{i.}}} and the addition of overall mean\eqn{\bar{X_{..}}}.
#' \deqn{X_{corrected ij} = X_{ij} - \bar{X_{i.}} + \bar{X_{..}}}
#' Then calculated the rank all genotypes for each environment j
#' \deqn{r_{ij} = rank(X_{corrected ij})}
#' Mean rank difference is calculated as the following equation.
#' \deqn{S_{i}1 = \frac{\sum_{j} |r_{ij}-\bar{r_{i.}}|}{\frac{E\cdot(E-1)}{2}}}
#' where \eqn{r_{ij}} is the rank of genotype i in environment j and \eqn{\bar{r_{i.}}} is the marginal rank of genotype i over environment,
#' based on the corrected \eqn{X_{ij}} values.
#'
#' @param data a dataframe containing trait, genotype and environment.
#' @param trait colname of a column containing a numeric vector of interested trait to be analysized.
#' @param genotype colname of a column containing a character or factor vector labeling different genotypic varieties
#' @param environment colname of a column containing a character or factor vector labeling different environments
#'
#' @return a data table with mean rank difference
#'
#' @author Tien Cheng Wang
#'
#' @references
#' \insertRef{nassar1987}{toolStability}
#'
#' @importFrom dplyr group_by summarise mutate
#' @importFrom data.table data.table
#' @importFrom Rdpack reprompt
#'
#' @export
#'
#' @examples
#' data(Data)
#' mean.rank.difference<- mean_rank_difference(Data,'Yield','Genotype','Environment')
#'
mean_rank_difference <- function(data,trait,genotype,environment){
  if(!is.numeric(data[[trait]])){stop('Trait must be a numeric vector')}
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
  Data <- data.table(X=data[[trait]],Genotype=data[[genotype]],Environment=data[[environment]])
  X..bar <- mean(data[[trait]])

  res <- summarise(
    group_by(
        mutate(
          group_by(
              mutate(
                group_by(Data,Genotype),
                corrected.X=X-mean(X)+X..bar),
            Environment),
          corrected.rank=rank(-corrected.X,na.last="keep", ties.method="min")),
      Genotype),
    mean.rank.difference= abs.dev.sum(corrected.rank))

  return(res[ ,c("Genotype","mean.rank.difference")] )
}
