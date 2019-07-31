#' @title variance of rank
#'
#' @description
#' \code{variance_of_rank} calculate variance of a genotype across environments.
#'
#' @keywords nonparametric approach
#'
#' @details
#' Variance of rank is calculatd based on regression function.
#' Variety with low variance of rank is considered as stable.
#'
#' \deqn{S_{i}4 = \frac{\sum_{j} (r_{ij}-bar(r)_{i.})^{2}{E-1}}
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
#' @importFrom Rdpack reprompt
#'
#' @export
#'
#' @examples
#' data(Data)
#' variance.of.rank <- variance_of_rank(Data$Yield,Data$Genotype,Data$Environment)
#'
variance_of_rank <- function(trait,genotype,environment){
  if(!is.numeric(trait)){stop('Trait must be a numeric vector')}

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
    mean.rank=mean(corrected.rank),
    variance.of.rank=sum((corrected.rank-mean.rank)^2/(length(X)-1)))

  return(res[ ,c("Genotype","variance.of.rank")] )
}
