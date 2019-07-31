#' @title Genotypic superiority measure
#'
#' @description
#' \code{genotypic_superiority_measure} calculate variance of a genotype across environments.
#'
#' @keywords dynamic stability
#'
#' @details
#' Genotypic superiority measure is calculatd based on regression function.
#' Variety with low genotypic superiority measure is considered as stable.
#'
#' \deqn{P_{i} = \sum_{j}^{n} (X_{ij}-M_{j})^{2}/(2n)}
#' where \eqn{X_{ij}} stands for observed trait and
#' \ean{M_{j}} stands for maximum response among all culitvars in the jth location.
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
#' \insertRef{lin1988}{toolStability}
#'
#' @importFrom dplyr group_by summarise mutate
#' @importFrom data.table data.table
#' @importFrom Rdpack reprompt
#'
#' @export
#'
#' @examples
#' data(Data)
#' genotypic.superiority.measure <- genotypic_superiority_measure(Data$Yield,Data$Genotype,Data$Environment)
#'
genotypic_superiority_measure <- function(trait,genotype,environment){
  if(!is.numeric(trait)){stop('Trait must be a numeric vector')}

  # combine vectors into data table
  Data <- data.table(X=trait,Genotype=genotype,Environment=environment)

res <- summarise(
  group_by(
    ungroup(
      mutate(
        group_by(x,Environment),       # for each environment
        Xj.max=max(X,na.rm=TRUE))),    # first calculate environmental mean
    Genotype),                         # for each genotype
  genotypic.superiority.measure=sum(X-Xj.max)^2/(2*length(X)))

return(res[,c('Genotype','genotypic.superiority.measure')])
}
