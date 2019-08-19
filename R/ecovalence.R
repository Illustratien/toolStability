utils::globalVariables(c('Bi','Bi1','Bi2','E','Environment','Genotype','Mean.Yield','Mj','X','Xi.bar','Xj.bar','Xj.max','corrected.X','corrected.rank','dev','deviation','mean.rank','s2d1','s2d2','s2di','s2xi','sqr','sqr1','wi'))
#' @title Ecovalence
#'
#' @description
#' \code{ecovalence} calculate genetic and environmental interaction.
#'
#' @keywords dynamic stability
#'
#' @details
#' Ecovalence (Wricke, 1962) is calcualted based on square and sum up the genotype–environment
#' interaction all over the environment.
#' Variety with low ecovalence is considered as stable.
#'
#' \deqn{S^{2}_{xi} = \frac{\sum_{j}(X_{ij} - \bar{X}_{i.})^2}{E-1}}
#' where \eqn{X_{ij}} is the observed phenotypic mean value of genotype i(i=1,..., G)
#' in environment j(j=1,...,E), with \eqn{bar(X)_{i.}} denoting marginal means of genotype i.
#'
#' Here we modified the original function by dividing (E-1) after summation in order to balance the different number of envionment condition.
#'
#' @param data a dataframe containing trait, genotype and environment.
#' @param trait colname of a column containing a numeric vector of interested trait to be analysized.
#' @param genotype colname of a column containing a character or factor vector labeling different genotypic varieties
#' @param environment colname of a column containing a character or factor vector labeling different environments
#'
#' @return a data table with ecovalence
#'
#' @author Tien Cheng Wang
#'
#' @references
#' \insertRef{wricke1962}{toolStability}
#'
#' @importFrom dplyr group_by summarise inner_join
#' @importFrom data.table data.table
#' @importFrom Rdpack reprompt
#'
#' @export
#'
#' @examples
#' data(Data)
#' eco.valance <- ecovalence(Data,'Yield','Genotype','Environment')

ecovalence <- function(data,trait, genotype,environment){

ecovalence <- function(trait, genotype,environment){
  if(!is.numeric(trait)){stop('Trait must be a numeric vector')}
  X..bar=mean(trait)
  res <- summarise(
    mutate(
      group_by(
        ungroup(
          mutate(
            group_by(Data,Environment),          # for each environment
            Xj.bar=mean(X))),                    # first calculate environmental mean
        Genotype),                               # for each genotype
      Xi.bar=mean(X),                            # then calculate genotypic mean
      sqr=(X-Xi.bar-Xj.bar+X..bar)^2),
    ecovalence= mean(sqr, na.rm=TRUE))

  return(res[,c('Genotype','ecovalence')])
}
