utils::globalVariables(c('Bi','Bi1','Bi2','E','Environment','Genotype','Mean.Yield','Mj','X','Xi.bar','Xj.bar','Xj.max','corrected.X','corrected.rank','dev','deviation','mean.rank','s2d1','s2d2','s2di','s2xi','sqr','sqr1','wi'))
#' @title Stability variance
#'
#' @description
#' \code{stability_variance} calculate variance of a genotype across environments.
#'
#' @keywords dynamic stability
#'
#' @details
#' Stability variance is calculatd based on lindear combination of ecovalence and mean square of genotype-environment interaction.
#' Variety with low stability variance is considered as stable.
#'
#' \deqn{\sigma^{2}_{i} = \frac{1}{(G-1)(G-2)(E-1)}\big[
#' \sum_{j}(X_{ij} -bar(X_{i.})_{i.})^2-\sum_{i}\sum_{j}(X_{ij} -bar(X_{i.})_{i.})^2
#' \big]}
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
#' stability.variance <- stability_variance(Data$Yield,Data$Genotype,Data$Environment)
#'
stability_variance <- function(trait,genotype,environment){
  if(!is.numeric(trait)){stop('Trait must be a numeric vector')}

  # combine vectors into data table
  Data <- data.table(X=trait,Genotype=genotype,Environment=environment)

  X..bar=mean(trait)               # overall mean of X
  G <- length(unique(genotype))
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
      sqr=(X-Xi.bar-Xj.bar+X..bar)^2),
    wi= sum(sqr, na.rm=TRUE)),
    stability.variance=sqrt((G*(G-1)*wi-sum(wi,na.rm=TRUE))/((E-1)*(G-2)*(G-2))))#end of mutate  sqr=(X-Xi.bar-Xj.bar+X..bar)^2),#end of mutate

return(res[ ,c("Genotype","stability.variance")] )
}
