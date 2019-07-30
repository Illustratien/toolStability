#' @title Ecovalence
#'
#' @description
#' \code{ecovalence} calculate genetic and environmental interaction.
#'
#' @keywords dynamic stability
#'
#' @details
#' Ecovalence is calcualted based on square and sum up the genotype–environment
#' interaction all over the environment.
#' Variety with low ecovalence is considered as stable.
#'
#' \deqn{S^{2}_{xi} = \frac{\sum_{j}(X_{ij} - \bar{X}_{i.})^2}{E-1}}
#' where \eqn{X_{ij}} is the observed phenotypic mean value of genotype i(i=1,..., G)
#' in environment j(j=1,...,E), with \eqn{bar(X)_{i.}} denoting marginal means of genotype i.
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
#' eco.valance <- ecovalence(Data$Yield,Data$Genotype,Data$Environment)

ecovalence <- function(trait, genotype,environment){
  if(!is.numeric(trait)){stop('Trait must be a numeric vector')}
  X..bar=mean(trait)
  # combine vectors into data table
  Data <- data.table(X=trait,Genotype=genotype,Environment=environment)

  #genotypic mean
  gen <- data.table(
         summarise(
         group_by(Data,Genotype), Xi.bar=mean(X)), key='Genotype')

  #environmental mean
  env <- data.table(
         summarise(
         group_by(Data,Environment),Xj.bar=mean(X)),key='Environment')

  #combine genotypic and environmental mean to the original data
  Data <- inner_join(Data,gen,by="Genotype")
  Data <- inner_join(Data,env,by="Environment")

  #calculate ecovalence
  res <- summarise(
  mutate(group_by(Data[ , -which(names(Data) %in% "Environment")],Genotype),
         sqr=(X-Xi.bar-Xj.bar+X..bar)^2),
  ecovalence= mean(sqr, na.rm=TRUE))

  return(res)
}
