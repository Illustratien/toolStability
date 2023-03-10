utils::globalVariables(c("Bi", "Bi1", "Bi2", "E", "Environment", "Genotype", "Mean.Yield", "Mj", "X", "Xi.bar", "Xj.bar", "Xj.max", "corrected.X", "corrected.rank", "dev", "deviation", "mean.rank", "s2d1", "s2d2", "s2di", "s2xi", "sqr", "sqr1", "wi"))
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
#' Equation of ecovalence can be found in vignette file.
#'
#' @param data a dataframe containing trait, genotype and environment.
#' @param trait colname of a column containing a numeric vector of interested trait to be analysized.
#' @param genotype colname of a column containing a character or factor vector labeling different genotypic varieties
#' @param environment colname of a column containing a character or factor vector labeling different environments
#' @param unit.correct logical, default is \code{FALSE}, returning the stability index with unit equals to squared unit of trait; when \code{TRUE}, returning stability index with the unit as same as unit of trait.
#' @param modify logical, default is \code{FALSE}, returning the original ecovalence; when \code{TRUE}, returning modified ecovalence in consideration of number of environment.
#'
#' @return a data table with ecovalence
#'
#' @author Tien-Cheng Wang
#'
#' @references
#' \insertRef{wricke1962}{toolStability}
#'
#' @importFrom dplyr group_by summarise mutate mutate_at select rename all_of
#' @importFrom data.table data.table
#' @importFrom Rdpack reprompt
#'
#' @export
#'
#' @examples
#' data(Data)
#' eco.valence <- ecovalence(
#'  data = Data,
#'  trait = "Yield",
#'  genotype = "Genotype",
#'  environment = "Environment",
#'  unit.correct = FALSE,
#'  modify=FALSE)
ecovalence <- function(data, trait, genotype, environment, unit.correct=FALSE, modify=FALSE) {
  if (!is.numeric(data[[trait]])) {
    stop("Trait must be a numeric vector")
  }
  # combine vecotr into data table
  if (length(environment) == 1){
    Data <- data.table(X =  data[[trait]] ,
                       Genotype = data[[genotype]],
                       Environment = data[[environment]])

  }else { # if input is the vector containing the name that are going to combine in one column
    data$Environment <- interaction(data[,environment],sep = '_')

    Data <- data.table(X = data[[trait]] ,
                       Genotype = data[[genotype]],
                       Environment = data[['Environment']])
  }
  X..bar <- mean(Data$X)


  if(modify==T){
    index.name <- 'ecovalence.modified'
    res <-dplyr::select(
        summarise(
          mutate(
            group_by(
              mutate(
                group_by(Data, Environment), # for each environment
                Xj.bar = mean(X)
              ), # first calculate environmental mean
              Genotype
            ), # for each genotype
            Xi.bar = mean(X), # then calculate genotypic mean
            sqr = (X - Xi.bar - Xj.bar + X..bar)^2
          ),
          Mean.trait = mean(X),
          ecovalence.modified = mean(sqr, na.rm = TRUE)),
        all_of(c('Genotype','Mean.trait',index.name)))
  }else{
    index.name <- 'ecovalence'
    res <-dplyr::select(
      summarise(
        mutate(
          group_by(
            mutate(
              group_by(Data, Environment), # for each environment
              Xj.bar = mean(X)
            ), # first calculate environmental mean
            Genotype
          ), # for each genotype
          Xi.bar = mean(X), # then calculate genotypic mean
          sqr = (X - Xi.bar - Xj.bar + X..bar)^2
        ),
        Mean.trait = mean(X),
        ecovalence = sum(sqr)),
      all_of(c('Genotype','Mean.trait',index.name)))
  }

  if (unit.correct==TRUE){
    res <- mutate_at(res,index.name, sqrt)
  }

  names(res)[names(res) == "Mean.trait"] <- sprintf("Mean.%s", trait)

  return(res)
}
