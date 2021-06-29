utils::globalVariables(c("Bi", "Bi1", "Bi2", "E", "Environment", "Genotype", "Mean.Yield", "Mj", "X", "Xi.bar", "Xj.bar", "Xj.max", "corrected.X", "corrected.rank", "dev", "deviation", "mean.rank", "s2d1", "s2d2", "s2di", "s2xi", "sqr", "sqr1", "wi"))
#' @title Genotypic superiority measure
#'
#' @description
#' \code{genotypic_superiority_measure} calculate variance of a genotype across environments.
#'
#' @keywords dynamic stability
#'
#' @details
#' Genotypic superiority measure (Lin and Binns, 1988) is calculatd based on means square distance between maximum value of environment j and genotype i.
#' Variety with low genotypic superiority measure is considered as stable.
#' Equation of genotypic superiority measure can be found in vignette file.
#'
#' @param data a dataframe containing trait, genotype and environment.
#' @param trait colname of a column containing a numeric vector of interested trait to be analysized.
#' @param genotype colname of a column containing a character or factor vector labeling different genotypic varieties
#' @param environment colname of a column containing a character or factor vector labeling different environments
#' @param unit.correct logical, default is \code{FALSE}, returning the stability index with unit equals to squared unit of trait; when \code{TRUE}, returning stability index with the unit as same as unit of trait.#'
#' @return a data table with genotypic superiority measure
#'
#' @author Tien-Cheng Wang
#'
#' @references
#' \insertRef{lin1988}{toolStability}
#'
#' @importFrom dplyr group_by summarise mutate mutate_at
#' @importFrom data.table data.table
#' @importFrom Rdpack reprompt
#'
#' @export
#'
#' @examples
#' data(Data)
#' res <- genotypic_superiority_measure(
#'  data = Data,
#'  trait = "Yield",
#'  genotype = "Genotype",
#'  environment = "Environment",
#'  unit.correct = FALSE)
genotypic_superiority_measure <- function(data, trait, genotype, environment, unit.correct = FALSE) {
  if (!is.numeric(data[[trait]])) {
    stop("Trait must be a numeric vector")
  }
  # combine vectors into data table
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
  varnam <- paste0("Mean.",trait)
  res <- summarise(
    mutate(
      group_by(
        mutate(
          group_by(Data, Environment), # for each environment
          Xj.max = max(X, na.rm = TRUE)
        ), # first calculate environmental mean
        Genotype
      ), # for each genotype
      Mj = (X - Xj.max)^2 / (2 * length(X))
    ),
    !!varnam := mean(X),
    genotypic.superiority.measure = (sum(Mj))
  )
  if (unit.correct==TRUE){
    res <- mutate_at(res,"genotypic.superiority.measure", sqrt)
  }
  return(res[, c("Genotype",varnam ,"genotypic.superiority.measure")])
}
