utils::globalVariables(c("Bi", "Bi1", "Bi2", "E", "Environment", "Genotype", "Mean.Yield", "Mj", "X", "Xi.bar", "Xj.bar", "Xj.max", "corrected.X", "corrected.rank", "dev", "deviation", "mean.rank", "s2d1", "s2d2", "s2di", "s2xi", "sqr", "sqr1", "wi"))
#' @title Environmental variance
#'
#' @description
#' \code{environmental_variance} is used to calculate variance of a genotype across environments.
#'
#' @keywords static stability
#'
#' @details
#' Environmental variance (Roemer, 1917) is calculated by squared and suming up all deviation from genotypic mean for each genotype.
#' The larger the environmental variance of one genotype is, the lower the stability.
#' Equation of environmental variance can be found in vignette file.
#'
#' @param data a dataframe containing trait, genotype and environment.
#' @param trait colname of a column containing a numeric vector of interested trait to be analysized.
#' @param genotype colname of a column containing a character or factor vector labeling different genotypic varieties
#' @param unit.correct logical, default is \code{FALSE}, returning the stability index with unit equals to squared unit of trait; when \code{TRUE}, returning stability index with the unit as same as unit of trait.#'
#' @return a data table with environmental variance
#'
#' @author Tien-Cheng Wang
#'
#' @references
#' \insertRef{roemer1917}{toolStability}
#'
#' @importFrom dplyr group_by summarise mutate mutate_at
#' @importFrom data.table data.table
#' @importFrom Rdpack reprompt
#'
#' @export
#'
#' @examples
#' data(Data)
#' environmental.variance <- environmental_variance(
#'  data = Data,
#'  trait = "Yield",
#'  genotype = "Genotype",
#'  unit.correct = FALSE)
environmental_variance <- function(data, trait, genotype, unit.correct=FALSE){
  if (!is.numeric(data[[trait]])) {
    stop("Trait must be a numeric vector")
  }
  # combine vectors into data table
  Data <- data.table(X = data[[trait]], Genotype = data[[genotype]])

  # calculate environmental variance
  res <- summarise(
    mutate(
      group_by(Data, Genotype),
      deviation = (X - mean(X))^2 / (length(X) - 1)),
    Mean.trait = mean(X),
    environmental.variance = sum(deviation, na.rm = TRUE)

  )

  if (unit.correct==TRUE){
    res <- mutate_at(res,"environmental.variance", sqrt)
  }
  names(res)[names(res) == "Mean.trait"] <- sprintf("Mean.%s", trait)
  return(res)
}
