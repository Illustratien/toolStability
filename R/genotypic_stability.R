utils::globalVariables(c("Bi", "Bi1", "Bi2", "E", "Environment", "Genotype", "Mean.Yield", "Mj", "X", "Xi.bar", "Xj.bar", "Xj.max", "corrected.X", "corrected.rank", "dev", "deviation", "mean.rank", "s2d1", "s2d2", "s2di", "s2xi", "sqr", "sqr1", "wi"))
#' @title Genotypic stability
#'
#' @description
#' \code{genotypic_stability} calculate variance of a genotype across environments.
#'
#' @keywords regression approach
#'
#' @details
#' Genotypic stability (Hanson, 1970) is calculatd based on regression function.
#' Variety with low stability variance is considered as stable.
#' Equation of genotypic stability can be found in vignette file.
#'
#' @param data a dataframe containing trait, genotype and environment.
#' @param trait colname of a column containing a numeric vector of interested trait to be analysized.
#' @param genotype colname of a column containing a character or factor vector labeling different genotypic varieties
#' @param environment colname of a column containing a character or factor vector labeling different environments
#'
#' @return a data table with genotypic stability
#'
#' @author Tien-Cheng Wang
#'
#' @references
#' \insertRef{hanson1970}{toolStability}
#'
#' @importFrom dplyr group_by summarise mutate
#' @importFrom data.table data.table
#' @importFrom Rdpack reprompt
#'
#' @export
#'
#' @examples
#' data(Data)
#' genotypic.stability <- genotypic_stability(Data, "Yield", "Genotype", "Environment")
genotypic_stability <- function(data, trait, genotype, environment) {
  if (!is.numeric(data[[trait]])) {
    stop("Trait must be a numeric vector")
  }
  # combine vectors into data table
  Data <- data.table(X = data[[trait]], Genotype = data[[genotype]], Environment = data[[environment]])

  X..bar <- mean(Data$X) # overall mean of X
  res <- mutate(
    group_by(
      mutate(
        group_by(Data, Environment), # for each environment
        Xj.bar = mean(X)
      ), # first calculate environmental mean
      Genotype
    ), # for each genotype
    Xi.bar = mean(X), # then calculate genotypic mean
    Bi1 = (X - Xi.bar - Xj.bar + X..bar) * (Xj.bar - X..bar),
    Bi2 = (Xj.bar - X..bar)^2,
    Bi = 1 + (sum(Bi1, na.rm = TRUE) / sum(Bi2, na.rm = TRUE))
  )
  bmin <- min(res$Bi)
  res <- summarise(res, genotypic.stability = sum((X - Xi.bar - bmin * Xj.bar + bmin * X..bar)^2))

  return(res)
}
