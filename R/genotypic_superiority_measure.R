utils::globalVariables(c("Bi", "Bi1", "Bi2", "E", "Environment", "Genotype", "Mean.Yield", "Mj", "X", "Xi.bar", "Xj.bar", "Xj.max", "corrected.X", "corrected.rank", "dev", "deviation", "mean.rank", "s2d1", "s2d2", "s2di", "s2xi", "sqr", "sqr1", "wi"))
#' @title Genotypic superiority measure
#'
#' @description
#' \code{genotypic_superiority_measure} calculate variance of a genotype across environments.
#'
#' @keywords dynamic stability
#'
#' @details
#' Genotypic superiority measure (Lin and Binns, 1988) is calculatd based on regression function.
#' Variety with low genotypic superiority measure is considered as stable.
#' Equation of genotypic superiority measure can be found in vignette file.
#'
#' @param data a dataframe containing trait, genotype and environment.
#' @param trait colname of a column containing a numeric vector of interested trait to be analysized.
#' @param genotype colname of a column containing a character or factor vector labeling different genotypic varieties
#' @param environment colname of a column containing a character or factor vector labeling different environments
#'
#' @return a data table with genotypic superiority measure
#'
#' @author Tien-Cheng Wang
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
#' res <- genotypic_superiority_measure(Data, "Yield", "Genotype", "Environment")
genotypic_superiority_measure <- function(data, trait, genotype, environment) {
  if (!is.numeric(data[[trait]])) {
    stop("Trait must be a numeric vector")
  }

  # combine vectors into data table
  Data <- data.table(X = data[[trait]], Genotype = data[[genotype]], Environment = data[[environment]])
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
    genotypic.superiority.measure = (sum(Mj))
  )

  return(res[, c("Genotype", "genotypic.superiority.measure")])
}
