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
#'
#' @return a data table with ecovalence
#'
#' @author Tien-Cheng Wang
#'
#' @references
#' \insertRef{wricke1962}{toolStability}
#'
#' @importFrom dplyr group_by summarise mutate
#' @importFrom data.table data.table
#' @importFrom Rdpack reprompt
#'
#' @export
#'
#' @examples
#' data(Data)
#' eco.valance <- ecovalence(Data, "Yield", "Genotype", "Environment")
ecovalence <- function(data, trait, genotype, environment) {
  if (!is.numeric(data[[trait]])) {
    stop("Trait must be a numeric vector")
  }
  # combine vecotr into data table
  Data <- data.table(X = data[[trait]], Genotype = data[[genotype]], Environment = data[[environment]])
  X..bar <- mean(Data$X)

  res <- summarise(
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
    ecovalence = mean(sqr, na.rm = TRUE)
  )

  return(res[, c("Genotype", "ecovalence")])
}
