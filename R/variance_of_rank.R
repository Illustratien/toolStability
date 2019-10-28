utils::globalVariables(c("Bi", "Bi1", "Bi2", "E", "Environment", "Genotype", "Mean.Yield", "Mj", "X", "Xi.bar", "Xj.bar", "Xj.max", "corrected.X", "corrected.rank", "dev", "deviation", "mean.rank", "s2d1", "s2d2", "s2di", "s2xi", "sqr", "sqr1", "wi"))
#' @title variance of rank
#'
#' @description
#' \code{variance_of_rank} calculate variance of a genotype across environments.
#'
#' @keywords nonparametric approach
#'
#' @details
#' Variance of rank (Nassar and Huehn, 1987) is calculatd based on regression function.
#' Variety with low variance of rank is considered as stable.
#' Equation of variance of rank can be found in vignette file.
#'
#' @param data a dataframe containing trait, genotype and environment.
#' @param trait colname of a column containing a numeric vector of interested trait to be analysized.
#' @param genotype colname of a column containing a character or factor vector labeling different genotypic varieties
#' @param environment colname of a column containing a character or factor vector labeling different environments
#'
#' @return a data table with variance of rank
#'
#' @author Tien-Cheng Wang
#'
#' @references
#' \insertRef{nassar1987}{toolStability}
#'
#' @importFrom dplyr group_by summarise mutate
#' @importFrom data.table data.table
#' @importFrom Rdpack reprompt
#'
#' @export
#'
#' @examples
#' data(Data)
#' variance.of.rank <- variance_of_rank(Data, "Yield", "Genotype", "Environment")
variance_of_rank <- function(data, trait, genotype, environment) {
  if (!is.numeric(data[[trait]])) {
    stop("Trait must be a numeric vector")
  }
  # combine vectors into data table
  Data <- data.table(X = data[[trait]], Genotype = data[[genotype]], Environment = data[[environment]])
  X..bar <- mean(data[[trait]])

  res <- summarise(
    group_by(
      mutate(
        group_by(
          mutate(
            group_by(Data, Genotype),
            corrected.X = X - mean(X) + X..bar
          ),
          Environment
        ),
        corrected.rank = rank(-corrected.X, na.last = "keep", ties.method = "min")
      ),
      Genotype
    ),
    mean.rank = mean(corrected.rank),
    variance.of.rank = sum((corrected.rank - mean.rank)^2 / (length(X) - 1))
  )

  return(res[, c("Genotype", "variance.of.rank")])
}
