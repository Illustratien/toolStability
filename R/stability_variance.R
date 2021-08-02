utils::globalVariables(c("Bi", "Bi1", "Bi2", "E", "Environment", "Genotype", "Mj", "X", "Xi.bar", "Xj.bar", "Xj.max", "corrected.X", "corrected.rank", "dev", "deviation", "mean.rank", "s2d1", "s2d2", "s2di", "s2xi", "sqr", "sqr1", "wi"))
#' @title Stability variance
#'
#' @description
#' \code{stability_variance} calculate variance of a genotype across environments.
#'
#' @keywords dynamic stability
#'
#' @details
#' Stability variance (Shukla, 1972) is calculatd based on lindear combination of ecovalence and mean square of genotype-environment interaction.
#' Variety with low stability variance is considered as stable.
#' Negative values of stability variance is replaced with 0.
#' Equation of stability variance can be found in vignette file.
#'
#' @param data a dataframe containing trait, genotype and environment.
#' @param trait colname of a column containing a numeric vector of interested trait to be analyzed.
#' @param genotype colname of a column containing a character or factor vector labeling different genotypic varieties.
#' @param environment colname of a column containing a character or factor vector labeling different environments.
#' @param unit.correct logical, default is \code{FALSE}, returning the stability index with unit equals to squared unit of trait; when \code{TRUE}, returning stability index with the unit as same as unit of trait.#'
#' @return a data table with stability variance
#'
#' @author Tien-Cheng Wang
#'
#' @references
#' \insertRef{shukla1972}{toolStability}
#'
#' @importFrom dplyr group_by summarise mutate mutate_at select rename
#' @importFrom data.table data.table
#' @importFrom Rdpack reprompt
#'
#' @export
#'
#' @examples
#' data(Data)
#' stability.variance <- stability_variance(
#'  data = Data,
#'  trait = "Yield",
#'  genotype = "Genotype",
#'  environment = "Environment")
stability_variance <- function(data, trait, genotype, environment, unit.correct = FALSE) {
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
  X..bar <- mean(data[[trait]])
  G <- length(unique(data[[genotype]]))

  res <- mutate(
    group_by(
      mutate(
        group_by(Data, Environment), # for each environment
        Xj.bar = mean(X)
      ), # first calculate environmental mean
      Genotype
    ), # for each genotype
    Xi.bar = mean(X), # then calculate genotypic mean
    sqr = ((X - Xi.bar - Xj.bar + X..bar)^2) / (length(X) - 1)
  )

  wisum <- sum(res$sqr)

  res <- dplyr::rename(
    dplyr::select(
      summarise(res,
                wi = sum(sqr, na.rm = TRUE),
                Mean.trait = mean(X),
                stability.variance = (G * (G - 1) * wi - wisum) / ((G - 1) * (G - 2))),
      c('Genotype','Mean.trait','stability.variance')),
    varnam = 'Mean.trait')
  # replace negative value to zero as stated by Shukla, 1972.
  res$stability.variance[res$stability.variance < 0] <- 0

  if (unit.correct==TRUE){
    res <- mutate_at(res,"stability.variance", sqrt)
  }
  return(res)
}
