utils::globalVariables(c("Bi", "Bi1", "Bi2", "E", "Environment", "Genotype", "Mean.Yield", "Mj", "X", "Xi.bar", "Xj.bar", "Xj.max", "corrected.X", "corrected.rank", "dev", "deviation", "mean.rank", "s2d1", "s2d2", "s2di", "s2xi", "sqr", "sqr1", "wi","Xi.logmean","Xi.logvar"))
#' @title Adjusted coefficient of variaiton
#'
#' @description
#' \code{adjusted_coefficient_of_variation} calculate variance of a genotype across environments.
#'
#' @keywords regression approach
#'
#' @details
#' Adjusted coefficient of variaiton (Doering and Reckling, 2018) is calculatd based on regression function.
#' Variety with low adjusted coefficient of variation is considered as stable. Equation of adjusted coefficient of variation can be found in vignette file.
#'
#' @param data a dataframe containing trait, genotype and environment.
#' @param trait colname of a column containing a numeric vector of interested trait to be analyzed.
#' @param genotype colname of a column containing a character or factor vector labeling different genotypic varieties
#' @param environment colname of a column containing a character or factor vector labeling different environments
#'
#' @return a data table with adjusted coefficient of variation
#'
#' @author Tien-Cheng Wang
#'
#' @references
#' \insertRef{doering2018}{toolStability}
#'
#' @importFrom dplyr group_by summarise mutate select rename
#' @importFrom data.table data.table
#' @importFrom Rdpack reprompt
#' @importFrom stats var
#'
#' @export
#'
#' @examples
#' data(Data)
#' res <- adjusted_coefficient_of_variation(
#'  data = Data,
#'  trait = "Yield",
#'  genotype = "Genotype",
#'  environment = "Environment")
adjusted_coefficient_of_variation <- function(data, trait, genotype, environment) {
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
  res <-summarise(
    group_by(
      mutate(
        group_by(Data, Environment), # for each environment
        Xj.bar = mean(X, na.rm = TRUE)
      ), # first calculate environmental mean
      Genotype
    ), # for each genotype
    Xi.bar = mean(X, na.rm = TRUE), # then calculate genotypic mean
    Xi.logvar = log10(var(X, na.rm = TRUE)),
    Xi.logmean = log10(mean(X, na.rm = TRUE)),
    Mean.trait = mean(X))
  b <- with(res,sum((Xi.logmean - mean(Xi.logmean)) * (Xi.logvar - mean(Xi.logvar))) / sum((Xi.logmean - mean(Xi.logmean))^2))

  res <- dplyr::rename(
    dplyr::select(mutate(res,
                         adjusted.coefficient.of.variation = 100 * (1 / Xi.bar) * sqrt(10^(((2 - b) * Xi.logmean) + ((b - 2) * (mean(Xi.logmean))) + Xi.logvar))),
                  c('Genotype','Mean.trait','adjusted.coefficient.of.variation')),
    varnam = 'Mean.trait')

  return(res)
}
