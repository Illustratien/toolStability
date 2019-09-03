utils::globalVariables(c("Bi", "Bi1", "Bi2", "E", "Environment", "Genotype", "Mean.Trait", "Mj", "X", "Xi.bar", "Xj.bar", "Xj.max", "corrected.X", "corrected.rank", "dev", "deviation", "mean.rank", "s2d1", "s2d2", "s2di", "s2xi", "sqr", "sqr1", "wi"))
#' @title Table stability
#'
#' @description
#' \code{table_stability} export all the stability indicies in the package.
#'
#' @keywords static, dynamic, regression, nonparametric and probablistic approach
#'
#' @details Combine all stability indices in this package and export as a table, including mean trait, normality of the trait across environment as well.
#'
#' @seealso \code{\link{adjusted_coefficient_of_variation}}
#' @seealso \code{\link{coefficient_of_determination}}
#' @seealso \code{\link{coefficient_of_regression}}
#' @seealso \code{\link{deviation_mean_squares}}
#' @seealso \code{\link{ecovalence}}
#' @seealso \code{\link{environmental_variance}}
#' @seealso \code{\link{genotypic_stability}}
#' @seealso \code{\link{genotypic_superiority_measure}}
#' @seealso \code{\link{stability_variance}}
#' @seealso \code{\link{variance_of_rank}}
#' @seealso \code{\link{safty_first_index}}
#'
#' @param data a dataframe containing trait, genotype and environment.
#' @param trait colname of a column containing a numeric vector of interested trait to be analysized.
#' @param genotype colname of a column containing a character or factor vector labeling different genotypic varieties
#' @param environment colname(s) of a column containing a character or factor vector labeling different environments,
#' if input is a vector containing multiple column names, then it will be merged into single environment column in the function.
#' @param lambda threshold value of trait that define stability for a genotype across environments through probabilistic approach.
#' @param normalize a logical value indicating whether stability indicies should be normalized to the range from 0 to 1, where 1 refer to stable and 0 is unstable. Default is \code{FALSE}.
#'
#' @return a data table with multiple stability indices
#'
#' @author Tien Cheng Wang
#'
#' @references
#' \insertRef{doering2018}{toolStability}
#' \insertRef{pinthus1973}{toolStability}
#' \insertRef{finlay1963}{toolStability}
#' \insertRef{eberhart1966}{toolStability}
#' \insertRef{wricke1962}{toolStability}
#' \insertRef{roemer1917}{toolStability}
#' \insertRef{hanson1970}{toolStability}
#' \insertRef{lin1988}{toolStability}
#' \insertRef{shukla1972}{toolStability}
#' \insertRef{nassar1987}{toolStability}
#' \insertRef{eskridge1990}{toolStability}
#'
#' @importFrom dplyr group_by summarise mutate mutate_at
#' @importFrom data.table data.table
#' @importFrom Rdpack reprompt
#' @importFrom stats pnorm sd median shapiro.test na.omit
#' @importFrom nortest ad.test
#' @export
#'
#' @examples
#' data(Data)
#' tb <- table_stability(Data,"Yield","Genotype","Environment",median(Data$Yield),normalize = TRUE)
#'
table_stability <- function(data, trait, genotype, environment, lambda, normalize=FALSE) {
  trait.value <- data[[trait]]
  geno.value <- data[[genotype]]
  no.na.trait <- na.omit(trait.value)
  trait_range <- range(no.na.trait)
  if (!is.numeric(trait.value )) {
    stop("Trait must be a numeric vector")
  }
  if (!is.numeric(lambda)) {
    stop("Lambda must be numeric!")
  }
  if (lambda > trait_range[2] | lambda < trait_range[1]) {
    stop("Lambda must in the range of trait")
  }
  if (missing(lambda)) {
    lambda <- median(no.na.trait)
    message(sprintf("lambda = %d (medain of %s) is used for Safty first index!", lambda, trait))
  }

  # combine vectors into data table
  Data <- data.table(X = data[[trait]], Genotype = data[[genotype]], Environment = data[[environment]]) # overall mean of X
  X..bar <- mean(data[[trait]])
  G <- length(unique(data[[genotype]]))
  sample_number <-range(table(Data$Environment))
  if (sample_number[1]<3) {
  Data <- na.omit(Data)
  X..bar <- mean(no.na.trait) # overall mean of X
  G <- length(unique(na.omit(geno.value)))
    stop("Environment number must above 3")
  } else if (sample_number[2] <= 5000 & sample_number[1] >= 3) {
    normtest <- function(x){
      return(shapiro.test(x)$p.value > 0.05)
    }
    norm.test.name <- "Shapiro"
  } else if (sample_number[2] > 5000) {
    normtest <- function(x){
      return(ad.test(x)$p.value > 0.05)
    }
    norm.test.name <- "Anderson-Darling"
  }

  # calculate doefficient determination
  res <- mutate(
    group_by(
      mutate(
        group_by(
          mutate(
            group_by(
              Data, Genotype
            ),
            corrected.X = X - mean(X) + X..bar
          ), # correction before ranking
          Environment
        ), # for each environment
        Xj.bar = mean(X), # environmental mean
        Xj.max = max(X), # maximum of each environment
        corrected.rank = rank(-corrected.X, na.last = "keep", ties.method = "min")
      ),
      Genotype
    ), # for each genotype
    Xi.bar = mean(X), # then calculate genotypic mean
    E = length(X), # number of environment
    s2d1 = ((X - Xi.bar - Xj.bar + X..bar)^2) / (E - 2),
    s2d2 = ((Xj.bar - X..bar)^2) / (E - 2),
    Bi1 = (X - Xi.bar - Xj.bar + X..bar) * (Xj.bar - X..bar),
    Bi2 = (Xj.bar - X..bar)^2,
    deviation = ((X - Xi.bar)^2) / (E - 1),
    sqr = (X - Xi.bar - Xj.bar + X..bar)^2,
    sqr1 = ((X - Xi.bar - Xj.bar + X..bar)^2) / (E - 1),
    Bi = 1 + (sum(Bi1) / sum(Bi2)),
    Mj = (X - Xj.max)^2 / (2 * length(X))
  )

  bmin <- min(res$Bi) # for genotypic stability
  wisum <- sum(res$sqr1)
  res <- summarise(res,
    Mean.Trait = mean(X),
    Xi.logvar = log10(var(X, na.rm = TRUE)),
    Xi.logmean = log10(mean(X, na.rm = TRUE)),
    Bi = 1 + (sum(Bi1, na.rm = TRUE) / sum(Bi2, na.rm = TRUE)),
    s2di = sum(s2d1, na.rm = TRUE) - ((Bi - 1)^2) * sum(s2d2, na.rm = TRUE),
    s2xi = sum(deviation, na.rm = TRUE),
    wi = sum(sqr1, na.rm = TRUE),
    mean.rank = mean(corrected.rank),
    # stability indices
    Ecovalence = mean(sqr, na.rm = TRUE),
    Coefficient.of.determination = 1 - (s2di / s2xi),
    Coefficient.of.regression = 1 + sum(Bi1, na.rm = TRUE) / sum(Bi2, na.rm = TRUE),
    Deviation.mean.squares = sum(s2d1, na.rm = TRUE) - ((Bi - 1)^2) * sum(s2d2, na.rm = TRUE),
    Environmental.variance = sum(deviation, na.rm = TRUE),
    Genotypic.stability = sum((X - Mean.Trait - bmin * Xj.bar + bmin * X..bar)^2),
    Genotypic.superiority.measure = sum(Mj),
    Variance.of.rank = sum((corrected.rank - mean.rank)^2 / (length(X) - 1)),
    Stability.variance = (G * (G - 1) * wi - wisum) / ((G - 1) * (G - 2)),
    Safty.first.index = pnorm((lambda - mean(X)) / sd(X)),
    Normality = normtest(X)
  )
  # for adjusted correlation variation
  b <- sum((res$Xi.logmean - mean(res$Xi.logmean)) * (res$Xi.logvar - mean(res$Xi.logvar))) / sum((res$Xi.logmean - mean(res$Xi.logmean))^2)

  res$Adjusted.coefficient.of.variation <- 100 * (1 / res$Mean.Trait) * sqrt(10^(((2 - b) * res$Xi.logmean) + ((b - 2) * (mean(res$Xi.logmean))) + res$Xi.logvar))
  # replace negative value to zero as stated by Shukla, 1972.
  res$Stability.variance[res$Stability.variance < 0] <- 0

    if (all(!res$Normality)) {
      warning(sprintf("All of your genotypes didn't pass the %s normality test!
 Safty_first Index may not be accurate.",norm.test.name))
    } else if (any(!res$Normality)){
      warning(sprintf("Part of your genotypes didn't pass the %s normality test!
 Safty_first Index may not be accurate.",norm.test.name))
    }

  # select output columns
  nam.list <- c(
    "Genotype", "Mean.Trait", "Normality", "Safty.first.index", "Coefficient.of.determination", "Coefficient.of.regression", "Deviation.mean.squares", "Environmental.variance",
    "Genotypic.stability", "Genotypic.superiority.measure", "Variance.of.rank", "Stability.variance",
    "Adjusted.coefficient.of.variation", "Ecovalence")
  res <- res[, nam.list]

  if (normalize == TRUE){
    scale1 <-  function (x) {
      revers.x <- x * (-1)
      (revers.x - min(revers.x)) / diff(range(revers.x))
    }
    res <- mutate_at(res, nam.list[-c(1:3)], scale1)
  }

  names(res)[names(res) == "Mean.Trait"] <- sprintf("Mean.%s", trait)
  return(res)
}
