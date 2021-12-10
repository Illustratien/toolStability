utils::globalVariables(c("Bi", "Bi1", "Bi2", "E", "Environment", "Genotype",
                         "Mean.Trait", "Mj", "X", "Xi.bar", "Xj.bar", "Xj.max",
                         "corrected.X", "corrected.rank", "dev", "deviation",
                         "mean.rank", "s2d1","s2d2", "s2di", "s2xi", "sqr", "sqr1",
                         "wi", "trait.value", "geno.value", "no.na.trait", "trait_range"))
#' @title Table of stability indices
#'
#' @description
#' \code{table_stability} export all the stability indices in the package.
#'
#' @keywords static dynamic regression nonparametric probablistic
#' @details Combine all stability indices in this package and export as a table, including mean trait, normality of the trait across environment.
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
#' @seealso \code{\link{safety_first_index}}
#'
#' @param data a data frame containing trait, genotype and environment.
#' @param trait colname of a column containing a numeric vector of interested trait to be analyzed.
#' @param genotype colname of a column containing a character or factor vector labeling different genotypic varieties.
#' @param environment colname(s) of a column containing a character or factor vector labeling different environments,
#' if input is a vector containing multiple column names, then it will be merged into single environment column in the function.
#' @param lambda the minimal acceptable value of trait that the user expected from crop across environments. Lambda should between the ranges of trait vlaue.
#' @param normalize logical, default is \code{FALSE}, indicating whether stability indices should be normalized to the range from 0 to 1, where 1 refer to stable and 0 is unstable.
#' @param unit.correct logical, default is \code{FALSE}, returning the stability index with unit equals to squared unit of trait; when \code{TRUE}, returning stability index with the unit as same as unit of trait.
#'
#' @return a data table with multiple stability indices
#'
#' @author Tien-Cheng Wang
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
#' tb <- table_stability(
#'  data = Data,
#'  trait = "Yield",
#'  genotype = "Genotype",
#'  environment = "Environment",
#'  lambda = median(Data$Yield),
#'  normalize = TRUE,
#'  unit.correct=TRUE)
table_stability <- function(data, trait, genotype, environment, lambda, normalize=FALSE, unit.correct=FALSE) {
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
  if (length(environment) == 1){
    Data <- data.table(X = trait.value ,
                       Genotype = data[[genotype]],
                       Environment = data[[environment]])

  }else { # if input is the vector containing the name that are going to combine in one column
    data$Environment <- interaction(data[,environment],sep = '_')

    Data <- data.table(X = trait.value ,
                       Genotype = geno.value,
                       Environment = data[['Environment']])
  }

  Data <- na.omit(Data)

  X..bar <- mean(no.na.trait) # overall mean of X
  G <- length(unique(na.omit(geno.value)))
  sample_number <- length(unique(Data[['Environment']]))
  if (sample_number < 3) {
    stop("Environment number must above 3")
  } else if (sample_number <= 5000 & sample_number >= 3) {
    normtest <- function(x){
      return(shapiro.test(x)$p.value > 0.05)
    }
    norm.test.name <- "Shapiro"
  } else if (sample_number > 5000) {
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
                   Xi.logvar = log10(var(X)),
                   Xi.logmean = log10(mean(X)),
                   Bi = 1 + (sum(Bi1) / sum(Bi2)),
                   s2di = sum(s2d1) - ((Bi - 1)^2) * sum(s2d2),
                   s2xi = sum(deviation),
                   wi = sum(sqr1),
                   mean.rank = mean(corrected.rank),
                   # stability indices
                   Ecovalence = sum(sqr),
                   Ecovalence.modified = mean(sqr),
                   Coefficient.of.determination = 1 - (s2di / s2xi),
                   Coefficient.of.regression = 1 + sum(Bi1) / sum(Bi2),
                   Deviation.mean.squares = sum(s2d1) - ((Bi - 1)^2) * sum(s2d2),
                   Environmental.variance = sum(deviation),
                   Genotypic.stability = sum((X - Mean.Trait - bmin * Xj.bar + bmin * X..bar)^2),
                   Genotypic.superiority.measure = sum(Mj),
                   Variance.of.rank = sum((corrected.rank - mean.rank)^2 / (length(X) - 1)),
                   Stability.variance = (G * (G - 1) * wi - wisum) / ((G - 1) * (G - 2)),
                   Safety.first.index = pnorm((lambda - mean(X)) / sd(X)),
                   Normality = normtest(X)
  )
  # for adjusted correlation variation
  b <- sum((res$Xi.logmean - mean(res$Xi.logmean)) * (res$Xi.logvar - mean(res$Xi.logvar))) / sum((res$Xi.logmean - mean(res$Xi.logmean))^2)

  res$Adjusted.coefficient.of.variation <- 100 * (1 / res$Mean.Trait) * sqrt(10^(((2 - b) * res$Xi.logmean) + ((b - 2) * (mean(res$Xi.logmean))) + res$Xi.logvar))
  # replace negative value to zero as stated by Shukla, 1972.
  res$Stability.variance[res$Stability.variance < 0] <- 0

  if (all(!res$Normality)) {
    warning(sprintf("\nAll of your genotypes didn't pass the %s normality test!
 Safety_first Index may not be accurate.",norm.test.name))
  } else if (any(!res$Normality)){
    warning(sprintf("\nPart of your genotypes didn't pass the %s normality test!
 Safety_first Index may not be accurate.",norm.test.name))
  }

  # select output columns
  nam.list <- c(
    "Genotype", "Mean.Trait", "Normality",
    "Safety.first.index", "Coefficient.of.determination",
    "Coefficient.of.regression", "Deviation.mean.squares",
    "Environmental.variance","Genotypic.stability",
    "Genotypic.superiority.measure", "Variance.of.rank",
    "Stability.variance","Adjusted.coefficient.of.variation",
    "Ecovalence","Ecovalence.modified")
  res <- res[, nam.list]
  need.squared.si<-c("Environmental.variance",
                     "Ecovalence",
                     "Ecovalence.modified",
                     "Stability.variance",
                     "Genotypic.stability",
                     "Variance.of.rank",
                     "Deviation.mean.squares",
                     "Genotypic.superiority.measure"
                     )
  if (unit.correct==TRUE){
    res <- mutate_at(res,need.squared.si, sqrt)
  }

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
