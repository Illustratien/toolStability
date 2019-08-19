#' @title Table stability
#'
#' @description
#' \code{table_stability} export all the stability indicies in the package.
#'
#' @keywords static, dynamic, regression and nonparametric approach
#'
#' @details hahahahahahahahahhahahahahahahahhahahaha
#'
#' @seealso \code{link{adjusted_coefficient_of_variaiton}}
#' @seealso \code{link{coefficient_of_determination}}
#' @seealso \code{link{coefficient_of_regression}}
#' @seealso \code{link{deviation_mean_squares}}
#' @seealso \code{link{ecovalence}}
#' @seealso \code{link{environmental_variance}}
#' @seealso \code{link{genotypic_stability}}
#' @seealso \code{link{genotypic_superiority_measure}}
#' @seealso \code{link{mean_rank_difference}}
#' @seealso \code{link{stability_variance}}
#' @seealso \code{link{variance_of_rank}}
#'
#' @param data a dataframe containing trait, genotype and environment.
#' @param trait colname of a column containing a numeric vector of interested trait to be analysized.
#' @param genotype colname of a column containing a character or factor vector labeling different genotypic varieties
#' @param environment colname of a column containing a character or factor vector labeling different environments
#' @param lambda threshold value of trait that define stability.
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
#'
#' @importFrom dplyr group_by summarise mutate
#' @importFrom data.table data.table
#' @importFrom Rdpack reprompt
#'
#' @export
#'
#' @examples
#' data(Data)
#' table.stability <- table_stability(Data$Yield,Data$Genotype,Data$Environment)
#'
table_stability<- function(data,trait,genotype,environment,lambda){
  if(!is.numeric(data[[trait]])){stop('Trait must be a numeric vector')}
  # combine vectors into data table
  Data <- data.table(X=data[[trait]],Genotype=data[[genotype]],Environment=data[[environment]])  # overall mean of X
  X..bar <- mean(data[[trait]])
  G <- length(unique(data[[genotype]]))

  # for calculating mean.rank.difference
  abs.dev.sum <- function(x){
    # calculating sum of absolute difference
    # for every combination of j< j'
    res <- c()
    n <- length(x)
    for (i in seq(1,n-1)){
      res <- sum(res,abs(x[(i+1):n]-x[i]))
    }
    res <- res*2/(n*(n-1))
    return(res)
  }

  # calculate doefficient determination
  res <-mutate(
    group_by(
        mutate(
          group_by(
              mutate(
                group_by(
                  Data,Genotype),
                corrected.X=X-mean(X)+X..bar),
            Environment),          # for each environment
          Xj.bar=mean(X),
          Xj.max=max(X),
          corrected.rank=rank(-corrected.X,na.last="keep", ties.method="min")),# first calculate environmental mean
      Genotype),                               # for each genotype
    Xi.bar=mean(X),                            # then calculate genotypic mean
    E=length(X),                               # number of environment
    s2d1=((X-Xi.bar-Xj.bar+X..bar)^2)/(E-2),
    s2d2=((Xj.bar-X..bar)^2)/(E-2),
    Bi1=(X-Xi.bar-Xj.bar+X..bar)*(Xj.bar-X..bar),
    Bi2=(Xj.bar-X..bar)^2,
    deviation=((X-Xi.bar)^2)/(E-1),
    sqr=(X-Xi.bar-Xj.bar+X..bar)^2,
    sqr1=((X-Xi.bar-Xj.bar+X..bar)^2)/(E-1),
    Bi=1+(sum(Bi1,na.rm=TRUE)/sum(Bi2,na.rm=TRUE)),
    s2di=sum(s2d1,na.rm=TRUE)-((Bi-1)^2)*sum(s2d2,na.rm=TRUE),
    s2xi=sum(deviation,na.rm=TRUE),
    coefficient.determination=1-(s2di/s2xi),
    coefficient.regression=1+sum(Bi1,na.rm=TRUE)/sum(Bi2,na.rm=TRUE),
    deviation.mean.squares=sum(s2d1,na.rm=TRUE)-((Bi-1)^2)*sum(s2d2,na.rm=TRUE),
    environmental.variance = sum(deviation, na.rm=TRUE),
    bmin=sum(Bi,na.rm=TRUE),
    genotypic.stability=sum(X-Xi.bar-bmin*Xj.bar+bmin*X..bar),
    genotypic.superiority.measure=sum(X-Xj.max)^2/(2*length(X)),
    mean.rank=mean(corrected.rank),
    variance.of.rank=sum((corrected.rank-mean.rank)^2/(length(X)-1)),
    wi= sum(sqr, na.rm=TRUE),
    stability.variance=sqrt((G1*(G1-1)*wi-sum(wi,na.rm=TRUE))/((E1-1)*(G1-2)*(G1-2))),
    mean.rank.difference= abs.dev.sum(corrected.rank))

  b= sum((res$Xi.logmean-mean(res$Xi.logmean))*(res$Xi.logvar-mean(res$Xi.logvar)))/sum((res$Xi.logmean-mean(res$Xi.logmean))^2)
  res$adjusted.coefficient.correlation=100*(1/res$Xi.bar)*sqrt(10^(((2-b)*res$Xi.logmean)+((b-2)*(mean(res$Xi.logmean)))+res$Xi.logvar))

nam.list <- c('coefficient.determination','coefficient.regression','deviation.mean.squares','environmental.variance',
              'genotypic.stability','genotypic.superiority.measure','variance.of.rank','stability.variance','mean.rank.difference',
              'adjusted.coefficient.correlation','ecovalence')
  return(res[,nam.list])
}
