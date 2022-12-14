#'Calculates Par Rate using the given Spot Rates.
#'@details
#'As explained by Adams and Smith (2019), suppose the spot rates on government bonds are 5.263 percent for one year, 5.616 percent for two years, 6.359 percent for three years, and 7.008 percent for four years. These are effective annual rates. Based on these rated, the one-year par rate is 5.263 percent,the two-year par rate is 5.606 percent,and three-year and four-year par rates are 6.306 percent and 6.899 percent, respectively.
#'In light of the information given, the method  \code{computingParRate()} is developed to calculate Par Rate using the given Spot Rates. So,  \code{computingParRate()} gives Par Rate for values passed to its five arguments. Here, \code{spotRates} is vector of the given spot rates, \code{times} is a vector of number of years ranging from 1 to any specified number of years, \code{mv} is the Maturity Value (Future Value) is taken as 100, \code{pv} is the Present Value or the Price is also taken as 100 to signify the fact that bond is trading at par of the Future Value, and \code{n} is the number of years. The output is rounded off to three decimal places. For example, an output value of 6.899 means that the a rate of 6.899 percent will cause the bond to trade at par to its Maturity Value. In other words, par rate causes the market price of bond to be at par of its face value.
#'@param spotRates A vector.
#'@param times A vector.
#'@param mv A number.
#'@param pv A number.
#'@param n A number.
#'@return Input values to five arguments  \code{spotRates} , \code{times},\code{mv},\code{pv} and  \code{n}.
#'@importFrom Rdpack reprompt
#'@author MaheshP Kumar, \email{maheshparamjitkumar@@gmail.com}
#'@references
#'Adams,J.F. & Smith,D.J.(2019). Introduction to fixed-income valuation. In CFA Program Curriculum 2020 Level I Volumes 1-6. (Vol. 5, pp. 107-151). Wiley Professional Development (P&T). ISBN 9781119593577, <https://bookshelf.vitalsource.com/books/9781119593577>
#'@examples
#'computingParRate(spotRates=c(0.05263),times=c(1),mv=100,pv=100,n=1)
#'computingParRate(spotRates=c(0.05263,0.05616),times=c(1,2),mv=100,pv=100,n=2)
#'computingParRate(spotRates=c(0.05263,0.05616,0.06359),times=c(1,2,3), mv=100,pv=100,n=3)
#'computingParRate(spotRates=c(0.05263,0.05616,0.06359,0.07008),times=c(1,2,3,4),mv=100,pv=100,n=4)
#'computingParRate(spotRates=c(0.10,0.12,0.13,0.14,0.19),times=c(1,2,3,4,5),mv=100,pv=100,n=5)
#'@export
computingParRate<-function (spotRates,times,mv, pv, n)
{
  all = list(spotRates, times,n )
    if (!is.vector(times) | !is.numeric(times))
    stop("times represents number of payments must be 1 to number of years as a numeric vector.")
  if (any(times <= 0))
    stop("occurance of spotRates cannot have negative values in times.")
  if (length(spotRates) == 0 | length(times) == 0)
    stop("Insufficient information.")
  if (length(spotRates) != length(times))
    stop("Number of SpotRates not equal to times.")
  parRate_numerator <- (1- (1/((1+spotRates[n])^n)))
  parRate_denominator <- sum((1/((1 + spotRates)^times)))
  ParRate_val<- parRate_numerator/parRate_denominator
  ParRate_val<- ParRate_val *100
  (ParRate_val = round(ParRate_val, digits=3))
}

