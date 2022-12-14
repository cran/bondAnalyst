#'Calculates Yearly Forward Rates using the given Spot Rates.
#'@details
#'According to information provided by Frank J. Fabozzi (2008), the method \code{forwards()} is developed to calculate Forward Rates using the given Spot Rates. Here, \code{spots} is vector of given spot rates, \code{yrsFRbegins} is year in which Forward Rate begins (for example, a value of 1 would mean 1 year from now, and value of 2 would means two years from now),  \code{yrsFRapplies} means number of years for which the Forward Rate Applies (for example, a value for 1 means for 1 year and \code{yrsFRapplies} value of 4 means for four years). So, \code{yrsFRbegins=2} and \code{yrsFRapplies=1} means computing implied Forward Rate of 2 years from now, for 1 year. This is also called as 1 year forward rate 2 years into future. Lastly, \code{t} is a vector of number of years ranging from 1 to any specified number of years for which the Spot Rates are available, and  \code{n} is number of years under consideration.
#'For understanding the value of output, it is to be noted that in the first example, an output value of 0.1404 means 1 year from now, for 1 year the implied forward rate works out to be 14.04 percent. In the second example, an output value of 0.2124 means 2 years from now for 1 year the implied forward rate works out to be 21.24 percent (this is also called as 1 year forward rate, 2 years into future).In the third example, an output value of 0.2748 means 3 years from now for 1 year the implied forward rate works out to be 27.48 percent (this is also called as 1 year forward rate, 3 years into future).
#'@param spots A vector.
#'@param yrsFRbegins A number.
#'@param yrsFRapplies A number.
#'@param t A vector.
#'@param n A number.
#'@return Input values to five arguments  \code{spots} ,\code{yrsFRbegins},\code{yrsFRapplies}, \code{t} and \code{n}.
#'@importFrom Rdpack reprompt
#'@author MaheshP Kumar, \email{maheshparamjitkumar@@gmail.com}
#'@references
#'Fabozzi, F. J. (2008). Handbook of Finance: Financial Markets and Instruments. John Wiley & Sons.
#'@examples
#'forwards(spots=c(0.10,0.12,0.15,0.18,0.20,0.22,0.24,0.30),yrsFRbegins=1,yrsFRapplies=1,t=c(1:8),n=8)
#'forwards(spots=c(0.10,0.12,0.15,0.18,0.20,0.22,0.24,0.30),yrsFRbegins=2,yrsFRapplies=1,t=c(1:8),n=8)
#'forwards(spots=c(0.10,0.12,0.15,0.18,0.20,0.22,0.24,0.30),yrsFRbegins=3,yrsFRapplies=1,t=c(1:8),n=8)
#'forwards(spots=c(0.10,0.12,0.15,0.18,0.20,0.22,0.24,0.30),yrsFRbegins=4,yrsFRapplies=1,t=c(1:8),n=8)
#'forwards(spots=c(0.10,0.12,0.15,0.18,0.20,0.22,0.24,0.30),yrsFRbegins=5,yrsFRapplies=1,t=c(1:8),n=8)
#'forwards(spots=c(0.10,0.12,0.15,0.18,0.20,0.22,0.24,0.30),yrsFRbegins=6,yrsFRapplies=1,t=c(1:8),n=8)
#'forwards(spots=c(0.10,0.12,0.15,0.18,0.20,0.22,0.24,0.30),yrsFRbegins=1,yrsFRapplies=4,t=c(1:8),n=8)
#'forwards(spots=c(0.10,0.12,0.15,0.18,0.20,0.22,0.24,0.30),yrsFRbegins=3,yrsFRapplies=4,t=c(1:8),n=8)
#'forwards(spots=c(0.10,0.12,0.15,0.18,0.20,0.22,0.24,0.30),yrsFRbegins=5,yrsFRapplies=2,t=c(1:8),n=8)
#'@export
forwards<-function (spots,yrsFRbegins,yrsFRapplies,t, n){
  all = list(spots, t,n )
  if (!is.vector(t) | !is.numeric(t))
    stop("t represents number of payments must be 1 to number of years as a numeric vector.")
  if (any(t <= 0))
    stop("occurance of spots cannot have negative values in t.")
  if (length(spots) == 0 | length(t) == 0)
    stop("Insufficient information.")
  if (yrsFRbegins == 0 | yrsFRapplies == 0)
    stop("Enter valid Information.")
  if (length(spots) != length(t))
    stop("Number of SpotRates not equal to t.")
  computed_m_a_FR_t_b=((((1+spots[yrsFRbegins+yrsFRapplies])^(yrsFRbegins+yrsFRapplies))/((1+spots[yrsFRbegins])^yrsFRbegins))^(1/yrsFRapplies))-1
  (computed_m_a_FR_t_b = round(computed_m_a_FR_t_b, digits=4))
}


