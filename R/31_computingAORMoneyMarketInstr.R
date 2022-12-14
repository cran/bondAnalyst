#'Calculates Add-on Rate (AOR) of Money Market Instruments.
#'@details
#'According to Adams and Smith (2019), bond yields-to-maturity are annualized and compounded. Yield measures in the money market are annualized but not compounded. Instead, the rate of return on a money market instrument is stated on a simple interest basis. In general, quoted money market rates are either discount rates or add-on rates. Although market conventions vary around the world, commercial paper, Treasury bills (a US government security issued with a maturity of one year or less), and bankers`’` acceptances are often quoted on a discount rate basis. Bank certificates of deposit (repos) and indexes, such as Libor and Euribor, are quoted on an add-on rate basis.
#'In light of the available information, the method \code{computingAORMoneyMarketInstr()} is developed to compute Add-on Rate (AOR) of Money Market Instruments for the values passed to its four arguments. Here, \code{pvMmi} is present value of the Money Market Instrument, \code{fvMmi} is future value of the Money Market Instrument (that is sale price and not the redemption amount), \code{daysToMaturity} is number of days till the maturity, and \code{daysInYear} is taken to be 365. For example, an output  value of 0.04934 means that the rate of return, stated on a 365-day add-on rate basis, is 4.934 percent.
#'@param pvMmi A number.
#'@param fvMmi A number.
#'@param daysToMaturity A number.
#'@param daysInYear A number.
#'@return Input values to four arguments  \code{pvMmi} ,\code{fvMmi}, \code{daysToMaturity} and  \code{daysInYear}.
#'@importFrom Rdpack reprompt
#'@author MaheshP Kumar, \email{maheshparamjitkumar@@gmail.com}
#'@references
#'Adams,J.F. & Smith,D.J.(2019). Introduction to fixed-income valuation. In CFA Program Curriculum 2020 Level I Volumes 1-6. (Vol. 5, pp. 107-151). Wiley Professional Development (P&T). ISBN 9781119593577, <https://bookshelf.vitalsource.com/books/9781119593577>
#'@examples
#'computingAORMoneyMarketInstr(pvMmi=10000000,fvMmi=10060829,daysToMaturity=45,daysInYear=365)
#'@export
computingAORMoneyMarketInstr <- function(pvMmi,fvMmi,daysToMaturity,daysInYear){
aor_val=(daysInYear/daysToMaturity)*((fvMmi-pvMmi)/pvMmi)
(aor_val = round(aor_val, digits=5))
}



