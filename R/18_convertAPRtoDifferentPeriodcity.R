#'Converting an Annual Percentage Rate (APR) from a periodicity of 2 to another periodicity of 4, 12, or 1.
#'@details
#'There are many ways to measure the rate of return on a fixed-rate bond investment. In general, an annualized and compounded yield on a fixed-rate bond depends on the assumed number of periods in the year which is called the periodicity of the annual rate. Typically, the periodicity matches the frequency of coupon payments. A bond that pays semi-annual coupons has a stated annual yield-to-maturity for a periodicity of two—the rate per semi-annual period times two A bond that pays quarterly coupons has a stated annual yield for a periodicity of four—the rate per quarter times four. It is always important to know the periodicity of a stated annual rate. The most common periodicity for USD-denominated bond yields is two because most bonds in the USD market make semi-annual coupon payments. An annual rate having a periodicity of two is known as a semi-annual Bond Basis Yield (BBY), or semi-annual Bond Equivalent Yield (BEY). An important tool used in fixed-income analysis is to convert an annual yield from one periodicity to another. These are called periodicity, or compounding, conversions. To compare this bond with others, an analyst converts this annualized yield-to-maturity to quarterly and monthly compounding (Adams & Smith, 2019).
#'The method \code{convertAPRtoDifferentPeriodcity()} is developed convert an Annual Percentage Rate (APR) from a given periodicity of say m = 2 to another desired periodicity say of n = 4 or n = 12 or n=1.
#'For Example, it converts 4.96 percent from a periodicity of two to a periodicity of four or converts 4.93 percent from a periodicity of four to a periodicity of two or converts 4.96 percent from a periodicity of two to a periodicity of one and so, on.
#'So, \code{convertAPRtoDifferentPeriodcity()} gives Annual Percentage Rate of the desired periodicity for values passed to its three arguments. Here, \code{givenAPR} represents given Annual Percentage Rate, \code{givenPeriodicity} represents given periodicity, and \code{desiredPeriodicity} desired periodicity for which the Annual Percentage Return is to be computed. The output is rounded off to four decimal places.
#'@param givenAPR A number.
#'@param givenPeriodicity A number.
#'@param desiredPeriodicity A number.
#'@return Input values to three arguments  \code{givenAPR} , \code{givenPeriodicity} and \code{desiredPeriodicity}.
#'@importFrom Rdpack reprompt
#'@author MaheshP Kumar, \email{maheshparamjitkumar@@gmail.com}
#'@references
#'Adams,J.F. & Smith,D.J.(2019). Introduction to fixed-income valuation. In CFA Program Curriculum 2020 Level I Volumes 1-6. (Vol. 5, pp. 107-151). Wiley Professional Development (P&T). ISBN 9781119593577, <https://bookshelf.vitalsource.com/books/9781119593577>
#'@examples
#'convertAPRtoDifferentPeriodcity(givenAPR=0.0496,givenPeriodicity=2,desiredPeriodicity=4)
#'convertAPRtoDifferentPeriodcity(givenAPR=0.0493,givenPeriodicity=4,desiredPeriodicity=2)
#'convertAPRtoDifferentPeriodcity(givenAPR=0.0496,givenPeriodicity=2,desiredPeriodicity=1)
#'convertAPRtoDifferentPeriodcity(givenAPR=0.1063,givenPeriodicity=2,desiredPeriodicity=4)
#'convertAPRtoDifferentPeriodcity(givenAPR=0.1049,givenPeriodicity=4,desiredPeriodicity=2)
#'convertAPRtoDifferentPeriodcity(givenAPR=0.1063,givenPeriodicity=2,desiredPeriodicity=1)
#'@export
convertAPRtoDifferentPeriodcity <- function(givenAPR,givenPeriodicity,desiredPeriodicity){
  APRdesiredPeriodicity <- (((1+givenAPR/givenPeriodicity)^givenPeriodicity)^(1/desiredPeriodicity)-1)*desiredPeriodicity
  (APRdesiredPeriodicity= round(APRdesiredPeriodicity, digits=4))
}

