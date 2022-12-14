#'Calculates Present Value or the Price of the Bond paying Annual Coupons.
#'@details
#'As mentioned by Adams and Smith (2019), on a traditional (option-free) fixed-rate bond, the promised future cash flows consist of two things: one is a coupon interest payment and another is repayment of the full principal at maturity. The coupon payments are on regularly scheduled dates, for example, an annual payment bond might pay interest on 15 June of each year for five years. The final coupon typically is paid together with the full principal on the maturity date. So, the price of the bond is the present value of the remaining cash flows discounted at the Market Discount Rate. The market discount rate is the rate of return required by investors given the risk of the investment in the bond. It is also called the required yield, or the required rate of return. For example, suppose the coupon rate on a bond is 4 percent and the payment is made once a year. If the time-to-maturity is five years and the market discount rate is 6 percent, the price of the bond is 91.575 per 100 of par value. The par value is the amount of principal on the bond. The price of the bond is the sum of the present values of the five coupon payments and maturity value. The price per 100 of par value may be interpreted as the percentage of par value. If the par value is USD100,000, the coupon payments are USD4,000 each year and the price of the bond is USD91,575. Its price is 91.575 percent of par value.
#'Based on the information given, the method  \code{bondPriceYearlyCoupons()} is developed to compute the price of the bond which is present value or discounted value of the coupon payments promised by the issuer and the Par Value of the Bond. So,  \code{bondPriceYearlyCoupons()} gives Price of the bond for values passed to its five arguments. Here \code{couponPmt} is the vector that has dollar values of Coupon Payments for periods till maturity, \code{times} is a vector of number of years ranging from 1 to any specified number of years till maturity, \code{bondMaturityVal} is amount of Par Value or Maturity Value of the Bond, \code{n} represents number of years till maturity and \code{r} is Market Discount Rate or Required Rate of return. The output is rounded off to three decimal places.
#'@param couponPmt A vector.
#'@param times A vector.
#'@param bondMaturityVal A number.
#'@param n A number.
#'@param r A number.
#'@return Input values to five arguments  \code{couponPmt} , \code{times}, \code{bondMaturityVal},\code{n} and \code{r}.
#'@importFrom Rdpack reprompt
#'@author MaheshP Kumar, \email{maheshparamjitkumar@@gmail.com}
#'@references
#'Adams,J.F. & Smith,D.J.(2019). Introduction to fixed-income valuation. In CFA Program Curriculum 2020 Level I Volumes 1-6. (Vol. 5, pp. 107-151). Wiley Professional Development (P&T). ISBN 9781119593577, <https://bookshelf.vitalsource.com/books/9781119593577>
#'@examples
#'bondPriceYearlyCoupons(couponPmt=c(4,4,4,4,4), times=c(1,2,3,4,5),bondMaturityVal=100,n=5,r=0.06)
#'bondPriceYearlyCoupons(couponPmt=c(4,4,4,4,4), times=c(1:5),bondMaturityVal=100,n=5,r=0.06)
#'bondPriceYearlyCoupons(couponPmt=c(rep(4,5)), times=c(1:5), bondMaturityVal=100,n=5,r=0.06)
#'bondPriceYearlyCoupons(c(rep(4,5)), c(1:5), 100,5,0.06)
#'@export
bondPriceYearlyCoupons<-function (couponPmt, times,bondMaturityVal,n, r)
  {
    all = list(couponPmt, times, r)
    if (!is.vector(couponPmt) | !is.numeric(couponPmt))
      stop("Note:coupon payments must be a numeric values.")
    if (any(times <= 0))
      stop("Note: occurance of couponPmt cannot have negative values in times.")
    if (r < 0)
      stop("Note: Market Discount rate (r) cannot be negative.")
    if (length(couponPmt) == 0 | length(times) == 0)
      stop("Note: Please enter Complete Information.")
    if (length(couponPmt) != length(times))
      stop("Note: Number of Coupon Payments not equal to times.")
    pv_coupon <- sum(couponPmt/(1 + r)^times)
    pv_bondMaturityVal <- bondMaturityVal/(1 + r)^n
    bond_price <- pv_coupon + pv_bondMaturityVal
    (bond_price= round(bond_price, digits=3))
  }


