#'Calculates Discounted Value of Coupon Payments of the Bond using Market Discount Rate or the Required Rate of Return.
#'@details
#'Bonds are a type of financial instrument in which the issuer under the contractual obligation agrees to pay the interest on the money borrowed from the investor and repay the amount borrowed through the issued bond. Bonds are also known as fixed-income securities because there is a contractual obligation for the issuer to pay interest at the specified Coupon Rate. This Coupon Rate is also called the nominal rate or the contract rate. Although Coupon is specified as a percentage; however, it is the dollar value of the Coupon Payment that is computed by multiplying the coupon rate with the par value of the debt instrument. For example, the interest payment for a debt instrument with a 5 percent coupon rate and a par value of $1,000 is $50 (5 percent times $1,000). The frequency of interest payments varies by the type of debt instrument. In the United States, the usual practice for bonds is for the issuer to pay the coupon interest in two semi-annual installments. Mortgage-backed securities and asset-backed securities typically pay interest monthly. For bonds issued in some markets outside the United States,Coupon Payments are made only once per year. Loan interest payments can be customized in any manner (Fabozzi, 2008).
#'As further described by Adams and Smith (2019), an investor in a fixed-Rate bond receives the promised coupon that is the main source of return. Coupon Payments are based on the coupon rate and discounting is done at the market discount rate, required yield, or required rate of return.
#'Based on this, the method  \code{disCouponPmtsBond()} is developed to compute the present value or discounted value of the coupon payments promised by the issuer. So,  \code{disCouponPmtsBond()} gives the discounted value of Coupon Payments to be received by the Investor till the maturity of the bond for values passed to its three arguments. Here \code{couponPmt} is the vector that has dollar values of Coupon Payments for periods till maturity, \code{times} is a vector of number of years ranging from 1 to any specified number of years till maturity and \code{r} is Market Discount Rate or Required Rate of return. The output is rounded off to three decimal places.
#'@param couponPmt A vector.
#'@param times A vector.
#'@param r A number.
#'@return Input values to three arguments  \code{couponPmt} , \code{times} and \code{r}.
#'@importFrom Rdpack reprompt
#'@author MaheshP Kumar, \email{maheshparamjitkumar@@gmail.com}
#'@references
#'Adams,J.F. & Smith,D.J.(2019). Introduction to fixed-income valuation. In CFA Program Curriculum 2020 Level I Volumes 1-6. (Vol. 5, pp. 107-151). Wiley Professional Development (P&T). ISBN 9781119593577, <https://bookshelf.vitalsource.com/books/9781119593577>
#'@references
#'\insertRef{fifa}{bondAnalyst}

#'@examples
#'disCouponPmtsBond(couponPmt=c(4,4,4,4,4), times=c(1,2,3,4,5), r=0.06)
#'disCouponPmtsBond(couponPmt=c(4,4,4,4,4), times=c(1:5), r=0.06)
#'disCouponPmtsBond(couponPmt=c(rep(4,5)), times=c(1:5), r=0.06)
#'disCouponPmtsBond(c(rep(4,5)), c(1:5),0.06)
#'@export
disCouponPmtsBond <-function (couponPmt, times, r)
{
  all = list(couponPmt, times, r)
  if (!is.vector(couponPmt) | !is.numeric(couponPmt))
    stop("Note:coupon payments must be a numeric values.")
  if (!is.vector(times) | !is.numeric(times))
    stop("Note: times represents number of coupon payments must be 1 to number of years as a numeric vector.")
  if (any(times <= 0))
    stop("Note: occurance of couponPmt cannot have negative values in times.")
 if (r < 0)
    stop("Note: Market Discount rate (r) cannot be negative.")
  if (length(couponPmt) == 0 | length(times) == 0)
    stop("Note: Please enter Complete Information.")
  if (length(couponPmt) != length(times))
    stop("Note: Number of Coupon Payments not equal to times.")
pv_couponPmt <- sum(couponPmt/(1 + r)^times)
(pv_couponPmt = round(pv_couponPmt, digits=3))
 }


