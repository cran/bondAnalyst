#'Calculates the accrued interest with actual-by-actual day convention.
#'@details
#'According to Adam and Smith(2019), when a bond is between coupon payment dates, its price has two parts: the first part is the flat price (PVFlat) and the other is accrued interest (AI). The sum of these two parts is the full price (PVFull), which also is called the invoice price or dirty price. The flat price, which is the full price minus the accrued interest, is also called the quoted price or clean price. The flat price usually is quoted by bond dealers. If a trade takes place, the accrued interest is added to the flat price to obtain the full price paid by the buyer and received by the seller on the settlement date. The settlement date is the date when the bond buyer makes cash payment, and the seller delivers the security. The reason for using the flat price for quotation is to avoid misleading investors about the market price trend for the bond. If the full price were to be quoted by dealers, investors would see the price rise day after day even if the yield-to-maturity did not change. That is because the amount of accrued interest increases each day. Then, after the coupon payment is made, the quoted price would drop dramatically. This is why, using the flat price for quotation avoids that misrepresentation.
#'So, accrued interest is the proportional share of the next coupon payment. Assume that the coupon period has (T) days between payment dates and that (t) days have gone by since the last payment. There are different conventions used in bond markets to count days. The two most common day-count conventions are actual-by-actual and 30-by-360. For the actual-by-actual method, the actual number of days is used, including weekends, holidays, and leap days. For example, a semi-annual payment bond pays interest on 15 May and 15 November of each year. The accrued interest for settlement on 27 June would be the actual number of days between 15 May and 27 June (t = 43 days) divided by the actual number of days between 15 May and 15 November (T = 184 days), times the coupon payment. If the stated coupon rate is 4.375 percent, the accrued interest is 0.511209 per 100 of par value. Day-count conventions vary from market to market. However, actual-by-actual is most common for government bonds (Adams & Smith, 2019).
#'Based on this, the method  \code{aiActDtCon()} is developed to compute the accrued interest with actual-by-actual day convention. So, \code{aiActDtCon()} gives accrued interest with actual-by-actual day convention for values passed to its four arguments. Here, \code{cPmt} is dollar value of Coupon Payment, \code{dt1} is date of first semi-annual interest payment, \code{dt2} is date of second semi-annual interest payment, and \code{stDt} is Settlement Date. The output is rounded off to six decimal places. The given examples show various ways in which the arguments can be passed to \code{aiActDtCon()}.
#'@param cPmt A number.
#'@param dt1 A character.
#'@param dt2 A character.
#'@param stDt A character.
#'@return Input values to four arguments  \code{r} , \code{dt1}  \code{dt2} and \code{stDt}.
#'@importFrom Rdpack reprompt
#'@author MaheshP Kumar, \email{maheshparamjitkumar@@gmail.com}
#'@references
#'Adams,J.F. & Smith,D.J.(2019). Introduction to fixed-income valuation. In CFA Program Curriculum 2020 Level I Volumes 1-6. (Vol. 5, pp. 107-151). Wiley Professional Development (P&T). ISBN 9781119593577, <https://bookshelf.vitalsource.com/books/9781119593577>
#'@examples
#'aiActDtCon(cPmt=4.375,dt1=as.Date("2019-5-15"),dt2=as.Date("2019-11-15"),stDt=as.Date("2019-6-27"))
#'aiActDtCon(4.375, as.Date("2019-5-15"), as.Date("2019-11-15"),as.Date("2019-6-27"))
#'@export
aiActDtCon <- function(cPmt,dt1,dt2,stDt){
  daysFromLastCoupon = stDt - dt1
    daysBtwCoupons = dt2 - dt1
   acruredInterest <- (cPmt/2)*(as.numeric(daysFromLastCoupon)/as.numeric(daysBtwCoupons))
  (acruredInterest = round(acruredInterest, digits=6))
}


