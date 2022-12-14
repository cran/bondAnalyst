% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/27_computingYTC.R
\name{computingYTC}
\alias{computingYTC}
\title{Calculates Yield-To-Call (YTC).}
\usage{
computingYTC(couponPmt, callableVal, bondPv, maturityYears, ytcYears)
}
\arguments{
\item{couponPmt}{A number.}

\item{callableVal}{A number.}

\item{bondPv}{A number.}

\item{maturityYears}{A number.}

\item{ytcYears}{A number.}
}
\value{
Input values to five arguments  \code{couponPmt} ,\code{callableVal}, \code{bondPv}, \code{maturityYears}, and  \code{ytcYears}.
}
\description{
Calculates Yield-To-Call (YTC).
}
\details{
If a fixed-rate bond contains an embedded option, other yield measures are used. An embedded option is part of the security and cannot be removed and sold separately. For example, a callable bond contains an embedded call option that gives the issuer the right to buy the bond back from the investor at specified prices on pre-determined dates. The preset dates usually coincide with coupon payment dates after a call protection period. A call protection period is the time during which the issuer of the bond is not allowed to exercise the call option. Suppose that a seven-year, 8 percent annual coupon payment bond is first callable in four years. That gives the investor four years of protection against the bond being called. After the call protection period, the issuer might exercise the call option if interest rates decrease, or if the issuer’s credit quality improves. Those circumstances allow the issuer to refinance the debt at a lower cost of funds. The pre-set prices that the issuer pays if the bond is called often are at a premium above par. For example, the call schedule for this bond might be that it is first callable at 102 (per 100 of par value) on the coupon payment date in four years, callable at 101 in five years, and at par value on coupon payment dates thereafter.The yield-to-maturity on this seven-year, 8 percent callable bond is just one of several traditional yield measures for the investment. Others are yield-to-first-call, yield-to-second call, and so on. If the current price for the bond is 105 per 100 of par value, the yield-to-first call in four years is 6.975 percent (Adams & Smith, 2019).
In light of the information given, the method \code{computingYTC()} is developed to compute the bond's Yield-To-Call (YTC) for the values passed to its five arguments. Here, \code{couponPmt} is dollar value of the coupon payment, \code{callableVal} is dollar of at which the bond will be called at, \code{bondPv} represents present value or price of the bond, \code{maturityYears} is number of years to Maturity, and \code{ytcYears} is number of years to call.
}
\examples{
computingYTC (couponPmt=8,callableVal=102,bondPv=105,maturityYears=7,ytcYears=4)
computingYTC (couponPmt=8,callableVal=101,bondPv=105,maturityYears=7,ytcYears=5)
computingYTC (couponPmt=8,callableVal=100,bondPv=105,maturityYears=7,ytcYears=6)
computingYTC (couponPmt=8,callableVal=100,bondPv=105,maturityYears=7,ytcYears=7)
}
\references{
Adams,J.F. & Smith,D.J.(2019). Introduction to fixed-income valuation. In CFA Program Curriculum 2020 Level I Volumes 1-6. (Vol. 5, pp. 107-151). Wiley Professional Development (P&T). ISBN 9781119593577, \url{https://bookshelf.vitalsource.com/books/9781119593577}
}
\author{
MaheshP Kumar, \email{maheshparamjitkumar@gmail.com}
}
