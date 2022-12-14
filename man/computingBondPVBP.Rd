% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/d12_computingPVBP.R
\name{computingBondPVBP}
\alias{computingBondPVBP}
\title{Calculates Price Value of a Basis Point (PVBP) for the Bond.}
\usage{
computingBondPVBP(pvPlus, pvMinus)
}
\arguments{
\item{pvPlus}{A number.}

\item{pvMinus}{A number.}
}
\value{
Input values to two arguments  \code{pvPlus} and \code{pvMinus}.
}
\description{
Calculates Price Value of a Basis Point (PVBP) for the Bond.
}
\details{
According to information provided by Adams and Smith (2019), the method \code{computingBondPVBP()} is developed to compute Price Value of a Basis Point (PVBP) for the Bond.
}
\examples{
computingBondPVBP(pvPlus=100.594327,pvMinus=100.765123)
}
\references{
Adams,J.F. & Smith,D.J.(2019). Understanding Fixed‑Income Risk and Return. In CFA Program Curriculum 2020 Level I Volumes 1-6. (Vol. 5, pp. 237-299). Wiley Professional Development (P&T). ISBN 9781119593577, \url{https://bookshelf.vitalsource.com/books/9781119593577}
}
\author{
MaheshP Kumar, \email{maheshparamjitkumar@gmail.com}
}
