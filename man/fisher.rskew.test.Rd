\name{fisher.rskew.test}
\alias{fisher.rskew.test}
\title{Perform a test for right skew p values between 0.00 and 0.05.}
\usage{
fisher.rskew.test(p)
}
\arguments{
  \item{p}{a vector of p values between 0.0 and 0.05
  (inclusive)}

  \item{...}{additional expressions passed to
  fisher.method() function from the MADAM package.}
}
\value{
This method returns a data frame containing the following
columns:

\item{S }{The test statistic from Fisher's test} \item{p
}{The p value from Fisher's test} \item{num.p }{The number
of p values used to compute the test statistic} \item{df
}{The number of degrees of freedom}
}
\description{
This function uses Fisher's test (implemented in
fisher.method () function of the MADAM package) to ask
whether the p values in the range 0.00 to 0.05 are more
right skewed than you would expect (i.e. bunched up towards
zero). This function replicates an analysis described in
Simonsohn et al (2013, see references). If you use it,
please cite that paper and the MADAM package (see
references).
}
\examples{
p <- c(0.00001, 0.024, 0.002, 0.045, 0.00003, 0.021, 0.0001, 0.0000948, 0.0000002)
fisher.rskew.test(p)
}
\references{
Simonsohn, Uri and Nelson, Leif D. and Simmons, Joseph P.,
P-Curve: A Key to the File Drawer (April 24, 2013). Journal
of Experimental Psychology: General, Forthcoming. Available
at SSRN: http://ssrn.com/abstract=2256237

Kugler KG, Mueller LA, Graber A: MADAM - An Open Source
Toolbox for Meta-Analysis. Source Code for Biology and
Medicine 2010, 5:3
}
\seealso{
\code{\link{binomial.all.test}}
}
\keyword{Fisher's}
\keyword{curve}
\keyword{method,}
\keyword{p}

