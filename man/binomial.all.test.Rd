\name{binomial.all.test}
\alias{binomial.all.test}
\title{Perform a two-tailed sign test on p values between 0.00 and 0.05.}
\usage{
binomial.all.test(p)
}
\arguments{
  \item{p}{a vector of p values between 0.0 and 0.05
  (inclusive)}
}
\value{
a list giving the number of p values in each bin, and the p
value of the two-tailed sign test.
}
\description{
This function tests the null hypothesis that there are an
equal number of p values in two bins: 0.0<=p<0.025, and
0.025<p<=0.05. Note that p values of exactly 0.025 are
excluded, because they do not fall in either bin. The test
uses a two-tailed sign test. A significant result indicates
that you can reject the null hypothesis of an equal number
of p values in each bin. Significantly more p values in the
smaller bin is expected for collections of p values that
contain evidential value. Significantly More p values in
the larger bin is consistent with intensive p hacking or
publication bias.
}
\examples{
p <- c(0.00001, 0.024, 0.002, 0.045, 0.00003, 0.021, 0.0001, 0.0000948, 0.0000002)
binomial.all.test(p)
}
\seealso{
\code{\link{binomial.bias.test}},
\code{\link{binomial.sns.test}}
}
\keyword{binomial,}
\keyword{curve}
\keyword{p}

