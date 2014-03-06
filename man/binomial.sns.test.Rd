\name{binomial.sns.test}
\alias{binomial.sns.test}
\title{Perform two one-tailed sign tests on p values between 0.00 and 0.05.}
\usage{
binomial.sns.test(p)
}
\arguments{
  \item{p}{a vector of p values between 0.0 and 0.05
  (inclusive)}
}
\value{
a list giving the number of p values in each bin, and the p
values of the two one-tailed sign tests.
}
\description{
This function replicates the analysis described in
Simonsohn et al (2013, see references), and is named for
the three authors of that paper. If you use it, please cite
that paper (see references). It performs two symmetrical
one-tailed sign tests. We do not recommend its use, because
doing two one-tailed tests will double your false-positive
rate compared to doing a single two- tailed test. So rather
than use this test, you should use
\code{\link{binomial.all.test}}.
}
\examples{
p <- c(0.00001, 0.024, 0.002, 0.045, 0.00003, 0.021, 0.0001, 0.0000948, 0.0000002)
binomial.sns.test(p)
}
\references{
Simonsohn, Uri and Nelson, Leif D. and Simmons, Joseph P.,
P-Curve: A Key to the File Drawer (April 24, 2013). Journal
of Experimental Psychology: General, Forthcoming. Available
at SSRN: http://ssrn.com/abstract=2256237
}
\seealso{
\code{\link{binomial.bias.test}},
\code{\link{binomial.all.test}}
}
\keyword{binomial,}
\keyword{curve}
\keyword{p}

