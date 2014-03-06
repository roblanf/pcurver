\name{binomial.bias.test}
\alias{binomial.bias.test}
\title{Perform a one-tailed sign test on p values between 0.03 and 0.05.}
\usage{
binomial.bias.test(p)
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
This function tests the null hypothesis that there are at
least as many p values in the 0.03 - 0.04 bin as in the
0.04 - 0.05 bin. Note that p values of exactly 0.04 are
excluded, because they do not fall in either bin. The test
uses a one-tailed sign test. Significantly more p values in
the smaller bin is consistent with collections of p values
with evidential value. Significantly more p values in the
larger bin is consistent with p-hacking or publication
bias. This is a more sensitive test of p-hacking or
publication bias than the related
\code{\link{binomial.all.test}}
}
\examples{
# here are some p values you might get from a strong effect size
p <- rexp(1000, 200)
p <- p[p<0.05]

# let's add some you might get from p-hacking and/or publication bias
h <- -1 * rexp(100, 200) + 0.05
h <- h[h>0.00]

p <- c(p, h)

# the binomial.all.test should show significant right skew
# that's expected - it uses all the data from 0.00 to 0.05
binomial.all.test(p)

# the binomial.bias.test is more sensitive to p-hacking and/or
# publication bias - it uses just the data from 0.03 to 0.05
binomial.bias.test(p)
}
\seealso{
\code{\link{binomial.all.test}},
\code{\link{binomial.sns.test}}
}
\keyword{binomial,}
\keyword{curve}
\keyword{p}

