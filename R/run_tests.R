library('testthat')
library('MADAM')

source('p.check.R')
source('limits.check.R')
source('p.to.pp.R')
source('pcurve.binomial.test.R')
source('pcurve.fisher.test.R')
source('pcurve.plot.R')

test_dir('tests', reporter = 'Summary')
