library('testthat')

source('p.check.R')
source('p.to.pp.R')
source('pcurve.binomial.test.R')
source('pcurve.fisher.test.R')

test_dir('tests', reporter = 'Summary')
