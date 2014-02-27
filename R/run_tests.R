library('testthat')

source('binomial.p.test.R')
source('p.check.R')
source('p.to.pp.R')
source('pcurve.fisher.test.R')

test_dir('tests', reporter = 'Summary')
