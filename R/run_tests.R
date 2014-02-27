library('testthat')

source('binomial_p_test.R')
source('p_check.R')
source('p_to_pp.R')
source('pcurve.fisher.test.R')

test_dir('tests', reporter = 'Summary')
