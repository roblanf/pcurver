
test_that("binomial_p_test works", {

	# This is the example given in this ms: 
	# http://papers.ssrn.com/sol3/papers.cfm?abstract_id=2256237
	p1 <- c(0.001, 0.002, 0.04)
	expect_that(pcurve.fisher.test(p1)$p.value, equals(0.02265281, tolerance=1e06))

	# a few more examples, computed by hand to test
	p2 <- c(0.01, 0.02, 0.03, 0.04, 0.05, 0.05, 0.05, 0.049, 0.049)
	expect_that(pcurve.fisher.test(p2)$p.value, equals(0.9930869, tolerance=1e06))

})