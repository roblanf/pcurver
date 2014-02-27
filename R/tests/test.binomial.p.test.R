
test_that("binomial.p.test works", {

	# This is the example given in this ms: 
	# http://papers.ssrn.com/sol3/papers.cfm?abstract_id=2256237
	p1 <- c(0.0, 0.0, 0.0, 0.0, 0.0)
	expect_that(binomial.p.test(p1)$'Number of p values >0.025', equals(0.0))
	expect_that(binomial.p.test(p1)$'Number of p values <0.025', equals(5.0))
	expect_that(binomial.p.test(p1)$'p value for right skew', equals(0.03125))
	expect_that(binomial.p.test(p1)$'p value for left skew', equals(1.00000))

	# This is the counter example, just for completeness	
	p2 <- c(0.05, 0.05, 0.05, 0.05, 0.05)
	expect_that(binomial.p.test(p2)$'Number of p values >0.025', equals(5.0))
	expect_that(binomial.p.test(p2)$'Number of p values <0.025', equals(0.0))
	expect_that(binomial.p.test(p2)$'p value for right skew', equals(1.00000))
	expect_that(binomial.p.test(p2)$'p value for left skew', equals(0.03125))

	# This one should return 1, because no data
	p3 <- c(0.025, 0.025, 0.025, 0.025, 0.025, 0.025)
	expect_that(binomial.p.test(p3)$'Number of p values >0.025', equals(0.0))
	expect_that(binomial.p.test(p3)$'Number of p values <0.025', equals(0.0))
	expect_that(binomial.p.test(p3)$'p value for right skew', equals(1.0000))
	expect_that(binomial.p.test(p3)$'p value for left skew', equals(1.0000))

})