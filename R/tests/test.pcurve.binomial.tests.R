
test_that("binomial.sns.test works", {

	test.function <- binomial.sns.test

	# This is the example given in this ms: 
	# http://papers.ssrn.com/sol3/papers.cfm?abstract_id=2256237
	p1 <- c(0.0, 0.0, 0.0, 0.0, 0.0)
	expect_that(test.function(p1)$'Number of p values >0.025', equals(0.0))
	expect_that(test.function(p1)$'Number of p values <0.025', equals(5.0))
	expect_that(test.function(p1)$'p value for right skew', equals(0.03125))
	expect_that(test.function(p1)$'p value for left skew', equals(1.00000))

	# This is the counter example, just for completeness	
	p2 <- c(0.05, 0.05, 0.05, 0.05, 0.05)
	expect_that(test.function(p2)$'Number of p values >0.025', equals(5.0))
	expect_that(test.function(p2)$'Number of p values <0.025', equals(0.0))
	expect_that(test.function(p2)$'p value for right skew', equals(1.00000))
	expect_that(test.function(p2)$'p value for left skew', equals(0.03125))

	# This one should return 1, because no data
	p3 <- c(0.025, 0.025, 0.025, 0.025, 0.025, 0.025)
	expect_that(test.function(p3)$'Number of p values >0.025', equals(0.0))
	expect_that(test.function(p3)$'Number of p values <0.025', equals(0.0))
	expect_that(test.function(p3)$'p value for right skew', equals(1.0000))
	expect_that(test.function(p3)$'p value for left skew', equals(1.0000))

})

test_that("binomial.all.test works", {

	test.function <- binomial.all.test

	p1 <- c(0.0, 0.0, 0.0, 0.0, 0.0)
	expect_that(test.function(p1)$'Number of p values >0.025', equals(0.0))
	expect_that(test.function(p1)$'Number of p values <0.025', equals(5.0))
	expect_that(test.function(p1)$'p value', equals(0.0625))

	# This is the counter example, just for completeness	
	p2 <- c(0.05, 0.05, 0.05, 0.05, 0.05)
	expect_that(test.function(p2)$'Number of p values >0.025', equals(5.0))
	expect_that(test.function(p2)$'Number of p values <0.025', equals(0.0))
	expect_that(test.function(p2)$'p value', equals(0.0625))

	# This one should return 1, because no data
	p3 <- c(0.025, 0.025, 0.025, 0.025, 0.025, 0.025)
	expect_that(test.function(p3)$'Number of p values >0.025', equals(0.0))
	expect_that(test.function(p3)$'Number of p values <0.025', equals(0.0))
	expect_that(test.function(p3)$'p value', equals(1))

})


test_that("binomial.bias.test works", {

	test.function <- binomial.bias.test

	p1 <- c(0.0, 0.0, 0.0, 0.0, 0.0)
	expect_that(test.function(p1)$'Number of p values 0.03<p<0.04', equals(0.0))
	expect_that(test.function(p1)$'Number of p values 0.04<p<0.05', equals(0.0))
	expect_that(test.function(p1)$'One-tailed p value for evidence of publication bias and/or p-hacking', equals(1.0))

	p2 <- c(0.05, 0.05, 0.05, 0.05, 0.05)
	expect_that(test.function(p2)$'Number of p values 0.03<p<0.04', equals(0.0))
	expect_that(test.function(p2)$'Number of p values 0.04<p<0.05', equals(5.0))
	expect_that(test.function(p2)$'One-tailed p value for evidence of publication bias and/or p-hacking', equals(0.03125))

	p3 <- c(0.05, 0.05, 0.05, 0.05, 0.039, 0.05, 0.04001, 0.041, 0.040)
	expect_that(test.function(p3)$'Number of p values 0.03<p<0.04', equals(1.0))
	expect_that(test.function(p3)$'Number of p values 0.04<p<0.05', equals(7.0))
	expect_that(test.function(p3)$'One-tailed p value for evidence of publication bias and/or p-hacking', equals(0.03515625))

})