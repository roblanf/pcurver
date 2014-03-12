
test_that("fisher.rskew.test works", {

	# This is the example given in this ms: 
	# http://papers.ssrn.com/sol3/papers.cfm?abstract_id=2256237
	p1 <- c(0.001, 0.002, 0.04)
	expect_that(fisher.rskew.test(p1)$p.value, equals(0.02265281, tolerance=1e06))

	# another test computed by hand
	p2 <- c(0.01, 0.02, 0.03, 0.04, 0.05, 0.05, 0.05, 0.049, 0.049)
	expect_that(fisher.rskew.test(p2)$p.value, equals(0.9930869, tolerance=1e06))

})

test_that("fisher.lskew.test works", {


	p1 <- c(0.001, 0.002, 0.04)
	p1r <- 0.05 - p1
	expect_that(fisher.lskew.test(p1)$p.value, equals(fisher.rskew.test(p1r)$p.value))

	# another test computed by hand
	p2 <- c(0.01, 0.02, 0.03, 0.04, 0.05, 0.05, 0.05, 0.049, 0.049)
	p2r <- 0.05 - p2
	expect_that(fisher.lskew.test(p2)$p.value, equals(fisher.rskew.test(p2r)$p.value))

})

test_that("fisher.bias.test works", {

	# should be larger p value because they're more bunched over a bigger range
	p1 <- c(0.03, 0.04, 0.045, 0.0475, 0.0497, 0.05)
	expect_that(fisher.bias.test(p1)$p.value, is_more_than(fisher.lskew.test(p1)$p.value))
	expect_that(fisher.bias.test(p1)$df, equals(12))
	expect_that(fisher.bias.test(p1, lower.limit=0.046)$df, equals(6))

})