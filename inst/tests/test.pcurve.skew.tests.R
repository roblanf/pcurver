test_that("p.skew works", {

	p1 <- c(0.0, 0.0, 0.0, 0.0, 0.0)
	p2 <- c(0.05, 0.05, 0.05, 0.05, 0.05)
	p3 <- c(0.025, 0.025, 0.025, 0.025, 0.025, 0.025)
	p4 <- c(0.0001, 0.0005, 0.04, 0.02, 0.05, 0.05, 0.05)
	p5 <- c(0.0, 0.0, 0.0, 0.0, 0.0, 0.01, 0.01, 0.05)

	expect_that(p.skew(p1), equals(0.0))
	expect_that(p.skew(p2), equals(0.0))
	expect_that(p.skew(p3), equals(0.0))
	expect_that(p.skew(p4), equals(-0.4317223, tolerance = 1e06))
	expect_that(p.skew(p5), equals(0.5066917, tolerance = 1e06))

})
