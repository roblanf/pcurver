
test_that("p.check raises errors for anything that's not a float >=0 and <=0.05", {

	expect_that(p.check(c(0.0)), equals(c(0.0)))
	expect_that(p.check(0.0), equals(c(0.0)))
	expect_that(p.check(c(0.05)), equals(c(0.05)))
	expect_that(p.check(0.05), equals(c(0.05)))
	expect_that(p.check(c(0.0500001)), throws_error())
	expect_that(p.check(c(-0.0000001)), throws_error())
	expect_that(p.check(c(0.0, 0.003, 0.05, 0.05)), equals(c(0.0, 0.003, 0.05, 0.05)))
	expect_that(p.check(c(0.0, -0.003, 0.05, 0.05)), throws_error())
	expect_that(p.check('egg'), throws_error())


})

test_that("p.check limits work well", {

	p1 <- c(0.0, 0.01, 0.02, 0.02, 0.03, 0.035, 0.05)

	expect_that(p.check(p1), is_identical_to(p1))
	expect_that(p.check(p1, limits=c(0.0, 0.05)), equals(p1))
	expect_that(p.check(p1, limits=c(-0.01, 0.01)), throws_error())
	expect_that(p.check(p1, limits=c(0.01, 0.05001)), throws_error())
	expect_that(length(p.check(p1, limits=c(0.01, 0.05))), equals(6))
	expect_that(length(p.check(p1, limits=c(0.02, 0.05))), equals(5))
	expect_that(length(p.check(p1, limits=c(0.03, 0.05))), equals(3))
	expect_that(length(p.check(p1, limits=c(0.00, 0.04))), equals(6))
	expect_that(length(p.check(p1, limits=c(0.00, 0.035))), equals(6))
	expect_that(length(p.check(p1, limits=c(0.00, 0.03499))), equals(5))
	expect_that(length(p.check(p1, limits=c(0.00, 0.029))), equals(4))
	expect_that(max(p.check(p1, limits=c(0.00, 0.029))), equals(0.02))
	expect_that(min(p.check(p1, limits=c(0.00, 0.029))), equals(0.0))
	expect_that(max(p.check(p1, limits=c(0.025, 0.05))), equals(0.05))
	expect_that(min(p.check(p1, limits=c(0.025, 0.05))), equals(0.03))

})