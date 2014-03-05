
test_that("limits.check works well", {

	expect_that(limits.check(c(0.00, 0.01)), is_true())
	expect_that(limits.check(c(-0.00, 0.01)), is_true())
	expect_that(limits.check(c(-0.01, 0.05)), throws_error())
	expect_that(limits.check(c(0.01, 0.06)), throws_error())
	expect_that(limits.check(c(-99, 99)), throws_error())
	expect_that(limits.check(c(0.0)), throws_error())
	expect_that(limits.check(c(0.0, 'egg')), throws_error())
	expect_that(limits.check(c(0.02, 0.01)), throws_error())

})