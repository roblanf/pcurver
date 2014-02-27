
test_that("p_check raises errors for anything that's not a float >=0 and <=0.05", {

	expect_that(p_check(c(0.0)), equals(c(0.0)))
	expect_that(p_check(0.0), equals(c(0.0)))
	expect_that(p_check(c(0.05)), equals(c(0.05)))
	expect_that(p_check(0.05), equals(c(0.05)))
	expect_that(p_check(c(0.0500001)), throws_error())
	expect_that(p_check(c(-0.0000001)), throws_error())
	expect_that(p_check(c(0.0, 0.003, 0.05, 0.05)), equals(c(0.0, 0.003, 0.05, 0.05)))
	expect_that(p_check(c(0.0, -0.003, 0.05, 0.05)), throws_error())
	expect_that(p_check('egg'), throws_error())

})