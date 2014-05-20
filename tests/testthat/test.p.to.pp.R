
test_that("p.to.pp works", {

	expect_that(p.to.pp(c(0.0)), equals(c(0.0)))
	expect_that(p.to.pp(c(0.01)), equals(c(0.2)))
	expect_that(p.to.pp(c(0.02)), equals(c(0.4)))
	expect_that(p.to.pp(c(0.05)), equals(c(1.0)))
	expect_that(p.to.pp(c(0.0, 0.01, 0.02)), equals(c(0.0, 0.2, 0.4)))
	expect_that(p.to.pp(c(-0.1, 0.01, 0.02)), throws_error())

})