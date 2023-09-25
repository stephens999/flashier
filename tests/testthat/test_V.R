context("V parameter")

set.seed(666)

n <- 40
p <- 60

LF <- outer(rep(1, n), rep(1, p))
M <- LF + 0.1 * rnorm(n * p)

test_that("setting V=0 works", {
  set.seed(1)
  f <- flash(M, var_type = 0, greedy_Kmax = 1, verbose = 0)
  f$flash_fit$t.init=0 # clear these fields to avoid failing test of equality
  f$flash_fit$t.final=0

  set.seed(1)
  f2 <- flash(M, var_type = 0, greedy_Kmax = 1, verbose = 0, V=matrix(0,nrow=n,ncol=p))
  f2$flash_fit$t.init=0
  f2$flash_fit$t.final=0

  expect_equal(f$flash_fit,f2$flash_fit)
})

test_that("setting V=1 runs", {
  set.seed(1)
  f <- flash(LF, var_type = 1, greedy_Kmax = 1, verbose = 0, V = matrix(1,nrow=n,ncol=p))
  expect_equal(f$flash_fit$baseR2, rep(p,n))
  f <- flash(LF, var_type = 2, greedy_Kmax = 1, verbose = 0, V = matrix(1,nrow=n,ncol=p))
  expect_equal(f$flash_fit$baseR2, rep(n,p))
})
