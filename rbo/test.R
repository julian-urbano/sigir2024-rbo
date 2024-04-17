source("src/common.R")
source("src/simulate.R")
source("rbo/rbo.R")

library(testthat)
library(future.apply)


# Test helper functions ############################################################################

# These functions are intended to replace testthat::expect_equal, so that numerical comparisons are
# always made with **absolute** tolerances. It is parameterized by the number of decimal digits that
# must match between actual and target for the comparison to be positive. This is useful because
# our target values are mostly literals with certain precision, not computed.
expect_eq <- function(object, expected, digits = 6) {
  act <- testthat::quasi_label(rlang::enquo(object))
  exp <- testthat::quasi_label(rlang::enquo(expected))
  stopifnot(is.numeric(object) && is.numeric(expected))

  tolerance <- 10^-(digits+1)
  comp <- abs(round(object, digits)-round(expected, digits)) < tolerance

  expect(
    comp,
    glue::glue("{act$lab} not equal to {exp$lab}.\n",
               "    actual: {act$val}\n",
               "  expected: {exp$val}\n")
  )

  invisible(act$val)
}

# Expect **not** equal
expect_neq <- function(object, expected, digits = 6) {
  act <- testthat::quasi_label(rlang::enquo(object))
  exp <- testthat::quasi_label(rlang::enquo(expected))
  stopifnot(is.numeric(object) && is.numeric(expected))

  tolerance <- 10^-(digits+1)
  comp <- abs(round(object, digits)-round(expected, digits)) < tolerance

  expect(
    !comp,
    glue::glue("{act$lab} equal to {exp$lab}.\n",
               "    actual: {act$val}\n",
               "  expected: (not) {exp$val}\n")
  )

  invisible(act$val)
}

expect_between <- function(object, lower, upper, digits = 6) {
  act <- testthat::quasi_label(rlang::enquo(object))
  low <- testthat::quasi_label(rlang::enquo(lower))
  up <- testthat::quasi_label(rlang::enquo(upper))

  stopifnot(is.numeric(object) && is.numeric(lower) && is.numeric(upper))

  tolerance <- 10^-(digits+1)
  comp <- object - lower > -tolerance && upper - object > - tolerance

  expect(
    comp,
    glue::glue("{act$lab} not between {low$lab} and {up$lab}.\n",
               "    actual: {act$val}\n",
               "  expected: between {low$val} and {up$val}\n")
  )

  invisible(act$val)
}

# RBO utility functions ############################################################################

fill_max <- function(x, y, max_d) {
  sl <- identify_SL(x, y)
  preS <- sl$S
  preL <- sl$L
  S <- flatten(sl$S)$id
  L <- flatten(sl$L)$id

  ul <- us <- m <- S[0]
  d <- 1
  while(d <= length(L) || length(ul)>0 || length(us)>0) {
    inl <- ifelse(d <= length(L), L[d], NA)
    ins <- ifelse(d <= length(S), S[d], NA)

    # match exactly at d
    if(!is.na(inl) && !is.na(ins) && inl==ins) {
      # nothing
      m <- c(m, ins)
    }else{
      # new item in L
      if(inl %in% us) {
        us <- us[us != inl]
        m <- c(m, inl)
      }else if(!is.na(inl)){
        ul <- c(ul, inl)
      }
      # new item in S
      if(ins %in% ul) {
        ul <- ul[ul != ins]
        m <- c(m, ins)
      }else if(!is.na(ins)){
        us <- c(us, ins)
      }
      # fill in L
      if(is.na(inl)) {
        m <- c(m, us[1])
        L[d] <- us[1]
        preL <- c(preL, us[1])
        us <- us[-1]
      }
      # fill in S
      if(is.na(ins)) {
        m <- c(m, ul[1])
        S[d] <- ul[1]
        preS <- c(preS, ul[1])
        ul <- ul[-1]
      }
    }
    d <- d+1
  }

  fill <- paste("ls", safeseq(length(L)+1, max_d), sep="")
  preL <- c(preL, fill)
  preS <- c(preS, fill)

  list(L = preL, S = preS, ul = ul, us = us, m  = m)
}
fill_min <- function(x, y, max_d) {
  sl <- identify_SL(x, y)
  preS <- sl$S
  preL <- sl$L
  S <- flatten(sl$S)$id
  L <- flatten(sl$L)$id

  fillL <- paste("l", safeseq(length(L)+1, max_d), sep="")
  fillS <- paste("s", safeseq(length(S)+1, max_d), sep="")
  L <- c(preL, fillL)
  S <- c(preS, fillS)

  list(preL = preL, preS = preS, L = L, S = S)
}

# Test RBO #########################################################################################

future_replicate(1000, future.seed = 0, {
  p <- runif(1, .9, .99)
  len <- sample(5:15, 2, replace = TRUE)
  xy <- simulate_rankings(min(len), max(len),
                          n = sum(len),
                          frac_ties_x = runif(1, .5, 1), frac_ties_y = runif(1, .5, 1))
  x <- xy$x
  y <- xy$y
  a <- list(unlist(x)) # x but fully tied
  d <- lapply(y, function(yy) paste0("_", yy)) # fully non-conjoint

  # w-variant ======================================================================================

  r <- rbo(x, y, p, "w")
  e <- r[1] # ext
  m <- r[2] # min
  M <- r[3] # max

  # consistency
  expect_between(e, m, M)
  expect_between(m, 0, 1)
  expect_between(M, 0, 1)

  # fills
  fmin <- fill_min(x, y, max_d = 1000)
  expect_eq(m, rbo(fmin$L, fmin$S, p, "w", "ext"))
  fmax <- fill_max(x, y, max_d = 1000)
  expect_eq(M, rbo(fmax$L, fmax$S, p, "w", "ext"))

  # edge cases: X with itself
  r <- rbo(x, x, p, "w")
  expect_eq(r[1], 1)
  expect_lt(r[2], 1)
  expect_eq(r[3], 1)

  # edge cases: X with itself fully tied
  r <- rbo(x, a, p, "w")
  expect_lte(r[1], 1)
  expect_lt(r[2], 1)
  expect_lte(r[3], 1)

  # edge cases: non-conjoint
  r <- rbo(x, d, p, "w")
  expect_eq(r[1], 0)
  expect_eq(r[2], 0)
  expect_lt(r[3], 1)

  # a-variant ======================================================================================

  r <- rbo(x, y, p, "a")
  e <- r[1] # ext
  m <- r[2] # min
  M <- r[3] # max

  # consistency
  expect_between(e, m, M)
  expect_between(m, 0, 1)
  expect_between(M, 0, 1)

  # fills
  fmin <- fill_min(x, y, max_d = 1000)
  expect_eq(m, rbo(fmin$L, fmin$S, p, "a", "ext"))
  fmax <- fill_max(x, y, max_d = 1000)
  expect_eq(M, rbo(fmax$L, fmax$S, p, "a", "ext"))

  # edge cases: X with itself
  r <- rbo(x, x, p, "a")
  expect_lt(r[1], 1)
  expect_lt(r[2], 1)
  expect_lt(r[3], 1)

  # edge cases: X with itself fully tied
  r <- rbo(x, a, p, "a")
  expect_lt(r[1], 1)
  expect_lt(r[2], 1)
  expect_lt(r[3], 1)

  # edge cases: non-conjoint
  r <- rbo(x, d, p, "a")
  expect_eq(r[1], 0)
  expect_eq(r[2], 0)
  expect_lt(r[3], 1)

  # b-variant ======================================================================================

  r <- rbo(x, y, p, "b")
  e <- r[1] # ext
  m <- r[2] # min
  M <- r[3] # max

  # consistency
  expect_between(e, m, M)
  expect_between(m, 0, 1)
  expect_between(M, 0, 1)

  # fills
  fmin <- fill_min(x, y, max_d = 1000)
  expect_eq(m, rbo(fmin$L, fmin$S, p, "b", "ext"))
  fmax <- fill_max(x, y, max_d = 1000)
  expect_eq(M, rbo(fmax$L, fmax$S, p, "b", "ext"))

  # edge cases: X with itself
  r <- rbo(x, x, p, "b")
  expect_eq(r[1], 1)
  expect_lt(r[2], 1)
  expect_eq(r[3], 1)

  # edge cases: X with itself fully tied
  r <- rbo(x, a, p, "b")
  expect_lte(r[1], 1)
  expect_lt(r[2], 1)
  expect_lte(r[3], 1)

  # edge cases: non-conjoint
  r <- rbo(x, d, p, "b")
  expect_eq(r[1], 0)
  expect_eq(r[2], 0)
  expect_lt(r[3], 1)
}) |> invisible()
