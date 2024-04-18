# Utility functions to handle rankings #############################################################

# Same as R's seq, but returns an empty vector if to < from.
# Useful to define summation domains.
safeseq <- function(from, to) {
  if(to < from)
    integer(0)
  else
    seq(from, to)
}
# Identify the Short and Long rankings, and return them in a named list.
identify_SL <- function(x, y) {
  if(length(unlist(x)) > length(unlist(y)))
    list(L = x, S = y)
  else
    list(L = y, S = x)
}
# Flatten a ranking (with possible ties), returning:
# - id: flattened ranking.
# - t: top ranks.
# - b: bottom ranks.
flatten <- function(x) {
  id <- unlist(x)
  t <- b <- numeric(length(id))

  r <- 0 # last rank
  for(i in seq_along(x)) {
    xx <- x[[i]] # current group
    n_xx <- length(xx)
    r_xx <- seq(r+1, r+n_xx) # ranks of the current group

    t[r_xx] <- r+1
    b[r_xx] <- r+n_xx

    r <- r+n_xx
  }

  list(id = id, t = t, b = b)
}

# RBO ##############################################################################################

# Compute a/b-variant item contributions of the flattened rankings fS and fL.
cc.a <- function(fS, fL) {
  omega <- union(fS$id, fL$id) # union of domains
  list(cS = cc.a_(fS, omega, length(fL$id)),
       cL = cc.a_(fL, omega, length(fL$id)))
}
cc.a_ <- function(f, omega, max_d) {
  m <- matrix(0, ncol = max_d, nrow = length(omega), dimnames = list(omega, NULL))
  for(i in seq_along(f$id)) {
    j <- which(omega == f$id[i]) # row
    m[j, safeseq(f$b[i], max_d)] <- 1
    m[j, safeseq(f$t[i], f$b[i])] <- safeseq(1, f$b[i]-f$t[i]+1) / (f$b[i]-f$t[i]+1)
  }
  m
}
# Compute w-variant item contributions of the flattened rankings fS and fL.
cc.w <- function(fS, fL) {
  omega <- union(fS$id, fL$id) # union of domains
  list(cS = cc.w_(fS, omega, length(fL$id)),
       cL = cc.w_(fL, omega, length(fL$id)))
}
cc.w_ <- function(f, omega, max_d) {
  m <- matrix(0, ncol = max_d, nrow = length(omega), dimnames = list(omega, NULL))
  for(i in seq_along(f$id)) {
    j <- which(omega == f$id[i]) # row
    m[j, safeseq(f$t[i], max_d)] <- 1
  }
  m
}
# Check that p is between 0 and 1.
check_p <- function(p) {
  if(!is.numeric(p) || length(p) != 1L || p <= 0 || p >= 1)
    stop("'p' must be such that 0 < p < 1.", call. = FALSE)
}
# Check that rankings are complete, non-empty, and without duplicates
check_rankings <- function(x, y) {
  x_all <- unlist(x)
  if(any(is.na(x_all)) || length(x_all) == 0L || length(x_all) != length(unique(x_all)))
    stop("'x' is not a valid representation of a ranking.", call. = FALSE)
  y_all <- unlist(y)
  if(any(is.na(y_all)) || length(y_all) == 0L || length(y_all) != length(unique(y_all)))
    stop("'y' is not a valid representation of a ranking.", call. = FALSE)
}


rbo <- function(x, y, p, ties = c("a","b","w"), score = c("ext", "min", "max", "res")) {
  check_rankings(x, y)
  check_p(p)

  score <- match.arg(score, several.ok = TRUE)
  ties <- match.arg(ties)

  # Flat representations
  sl <- identify_SL(x, y)
  fS <- flatten(sl$S)
  fL <- flatten(sl$L)
  s <- length(fS$id)
  l <- length(fL$id)

  # Calculate individual item contributions
  if(ties == "w")
    CC <- cc.w(fS, fL)
  else
    CC <- cc.a(fS, fL)

  # First section: 1 to s
  d <- safeseq(1, s)
  X1 <- colSums(CC$cS[,d,drop=FALSE] * CC$cL[,d,drop=FALSE])
  if(ties == "w")
    A1 <- 2*X1 / ( colSums(CC$cS[,d,drop=FALSE]) + colSums(CC$cL[,d,drop=FALSE]) )
  else if(ties == "a")
    A1 <- X1 / d
  else
    A1 <- X1 / sqrt(colSums(CC$cS[,d,drop=FALSE]^2)) / sqrt(colSums(CC$cL[,d,drop=FALSE]^2))

  # Second section: s+1 to l
  d <- safeseq(s+1, l)
  X2_seen <- colSums(CC$cS[,d,drop=FALSE] * CC$cL[,d,drop=FALSE])

  if("min" %in% score || "res" %in% score) {
    X2.min <- X2_seen
    if(ties == "w") {
      A2.min <- 2*X2.min / ( d + colSums(CC$cL[,d,drop=FALSE]) )
    }else if(ties == "a")
      A2.min <- X2.min / d
    else
      A2.min <- X2.min / sqrt(d) / sqrt(colSums(CC$cL[,d,drop=FALSE]^2))
  }
  if("max" %in% score || "res" %in% score) {
    LdS <- setdiff(fL$id, fS$id) # uniques in L
    cL_unseen.max <- CC$cL[LdS[d-s],d,drop=FALSE]
    cL_unseen.max[lower.tri(cL_unseen.max)] <- NA
    X2_unseen.max <- colSums(cL_unseen.max, na.rm = TRUE)

    X2.max <- X2_seen + X2_unseen.max
    if(ties == "w") {
      A2.max <- 2*X2.max / ( d + colSums(CC$cL[,d,drop=FALSE]) )
    }else if(ties == "a")
      A2.max <- X2.max / d
    else
      A2.max <- X2.max / sqrt(d) / sqrt(colSums(CC$cL[,d,drop=FALSE]^2))
  }
  if("ext" %in% score) {
    LdS <- setdiff(fL$id, fS$id) # uniques in L
    cL_unseen <- CC$cL[LdS,d,drop=FALSE]
    cL_unseen[cL_unseen==0] <- NA
    cL_unseen <- colMeans(cL_unseen, na.rm = TRUE)
    X2_unseen.ext <- (d-s)*A1[s]*cL_unseen

    X2.ext <- X2_seen + X2_unseen.ext
    if(ties == "w") {
      A2.ext <- 2*X2.ext / ( d + colSums(CC$cL[,d,drop=FALSE]) )
    }else if(ties == "a")
      A2.ext <- X2.ext / d
    else
      A2.ext <- X2.ext / sqrt(d) / sqrt(colSums(CC$cL[,d,drop=FALSE]^2))
  }

  # Third section: l+1 to inf
  X_seen <- c(X1, X2_seen)
  Xl <- X_seen[l] # X_l
  if("min" %in% score || "res" %in% score) {
    d <- safeseq(1, l)
    sec3.min <- Xl*( log(1/(1-p)) - sum(p^d / d) )
  }
  if("max" %in% score || "res" %in% score) {
    f <- l+s-Xl
    d <- safeseq(l+1,f)
    sec3.max <- sum((2*d-l-s+Xl)/d * p^d) + p^(f+1) / (1-p)
  }
  if("ext" %in% score) {
    sec3.ext <- (Xl + (l-s)*A1[s])/l * p^(l+1) / (1-p)
  }

  # All sections
  d <- safeseq(1,l)
  if("min" %in% score || "res" %in% score)
    rbo.min <- (1-p)/p*( sum(c(A1, A2.min)*p^d) + sec3.min )
  if("max" %in% score || "res" %in% score)
    rbo.max <- (1-p)/p*( sum(c(A1, A2.max)*p^d) + sec3.max )
  if("res" %in% score)
    rbo.res <- rbo.max-rbo.min
  if("ext" %in% score)
    rbo.ext <- (1-p)/p*( sum(c(A1, A2.ext)*p^d) + sec3.ext )

  r <- sapply(score, function(s) get(paste0("rbo.",s)))
  r <- pmin(1,pmax(0,r)) # for numerical stability
  names(r) <- score
  r
}
