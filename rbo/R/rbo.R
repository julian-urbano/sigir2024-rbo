####################################################################################################
# Copyright 2024 Julián Urbano <urbano.julian@gmail.com>                               MIT LICENSE #
#                                                                                                  #
# Permission is hereby granted, free of charge, to any person obtaining a copy of this software    #
# and associated documentation files (the "Software"), to deal in the Software without             #
# restriction, including without limitation the rights to use, copy, modify, merge, publish,       #
# distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the    #
# Software is furnished to do so, subject to the following conditions:                             #
#                                                                                                  #
# The above copyright notice and this permission notice shall be included in all copies or         #
# substantial portions of the Software.                                                            #
#                                                                                                  #
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING    #
# BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND       #
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,     #
# DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,   #
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.          #
####################################################################################################
# Always check for the latest version at https://github.com/julian-urbano/sigir2024-rbo            #
#                                                                                                  #
# If you use this software, please cite the following paper:                                       #
#                                                                                                  #
# M. Corsi and J. Urbano, "The Treatment of Ties in Rank-Biased Overlap", International ACM SIGIR  #
# Conference on Research and Development in Information Retrieval, 2024.                           #
####################################################################################################

# Utility functions to create rankings #############################################################

#' Create a ranking from a string representation.
#'
#' Item names must use alphanumeric characters only. Items must be separated by a blank space, and
#' tie groups must be enclosed in parentheses. For example: "red (blue green) yellow pink".
from_string <- function(s) {
  if(!is.character(s) || !is.atomic(s) || !is.vector(s) || length(s) != 1 ||
     !grepl("^(\\w+|\\(\\w+( \\w+)+\\))( (\\w+|\\(\\w+( \\w+)+\\)))*$", s))
    stop("'s' must be a string representation of a ranking using alphanumeric characters",
         call. = FALSE)
  # separate letters, but retain ( and ) for groups
  g <- strsplit(s, split = " +", perl = TRUE)[[1]]

  r <- list() # to build up the output vector
  in_group <- FALSE # to keep track of whether we're in a group or not

  for(i in seq_along(g)) {
    gg <- g[i]
    if(startsWith(gg, "(") || endsWith(gg, ")")){
      if(in_group) {
        # end of group
        gg <- substring(gg, 1, nchar(gg)-1)
        r[[length(r)]] <- c(r[[length(r)]], gg)
        in_group <- FALSE
      }else{
        # beginning of group
        gg <- substring(gg, 2, nchar(gg))
        r <- c(r, gg)
        in_group <- TRUE
      }
    }else{
      if(in_group) {
        #tied, inner
        r[[length(r)]] <- c(r[[length(r)]], gg)
      }else{
        # untied
        r <- c(r, gg)
      }
    }
  }

  r
}
#' Create a string representation from a ranking.
to_string <- function(r) {
  sapply(r, function(rr) {
    if(length(rr)==1)
      glue::glue(" {rr}")
    else
      glue::glue(" ({paste(rr, collapse=' ')})")
  }) |>
    paste(collapse = "") |>
    trimws()
}
#' Create a ranking of `items`, sorting by `scores` in descending order. Items with the same score
#' will be grouped in a tie.
#' This is useful for instance to extract the ranking of documents by their retrieval value.
extract_ranking <- function(items, scores) {
  # Sort by score first
  o <- order(scores, decreasing = TRUE)
  items <- items[o]
  scores <- scores[o]

  i <- 1
  prev <- scores[1]
  r <- list(character(0))
  while(i <= length(scores)) {
    if(scores[i] == prev){
      r[[length(r)]] <- c(r[[length(r)]], items[i])
    }else{
      r[[length(r)+1]] <- items[i]
    }
    prev <- scores[i]
    i <- i+1
  }
  r
}

# RBO ##############################################################################################

# Same as R's seq, but returns an empty vector if to < from.
# Useful to define summation domains.
safeseq <- function(from, to) {
  if(to < from)
    integer(0)
  else
    seq(from, to)
}
# Flatten rankings (with possible ties), returning:
# - `id`: flattened ranking.
# - `t`: top ranks.
# - `b`: bottom ranks.
# If both `x` and `y` are given, it returns a list identifying the Longer and Shorter rankings.
flatten <- function(x, y = NULL) {
  if(!is.null(y)) {
    x <- flatten(x)
    y <- flatten(y)

    if(length(x$id) >= length(y$id))
      list(L = x, S = y)
    else
      list(L = y, S = x)
  } else {
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
}

# Compute w-variant item contributions of the flattened rankings fS and fL.
cc.w <- function(fS, fL) {
  omega <- union(fS$id, fL$id) # union of domains, keeping order
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

# Compute a/b-variant item contributions of the flattened rankings fS and fL.
cc.a <- function(fS, fL) {
  omega <- union(fS$id, fL$id) # union of domains, keeping order
  list(cS = cc.a_(fS, omega, length(fL$id)),
       cL = cc.a_(fL, omega, length(fL$id)))
}
cc.a_ <- function(f, omega, max_d) {
  m <- matrix(0, ncol = max_d, nrow = length(omega), dimnames = list(omega, NULL))
  for(i in seq_along(f$id)) {
    j <- which(omega == f$id[i]) # row
    m[j, safeseq(f$b[i], max_d)] <- 1
    m[j, safeseq(f$t[i], f$b[i]-1)] <- safeseq(1, f$b[i]-f$t[i]) / (f$b[i]-f$t[i]+1)
  }
  m
}

#' Check that rankings are complete, non-empty, and without duplicates, and that 0 < p < 1.
check_rbo <- function(x, y, p) {
  if(!is.numeric(p) || length(p) != 1L || p <= 0 || p >= 1)
    stop("'p' must be such that 0 < p < 1.", call. = FALSE)

  x_all <- unlist(x)
  if(any(is.na(x_all)) || length(x_all) == 0L || length(x_all) != length(unique(x_all)))
    stop("'x' is not a valid representation of a ranking.", call. = FALSE)

  y_all <- unlist(y)
  if(any(is.na(y_all)) || length(y_all) == 0L || length(y_all) != length(unique(y_all)))
    stop("'y' is not a valid representation of a ranking.", call. = FALSE)
}

#' Compute RBO between `x` and `y`, with persistence parameter `p`, and handling ties when present.
# '
#' The rankings should be represented as a list of items, where ties appear in the same vector, e.g.:
#'   ["red", {"blue", "green"}, "yellow", "pink"]
#' Utility functions `from_string`, `to_string` and `extract_ranking` can ease the process of
#' creating valid rankings from string representations or raw ranking data.
#'
#' Parameters
#' ----------
#' ties: 'a' (default), 'b' or 'w'
#'   Please see section 1.1 of the SIGIR 2024 paper to decide which one should be used. In summary:
#'     - When ties represent equality (i.e. sports rankings), use 'w'.
#'     - When ties represent uncertainty (i.e. don't know which items go first):
#'       - Use 'a' to compute the average RBO across permutations of the ties.
#'       - Use 'b' if the RBO score should be corrected by the information lost due to ties.
#'
#' score: vector with any of 'ext', 'min', 'max' and 'res' (all by default)
#'   - 'ext': extrapolate the agreement in the seen part to the unseen part.
#'   - 'min': lower bound by assuming no additional overlap in the unseen part.
#'   - 'max': upper bound by assuming maximum overlap in the unseen part.
#'   - 'res': size of the residual, i.e. 'max'-'min'.
rbo <- function(x, y, p, ties = c("a", "b", "w"), score = c("ext", "min", "max", "res")) {
  check_rbo(x, y, p)

  score <- match.arg(score, several.ok = TRUE)
  ties <- match.arg(ties)

  # Flat representations
  sl <- flatten(x, y)
  S <- sl$S
  L <- sl$L
  s <- length(S$id)
  l <- length(L$id)

  # If there are no ties, use w-variant for efficiency
  if(all(S$t == S$b) && all(L$t == L$b))
    ties <- "w"

  # Calculate individual item contributions
  if(ties == "w")
    CC <- cc.w(S, L)
  else
    CC <- cc.a(S, L)

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
    LdS <- setdiff(L$id, S$id) # uniques in L
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
    LdS <- setdiff(L$id, S$id) # uniques in L
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
  if("ext" %in% score)
    rbo.ext <- (1-p)/p*( sum(c(A1, A2.ext)*p^d) + sec3.ext )

  # Output
  r <- numeric(0)
  if("ext" %in% score)
    r["ext"] <- rbo.ext
  if("min" %in% score)
    r["min"] <- rbo.min
  if("max" %in% score)
    r["max"] <- rbo.max
  if("res" %in% score)
    r["res"] <- rbo.max-rbo.min
  r <- pmin(pmax(r,0),1) # for numerical stability

  r
}
