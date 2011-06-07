# 因子分析
pfa <- function(dat,
    method=c("Varimax", "Biquartimax", "Quartimax", "Equimax", "None"),
    eps1=1e-5,
    eps2=1e-5,
    max1=999,
    max2=999,
    factors=0)
{
  method <- match.arg(method)
  dat <- subset(dat, complete.cases(dat))
  nr <- nrow(dat)
  nc <- ncol(dat)
  if (is.null(colnames(dat))) {
    colnames(dat) <- paste("Var", 1:nc, sep=".")
  }
  vnames <- colnames(dat)
  if (nr != nc && is.null(rownames(dat))) {
    rownames(dat) <- paste("Obs", 1:nr, sep=".")
  }
  cnames <- rownames(dat)
  r0 <- r <- if (nr == nc) dat else cor(dat)
  communality0 <- sqrt(1-1/diag(solve(r)))
  diag(r) <- communality0
  result <- eigen(r)
  eval <- result$values
  evec <- result$vectors
  if (factors == 0) {
    factors <- sum(eval >= 1)
  }
  converged <- FALSE
  for (i in 1:max1) {
    eval <- eval[1:factors]
    evec <- evec[,1:factors]
    evec <- t(sqrt(eval)*t(evec))
    r <- r0
    communality <- rowSums(evec^2)
    if (all(abs(communality-communality0) < eps1)) {
      converged <- TRUE
      break
    }
    communality0 <- communality
    diag(r) <- communality
    result <- eigen(r)
    eval <- result$values
    evec <- result$vectors
  }
  if (converged == FALSE) {
    warning("Not converged.")
  }
  else if (any(communality >= 1)) {
    warning("Communality >= 1.")
  }

  if (factors == 1 || method == "None") {
    w <- solve(r0)%*%evec
    scores <- (scale(dat)*sqrt(nr/(nr-1)))%*%w
    rownames(evec) <- names(communality) <- vnames
    rownames(scores) <- cnames
    colnames(scores) <- colnames(evec) <- names(eval) <- paste("FAC", 1:factors, sep=".")
    return(structure(list(rotation=method, correlation.matrix=r0, communality=communality,
      before.rotation.fl=evec, before.rotation.eval=eval, scores=scores), class="pfa"))
  }
  else {
    fl <- evec/sqrt(communality)
    eig <- numeric(factors)
    ov <- 0
    wt <- switch (method,
      "Varimax" = 1,
      "biQuartimax" = 0.5,
      "Quartimax" = 0,
      "Equimax"       = nc/2)
    fnp <- nc
    for (loop in 1:max2) {
      for (k in 1:(factors-1)) {
        for (k1 in (k+1):factors) {
          x <- fl[,k]
          y <- fl[,k1]
          xy <- x^2-y^2
          a <- sum(xy)
          b <- 2*sum(x*y)
          c <- sum(xy^2-4*x^2*y^2)
          d <- 4*sum(x*y*xy)
  
          dd <- d-2*wt*a*b/fnp
          theta <- atan(dd/(c-wt*(a^2-b^2)/fnp))
          if(sin(theta)*dd < 0) {
            if (theta > 0) {
              theta <- theta-pi
            }
            else {
              theta <- theta+pi
            }
          }
          theta <- theta/4
          cs <- cos(theta)
          si <- sin(theta)
          fljk <- fl[,k]
          fljk1 <- fl[,k1]
          fl[,k] <- fljk*cs+fljk1*si
          fl[,k1] <- fljk1*cs-fljk*si
        }
      }
      v <- sum((t(fl)^2-colSums(fl^2)*wt/fnp)^2)
  
      if (abs(v-ov) < eps2) {
        break
      }
      ov <- v
    }
    fl <- fl*sqrt(communality)
    w <- solve(r0)%*%fl
    scores <- (scale(dat)*sqrt(nr/(nr-1)))%*%w
    eval2 <- colSums(fl^2)
    rownames(evec) <- rownames(fl) <- names(communality) <- vnames
    rownames(scores) <- cnames
    colnames(scores) <- colnames(evec) <- colnames(fl) <- names(eval) <- names(eval2) <- paste("FAC", 1:factors, sep=".")
    return(structure(list(rotation=method, correlation.matrix=r0, communality=communality,
      before.rotation.fl=evec, before.rotation.eval=eval,
      after.rotation.fl=fl, after.rotation.eval=eval2, scores=scores), class="pfa"))
  }
}


print.pfa <- function(  result,
      before=FALSE)
{
  communality <- result$communality
  vnames <- sapply(names(communality), function(i) substring(i, 1, min(nchar(i), 7)))
  if (before || is.null(result$after.rotation.fl)) {
    fl <- result$before.rotation.fl
    eval <- result$before.rotation.eval
    label <- "E-value"
    if (result$rotation == "None") {
      printf("\nResult without rotation\n\n")
    }
    else {
      printf("\nBefore %s rotation\n\n", result$rotation)
    }
  }
  else {
    fl <- result$after.rotation.fl
    eval <- result$after.rotation.eval
    label <- "Sq.Sum"
    printf("\nAfter %s rotation\n\n", result$rotation)
  }
  nv <- nrow(fl)
  nc <- ncol(fl)
  cat("       ", sprintf(" Fac.%02i", 1:nc), " Communality\n", sep="")
  for (i in 1:nv) {
    cat(sprintf("%-7s", vnames[i]), sprintf("%7.3f", fl[i,]), sprintf("%7.3f\n", communality[i]), sep="")
  }
  cat(sprintf("%-7s", label),   sprintf("%7.3f", eval), "\n", sep="")
  cat(sprintf("%-7s", "Cont."), sprintf("%7.1f", eval/nv*100), "\n", sep="")
  cat(sprintf("%-7s", "Cum."),  sprintf("%7.1f", cumsum(eval/nv*100)), "\n", sep="")
}


plot.pfa <- function(   result,
      before=FALSE,
      fac.no=1:2,
      scores=FALSE,
      xlab=NULL, ylab=NULL,
      axis=TRUE,
      label.cex=0.7,
      ...)
{
  fac.name <- names(result$before.rotation.eval)
  if (length(fac.name) > 1) {
    ax1 <- fac.no[1]
    ax2 <- fac.no[2]
    if (is.null(xlab)) {
      xlab <- fac.name[ax1]
    }
    if (is.null(ylab)) {
      ylab <- fac.name[ax2]
    }
    if (scores) {
      x <- result$scores[, ax1]
      y <- result$scores[, ax2]
      labels <- 1:length(x)
    }
    else {
      if (before || is.na(result$after.rotation.fl)) {
        fl <- result$before.rotation.fl
      }
      else {
        fl <- result$after.rotation.fl
      }
      x <- fl[, ax1]
      y <- fl[, ax2]
      labels <- names(result$communality)
    }
    plot(x, y, xlab=xlab, ylab=ylab, ...)
    old <- par(xpd=TRUE)
    text(x, y, labels, cex=label.cex, pos=1)
    par(old)
  }
  if (axis) {
    abline(h=0, v=0)
  }
}


printf <- function(fmt, ...)
{
  cat(sprintf(fmt, ...))
}
