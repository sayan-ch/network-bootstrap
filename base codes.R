library(irlba)
library(parallel)
library(data.table)
library(tidyverse)
library(ggpubr)
library(Matrix)
library(igraph)

################################################################################
## Generate network from RDPG with X ~ Beta(a, b) with dimension d

gen.rdpg.beta <- function(n, d, a, b, ncore = 1){
  X <- matrix(rbeta(n*d, a, b), nrow = n, ncol = d)
  
  P <- tcrossprod(X)
  # P1 <- tcrossprod(X)
  # P <- P1/max(P1)
  
  diag(P) <- 0
  
  stor <- do.call('rbind',
                  mclapply(1:(n-1), function(i) {
                    tmp <- which(rbinom(n-i, 1, P[i,(i+1):n]) == 1)
                    
                    if(length(tmp) == 0)
                      return(NULL)
                    else
                      return(cbind(rep(i, length(tmp)), i + tmp))
                  }, mc.cores = ncore))
  
  A <- sparseMatrix(i = stor[,1], j = stor[,2], dims = c(n,n), symmetric = T)
  
  return(list('A' = A, 'P' = P, 'X' = X))
}

## Adjacency spectral embedding with dimension d on adjacency matrix A

ASE <- function(A, d){
  eig <- partial_eigen(A, n = d, symmetric = T)  
  
  U.hat <- eig$vectors
  S.hat <- eig$values
  
  U.hat %*% diag(x = sqrt(S.hat), nrow = d, ncol = d)
}

################################################################################
## U-statistic with additive weight computation, h being the kernel
## Averages U.tilde with weights

Ustat <- function(PP, m, h, W = rep(1, nrow(PP))){
  
  return(mean(h(PP, m)*W))
  
}

## U.tilde function for average degree

h.avg.deg <- function(PP, m = 2){
  nn <- nrow(PP)
  
  denom <- choose(nn - 1, m - 1)
  
  PP[lower.tri(PP, diag = T)] <- 0
  
  # uni <- (rowSums(PP) - diag(PP))/denom
  uni <- 2*rowSums(PP)/denom
  
  return(uni)  
}

## U.tilde function for triangle density

h.tri.den <- function(PP, m = 3){
  nn <- nrow(PP)
  
  denom <- choose(nn - 1, m - 1)
  
  PP2 <- as(PP, 'dMatrix')
  
  PP2[lower.tri(PP2, diag = T)] <- 0
  
  uni <- 3*diag(crossprod(PP2, tcrossprod(PP2)))/denom
  
  return(uni)  
}

################################################################################
## All network statistics

## Average degree

avg.deg <- function(A){
  n_ <- nrow(A)
  (sum(rowSums(A)) - sum(diag(A)))/(n_*(n_ - 1))
}

## Triangle density

tri.den <- function(A){
  A_ <- A
  diag(A_) <- 0
  sum(rowSums(tcrossprod(A_)*A_))/choose(nrow(A), 3)/6
}

## Average shortest path

avg.short.path <- function(A){
  gg <- graph_from_adjacency_matrix(A, mode = "undirected")
  mean_distance(gg, directed = F)
}

## kth Largest eigenvalue scaled by n*rho_n

largest.eig <- function(A, k = 1){
  eig <- partial_eigen(x = A, n = k, symmetric = T)
  eig$values[k]/(nrow(A)*avg.deg(A))
}

## Spectral gap scaled by n*rho_n

spectral.gap <- function(A){
  eig <- partial_eigen(x = A, n = 2, symmetric = T)
  (eig$values[1] - eig$values[2])/(nrow(A)*avg.deg(A))
}

## Ratio of largest to smallest eigenvalue

eig.ratio <- function(A, d){
  eig <- partial_eigen(x = A, n = d, symmetric = T)
  eig$values[1]/eig$values[2]
}

## Approximate trace of A^p / # closed walks of length p

approx.trace <- function(A, d, p = 1){
  A2 <- A / (nrow(A) * avg.deg(A))
  
  eig <- partial_eigen(x = A2, n = d, symmetric = T)
  
  sum(eig$values^p)
}


################################################################################
## Qianhua's subsampling based bootstrap for any statistic
## Node subsampling
## A = adjacency matrix, B = number of bootstrap samples, 
## eta = number of nodes to sample,
## fn = function to compute, 
## ncore = number of cores to use (parallelizes across B computations)

node.samp.boot <- function(A, B = 100, eta = nrow(A), fn = avg.deg, 
                           ncore = 1){
  n_ <- nrow(A)
  
  return(do.call(c, mclapply(1:B, function(xx){
    nodes <- sample.int(n_, eta, T)
    
    a.samp <- A[nodes, nodes]
    
    fn(a.samp)
  }, mc.cores = ncore)))
}

################################################################################
## Keith's ASE based bootstrap (unweighted) for any statistic
## A = adjacency matrix, B = number of bootstrap samples, 
## eta = number of nodes to sample in eta-out-of-n bootstrap,
## d = dimension of ASE,
## nonpara = whether to use nonparametric bootstrap, fn = function to compute,
## ncore = number of cores to use (parallelizes across B computations)

ASE.boot <- function(A, B = 100, eta = nrow(A), d = 1, 
                     nonpara = F, fn = avg.deg, ncore = 1){
  n_ <- nrow(A)
  X.hat <- ASE(A, d)
  
  if(!nonpara){
    return(do.call(c, mclapply(1:B, function(xx){
      nodes <- sample.int(n_, eta, T)
      
      X.boot <- X.hat[nodes, ]
      
      P.boot <- tcrossprod(X.boot)
      
      fn(P.boot)
    }, mc.cores = ncore)))
  }
  
  return(do.call(c, mclapply(1:B, function(xx) {
    nodes <- sample.int(n_, eta, T)
    
    X.boot <- X.hat[nodes, ]
    
    P.boot <- tcrossprod(X.boot)
    
    diag(P.boot) <- 0
    
    stor <- do.call('rbind', mclapply(1:(eta - 1), function(i) {
      tmp <- which(rbinom(eta - i, 1, P.boot[i, (i + 1):eta]) == 1)
      
      if (length(tmp) == 0)
        return(NULL)
      else
        return(cbind(rep(i, length(tmp)), i + tmp))
    }, mc.cores = 1))
    
    A.boot <- sparseMatrix(i = stor[, 1],
                           j = stor[, 2],
                           dims = c(eta, eta), symmetric = T)
    
    fn(A.boot)
  }, mc.cores = ncore)))
}

################################################################################
## Keith's ASE based parametric weighted bootstrap for U-statistic with additive weights
## A = adjacency matrix, B = number of bootstrap samples,
## m = order of U-statistic, h = function returning U.tilde,
## d = dimension of ASE, ncore = number of cores to use
## ncore = number of cores to use (parallelizes across B computations)

ASE.boot.wgt <- function(A, B = 100, m = 2, h = h.avg.deg, d = 1,
                         ncore = 1){
  n_ <- nrow(A)
  X.hat <- ASE(A, d)
  
  P.hat <- tcrossprod(X.hat)
  
  do.call(c, mclapply(1:B, function(xx){
    
    WW <- as.vector(rmultinom(n = 1, size = n_, prob = rep(1/n_,n_)))
    # WW <- rep(1, n_)
    # WW <- rpois(n_, lambda = 1)
    
    Ustat(PP = P.hat, m = m, h = h, W = WW)
    
  }, mc.cores = ncore))
}

################################################################################
## SONNET based unweighted bootstrap for any statistic
## A = adjacency matrix, B = number of bootstrap samples,
## s = number of subsamples, o = number of nodes to overlap,
## d = dimension of ASE, nonpara = whether to use nonparametric bootstrap,
## boot.prop = proportion of nodes to sample in each bootstrap sample,
## fn = function to compute, 
## ncore = number of cores to use (parallelizes across B computations)

SONNET.boot <- function(A, B = 100, s = 5, o = floor(nrow(A)/10), 
                            d = 1, nonpara = F,
                            boot.prop = 1, fn = norm.avg.deg, ncore = 1){
  n <- nrow(A)
  # no <- (n - o)/s
  
  X.hat <- ASE(A, d)
  
  if(!nonpara){
    return(do.call(c, mclapply(1:B, function(xx){
      np <- floor(n*boot.prop)
      nb <- sample.int(n, np, replace = T)
      
      X.b <- cbind(X.hat[nb, ])
      
      if(o > 0){
        over <- sample.int(np, o, F)
        non.over <- sample((1:np)[-over], np-o, replace = F)
      }else{
        over <- NULL
        non.over <- sample(1:np, np, replace = F)
      }
      
      # over <- sample.int(np, o, F)
      # non.over <- sample((1:np)[-over], np-o, replace = F)
      
      no <- (np - o)/s
      
      tmp.stat <- vector()
      for(q in 1:s){
        nodes <- c(over, non.over[((q-1)*no+1):(q*no)])
        X.boot <- X.b[nodes, ]
        
        P.boot <- tcrossprod(X.boot)
        tmp.stat[q] <- fn(P.boot)
      }
      
      mean(tmp.stat)
    },
    mc.cores = ncore)))
  }
  
  return(do.call(c, mclapply(1:B, function(xx){
    np <- floor(n*boot.prop)
    nb <- sample.int(n, np, replace = T)
    
    X.b <- cbind(X.hat[nb, ])
    
    if(o > 0){
      over <- sample.int(np, o, F)
      non.over <- sample((1:np)[-over], np-o, replace = F)
    }else{
      over <- NULL
      non.over <- sample(1:np, np, replace = F)
    }
    # over <- sample.int(np, o, F)
    # non.over <- sample((1:np)[-over], np-o, replace = F)
    
    no <- (np - o)/s
    
    tmp.stat <- vector()
    for(q in 1:s){
      nodes <- c(over, non.over[((q-1)*no+1):(q*no)])
      X.boot <- X.b[nodes, ]
      
      P.boot <- tcrossprod(X.boot)
      
      diag(P.boot) <- 0
      
      mm <- o + no
      
      stor <- do.call('rbind', mclapply(1:(mm - 1), function(i) {
        tmp <- which(rbinom(mm - i, 1, P.boot[i, (i + 1):mm]) == 1)
        
        if (length(tmp) == 0)
          return(NULL)
        else
          return(cbind(rep(i, length(tmp)), i + tmp))
      }, mc.cores = 1))
      
      A.boot <- sparseMatrix(i = stor[, 1],
                             j = stor[, 2],
                             dims = c(mm, mm), symmetric = T)
      
      tmp.stat[q] <- fn(A.boot)
    }
    
    mean(tmp.stat)
  },
  mc.cores = ncore)))
}

################################################################################
## SONNET based parametric weighted bootstrap for U-statistic with additive weights
## A = adjacency matrix, B = number of bootstrap samples,
## m = order of U-statistic, h = function returning U.tilde,
## s = number of subsamples, o = number of nodes to overlap,
## d = dimension of ASE, 
## boot.prop = proportion of nodes to sample in each bootstrap sample,
## ncore = number of cores to use (parallelizes across B computations)

SONNET.boot.wgt <- function(A, B = 100, m = 2, h = h.avg.deg, 
                            s = 5, o = floor(nrow(A)/10), 
                            d = 1, boot.prop = 1, ncore = 1){
  n <- nrow(A)
  # no <- (n - o)/s
  
  X.hat <- ASE(A, d)
  
  do.call(c, mclapply(1:B, function(xx){
    np <- floor(n*boot.prop)
    
    if(o > 0){
      over <- sample.int(np, o, F)
      non.over <- sample((1:np)[-over], np-o, replace = F)
    }else{
      over <- NULL
      non.over <- sample(1:np, np, replace = F)
    }
    
    no <- (np - o)/s
    
    tmp.stat <- vector()
    for(q in 1:s){
      nodes <- c(over, non.over[((q-1)*no+1):(q*no)])
      WW <- as.vector(rmultinom(n = 1, size = o+no, prob = rep(1/(o+no), o+no)))
      
      X.boot <- X.hat[nodes]
      
      P.boot <- tcrossprod(X.boot)
      
      tmp.stat[q] <- Ustat(PP = P.boot, m = m, h = h, W = WW)
    }
    
    mean(tmp.stat)
  },
  mc.cores = ncore))
  
}


################################################################################
## Returns percentile and sd based CIs from bootstrap sample
## vec = bootstrap sample, nn = number of nodes, method = method name,
## alpha = significance level, expected = expected value of theta,
## theta.hat = estimated value of theta from original sample

make_CI <- function(vec, nn = "N/A", method = "N/A", alpha = 0.05, 
                    expected = NULL, theta.hat = NULL){
  len <- length(vec)
  
  pct_lower <- quantile(vec, alpha/2)
  pct_upper <- quantile(vec, 1 - alpha/2)
  
  avg <- mean(vec)
  sd_ <- sd(vec)
  sd_lower <- avg - qnorm(1 - alpha/2)*sd_
  sd_upper <- avg + qnorm(1 - alpha/2)*sd_
  
  list(
    plot.table = data.table(method = rep(method, 2), n = rep(nn, 2), 
                            type = c("pct", "sd"),
                            lower = c(pct_lower, sd_lower),
                            upper = c(pct_upper, sd_upper),
                            avg = c(median(vec), avg), 
                            sd = c(IQR(vec), sd_),
                            expected = rep(expected, 2),
                            thetahat = rep(theta.hat, 2)
                            ),
    coverage = data.table(method = method, n = nn,
                          pct_covered = as.numeric(pct_lower <= expected & 
                                                     expected <= pct_upper),
                          pct_width = pct_upper - pct_lower,
                          sd_covered = as.numeric(sd_lower <= expected & 
                                                    expected <= sd_upper),
                          sd_width = sd_upper - sd_lower,
                          expected = expected,
                          thetahat = theta.hat
                          )
  )
}

























