# X estimated within SONNET
ase.sonnet.within <- function(A, B = 100, s = 5, o = floor(nrow(A)/10), 
                              d = 1, nonpara = F,
                              boot.prop = 1, fn = norm.avg.deg, ncore = 1){
  n <- nrow(A)
  # no <- (n - o)/s
  
  # X.hat <- ASE(A, d)
  
  if(!nonpara){
    return(do.call(c, mclapply(1:B, function(xx){
      np <- floor(n*boot.prop)
      nb <- sample.int(n, np, replace = T)
      
      # X.b <- cbind(X.hat[nb, ])
      
      over <- nb[sample.int(np, o, F)]
      non.over <- nb[sample((1:np)[-over], np-o, replace = F)]
      
      no <- (np - o)/s
      
      tmp.stat <- vector()
      for(q in 1:s){
        nodes <- c(over, non.over[((q-1)*no+1):(q*no)])
        # X.boot <- X.b[nodes, ]
        X.boot <- ASE(A[nodes, nodes], d)
        
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
    
    # X.b <- cbind(X.hat[nb, ])
    
    over <- nb[sample.int(np, o, F)]
    non.over <- nb[sample((1:np)[-over], np-o, replace = F)]
    
    no <- (np - o)/s
    
    tmp.stat <- vector()
    for(q in 1:s){
      nodes <- c(over, non.over[((q-1)*no+1):(q*no)])
      # X.boot <- X.b[nodes, ]
      X.boot <- ASE(A[nodes, nodes], d)
      
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