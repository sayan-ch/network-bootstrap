setwd("~/BONNET/new_sim/")
devtools::source_url("https://github.com/sayan-ch/network-bootstrap/blob/main/base_codes.R?raw=TRUE")

# average shortest path

if(!dir.exists("short_path"))
  dir.create("short_path")

write_csv(data.table(
  rep = NA,
  method = NA, n = NA, s = NA, o = NA, d = NA,
  type = NA, lower = NA, upper = NA, avg = NA,
  sd = NA, expected = NA, thetahat = NA
), "short_path/short_path_all.csv")

write_csv(data.table(
  rep = NA,
  method = NA, n = NA, s = NA, o = NA, d = NA,
  pct_covered = NA, pct_width = NA, sd_covered = NA,
  sd_width = NA, expected = NA, thetahat = NA,
  runtime = NA
), "short_path/short_path_coverage.csv")

enum <- 200

nn <- c(500, 1000, 1500, 2000)

dd <- 1
aa <- 2
bb <- 3

ncnc <- 20

# estimating the expected average shortest path
exp.short.path <- vector()

for(ii in seq_along(nn)){
  out <- do.call(c, mclapply(1:1000, function(jj){
    net <- gen.rdpg.beta(nn[ii], dd, aa, bb, ncore = 1)
    
    avg.short.path(net$A)
  }, mc.cores = ncnc))
  exp.short.path[ii] <- mean(out)
}

plot.table <- list()
cover.table <- list()

for(rr in 1:enum){
  cat("\n", rep("=", rr-1), round((rr-1)/length(nn)*100), "%")
  
  time.ns.boot <- time.ase.par <- time.ase.wgt <-
    time.ase.nonpar <- time.son.par <- time.son.wgt <-
    time.son.nonpar <- vector()
  
  ns.boot <- ase.par <- ase.wgt <- ase.nonpar <- 
    son.par <- son.wgt <- son.nonpar <- list()
  
  plot.table[[rr]] <- list()
  cover.table[[rr]] <- list()
  
  for(ii in seq_along(nn)){
    gen <- gen.rdpg.beta(n = nn[ii], d = dd, a = aa, b = bb, 
                         ncore = ncnc)
    
    theta.hat <- avg.short.path(gen$A)
    
    time.ns.boot[ii] <- system.time(
      ns.boot[[ii]] <- make_CI(node.samp.boot(
        A = gen$A, B = 100, eta = nn[ii], fn = avg.short.path, ncore = ncnc),
        nn = nn[ii], method = "NSBoot", alpha = 0.05, 
        expected = exp.short.path[ii], theta.hat = theta.hat,
        s = 0, o = 0, d = dd)
    )[3]
    
    # time.ase.par[ii] <- system.time(
    #   ase.par[[ii]] <- make_CI(ASE.boot(
    #     A = gen$A, B = 100, eta = nn[ii], d = dd, nonpara = F,
    #     fn = avg.short.path, ncore = ncnc),
    #     nn = nn[ii], method = "ASEpar", alpha = 0.05,
    #     expected = exp.short.path, theta.hat = theta.hat,
    #     s = 0, o = 0, d = dd)
    #   )[3]
    
    # time.ase.wgt[ii] <- system.time(
    #   ase.wgt[[ii]] <- make_CI(ASE.boot.wgt(
    #     A = gen$A, B = 100, m = 2, h = h.avg.short.path, d = dd, ncore = ncnc),
    #     nn = nn[ii], method = "ASEWeighted", alpha = 0.05,
    #     expected = exp.short.path, theta.hat = theta.hat,
    #     s = 0, o = 0, d = dd)
    # )[3]
    
    time.ase.nonpar[ii] <- system.time(
      ase.nonpar[[ii]] <- make_CI(ASE.boot(
        A = gen$A, B = 100, eta = nn[ii], d = dd, nonpara = T,
        fn = avg.short.path, ncore = ncnc),
        nn = nn[ii], method = "ASEnonpar", alpha = 0.05,
        expected = exp.short.path[ii], theta.hat = theta.hat,
        s = 0, o = 0, d = dd)
    )[3]
    
    
    plot.table[[rr]][[ii]] <- bind_rows(
      ns.boot[[ii]]$plot.table, 
      # ase.par[[ii]]$plot.table, ase.wgt[[ii]]$plot.table, 
      ase.nonpar[[ii]]$plot.table
    ) %>%
      mutate(rep = rr, .before = 1)
    
    cover.table[[rr]][[ii]] <- bind_rows(
      ns.boot[[ii]]$coverage, 
      # ase.par[[ii]]$coverage, ase.wgt[[ii]]$coverage,
      ase.nonpar[[ii]]$coverage
    ) %>%
      mutate(rep = rr, .before = 1) %>%
      mutate(runtime = c(
        time.ns.boot[ii], 
        # time.ase.par[ii], time.ase.wgt[ii],
        time.ase.nonpar[ii]
      ))
    
    ss <- c(2, 5, 10)
    oo <- c(0, nn[ii]/10, nn[ii]/5, nn[ii]/2 )
    
    cat("\nenum = ", rr, " NS and ASE done.")
    
    cc <- 1
    for(si in ss)
      for(oi in oo){
        cat("\nSONNET: s = ", si, " o = ", oi, "run = ", rr, 
            "\n", rep("-", length(ss)*length(oo)), 
            cc/(length(ss)*length(oo))*100, "%")
        
        # time.son.par[ii] <- system.time(
        #   son.par[[ii]] <- make_CI(SONNET.boot(
        #     A = gen$A, B = 100, s = si, o = oi, d = dd, nonpara = F,
        #     boot.prop = 1, fn = avg.short.path, ncore = ncnc),
        #     nn = nn[ii], method = "SONNETpar", alpha = 0.05,
        #     expected = exp.short.path, theta.hat = theta.hat,
        #     s = si, o = oi, d = dd)
        # )[3]    
        # 
        # time.son.wgt[ii] <- system.time(
        #   son.wgt[[ii]] <- make_CI(SONNET.boot.wgt(
        #     A = gen$A, B = 100, m =2, h = h.avg.short.path,
        #     s = si, o = oi, d = dd, ncore = ncnc),
        #     nn = nn[ii], method = "SONNETWeighted", alpha = 0.05,
        #     expected = exp.short.path, theta.hat = theta.hat,
        #     s = si, o = oi, d = dd)
        # )[3]
        
        time.son.nonpar[ii] <- system.time(
          son.nonpar[[ii]] <- make_CI(SONNET.boot(
            A = gen$A, B = 100, s = si, o = oi, d = dd, nonpara = T,
            boot.prop = 1, fn = avg.short.path, ncore = ncnc),
            nn = nn[ii], method = "SONNETnonpar", alpha = 0.05,
            expected = exp.short.path[ii], theta.hat = theta.hat,
            s = si, o = oi, d = dd)
        )[3]
        
        plot.table[[rr]][[ii]] <- bind_rows(
          plot.table[[rr]][[ii]],
          bind_rows(
                    # son.par[[ii]]$plot.table,
                    # son.wgt[[ii]]$plot.table,
                    son.nonpar[[ii]]$plot.table) %>%
            mutate(rep = rr, .before = 1)
        )
        
        cover.table[[rr]][[ii]] <- bind_rows(
          cover.table[[rr]][[ii]],
          bind_rows(
                    # son.par[[ii]]$coverage,
                    # son.wgt[[ii]]$coverage,
                    son.nonpar[[ii]]$coverage) %>%
            mutate(rep = rr, .before = 1) %>%
            mutate(runtime = c(
                               # time.son.par[ii],
                               # time.son.wgt[ii],
                               time.son.nonpar[ii]))
        )
        
        cc <- cc + 1
      }
    
    write_csv(plot.table[[rr]][[ii]], "short_path/short_path_all.csv", 
              append = T)
    
    write_csv(cover.table[[rr]][[ii]], "short_path/short_path_coverage.csv",
              append = T)
    
    gc()
  }
  
  save.image(file = "short_path/tmp_rdata.RData")
  
  cat("enum = ", rr, " done")
}

