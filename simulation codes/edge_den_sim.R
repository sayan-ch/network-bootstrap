setwd("~/BONNET/new_sim/")
devtools::source_url("https://github.com/sayan-ch/CROISSANT/blob/main/all_base.R?raw=TRUE")



library(ggpubr)

# edge density

if(!dir.exists("Edge_Density"))
  dir.create("Edge_Density")

enum <- 100

# nn <- c(1000, 3000, 5000, 10000)
nn <- 1000

dd <- 1
aa <- 2
bb <- 3

exp.edge.density <- (aa/(aa+bb))^2

ncnc <- 8

plot.table <- list()
cover.table <- list()

for(rr in 1:enum){
  
  time.NSBoot <- time.NSBoot.mn <-
    time.ase.boot.par <- time.ase.boot.nonpar <-
    time.ase.boot.par.mn <- time.ase.boot.nonpar.mn <- 
    time.son.par <- time.son.nonpar <- 
    time.son.par.mn <- time.son.nonpar.mn <- 
    time.son.within.par <- time.son.within.nonpar <- 
    time.son.within.par.mn <- time.son.within.nonpar.mn <- vector()
  
  NSBoot <- NSBoot.mn <-
    ase.boot.par <- ase.boot.nonpar <-
    ase.boot.par.mn <- ase.boot.nonpar.mn <- 
    son.par <- son.nonpar <- 
    son.par.mn <- son.nonpar.mn <- 
    son.within.par <- son.within.nonpar <- 
    son.within.par.mn <- son.within.nonpar.mn <- list()
  
  plot.table[[rr]] <- list()
  cover.table[[rr]] <- list()
  
  for(ii in seq_along(nn)){
    gen <- gen.rdpg.beta(n = nn[ii], d = dd, a = aa, b = bb, 
                         ncore = ncnc)
    
    ss <- 5
    oo <- nn[ii]/5
    
    mm <- oo + (nn[ii] - oo)/ss
    
    # time.NSBoot[ii] <- system.time(
    #   NSBoot[[ii]] <- make_CI(node.samp.boot(
    #     A = gen$A, B = 100, m = nn[ii], fn = norm.avg.deg, ncore = ncnc),
    #     nn = nn[ii], method = "NSBoot", expected = exp.edge.density)
    # )[3]
    # 
    # time.NSBoot.mn[ii] <- system.time(
    #   NSBoot.mn[[ii]] <- make_CI(node.samp.boot(
    #     A = gen$A, B = 100, m = mm, fn = norm.avg.deg, ncore = ncnc),
    #     nn = nn[ii], method = "NSBoot m/n", expected = exp.edge.density)
    # )[3]
    # 
    time.ase.boot.par[ii] <- system.time(
      ase.boot.par[[ii]] <- make_CI(ase.boot(
        A = gen$A, B = 100, m = nn[ii], d = dd, nonpara = F,
        fn = norm.avg.deg, ncore = ncnc),
        nn = nn[ii], method = "ASEBoot par", expected = exp.edge.density)
    )[3]
    
    # time.ase.boot.nonpar[ii] <- system.time(
    #   ase.boot.nonpar[[ii]] <- make_CI(ase.boot(
    #     A = gen$A, B = 100, m = nn[ii], d = dd, nonpara = T,
    #     fn = norm.avg.deg, ncore = ncnc),
    #     nn = nn[ii], method = "ASEBoot nonpar", expected = exp.edge.density)
    # )[3]
    # 
    # time.ase.boot.par.mn[ii] <- system.time(
    #   ase.boot.par.mn[[ii]] <- make_CI(ase.boot(
    #     A = gen$A, B = 100, m = mm, d = dd, nonpara = F,
    #     fn = norm.avg.deg, ncore = ncnc),
    #     nn = nn[ii], method = "ASEBoot par m/n", expected = exp.edge.density)
    # )[3]
    # 
    # time.ase.boot.nonpar.mn[ii] <- system.time(
    #   ase.boot.nonpar.mn[[ii]] <- make_CI(ase.boot(
    #     A = gen$A, B = 100, m = mm, d = dd, nonpara = T,
    #     fn = norm.avg.deg, ncore = ncnc),
    #     nn = nn[ii], method = "ASEBoot nonpar m/n", expected = exp.edge.density)
    # )[3]
    
    time.son.par[ii] <- system.time(
      son.par[[ii]] <- make_CI(ase.sonnet.boot(
        A = gen$A, B = 100, s = ss, o = oo, d = dd, nonpara = F,
        boot.prop = 1, fn = norm.avg.deg, ncore = ncnc
      ),
      nn = nn[ii], method = "SONNET par", expected = exp.edge.density)
    )[3]
    
    # time.son.nonpar[ii] <- system.time(
    #   son.nonpar[[ii]] <- make_CI(ase.sonnet.boot(
    #     A = gen$A, B = 100, s = ss, o = oo, d = dd, nonpara = T,
    #     boot.prop = 1, fn = norm.avg.deg, ncore = ncnc
    #   ),
    #   nn = nn[ii], method = "SONNET nonpar", expected = exp.edge.density)
    # )[3]
    # 
    # time.son.par.mn[ii] <- system.time(
    #   son.par.mn[[ii]] <- make_CI(ase.sonnet.boot(
    #     A = gen$A, B = 100, s = ss, o = oo, d = dd, nonpara = F,
    #     boot.prop = 0.5, fn = norm.avg.deg, ncore = ncnc
    #   ),
    #   nn = nn[ii], method = "SONNET par m/n", expected = exp.edge.density)
    # )[3]
    # 
    # time.son.nonpar.mn[ii] <- system.time(
    #   son.nonpar.mn[[ii]] <- make_CI(ase.sonnet.boot(
    #     A = gen$A, B = 100, s = ss, o = oo, d = dd, nonpara = T,
    #     boot.prop = 0.5, fn = norm.avg.deg, ncore = ncnc
    #   ),
    #   nn = nn[ii], method = "SONNET nonpar m/n", expected = exp.edge.density)
    # )[3]
    # 
    # time.son.within.par[ii] <- NA
    # son.within.par[[ii]] <- list()
    # son.within.par[[ii]]$plot.table <- data.table(
    #   method = rep("SONNET within par", 2), n = nn[ii],
    #   type = c("pct", "se"), lower = NA, upper = NA,
    #   avg = NA
    # )
    # son.within.par[[ii]]$coverage <- data.table(
    #   method = "SONNET within par", n = nn[ii],
    #   pct_covered = NA, pct_width = NA, sd_covered = NA,
    #   sd_width = NA
    # )
    # 
    # tryCatch({time.son.within.par[ii] <- system.time(
    #   son.within.par[[ii]] <- make_CI(ase.sonnet.within(
    #     A = gen$A, B = 100, s = ss, o = oo, d = dd, nonpara = F,
    #     boot.prop = 1, fn = norm.avg.deg, ncore = ncnc
    #   ),
    #   nn = nn[ii], method = "SONNET within par", expected = exp.edge.density)
    # )[3]},
    # error = function(e){cat("son.within.par failed at rr = ", rr, " and nn = ",
    #                         nn[ii])
    #   })
    # 
    # time.son.within.nonpar[ii] <- NA
    # son.within.nonpar[[ii]] <- list()
    # son.within.nonpar[[ii]]$plot.table <- data.table(
    #   method = rep("SONNET within nonpar", 2), n = nn[ii],
    #   type = c("pct", "se"), lower = NA, upper = NA,
    #   avg = NA
    # )
    # son.within.nonpar[[ii]]$coverage <- data.table(
    #   method = "SONNET within nonpar", n = nn[ii],
    #   pct_covered = NA, pct_width = NA, sd_covered = NA,
    #   sd_width = NA
    # )
    # 
    # tryCatch({time.son.within.nonpar[ii] <- system.time(
    #   son.within.nonpar[[ii]] <- make_CI(ase.sonnet.within(
    #     A = gen$A, B = 100, s = ss, o = oo, d = dd, nonpara = T,
    #     boot.prop = 1, fn = norm.avg.deg, ncore = ncnc
    #   ),
    #   nn = nn[ii], method = "SONNET within nonpar", expected = exp.edge.density)
    # )[3]},
    # error = function(e){cat("son.within.nonpar failed at rr = ", rr, " and nn = ",
    #                         nn[ii])
    # })
    # 
    # time.son.within.par.mn[ii] <- NA
    # son.within.par.mn[[ii]] <- list()
    # son.within.par.mn[[ii]]$plot.table <- data.table(
    #   method = rep("SONNET within par m/n", 2), n = nn[ii],
    #   type = c("pct", "se"), lower = NA, upper = NA,
    #   avg = NA
    # )
    # son.within.par.mn[[ii]]$coverage <- data.table(
    #   method = "SONNET within par m/n", n = nn[ii],
    #   pct_covered = NA, pct_width = NA, sd_covered = NA,
    #   sd_width = NA
    # )
    # 
    # tryCatch({time.son.within.par.mn[ii] <- system.time(
    #   son.within.par.mn[[ii]] <- make_CI(ase.sonnet.within(
    #     A = gen$A, B = 100, s = ss, o = oo, d = dd, nonpara = F,
    #     boot.prop = 0.5, fn = norm.avg.deg, ncore = ncnc
    #   ),
    #   nn = nn[ii], method = "SONNET within par m/n", expected = exp.edge.density)
    # )[3]},
    # error = function(e){cat("son.within.par.mn failed at rr = ", rr, " and nn = ",
    #                         nn[ii])
    #   })
    # 
    # time.son.within.nonpar.mn[ii] <- NA
    # son.within.nonpar.mn[[ii]] <- list()
    # son.within.nonpar.mn[[ii]]$plot.table <- data.table(
    #   method = rep("SONNET within nonpar m/n", 2), n = nn[ii],
    #   type = c("pct", "se"), lower = NA, upper = NA,
    #   avg = NA
    # )
    # son.within.nonpar.mn[[ii]]$coverage <- data.table(
    #   method = "SONNET within nonpar m/n", n = nn[ii],
    #   pct_covered = NA, pct_width = NA, sd_covered = NA,
    #   sd_width = NA
    # )
    # 
    # tryCatch({time.son.within.nonpar.mn[ii] <- system.time(
    #   son.within.nonpar.mn[[ii]] <- make_CI(ase.sonnet.within(
    #     A = gen$A, B = 100, s = ss, o = oo, d = dd, nonpara = T,
    #     boot.prop = 0.5, fn = norm.avg.deg, ncore = ncnc
    #   ),
    #   nn = nn[ii], method = "SONNET within nonpar m/n", expected = exp.edge.density)
    # )[3]},
    # error = function(e){cat("son.within.nonpar.mn failed at rr = ", rr, " and nn = ",
    #                         nn[ii])
    # })
    
    plot.table[[rr]][[ii]] <- bind_rows(
      # NSBoot[[ii]]$plot.table, NSBoot.mn[[ii]]$plot.table,
      ase.boot.par[[ii]]$plot.table, 
      # ase.boot.par.mn[[ii]]$plot.table,
      # ase.boot.nonpar[[ii]]$plot.table, ase.boot.nonpar.mn[[ii]]$plot.table,
      son.par[[ii]]$plot.table
      # son.par.mn[[ii]]$plot.table,
      # son.nonpar[[ii]]$plot.table, son.nonpar.mn[[ii]]$plot.table,
      # son.within.par[[ii]]$plot.table, son.within.par.mn[[ii]]$plot.table,
      # son.within.nonpar[[ii]]$plot.table, son.within.nonpar.mn[[ii]]$plot.table
    ) %>%
      mutate(rep = rr, .before = 1) 
    
    write_csv(plot.table[[rr]][[ii]], "Edge_Density/edge_density_all.csv", 
              append = T)
    
    cover.table[[rr]][[ii]] <- bind_rows(
      # NSBoot[[ii]]$coverage, NSBoot.mn[[ii]]$coverage,
      ase.boot.par[[ii]]$coverage, 
      # ase.boot.par.mn[[ii]]$coverage,
      # ase.boot.nonpar[[ii]]$coverage, ase.boot.nonpar.mn[[ii]]$coverage,
      son.par[[ii]]$coverage
      # son.par.mn[[ii]]$coverage,
      # son.nonpar[[ii]]$coverage, son.nonpar.mn[[ii]]$coverage,
      # son.within.par[[ii]]$coverage, son.within.par.mn[[ii]]$coverage,
      # son.within.nonpar[[ii]]$coverage, son.within.nonpar.mn[[ii]]$coverage
    ) %>%
      mutate(rep = rr, .before = 1) %>%
      mutate(runtime = c(
        # time.NSBoot[ii], time.NSBoot.mn[ii],
        time.ase.boot.par[ii], 
        # time.ase.boot.nonpar[ii],
        # time.ase.boot.par.mn[ii], time.ase.boot.nonpar.mn[ii], 
        time.son.par[ii] 
        # time.son.nonpar[ii], 
        # time.son.par.mn[ii], time.son.nonpar.mn[ii], 
        # time.son.within.par[ii], time.son.within.nonpar[ii], 
        # time.son.within.par.mn[ii], time.son.within.nonpar.mn[ii]
      ))
    
    write_csv(cover.table[[rr]][[ii]], "Edge_Density/edge_density_coverage.csv", 
              append = T)
  }
  
  save.image(file = "Edge_Density/tmp_rdata.RData")
  
  cat("enum = ", rr, " done")
}


ctab <- do.call(bind_rows, cover.table)
ptab <- do.call(bind_rows, plot.table)

ctab %>% group_by(method) %>%
  summarize(time = mean(runtime))

ptab %>% mutate(covered = as.numeric(lower < expected & upper > expected),
                width = upper - lower) %>%
  group_by(method, n, type) %>%
  summarize(coverage = 100*mean(covered),
            mean_width = mean(width))

# trying different combinations of s and o:

plot.table <- list()
cover.table <- list()

nn <- 3000
ncnc <- 8

s.run <- c(2, 5, 10, 20)
o.run <- c(100, 200, 500, 1000, 2000)

time.ase.boot.par <- time.NSBoot <- vector()

ase.boot.par <- NSBoot <- list()


for(rr in 1:enum){
  cat("\n",rep("=", rr-1), (rr-1)/enum*100, "%")
  
  time.son.par <- time.son.nonpar <-
    # time.ase.boot.par <-
    vector()
  
  son.par <- son.nonpar <- 
    # ase.boot.par <-
    list()
  
  plot.table[[rr]] <- list()
  cover.table[[rr]] <- list()
  
  ii <- 1
  
  gen <- gen.rdpg.beta(n = nn, d = dd, a = aa, b = bb, 
                       ncore = ncnc)
  
  cat("\nNSBoot + ASEBoot par started", append = T)
  
  time.NSBoot[rr] <- system.time(
    NSBoot[[rr]] <- make_CI(node.samp.boot(
      A = gen$A, B = 100, m = nn, fn = norm.avg.deg, ncore = ncnc),
      nn = nn, method = "NSBoot", expected = exp.edge.density)
  )[3]
  
  time.ase.boot.par[rr] <- system.time(
    ase.boot.par[[rr]] <- make_CI(ase.boot(
      A = gen$A, B = 100, m = nn, d = dd, nonpara = F,
      fn = norm.avg.deg, ncore = ncnc),
      nn = nn, method = "ASEBoot par", expected = exp.edge.density)
  )[3]
  
  plot.table[[rr]][[1]] <- bind_rows(
    NSBoot[[rr]]$plot.table,
    ase.boot.par[[rr]]$plot.table
  ) %>%
    mutate(rep = rr, .before = 1) %>%
    mutate(s = 0, o = 0, eta = 0, .before = 5)
  
  write_csv(plot.table[[rr]][[1]], "Edge_Density/edge_density_para_plot.csv", 
            append = T)
  
  cover.table[[rr]][[1]] <- bind_rows(
    NSBoot[[rr]]$coverage,
    ase.boot.par[[rr]]$coverage
  ) %>%
    mutate(rep = rr, .before = 1) %>%
    mutate(runtime = c(
      time.NSBoot[rr],
      time.ase.boot.par[rr]
    ),
    s = 0, o = 0, eta = 0)
  
  write_csv(cover.table[[rr]][[1]], "Edge_Density/edge_density_para_coverage.csv", 
            append = T)
  
  cat("\nNSBoot + ASEBoot par ended", append = T)
  
  for(s.for in seq_along(s.run))
    for(o.for in seq_along(o.run)){
      
      ss <- s.run[s.for]
      oo <- o.run[o.for]
      
      mm <- oo + (nn - oo)/ss
      
      cat("\nSONNET: s = ", ss, " o = ", oo, "run = ", rr, 
          "\n", rep("-", length(s.run)*length(o.run)), 
          ii/(length(s.run)*length(o.run))*100, "%")
      
      time.son.par[ii] <- system.time(
        son.par[[ii]] <- make_CI(ase.sonnet.boot(
          A = gen$A, B = 100, s = ss, o = oo, d = dd, nonpara = F,
          boot.prop = 1, fn = norm.avg.deg, ncore = ncnc
        ),
        nn = nn, method = "SONNET par", expected = exp.edge.density)
      )[3]
      
      # time.son.nonpar[ii] <- system.time(
      #   son.nonpar[[ii]] <- make_CI(ase.sonnet.boot(
      #     A = gen$A, B = 100, s = ss, o = oo, d = dd, nonpara = T,
      #     boot.prop = 1, fn = norm.avg.deg, ncore = ncnc
      #   ),
      #   nn = nn, method = "SONNET nonpar", expected = exp.edge.density)
      # )[3]
      # 
      
      plot.table[[rr]][[1+ii]] <- bind_rows(
        son.par[[ii]]$plot.table
        # son.nonpar[[ii]]$plot.table
      ) %>%
        mutate(rep = rr, .before = 1) %>%
        mutate(s = ss, o = oo, eta = oo + mm, .before = 5)
      
      write_csv(plot.table[[rr]][[1+ii]], "Edge_Density/edge_density_para_plot.csv", 
                append = T)
      
      cover.table[[rr]][[1+ii]] <- bind_rows(
        son.par[[ii]]$coverage
        # son.nonpar[[ii]]$coverage
      ) %>%
        mutate(rep = rr, .before = 1) %>%
        mutate(runtime = c(
          time.son.par[ii] 
          # time.son.nonpar[ii]
        ),
        s = ss, o = oo, eta = oo + mm)
      
      write_csv(cover.table[[rr]][[1+ii]], "Edge_Density/edge_density_para_coverage.csv", 
                append = T)
      
      ii <- ii + 1
    }
  
  save.image(file = "Edge_Density/para_rdata.RData")
}

ctab <- do.call(bind_rows, cover.table)
ptab <- do.call(bind_rows, plot.table)

sonnet.data <- bind_cols(
  ptab %>%
    filter(method == "SONNET par") %>%
    mutate(
      covered = as.numeric(lower < expected & upper > expected),
      width = upper - lower
    ) %>%
    group_by(type, s, o) %>%
    summarize(
      coverage = 100 * mean(covered),
      mean_width = mean(width),
      .groups = "keep"
    ),
  
  data.table(runtime = rep(
    ctab %>% 
      filter(method == "SONNET par") %>%
      group_by(s, o) %>%
      summarize(runtime = mean(runtime), .groups = "keep") %>%
      pull(runtime),
    2
  ))
)

ptab %>%
  filter(type == "pct", method != "NSBoot") %>%
  mutate(
    covered = as.numeric(lower < expected & upper > expected),
    width = upper - lower
  ) %>%
  group_by(type, s, o) %>%
  summarize(
    coverage = 100 * mean(covered),
    mean_width = mean(width),
    .groups = "keep"
  ) %>%
  mutate(s = factor(s)) %>%
  ggplot(aes(x = o, y = coverage, color = s)) +
  geom_line() + 
  theme_pubr() +
  scale_x_continuous(breaks = oo) +
  geom_hline(yintercept = 95, linetype = "dashed")

write_csv(sonnet.data, "Edge_Density/sonnet_data_3k.csv")

ss <- unique(sonnet.data$s)
oo <- unique(sonnet.data$o)

sonnet.data %>%
  filter(type == "sd") %>%
  mutate(s = factor(s)) %>%
  ggplot(aes(x = o, y = coverage, color = s)) +
  geom_line() + 
  theme_pubr() +
  scale_x_continuous(breaks = oo) +
  geom_hline(yintercept = 95, linetype = "dashed")

sonnet.data %>%
  filter(type == "pct") %>%
  mutate(s = factor(s)) %>%
  ggplot(aes(x = o, y = mean_width, color = s)) +
  geom_line() + 
  theme_pubr() +
  scale_x_continuous(breaks = oo)


other.data <- bind_cols(
  ptab %>%
    filter(method != "SONNET par") %>%
    mutate(
      covered = as.numeric(lower < expected & upper > expected),
      width = upper - lower
    ) %>%
    group_by(method, type, s, o) %>%
    summarize(
      coverage = 100 * mean(covered),
      mean_width = mean(width),
      .groups = "keep"
    ),
  
  data.table(runtime = rep(
    ctab %>% 
      filter(method != "SONNET par") %>%
      group_by(method, s, o) %>%
      summarize(runtime = mean(runtime), .groups = "keep") %>%
      pull(runtime),
    2
  ))
)



all.data <- read_csv("Edge_Density/edge_density_all.csv")
all.cover <- read_csv("Edge_Density/edge_density_coverage.csv")

ase.data <- bind_cols(all.data %>%
                        filter(method == "ASEBoot par") %>%
                        mutate(
                          covered = as.numeric(lower < expected & upper > expected),
                          width = upper - lower
                        ) %>%
                        group_by(type) %>%
                        summarize(
                          coverage = 100 * mean(covered),
                          mean_width = mean(width),
                          .groups = "keep"
                        ),
                      
                      data.table(runtime = rep(
                        all.cover %>% filter(method == "ASEBoot par") %>%
                          summarize(runtime = mean(runtime), .groups = "keep") %>%
                          pull(runtime),
                        2
                      ))
)



















