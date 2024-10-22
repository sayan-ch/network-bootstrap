setwd("cluster_output")

## Edge Density
library(tidyverse)
library(data.table)
library(rempsyc)
library(ggpubr)


# edge density coverage
edc <- read_csv("edge_density_coverage.csv")

edc <- edc %>% drop_na()

method <- unique(edc$method)
nn <- unique(edc$n)

ss <- unique(edc$s)
ss <- ss[ss != 0]

################################################################################
edc.sum <- edc %>% 
  group_by(method, n, s, o) %>%
  summarize(
    mean_pct_covered = 100*mean(pct_covered),
    mean_pct_width = mean(pct_width),
    mean_sd_covered = 100*mean(sd_covered),
    mean_sd_width = mean(sd_width),
    mean_runtime = mean(runtime)
  )

## plotting width, coverage and time for different parameters of SONNET pct
## n = 2000
## coverage vs s
edc.cov.pct <- edc.sum %>%
  filter(n == 2000, method %in% c("SONNETpar", "SONNETWeighted", "SONNETnonpar")) %>%
  ggplot(aes(x = s, y = mean_pct_covered, color = factor(o), linetype = factor(o))) +
  geom_line() + 
  theme_pubr() +
  scale_x_continuous(breaks = ss) +
  geom_hline(yintercept = 95, linetype = "dashed") +
  facet_wrap( ~ method)

## width vs s
edc.width.pct <- edc.sum %>%
  filter(n == 2000, method %in% c("SONNETpar", "SONNETWeighted", "SONNETnonpar")) %>%
  ggplot(aes(x = s, y = mean_pct_width, color = factor(o), linetype = factor(o))) +
  geom_line() + 
  theme_pubr() +
  scale_x_continuous(breaks = ss) +
  facet_wrap( ~ method)

## runtime vs s
edc.runtime.pct <- edc.sum %>%
  filter(n == 2000, method %in% c("SONNETpar", "SONNETWeighted", "SONNETnonpar")) %>%
  ggplot(aes(x = s, y = mean_runtime, color = factor(o), linetype = factor(o))) +
  geom_line() + 
  theme_pubr() +
  scale_x_continuous(breaks = ss) +
  facet_wrap( ~ method)

ggarrange(edc.cov.pct, edc.width.pct, edc.runtime.pct, ncol = 1, nrow = 3,
          common.legend = T) %>%
  annotate_figure(top = text_grob("SONNET percentile CI", 
                                  face = "bold", size = 14))

## plotting width, coverage and time for different parameters of SONNET sd
## coverage vs s
edc.cov.sd <- edc.sum %>%
  filter(n == 2000, method %in% c("SONNETpar", "SONNETWeighted", "SONNETnonpar")) %>%
  ggplot(aes(x = s, y = mean_sd_covered, color = factor(o), linetype = factor(o))) +
  geom_line() + 
  theme_pubr() +
  scale_x_continuous(breaks = ss) +
  geom_hline(yintercept = 95, linetype = "dashed") +
  facet_wrap( ~ method)

## width vs s
edc.width.sd <- edc.sum %>%
  filter(n == 2000, method %in% c("SONNETpar", "SONNETWeighted", "SONNETnonpar")) %>%
  ggplot(aes(x = s, y = mean_sd_width, color = factor(o), linetype = factor(o))) +
  geom_line() + 
  theme_pubr() +
  scale_x_continuous(breaks = ss) +
  facet_wrap( ~ method)

## runtime vs s
edc.runtime.sd <- edc.sum %>%
  filter(n == 2000, method %in% c("SONNETpar", "SONNETWeighted", "SONNETnonpar")) %>%
  ggplot(aes(x = s, y = mean_runtime, color = factor(o), linetype = factor(o))) +
  geom_line() + 
  theme_pubr() +
  scale_x_continuous(breaks = ss) +
  facet_wrap( ~ method)

ggarrange(edc.cov.sd, edc.width.sd, edc.runtime.sd, ncol = 1, nrow = 3,
          common.legend = T) %>%
  annotate_figure(top = text_grob("SONNET sd CI", 
                                  face = "bold", size = 14))


#################
edc.all <- bind_rows(
  edc.sum %>%
    filter(method %in% c("NSBoot", "ASEpar", "ASEnonpar", "ASEWeighted")),
  
  edc.sum %>% 
    filter(method == "SONNETnonpar", s == 2, o == 0 ),
  
  edc.sum %>%
    filter(method == "SONNETpar", s == 2, o == n/10), 
  
  edc.sum %>%
    filter(method == "SONNETWeighted", s == 5, o == n/10),
)

## pct
## coverage vs n
edc.cov.pct <- edc.all %>%
  filter(method %in% c("ASEpar", "SONNETpar", "ASEWeighted", "SONNETWeighted")) %>%
  ggplot(aes(x = n, y = mean_pct_covered, color = method, linetype = method)) +
  geom_line() + 
  theme_pubr() +
  scale_x_continuous(breaks = nn) +
  geom_hline(yintercept = 95, linetype = "dashed")

## width vs n
edc.width.pct <- edc.all %>%
  filter(method %in% c("ASEpar", "SONNETpar", "ASEWeighted", "SONNETWeighted")) %>%
  ggplot(aes(x = n, y = mean_pct_width, color = method, linetype = method)) +
  geom_line() + 
  theme_pubr() +
  scale_x_continuous(breaks = nn)

## runtime vs n
edc.runtime.pct <- edc.all %>%
  filter(method %in% c("ASEpar", "SONNETpar", "ASEWeighted", "SONNETWeighted")) %>%
  ggplot(aes(x = n, y = mean_runtime, color = method, linetype = method)) +
  geom_line() + 
  theme_pubr() +
  scale_x_continuous(breaks = nn)

ggarrange(edc.cov.pct, edc.width.pct, edc.runtime.pct, ncol = 1, nrow = 3,
          common.legend = T) %>%
  annotate_figure(top = text_grob("Percentile CI", 
                                  face = "bold", size = 14))

## sd
## coverage vs n
edc.cov.sd <- edc.all %>%
  filter(method %in% c("ASEpar", "SONNETpar", "ASEWeighted", "SONNETWeighted")) %>%
  ggplot(aes(x = n, y = mean_sd_covered, color = method, linetype = method)) +
  geom_line() + 
  theme_pubr() +
  scale_x_continuous(breaks = nn) +
  geom_hline(yintercept = 95, linetype = "dashed")

## width vs n
edc.width.sd <- edc.all %>%
  filter(method %in% c("ASEpar", "SONNETpar", "ASEWeighted", "SONNETWeighted")) %>%
  ggplot(aes(x = n, y = mean_sd_width, color = method, linetype = method)) +
  geom_line() + 
  theme_pubr() +
  scale_x_continuous(breaks = nn)

## runtime vs n
edc.runtime.sd <- edc.all %>%
  filter(method %in% c("ASEpar", "SONNETpar", "ASEWeighted", "SONNETWeighted")) %>%
  ggplot(aes(x = n, y = mean_runtime, color = method, linetype = method)) +
  geom_line() + 
  theme_pubr() +
  scale_x_continuous(breaks = nn)

ggarrange(edc.cov.sd, edc.width.sd, edc.runtime.sd, ncol = 1, nrow = 3,
          common.legend = T) %>%
  annotate_figure(top = text_grob("SD CI", 
                                  face = "bold", size = 14))



################################################################################

## shortest path
edc <- read_csv("short_path_coverage.csv")

edc <- edc %>% drop_na()

method <- unique(edc$method)
nn <- unique(edc$n)

ss <- unique(edc$s)
ss <- ss[ss != 0]

################################################################################
edc.sum <- edc %>% 
  group_by(method, n, s, o) %>%
  summarize(
    mean_pct_covered = 100*mean(pct_covered),
    mean_pct_width = mean(pct_width),
    mean_sd_covered = 100*mean(sd_covered),
    mean_sd_width = mean(sd_width),
    mean_runtime = mean(runtime)
  )

## plotting width, coverage and time for different parameters of SONNET pct
## n = 2000
## coverage vs s
edc.cov.pct <- edc.sum %>%
  filter(n == 2000, method %in% c("SONNETpar", "SONNETWeighted", "SONNETnonpar")) %>%
  ggplot(aes(x = s, y = mean_pct_covered, color = factor(o), linetype = factor(o))) +
  geom_line() + 
  theme_pubr() +
  scale_x_continuous(breaks = ss) +
  geom_hline(yintercept = 95, linetype = "dashed") +
  facet_wrap( ~ method)

## width vs s
edc.width.pct <- edc.sum %>%
  filter(n == 2000, method %in% c("SONNETpar", "SONNETWeighted", "SONNETnonpar")) %>%
  ggplot(aes(x = s, y = mean_pct_width, color = factor(o), linetype = factor(o))) +
  geom_line() + 
  theme_pubr() +
  scale_x_continuous(breaks = ss) +
  facet_wrap( ~ method)

## runtime vs s
edc.runtime.pct <- edc.sum %>%
  filter(n == 2000, method %in% c("SONNETpar", "SONNETWeighted", "SONNETnonpar")) %>%
  ggplot(aes(x = s, y = mean_runtime, color = factor(o), linetype = factor(o))) +
  geom_line() + 
  theme_pubr() +
  scale_x_continuous(breaks = ss) +
  facet_wrap( ~ method)

ggarrange(edc.cov.pct, edc.width.pct, edc.runtime.pct, ncol = 1, nrow = 3,
          common.legend = T) %>%
  annotate_figure(top = text_grob("SONNET percentile CI", 
                                  face = "bold", size = 14))

## plotting width, coverage and time for different parameters of SONNET sd
## coverage vs s
edc.cov.sd <- edc.sum %>%
  filter(n == 2000, method %in% c("SONNETpar", "SONNETWeighted", "SONNETnonpar")) %>%
  ggplot(aes(x = s, y = mean_sd_covered, color = factor(o), linetype = factor(o))) +
  geom_line() + 
  theme_pubr() +
  scale_x_continuous(breaks = ss) +
  geom_hline(yintercept = 95, linetype = "dashed") +
  facet_wrap( ~ method)

## width vs s
edc.width.sd <- edc.sum %>%
  filter(n == 2000, method %in% c("SONNETpar", "SONNETWeighted", "SONNETnonpar")) %>%
  ggplot(aes(x = s, y = mean_sd_width, color = factor(o), linetype = factor(o))) +
  geom_line() + 
  theme_pubr() +
  scale_x_continuous(breaks = ss) +
  facet_wrap( ~ method)

## runtime vs s
edc.runtime.sd <- edc.sum %>%
  filter(n == 2000, method %in% c("SONNETpar", "SONNETWeighted", "SONNETnonpar")) %>%
  ggplot(aes(x = s, y = mean_runtime, color = factor(o), linetype = factor(o))) +
  geom_line() + 
  theme_pubr() +
  scale_x_continuous(breaks = ss) +
  facet_wrap( ~ method)

ggarrange(edc.cov.sd, edc.width.sd, edc.runtime.sd, ncol = 1, nrow = 3,
          common.legend = T) %>%
  annotate_figure(top = text_grob("SONNET sd CI", 
                                  face = "bold", size = 14))


#################
edc.all <- bind_rows(
  edc.sum %>%
    filter(method %in% c("NSBoot", "ASEpar", "ASEnonpar", "ASEWeighted")),
  
  edc.sum %>% 
    filter(method == "SONNETnonpar", s == 5, o == n/2 ),
  
  edc.sum %>%
    filter(method == "SONNETpar", s == 2, o == n/10), 
  
  edc.sum %>%
    filter(method == "SONNETWeighted", s == 5, o == n/2),
)

## pct
## coverage vs n
edc.cov.pct <- edc.all %>%
  # filter(method %in% c("ASEpar", "SONNETpar", "ASEWeighted", "SONNETWeighted")) %>%
  ggplot(aes(x = n, y = mean_pct_covered, color = method, linetype = method)) +
  geom_line() + 
  theme_pubr() +
  scale_x_continuous(breaks = nn) +
  geom_hline(yintercept = 95, linetype = "dashed")

## width vs n
edc.width.pct <- edc.all %>%
  # filter(method %in% c("ASEpar", "SONNETpar", "ASEWeighted", "SONNETWeighted")) %>%
  ggplot(aes(x = n, y = mean_pct_width, color = method, linetype = method)) +
  geom_line() + 
  theme_pubr() +
  scale_x_continuous(breaks = nn)

## runtime vs n
edc.runtime.pct <- edc.all %>%
  # filter(method %in% c("ASEpar", "SONNETpar", "ASEWeighted", "SONNETWeighted")) %>%
  ggplot(aes(x = n, y = mean_runtime, color = method, linetype = method)) +
  geom_line() + 
  theme_pubr() +
  scale_x_continuous(breaks = nn)

ggarrange(edc.cov.pct, edc.width.pct, edc.runtime.pct, ncol = 1, nrow = 3,
          common.legend = T) %>%
  annotate_figure(top = text_grob("Percentile CI", 
                                  face = "bold", size = 14))

## sd
## coverage vs n
edc.cov.sd <- edc.all %>%
  # filter(method %in% c("ASEpar", "SONNETpar", "ASEWeighted", "SONNETWeighted")) %>%
  ggplot(aes(x = n, y = mean_sd_covered, color = method, linetype = method)) +
  geom_line() + 
  theme_pubr() +
  scale_x_continuous(breaks = nn) +
  geom_hline(yintercept = 95, linetype = "dashed")

## width vs n
edc.width.sd <- edc.all %>%
  # filter(method %in% c("ASEpar", "SONNETpar", "ASEWeighted", "SONNETWeighted")) %>%
  ggplot(aes(x = n, y = mean_sd_width, color = method, linetype = method)) +
  geom_line() + 
  theme_pubr() +
  scale_x_continuous(breaks = nn)

## runtime vs n
edc.runtime.sd <- edc.all %>%
  # filter(method %in% c("ASEpar", "SONNETpar", "ASEWeighted", "SONNETWeighted")) %>%
  ggplot(aes(x = n, y = mean_runtime, color = method, linetype = method)) +
  geom_line() + 
  theme_pubr() +
  scale_x_continuous(breaks = nn)

ggarrange(edc.cov.sd, edc.width.sd, edc.runtime.sd, ncol = 1, nrow = 3,
          common.legend = T) %>%
  annotate_figure(top = text_grob("SD CI", 
                                  face = "bold", size = 14))



################################################################################
## Largest eigenvalue

## shortest path
edc <- read_csv("large_eigen_coverage.csv")

edc <- edc %>% drop_na()

method <- unique(edc$method)
nn <- unique(edc$n)

ss <- unique(edc$s)
ss <- ss[ss != 0]

################################################################################
edc.sum <- edc %>% 
  group_by(method, n, s, o) %>%
  summarize(
    mean_pct_covered = 100*mean(pct_covered),
    mean_pct_width = mean(pct_width),
    mean_sd_covered = 100*mean(sd_covered),
    mean_sd_width = mean(sd_width),
    mean_runtime = mean(runtime)
  )

## plotting width, coverage and time for different parameters of SONNET pct
## n = 2000
## coverage vs s
edc.cov.pct <- edc.sum %>%
  filter(n == 2000, method %in% c("SONNETpar", "SONNETWeighted", "SONNETnonpar")) %>%
  ggplot(aes(x = s, y = mean_pct_covered, color = factor(o), linetype = factor(o))) +
  geom_line() + 
  theme_pubr() +
  scale_x_continuous(breaks = ss) +
  geom_hline(yintercept = 95, linetype = "dashed") +
  facet_wrap( ~ method)

## width vs s
edc.width.pct <- edc.sum %>%
  filter(n == 2000, method %in% c("SONNETpar", "SONNETWeighted", "SONNETnonpar")) %>%
  ggplot(aes(x = s, y = mean_pct_width, color = factor(o), linetype = factor(o))) +
  geom_line() + 
  theme_pubr() +
  scale_x_continuous(breaks = ss) +
  facet_wrap( ~ method)

## runtime vs s
edc.runtime.pct <- edc.sum %>%
  filter(n == 2000, method %in% c("SONNETpar", "SONNETWeighted", "SONNETnonpar")) %>%
  ggplot(aes(x = s, y = mean_runtime, color = factor(o), linetype = factor(o))) +
  geom_line() + 
  theme_pubr() +
  scale_x_continuous(breaks = ss) +
  facet_wrap( ~ method)

ggarrange(edc.cov.pct, edc.width.pct, edc.runtime.pct, ncol = 1, nrow = 3,
          common.legend = T) %>%
  annotate_figure(top = text_grob("SONNET percentile CI", 
                                  face = "bold", size = 14))

## plotting width, coverage and time for different parameters of SONNET sd
## coverage vs s
edc.cov.sd <- edc.sum %>%
  filter(n == 2000, method %in% c("SONNETpar", "SONNETWeighted", "SONNETnonpar")) %>%
  ggplot(aes(x = s, y = mean_sd_covered, color = factor(o), linetype = factor(o))) +
  geom_line() + 
  theme_pubr() +
  scale_x_continuous(breaks = ss) +
  geom_hline(yintercept = 95, linetype = "dashed") +
  facet_wrap( ~ method)

## width vs s
edc.width.sd <- edc.sum %>%
  filter(n == 2000, method %in% c("SONNETpar", "SONNETWeighted", "SONNETnonpar")) %>%
  ggplot(aes(x = s, y = mean_sd_width, color = factor(o), linetype = factor(o))) +
  geom_line() + 
  theme_pubr() +
  scale_x_continuous(breaks = ss) +
  facet_wrap( ~ method)

## runtime vs s
edc.runtime.sd <- edc.sum %>%
  filter(n == 2000, method %in% c("SONNETpar", "SONNETWeighted", "SONNETnonpar")) %>%
  ggplot(aes(x = s, y = mean_runtime, color = factor(o), linetype = factor(o))) +
  geom_line() + 
  theme_pubr() +
  scale_x_continuous(breaks = ss) +
  facet_wrap( ~ method)

ggarrange(edc.cov.sd, edc.width.sd, edc.runtime.sd, ncol = 1, nrow = 3,
          common.legend = T) %>%
  annotate_figure(top = text_grob("SONNET sd CI", 
                                  face = "bold", size = 14))


#################
edc.all <- bind_rows(
  edc.sum %>%
    filter(method %in% c("NSBoot", "ASEpar", "ASEnonpar", "ASEWeighted")),
  
  edc.sum %>% 
    filter(method == "SONNETnonpar", s == 5, o == n/10 ),
  
  edc.sum %>%
    filter(method == "SONNETpar", s == 5, o == n/10), 
  
  edc.sum %>%
    filter(method == "SONNETWeighted", s == 5, o == n/2),
)

## pct
## coverage vs n
edc.cov.pct <- edc.all %>%
  filter(method %in% c("ASEnonpar", "SONNETnonpar", "NSBoot")) %>%
  # filter(method %in% c("ASEpar", "SONNETpar", "ASEWeighted", "SONNETWeighted")) %>%
  ggplot(aes(x = n, y = mean_pct_covered, color = method, linetype = method)) +
  geom_line() + 
  theme_pubr() +
  scale_x_continuous(breaks = nn) +
  geom_hline(yintercept = 95, linetype = "dashed")

## width vs n
edc.width.pct <- edc.all %>%
  filter(method %in% c("ASEnonpar", "SONNETnonpar", "NSBoot")) %>%
  # filter(method %in% c("ASEpar", "SONNETpar", "ASEWeighted", "SONNETWeighted")) %>%
  ggplot(aes(x = n, y = mean_pct_width, color = method, linetype = method)) +
  geom_line() + 
  theme_pubr() +
  scale_x_continuous(breaks = nn)

## runtime vs n
edc.runtime.pct <- edc.all %>%
  filter(method %in% c("ASEnonpar", "SONNETnonpar", "NSBoot")) %>%
  # filter(method %in% c("ASEpar", "SONNETpar", "ASEWeighted", "SONNETWeighted")) %>%
  ggplot(aes(x = n, y = mean_runtime, color = method, linetype = method)) +
  geom_line() + 
  theme_pubr() +
  scale_x_continuous(breaks = nn)

ggarrange(edc.cov.pct, edc.width.pct, edc.runtime.pct, ncol = 1, nrow = 3,
          common.legend = T) %>%
  annotate_figure(top = text_grob("Percentile CI", 
                                  face = "bold", size = 14))

## sd
## coverage vs n
edc.cov.sd <- edc.all %>%
  filter(method %in% c("ASEnonpar", "SONNETnonpar", "NSBoot")) %>%
  # filter(method %in% c("ASEpar", "SONNETpar", "ASEWeighted", "SONNETWeighted")) %>%
  ggplot(aes(x = n, y = mean_sd_covered, color = method, linetype = method)) +
  geom_line() + 
  theme_pubr() +
  scale_x_continuous(breaks = nn) +
  geom_hline(yintercept = 95, linetype = "dashed")

## width vs n
edc.width.sd <- edc.all %>%
  filter(method %in% c("ASEnonpar", "SONNETnonpar", "NSBoot")) %>%
  # filter(method %in% c("ASEpar", "SONNETpar", "ASEWeighted", "SONNETWeighted")) %>%
  ggplot(aes(x = n, y = mean_sd_width, color = method, linetype = method)) +
  geom_line() + 
  theme_pubr() +
  scale_x_continuous(breaks = nn)

## runtime vs n
edc.runtime.sd <- edc.all %>%
  filter(method %in% c("ASEnonpar", "SONNETnonpar", "NSBoot")) %>%
  # filter(method %in% c("ASEpar", "SONNETpar", "ASEWeighted", "SONNETWeighted")) %>%
  ggplot(aes(x = n, y = mean_runtime, color = method, linetype = method)) +
  geom_line() + 
  theme_pubr() +
  scale_x_continuous(breaks = nn)

ggarrange(edc.cov.sd, edc.width.sd, edc.runtime.sd, ncol = 1, nrow = 3,
          common.legend = T) %>%
  annotate_figure(top = text_grob("SD CI", 
                                  face = "bold", size = 14))



################################################################################


################################################################################
## Approximate Trace

## Approximate Trace
edc <- read_csv("approx_trace_coverage.csv")

edc <- edc %>% drop_na()

method <- unique(edc$method)
nn <- unique(edc$n)

ss <- unique(edc$s)
ss <- ss[ss != 0]

################################################################################
edc.sum <- edc %>% 
  group_by(method, n, s, o) %>%
  summarize(
    mean_pct_covered = 100*mean(pct_covered),
    mean_pct_width = mean(pct_width),
    mean_sd_covered = 100*mean(sd_covered),
    mean_sd_width = mean(sd_width),
    mean_runtime = mean(runtime)
  )

## plotting width, coverage and time for different parameters of SONNET pct
## n = 2000
## coverage vs s
edc.cov.pct <- edc.sum %>%
  filter(n == 2000, method %in% c("SONNETpar", "SONNETWeighted", "SONNETnonpar")) %>%
  ggplot(aes(x = s, y = mean_pct_covered, color = factor(o), linetype = factor(o))) +
  geom_line() + 
  theme_pubr() +
  scale_x_continuous(breaks = ss) +
  geom_hline(yintercept = 95, linetype = "dashed") +
  facet_wrap( ~ method)

## width vs s
edc.width.pct <- edc.sum %>%
  filter(n == 2000, method %in% c("SONNETpar", "SONNETWeighted", "SONNETnonpar")) %>%
  ggplot(aes(x = s, y = mean_pct_width, color = factor(o), linetype = factor(o))) +
  geom_line() + 
  theme_pubr() +
  scale_x_continuous(breaks = ss) +
  facet_wrap( ~ method)

## runtime vs s
edc.runtime.pct <- edc.sum %>%
  filter(n == 2000, method %in% c("SONNETpar", "SONNETWeighted", "SONNETnonpar")) %>%
  ggplot(aes(x = s, y = mean_runtime, color = factor(o), linetype = factor(o))) +
  geom_line() + 
  theme_pubr() +
  scale_x_continuous(breaks = ss) +
  facet_wrap( ~ method)

ggarrange(edc.cov.pct, edc.width.pct, edc.runtime.pct, ncol = 1, nrow = 3,
          common.legend = T) %>%
  annotate_figure(top = text_grob("SONNET percentile CI", 
                                  face = "bold", size = 14))

## plotting width, coverage and time for different parameters of SONNET sd
## coverage vs s
edc.cov.sd <- edc.sum %>%
  filter(n == 2000, method %in% c("SONNETpar", "SONNETWeighted", "SONNETnonpar")) %>%
  ggplot(aes(x = s, y = mean_sd_covered, color = factor(o), linetype = factor(o))) +
  geom_line() + 
  theme_pubr() +
  scale_x_continuous(breaks = ss) +
  geom_hline(yintercept = 95, linetype = "dashed") +
  facet_wrap( ~ method)

## width vs s
edc.width.sd <- edc.sum %>%
  filter(n == 2000, method %in% c("SONNETpar", "SONNETWeighted", "SONNETnonpar")) %>%
  ggplot(aes(x = s, y = mean_sd_width, color = factor(o), linetype = factor(o))) +
  geom_line() + 
  theme_pubr() +
  scale_x_continuous(breaks = ss) +
  facet_wrap( ~ method)

## runtime vs s
edc.runtime.sd <- edc.sum %>%
  filter(n == 2000, method %in% c("SONNETpar", "SONNETWeighted", "SONNETnonpar")) %>%
  ggplot(aes(x = s, y = mean_runtime, color = factor(o), linetype = factor(o))) +
  geom_line() + 
  theme_pubr() +
  scale_x_continuous(breaks = ss) +
  facet_wrap( ~ method)

ggarrange(edc.cov.sd, edc.width.sd, edc.runtime.sd, ncol = 1, nrow = 3,
          common.legend = T) %>%
  annotate_figure(top = text_grob("SONNET sd CI", 
                                  face = "bold", size = 14))


#################
edc.all <- bind_rows(
  edc.sum %>%
    filter(method %in% c("NSBoot", "ASEpar", "ASEnonpar", "ASEWeighted")),
  
  edc.sum %>% 
    filter(method == "SONNETnonpar", s == 5, o == n/10 ),
  
  edc.sum %>%
    filter(method == "SONNETpar", s == 5, o == n/10), 
  
  edc.sum %>%
    filter(method == "SONNETWeighted", s == 5, o == n/2),
)

## pct
## coverage vs n
edc.cov.pct <- edc.all %>%
  # filter(method %in% c("ASEnonpar", "SONNETnonpar", "NSBoot")) %>%
  filter(method %in% c("ASEpar", "SONNETpar", "ASEWeighted", "SONNETWeighted")) %>%
  ggplot(aes(x = n, y = mean_pct_covered, color = method, linetype = method)) +
  geom_line() + 
  theme_pubr() +
  scale_x_continuous(breaks = nn) +
  geom_hline(yintercept = 95, linetype = "dashed")

## width vs n
edc.width.pct <- edc.all %>%
  # filter(method %in% c("ASEnonpar", "SONNETnonpar", "NSBoot")) %>%
  filter(method %in% c("ASEpar", "SONNETpar", "ASEWeighted", "SONNETWeighted")) %>%
  ggplot(aes(x = n, y = mean_pct_width, color = method, linetype = method)) +
  geom_line() + 
  theme_pubr() +
  scale_x_continuous(breaks = nn)

## runtime vs n
edc.runtime.pct <- edc.all %>%
  # filter(method %in% c("ASEnonpar", "SONNETnonpar", "NSBoot")) %>%
  filter(method %in% c("ASEpar", "SONNETpar", "ASEWeighted", "SONNETWeighted")) %>%
  ggplot(aes(x = n, y = mean_runtime, color = method, linetype = method)) +
  geom_line() + 
  theme_pubr() +
  scale_x_continuous(breaks = nn)

ggarrange(edc.cov.pct, edc.width.pct, edc.runtime.pct, ncol = 1, nrow = 3,
          common.legend = T) %>%
  annotate_figure(top = text_grob("Percentile CI", 
                                  face = "bold", size = 14))

## sd
## coverage vs n
edc.cov.sd <- edc.all %>%
  # filter(method %in% c("ASEnonpar", "SONNETnonpar", "NSBoot")) %>%
  filter(method %in% c("ASEpar", "SONNETpar", "ASEWeighted", "SONNETWeighted")) %>%
  ggplot(aes(x = n, y = mean_sd_covered, color = method, linetype = method)) +
  geom_line() + 
  theme_pubr() +
  scale_x_continuous(breaks = nn) +
  geom_hline(yintercept = 95, linetype = "dashed")

## width vs n
edc.width.sd <- edc.all %>%
  # filter(method %in% c("ASEnonpar", "SONNETnonpar", "NSBoot")) %>%
  filter(method %in% c("ASEpar", "SONNETpar", "ASEWeighted", "SONNETWeighted")) %>%
  ggplot(aes(x = n, y = mean_sd_width, color = method, linetype = method)) +
  geom_line() + 
  theme_pubr() +
  scale_x_continuous(breaks = nn)

## runtime vs n
edc.runtime.sd <- edc.all %>%
  # filter(method %in% c("ASEnonpar", "SONNETnonpar", "NSBoot")) %>%
  filter(method %in% c("ASEpar", "SONNETpar", "ASEWeighted", "SONNETWeighted")) %>%
  ggplot(aes(x = n, y = mean_runtime, color = method, linetype = method)) +
  geom_line() + 
  theme_pubr() +
  scale_x_continuous(breaks = nn)

ggarrange(edc.cov.sd, edc.width.sd, edc.runtime.sd, ncol = 1, nrow = 3,
          common.legend = T) %>%
  annotate_figure(top = text_grob("SD CI", 
                                  face = "bold", size = 14))



################################################################################













