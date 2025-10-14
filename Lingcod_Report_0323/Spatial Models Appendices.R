################################################################################
# Using code from the RF report to create data tables for lingcod models
# These are Appendices H and I in the rockfish report
################################################################################
library(here)
library(tidyverse)
library(flextable)
source(here("functions.R"))

####
# Appendix *H* - Effort model selection criteria

###
# Effort

tab_sd <- readRDS(here("Data/tab_sd.rds"))

#input loocv log likelihoods
ll_H <- readRDS(here("Data/ll_H.rds"))

ll_E <- readRDS(here("Data/ll_E.rds"))

#tables with model selection criteria. One for each composition
mod_H <-
  Map(function(x, y){tab_ll(x) %>% dplyr::mutate(port = y, composition = "Harvest")}, 
      x = ll_H,
      y = factor(c("Homer", "Kodiak", "Seward", "Valdez", "Whittier"), ordered = TRUE)) %>%
  do.call(rbind, .) %>% 
  dplyr::arrange(desc(composition), port) %>%
  dplyr::select(port, composition, model, ll, diff, p_diff) %>%
  dplyr::left_join(tab_sd, by = c("port", "composition", "model")) 

mod_E <-
  Map(function(x, y){tab_ll(x) %>% dplyr::mutate(port = y, composition = "Effort")}, 
      x = ll_E,
      y = factor(c("Homer", "Kodiak", "Seward", "Valdez", "Whittier"), ordered = TRUE)) %>%
  do.call(rbind, .) %>% 
  dplyr::arrange(desc(composition), port) %>%
  dplyr::select(port, composition, model, ll, diff, p_diff) %>%
  dplyr::left_join(tab_sd, by = c("port", "composition", "model")) 

flextable(mod_E[, -2]) %>%
  merge_v(j = ~ port) %>%
  valign(j = 1, valign = "top") %>%
  align(j = c(3:6), align = "right") %>%
  align(part = "header", j = c(3:6), align = "right") %>%
  line_spacing(space = 0.5) %>%
  font(fontname = "Times New Roman") %>%
  fontsize(size = 10) %>%
  mk_par(part = "header", j = "port", value = as_paragraph("Port")) %>%
  mk_par(part = "header", j = "model", value = as_paragraph("Model")) %>%
  mk_par(part = "header", j = "ll", value = as_paragraph("lppd", as_sub("m"))) %>%
  mk_par(part = "header", j = "diff", value = as_paragraph("\U0394llpd", as_sub("m"), "(SE)")) %>%
  mk_par(part = "header", j = "p_diff", value = as_paragraph("P(\U0394llpd", as_sub("m"), ">0)")) %>%
  mk_par(part = "header", j = "sd", value = as_paragraph("\U03C3", as_sub("re"))) %>%
  bold(i = c(3, 6, 10, 15, 18), j = 2:6) %>%
  autofit()

####
# Harvest
flextable(mod_H[, -2]) %>%
  merge_v(j = ~ port) %>%
  valign(j = 1, valign = "top") %>%
  align(j = c(3:6), align = "right") %>%
  align(part = "header", j = c(3:6), align = "right") %>%
  line_spacing(space = 0.5) %>%
  font(fontname = "Times New Roman") %>%
  fontsize(size = 10) %>% 
  mk_par(part = "header", j = "port", value = as_paragraph("Port")) %>%
  mk_par(part = "header", j = "model", value = as_paragraph("Model")) %>%
  mk_par(part = "header", j = "ll", value = as_paragraph("lppd", as_sub("m"))) %>% 
  mk_par(part = "header", j = "diff", value = as_paragraph("\U0394llpd", as_sub("m"), "(SE)")) %>% 
  mk_par(part = "header", j = "p_diff", value = as_paragraph("P(\U0394llpd", as_sub("m"), ">0)")) %>% 
  mk_par(part = "header", j = "sd", value = as_paragraph("\U03C3", as_sub("re"))) %>%
  bold(i = c(1, 6, 12, 15, 19), j = 2:6) %>%
  autofit()


#########
# Appendix *I* - Mean Spatial composition of effort and havest

# Homer
mod_p <- readRDS(here("Data/mod_p.rds"))
mod_p2 <- mapply(function(x, y) mutate(x, statistic = y), x = mod_p, y = rep(c("Effort", "Harvest"), times = 5))

mod_p2[1:2] %>% 
  do.call(rbind, .) %>%
  select(statistic, everything()) %>%
  mutate(year = as.character(year)) %>%
  flextable() %>%
  merge_v(j = ~ statistic) %>%
  valign(j = c(1, 2), valign = "top") %>%
  line_spacing(space = 0.4) %>%
  font(fontname = "Times New Roman") %>%
  fontsize(size = 9) %>%
  width(width = 1/12 * c(11, 7, 6, 12, 12, 12, 12, 12))

# Kodiak
mod_p2[3:4] %>% 
  do.call(rbind, .) %>%
  select(statistic, everything()) %>%
  mutate(year = as.character(year)) %>%
  flextable() %>%
  merge_v(j = "statistic") %>%
  valign(j = c(1, 2), valign = "top") %>%
  line_spacing(space = 0.4) %>%
  font(fontname = "Times New Roman") %>%
  fontsize(size = 9) %>%
  width(width = 1/12 * c(11, 7, 6, 12, 12, 12, 12))

# Seward
mod_p2[5:6] %>% 
  do.call(rbind, .) %>%
  select(statistic, everything()) %>%
  mutate(year = as.character(year)) %>%
  flextable() %>%
  merge_v(j = c("statistic", "fleet")) %>%
  valign(j = c(1, 2), valign = "top") %>%
  line_spacing(space = 0.4) %>%
  font(fontname = "Times New Roman") %>%
  fontsize(size = 9) %>%
  width(width = 1/12 * c(11, 7, 6, 12, 12, 13, 12))

# Valdez
mod_p2[7:8] %>% 
  do.call(rbind, .) %>%
  select(statistic, everything()) %>%
  mutate(year = as.character(year)) %>%
  flextable() %>%
  merge_v(j = c("statistic", "fleet")) %>%
  valign(j = c(1, 2), valign = "top") %>%
  line_spacing(space = 0.4) %>%
  font(fontname = "Times New Roman") %>%
  fontsize(size = 9) %>%
  width(width = 1/12 * c(11, 7, 6, 12, 12, 12))

# Whittier
mod_p2[9:10] %>% 
  do.call(rbind, .) %>%
  select(statistic, everything()) %>%
  mutate(year = as.character(year)) %>%
  flextable() %>%
  merge_v(j = c("statistic", "fleet")) %>%
  valign(j = c(1, 2), valign = "top") %>%
  line_spacing(space = 0.4) %>%
  font(fontname = "Times New Roman") %>%
  fontsize(size = 9) %>%
  width(width = 1/12 * c(11, 7, 6, 12, 12, 12))