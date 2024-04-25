source("src/common.R")

library(latex2exp)
library(ggplot2)
theme_set(theme_bw() +
            theme(plot.subtitle = element_text(hjust = 0.5),
                  plot.margin = margin(0,5,0,5)))
.W <- 7500
.RES <- 1000

path <- "m:/Papers/rbo-paper/paper/"
dir.create(path, showWarnings = FALSE, recursive = TRUE)

# Table 1 ##########################################################################################

d <- rio::import_list(list.files("output/trec-stats/", full.names = TRUE), rbind = TRUE) |>
  select(-`_file`)

# Number of groups
d |> distinct(year, group) |> count(year)
# Number of systems
ns <- d |> distinct(year, sys) |> count(year)
# Number of topics
nt <- d |> distinct(year, topic) |> count(year)

# Fraction of runs that have at least one tie somewhere
d |> group_by(year, sys) |>
  summarize(ties = any(n_t > 0)) |>
  filter(ties) |>
  count(year) |>
  ungroup() |>
  mutate(n_pct = n/ns$n*100)
# Fraction of rankings that have ties
d |> count(year, ties = n_t>0) |>
  filter(ties) |>
  mutate(n_pct = n/ns$n/nt$n *100)
# Average fraction of docs tied in a ranking (only for rankings with ties)
d |> filter(n_t > 0) |>
  group_by(year) |>
  summarize(mean(n_t/n)*100)
# Average size of tie groups (only for rankings with ties)
d |> filter(n_t > 0) |>
  group_by(year) |>
  summarize(mean(avg_t))

# Figure 1 #########################################################################################

# TREC

d_trec <- lapply(.WEB_YEAR, function(year) {
  d <- rio::import(glue("output/rbo-trec/web{year}.csv"))
  d_stats <- rio::import(glue("output/trec-stats/web{year}.csv"))

  left_join(d, d_stats, by = c("sys1"="sys", "topic"="topic"))
}) |> bind_rows() |>
  filter(n_t > 0) # only rankings with ties

d_trec |>
  filter(p == .9) |>
  mutate(diff = abs(rbo_docid-rbo_rand)) |>
  summarize(mean(diff), max(diff),
            sum(diff>.01) / n()) |>
  round(2)

png(glue("{path}/breaker_trec_9.png"), width = .W/3, height = .W/3*.98, res = .RES)
d_trec |> filter(p == .9) |>
  ggplot(aes(rbo_rand, rbo_docid)) +
  geom_point(alpha = .5, size = .25, shape = 19) +
  geom_abline(intercept = 0, slope = 1, linewidth = .25, color = "red") +
  labs(x = TeX("$RBO~(random)$"), y = TeX("$RBO~(docID)$"), subtitle = "TREC data")
dev.off()

# Synthetic

d_syn <- rio::import("output/rbo-synthetic/rbo.csv")

d_syn |>
  filter(p == .9) |>
  mutate(diff = abs(rbo_docid-rbo_rand)) |>
  summarize(mean(diff), max(diff),
            sum(diff>.01) / n()) |>
  round(2)

png(glue("{path}/breaker_syn_9.png"), width = .W/3, height = .W/3*.98, res = .RES)
d_syn |> filter(p == .9) |>
  ggplot(aes(rbo_rand, rbo_docid, alpha = abs(rbo_rand-rbo_docid))) +
  geom_point(size = .25, shape = 19) +
  geom_abline(intercept = 0, slope = 1, linewidth = .25, color = "red") +
  labs(x = TeX("$RBO~(random)$"), y = TeX("$RBO~(docID)$"), subtitle = "Synthetic data") +
  scale_alpha_continuous(range = c(.02,.5)) + # for better display of large differences
  theme(legend.position = "none")
dev.off()

# Figure 4 #########################################################################################

png(glue("{path}/trec_random_scatter_w.png"), width = .W/3, height = .W/3*.9, res = .RES)
d_trec |> filter(p == .9) |>
  ggplot(aes(rbo_rand, rbow)) +
  geom_point(alpha = .5, size = .25, shape = 19) +
  geom_abline(intercept = 0, slope = 1, linewidth = .25, color = "red") +
  labs(x = TeX("$RBO$"), y = TeX("$RBO^w$"))
dev.off()
png(glue("{path}/trec_random_scatter_a.png"), width = .W/3, height = .W/3*.9, res = .RES)
d_trec |> filter(p == .9) |>
  ggplot(aes(rbo_rand, rboa)) +
  geom_point(alpha = .5, size = .25, shape = 19) +
  geom_abline(intercept = 0, slope = 1, linewidth = .25, color = "red") +
  labs(x = TeX("$RBO$"), y = TeX("$RBO^a$"))
dev.off()
png(glue("{path}/trec_random_scatter_b.png"), width = .W/3, height = .W/3*.9, res = .RES)
d_trec |> filter(p == .9) |>
  ggplot(aes(rbo_rand, rbob)) +
  geom_point(alpha = .5, size = .25, shape = 19) +
  geom_abline(intercept = 0, slope = 1, linewidth = .25, color = "red") +
  labs(x = TeX("$RBO$"), y = TeX("$RBO^b$"))
dev.off()

png(glue("{path}/trec_docid_scatter_w.png"), width = .W/3, height = .W/3*.9, res = .RES)
d_trec |> filter(p == .9) |>
  ggplot(aes(rbo_docid, rbow)) +
  geom_point(alpha = .5, size = .25, shape = 19) +
  geom_abline(intercept = 0, slope = 1, linewidth = .25, color = "red") +
  labs(x = TeX("$RBO$"), y = TeX("$RBO^w$"))
dev.off()
png(glue("{path}/trec_docid_scatter_a.png"), width = .W/3, height = .W/3*.9, res = .RES)
d_trec |> filter(p == .9) |>
  ggplot(aes(rbo_docid, rboa)) +
  geom_point(alpha = .5, size = .25, shape = 19) +
  geom_abline(intercept = 0, slope = 1, linewidth = .25, color = "red") +
  labs(x = TeX("$RBO$"), y = TeX("$RBO^a$"))
dev.off()
png(glue("{path}/trec_docid_scatter_b.png"), width = .W/3, height = .W/3*.9, res = .RES)
d_trec |> filter(p == .9) |>
  ggplot(aes(rbo_docid, rbob)) +
  geom_point(alpha = .5, size = .25, shape = 19) +
  geom_abline(intercept = 0, slope = 1, linewidth = .25, color = "red") +
  labs(x = TeX("$RBO$"), y = TeX("$RBO^b$"))
dev.off()

# Table 3 ##########################################################################################

d_trec |>
  mutate(diffw = abs(rbo_rand-rbow),
         diffa = abs(rbo_rand-rboa),
         diffb = abs(rbo_rand-rbob)) |>
  group_by(p) |>
  summarize(mean(diffw), max(diffw), sum(between(diffw,.01,.1)) / n(), sum(diffw>.1) / n(),
            mean(diffa), max(diffa), sum(between(diffa,.01,.1)) / n(), sum(diffa>.1) / n(),
            mean(diffb), max(diffb), sum(between(diffb,.01,.1)) / n(), sum(diffb>.1) / n()) |>
  round(2) |> as.data.frame()
d_trec |>
  mutate(diffw = abs(rbo_docid-rbow),
         diffa = abs(rbo_docid-rboa),
         diffb = abs(rbo_docid-rbob)) |>
  group_by(p) |>
  summarize(mean(diffw), max(diffw), sum(between(diffw,.01,.1)) / n(), sum(diffw>.1) / n(),
            mean(diffa), max(diffa), sum(between(diffa,.01,.1)) / n(), sum(diffa>.1) / n(),
            mean(diffb), max(diffb), sum(between(diffb,.01,.1)) / n(), sum(diffb>.1) / n()) |>
  round(2) |> as.data.frame()

# Figure 6 #########################################################################################

png(glue("{path}/syn_random_scatter_w.png"), width = .W/3, height = .W/3*.9, res = .RES)
d_syn |> filter(p == .9) |>
  ggplot(aes(rbo_rand, rbow, alpha = abs(rbo_rand-rbow))) +
  geom_point(size = .25, shape = 19) +
  geom_abline(intercept = 0, slope = 1, linewidth = .25, color = "red") +
  labs(x = TeX("$RBO$"), y = TeX("$RBO^w$")) +
  scale_alpha_continuous(range = c(.02,.5)) + # for better display of large differences
  theme(legend.position = "none")
dev.off()
png(glue("{path}/syn_random_scatter_a.png"), width = .W/3, height = .W/3*.9, res = .RES)
d_syn |> filter(p == .9) |>
  ggplot(aes(rbo_rand, rboa, alpha = abs(rbo_rand-rboa))) +
  geom_point(size = .25, shape = 19) +
  geom_abline(intercept = 0, slope = 1, linewidth = .25, color = "red") +
  labs(x = TeX("$RBO$"), y = TeX("$RBO^a$")) +
  scale_alpha_continuous(range = c(.02,.5)) + # for better display of large differences
  theme(legend.position = "none")
dev.off()
png(glue("{path}/syn_random_scatter_b.png"), width = .W/3, height = .W/3*.9, res = .RES)
d_syn |> filter(p == .9) |>
  ggplot(aes(rbo_rand, rbob, alpha = abs(rbo_rand-rbob))) +
  geom_point(size = .25, shape = 19) +
  geom_abline(intercept = 0, slope = 1, linewidth = .25, color = "red") +
  labs(x = TeX("$RBO$"), y = TeX("$RBO^b$")) +
  scale_alpha_continuous(range = c(.02,.5)) + # for better display of large differences
  theme(legend.position = "none")
dev.off()

# Table 4 ##########################################################################################

d_syn |>
  mutate(diffw = abs(rbo_rand-rbow),
         diffa = abs(rbo_rand-rboa),
         diffb = abs(rbo_rand-rbob)) |>
  group_by(p) |>
  summarize(mean(diffw), max(diffw), sum(between(diffw,.01,.1)) / n(), sum(diffw>.1) / n(),
            mean(diffa), max(diffa), sum(between(diffa,.01,.1)) / n(), sum(diffa>.1) / n(),
            mean(diffb), max(diffb), sum(between(diffb,.01,.1)) / n(), sum(diffb>.1) / n()) |>
  round(2) |> as.data.frame()
# d_syn |>
#   mutate(diffw = abs(rbo_docid-rbow),
#          diffa = abs(rbo_docid-rboa),
#          diffb = abs(rbo_docid-rbob)) |>
#   group_by(p) |>
#   summarize(mean(diffw), max(diffw), sum(between(diffw,.01,.1)) / n(), sum(diffw>.1) / n(),
#             mean(diffa), max(diffa), sum(between(diffa,.01,.1)) / n(), sum(diffa>.1) / n(),
#             mean(diffb), max(diffb), sum(between(diffb,.01,.1)) / n(), sum(diffb>.1) / n()) |>
#   round(2) |> as.data.frame()
