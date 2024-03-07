# Ideas:
# Focus on the actual flow network itself, examine changes in the network
# good abstract ref: I:\RTs_and_Projects\Financial Resources for Health_RT\6. Conferences\CUGH\2023\malaria paper 2
#
# Notes:
# The amount of interactions occurring through a channel grew tremendously over
# the years. An interesting story is that distribution of DAH across countries in
# need was rather unequal at the start of the series, in 1990, but seems to have
# balanced out over time, which could be associated with the increasing number of
# intermediary channels - some kind of "matching efficiency" going on? intermediaries
# improve the distribution of aid?
# 

pacman::p_load(here, ggplot2, ggridges, data.table)

source(here::here("R/prepdata.R"))

FIGDIR <- here::here("figs")
dir.create(FIGDIR, showWarnings = FALSE)


dah <- load_dahnetwork(here::here("data/dahnet.csv"), rebuild = FALSE)
dah <- dah[dah_21 > 0 & year < 2021, ]


# dropping 7 recipients without gdp data
dah[is.na(dst_gdp), .(dest_iso, dest_name)] |> unique()
dah <- dah[!is.na(dst_gdp)]

dah[, src_dst := as.numeric(as.factor(paste0(source, "_", dest_iso)))]
dah[, agency := data.table::fifelse(channel %like% "^BIL" | channel == "EC", 0, 1)]

dah[order(year), .(n = sum(agency)), by = year]


names(dah)
unique(dah$year)
length(unique(dah$source))
length(unique(dah$channel))
length(unique(dah$dest_iso))



add_diffs <- function(dt, var, n, inplace = FALSE) {
    if (! inplace) {
        dt <- data.table::copy(dt)
    }
    lagv <- paste0("lag", 1:n, "_", var)
    difv <- paste0("dif", 1:n, "_", var)
    for (i in seq_along(difv)) {
        dt[, eval(lagv[i]) := data.table::shift(get(var), n = i, type = "lag")]
        dt[, eval(difv[i]) := get(var) - get(lagv[i])]
    }
    return(dt) 
}

sum_top_til <- function(vec, til) {
    vec <- sort(vec, decreasing = TRUE)
    tot <- 0
    for (i in seq_along(vec)) {
       tot <- tot + vec[i]
       if (tot >= til)
           return(i)
    }
    # never reached 'til'
    return(NA_integer_)
}



# SRC->DST FLOW NETWORK
flow <- dah[,
              # total dah disbursed from src to dst
            .(dah = sum(as.numeric(dah_21), na.rm = TRUE),
              # n channels interacting with both src and dst
              nchannels = length(unique(channel[agency == 1]))
              ),
            by = .(year, src_dst, source, dest_iso, src_gdp, dst_gdp)]

# flow <- flow[nchannels == 0, ]
flow[, src_dah := sum(dah), by = .(source, year)]
flow[, dst_dah := sum(dah), by = .(dest_iso, year)]


# degree - n edges, ie, how many connections each vertex has
flow[, src_degree := .N, by = .(year, source)]
flow[, dst_degree := .N, by = .(year, dest_iso)]
flow <- add_diffs(flow, "dst_degree", n = 1)
flow <- add_diffs(flow, "src_degree", n = 1)

flow[, n_src := length(unique(source)), by = year]
flow[, n_dst := length(unique(dest_iso)), by = year]


# unique src dst pairs
all_possible <- length(unique(flow$source)) * length(unique(flow$dest_iso))
flow[, .(n_pairs = .N), .(year)] |> #unique src_dst pairs
    ggplot() +
    geom_line(aes(x = year, y = n_pairs/all_possible)) +
    geom_hline(yintercept = 1, linetype = "dashed")

# mean degree of src
flow[, .(mean_degree = mean(src_degree / n_dst)), .(year)] |>
    ggplot() +
    geom_line(aes(x = year, y = mean_degree)) # color = dest_iso, group = dest_iso))

# mean degree of dst 
flow[, .(mean_degree = mean(dst_degree / n_src)), .(year)] |>
    ggplot() +
    geom_line(aes(x = year, y = mean_degree)) # color = dest_iso, group = dest_iso))


# mean change in degree
flow[year > min(year),
     .(degree_diff = mean(dif1_dst_degree, na.rm = TRUE)),
     by = .(year)] |>
    ggplot() +
    geom_line(aes(x = year, y = degree_diff))


# degree distribution
## src
#  - fraction of graph vertices w a certain degree
src_fd <- flow[, .(fd = length(unique(source)) / n_src), by = .(src_degree, year)]
ggplot(src_fd) +
    # geom_smooth(aes(x=src_degree, y=fd))
    geom_point(aes(x = src_degree, y = fd, group = year, color = year))

ggplot(flow, aes(x=src_degree, y=year, fill=after_stat(x), group=year)) +
    geom_density_ridges_gradient(bandwidth=10) + coord_flip()

## dst 
flow[, dst_nv := length(unique(dest_iso)), by = year]
dst_fd <- flow[, .(fd = length(unique(source)) / dst_nv), by = .(src_degree, year)]

ggplot(flow, aes(x=dst_degree, y=year, fill=after_stat(x), group=year)) +
  geom_density_ridges_gradient(bandwidth = 0.9, color = "white") +
  coord_flip() +
  scale_y_continuous(breaks = seq(min(flow$year), max(flow$year), by = 2)) +
  scale_fill_viridis_c(option = "mako", direction = 1) +
  labs(title = "Recipient country degree distribution",
       subtitle = paste(range(flow$year), collapse = " - "),
       caption = "Kernel Density Estimate",
       y = "",
       x = "Number of connections to donor countries") +
  hrbrthemes::theme_ipsum_pub() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 16),
    plot.caption = element_text(size = 12),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 16),
    panel.grid.minor.x = element_line(color = alpha("gray", 0.33))
  )
ggsave(file.path(FIGDIR, "dst_degree_dist.jpg"), width = 10, height = 6, dpi = 300)






# ---- number plugging ---- #
# change in num connections
flow[year == 1990, src_dst] |> unique() |> length()
flow[year == 2020, src_dst] |> unique() |> length()

# change in total dah
flow[order(year), .(dah = sum(dah) / 1e9), by = year]

# ---- number plugging ---- #

# holding src constant, do flows to recipients follow a trend (increased flows
# for certain regions rather than others? former colonies? un involvement?)

# proportion of aid to each recipient
flow[, prop_dah_to_dst := dah / src_dah, by = .(source, year)]

flow[order(year), .(mean_prop = mean(prop_dah_to_dst)), by = year] |> plot()

#
# MODELING ====================================================================
#
#
#

# line: mean prop of dah received by dests if each source's dah was split evenly
#     among all of the recipients
# points: the actual mean prop of dah received by dests
# nchannels == 0 - inefficiency even now
# nchannels == 1,2,... improving efficiency
library(mgcv)
ex <- copy(flow)
ex[, m := as.integer(src_dah/1e3)] # the total amount of dah given out by source i
ex[, successes := as.integer(dah/1e3)] # the dah that flows from src i to dst j
ex[, uniform_success := as.integer((src_dah / n_dst) / 1e3)] # the amount of dah that would flow from src i to dst j if it were split evenly among all the dests
ex <- ex[m > 0 & successes > 0]

mod_propdah <- gam(successes ~ s(year, bs = "cr", k = -1) +
                     # s(dst_gdp, bs = "cr", k = -1) +
                     # s(src_gdp, bs = "cr", k = -1) +
                     + offset(log(m)),
                   family = nb(),
                   method = "REML",
                   data = ex)

mod_uniform <- gam(uniform_success ~ s(year, bs = "cr", k = -1) +
                     # s(dst_gdp, bs = "cr", k = -1) +
                     # s(src_gdp, bs = "cr", k = -1) +
                     + offset(log(m)),
                   family = nb(),
                   method = "REML",
                   data = ex)

## create new data set for model predictions
newd <- data.table(year = min(flow$year):max(flow$year),
                   dst_gdp = median(flow[year == 2020]$dst_gdp),
                   src_gdp = median(flow[year == 2020]$src_gdp),
                   # dest_iso = "BWA",
                   m = max(flow[]$src_dah))
## add model predictions
newd <- cbind(
  newd,
  as.data.frame(predict(mod_propdah, newd, type = "link", se.fit = TRUE))
)
newd[, `:=` (
  fit_real = 100 * exp(fit)/m,
  lwr_real = 100 * exp(fit - 1.96 * se.fit)/m,
  upr_real = 100 * exp(fit + 1.96 * se.fit)/m,
  fit=NULL,se.fit=NULL
)]
## add uniform predictions
newd <- cbind(
  newd,
  as.data.frame(predict(mod_uniform, newd, type = "link", se.fit = TRUE))
)
newd[, `:=` (
  fit_unif = 100 * exp(fit)/m,
  lwr_unif = 100 * exp(fit - 1.96 * se.fit)/m,
  upr_unif = 100 * exp(fit + 1.96 * se.fit)/m,
  fit=NULL,se.fit=NULL
)]
## add observed averages
newd <- merge(
  newd,
  flow[, .(obs_avg = 100 * mean(prop_dah_to_dst)), by = .(year)],
  by = "year", all.x = TRUE
)
newd <- merge(
  newd,
  # distribute total dah evenly among all recipients, then determine what
  # proportion of total DAH that would be
  flow[, .(uniform_dah = 100 * mean((src_dah / n_dst) / src_dah)), by = .(year)],
  by = "year", all.x = TRUE
)

ggplot(newd, aes(x = year)) +
  geom_ribbon(aes(ymin = lwr_real, ymax = upr_real), alpha = 0.2) +
  geom_line(aes(y = fit_real)) +
  geom_ribbon(aes(ymin = lwr_unif, ymax = upr_unif), alpha = 0.2) +
  geom_line(aes(y = fit_unif), linetype = "dashed", size = 1) +
  # geom_point(aes(y = obs_avg)) +
  # geom_point(aes(y = uniform_dah), color = "red") +
  geom_text(aes(x = 1998, y = 0.6,
                label = "Theoretical Uniform Proportion"),
            size = 7) +
  scale_x_continuous(breaks = seq(min(newd$year), max(newd$year), by = 2)) +
  coord_cartesian(ylim = c(0, max(max(newd$upr_real), max(newd$upr_unif))),
                  xlim = range(newd$year)) +
  labs(title = "Average proportion of donor aid received by a single recipient country",
       subtitle = paste(range(flow$year), collapse = " - "),
       x = "Year",
       y = "Proportion of Total Donor Aid (%)") +
  hrbrthemes::theme_ipsum_pub() +
  theme(
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 16),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 16),
    panel.grid.minor.x = element_line(color = alpha("gray", 0.33))
  )
ggsave(file.path(FIGDIR, "prop_dah_to_dst.jpg"), width = 12, height = 8, dpi = 300)
#
# note - trend is robust to inclusion of source fixed effects, dest fixed effects



#
# proportion of donor aid received and degree
#
ex <- copy(flow)
ex[, m := as.integer(src_dah/1e3)] # the total amount of dah given out by source i
ex[, successes := as.integer(dah/1e3)] # the dah that flows from src i to dst j
ex[, uniform_success := as.integer((src_dah / n_dst) / 1e3)] # the amount of dah that would flow from src i to dst j if it were split evenly among all the dests
ex <- ex[m > 0 & successes > 0]

mod_propdah <- gam(successes ~ s(year, bs = "cr", k = -1) +
                     # s(dst_gdp, bs = "cr", k = -1) +
                     # s(src_gdp, bs = "cr", k = -1) +
                     + offset(log(m)),
                   family = nb(),
                   method = "REML",
                   data = ex)

mod_uniform <- gam(uniform_success ~ s(year, bs = "cr", k = -1) +
                     # s(dst_gdp, bs = "cr", k = -1) +
                     # s(src_gdp, bs = "cr", k = -1) +
                     + offset(log(m)),
                   family = nb(),
                   method = "REML",
                   data = ex)

## create new data set for model predictions
newd <- data.table(year = min(flow$year):max(flow$year),
                   dst_gdp = median(flow[year == 2020]$dst_gdp),
                   src_gdp = median(flow[year == 2020]$src_gdp),
                   # dest_iso = "BWA",
                   m = max(flow[]$src_dah))
## add model predictions
newd <- cbind(
  newd,
  as.data.frame(predict(mod_propdah, newd, type = "link", se.fit = TRUE))
)
newd[, `:=` (
  fit_real = 100 * exp(fit)/m,
  lwr_real = 100 * exp(fit - 1.96 * se.fit)/m,
  upr_real = 100 * exp(fit + 1.96 * se.fit)/m,
  fit=NULL,se.fit=NULL
)]
## add uniform predictions
newd <- cbind(
  newd,
  as.data.frame(predict(mod_uniform, newd, type = "link", se.fit = TRUE))
)
newd[, `:=` (
  fit_unif = 100 * exp(fit)/m,
  lwr_unif = 100 * exp(fit - 1.96 * se.fit)/m,
  upr_unif = 100 * exp(fit + 1.96 * se.fit)/m,
  fit=NULL,se.fit=NULL
)]
## add observed averages
newd <- merge(
  newd,
  flow[, .(obs_avg = 100 * mean(prop_dah_to_dst)), by = .(year)],
  by = "year", all.x = TRUE
)
newd <- merge(
  newd,
  # distribute total dah evenly among all recipients, then determine what
  # proportion of total DAH that would be
  flow[, .(uniform_dah = 100 * mean((src_dah / n_dst) / src_dah)), by = .(year)],
  by = "year", all.x = TRUE
)

ggplot(newd, aes(x = year)) +
  geom_ribbon(aes(ymin = lwr_real, ymax = upr_real), alpha = 0.2) +
  geom_line(aes(y = fit_real)) +
  geom_ribbon(aes(ymin = lwr_unif, ymax = upr_unif), alpha = 0.2) +
  geom_line(aes(y = fit_unif), linetype = "dashed") +
  # geom_point(aes(y = obs_avg)) +
  # geom_point(aes(y = uniform_dah), color = "red") +
  geom_text(aes(x = 1998, y = 0.6,
                label = "Theoretical Uniform Allocation"),
            size = 6) +
  scale_x_continuous(breaks = seq(min(newd$year), max(newd$year), by = 2)) +
  coord_cartesian(ylim = c(0, max(max(newd$upr_real), max(newd$upr_unif))),
                  xlim = range(newd$year)) +
  labs(title = "Average proportion of donor aid received by a single recipient country",
       subtitle = paste(range(flow$year), collapse = " - "),
       x = "Year",
       y = "Proportion of Total Donor Aid (%)") +
  hrbrthemes::theme_ipsum_pub() +
  theme(
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 16),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor.x = element_line(color = alpha("gray", 0.33))
  )
ggsave(file.path(FIGDIR, "prop_dah_to_dst.jpg"), width = 12, height = 8, dpi = 300)
#
# note - trend is robust to inclusion of source fixed effects, dest fixed effects



#
# donor-recipient-connection and gdp
#
ex <- flow[, .(year, source, dest_iso, src_gdp, dst_gdp)]
net <- expand.grid(
  year = unique(ex$year),
  source = unique(ex$source),
  dest_iso = unique(ex$dest_iso)
)
setDT(net)
net <- merge(net, unique(ex[, .(year, dest_iso, dst_gdp)]),
             by = c("year", "dest_iso"), all.x = TRUE)
net <- merge(net, unique(ex[, .(year, source, src_gdp)]),
             by = c("year", "source"), all.x = TRUE)
net <- merge(net, ex[, .(year, source, dest_iso, connected = 1)],
             by = c("year", "source", "dest_iso"), all.x = TRUE)
setnafill(net, fill = 0, cols = "connected")

mod_conn <- gam(connected ~ s(year, bs = "cr", k = -1) +
                  s(src_gdp, bs = "cr", k = -1) +
                  s(dst_gdp, bs = "cr", k = -1),
                family = binomial(),
                data = net,
                method = "REML")
invlogit <- mod_conn$family$linkinv

yr <- 2020
newd <- expand.grid(year = c(1990, 2000, 2010, 2020),
                    src_gdp = median(flow[year == yr, src_gdp]),
                    dst_gdp = seq(min(flow[year == yr, dst_gdp]),
                                  max(flow[year == yr, dst_gdp]),
                                  length.out = 1000)
                   # src_gdp = seq(min(flow[year == yr, src_gdp]),
                   #               max(flow[year == yr, src_gdp]),
                   #               length.out = 1000),
                   # dst_gdp = median(flow[year == yr, dst_gdp])
)
setDT(newd)
newd <- cbind(
  newd,
  as.data.frame(predict(mod_conn, newd, type = "link", se.fit = TRUE))
)
newd[, `:=` (
  fit = invlogit(fit),
  lwr = invlogit(fit - 1.96 * se.fit),
  upr = invlogit(fit + 1.96 * se.fit)
)]

ggplot(newd, aes(x = dst_gdp)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
  geom_line(aes(y = fit)) +
  geom_point(
    data = net[year %in% c(1990, 2000, 2010, 2020)],
    aes(x = dst_gdp, y = connected),
    alpha = 0.05
  ) +
  facet_wrap(~year, scales = "free_x",
             labeller = as_labeller(\(x) paste("Year:", x))) +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Probability of any flow from a donor to a recipient",
       x = "Recipient GDP per-capita (2021 US$)",
       y = "Modeled Probability") +
  hrbrthemes::theme_ipsum_pub() +
  theme(
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16)
  )
ggsave(file.path(FIGDIR, "prob_conn_to_dst.jpg"),
       width = 12, height = 8, dpi = 300)



#
# dah level on recipient gdp
#
ex <- flow[, .(year, source, src_gdp, dest_iso, dst_gdp, dah, nchannels)]
ex[, anychannel := as.numeric(nchannels > 0)]

mod_dah <- gam(dah ~ s(year, bs = "cr", k = -1) +
                 s(src_gdp, bs = "cr", k = -1) +
                 s(dst_gdp, bs = "cr", k = -1),
               # family = Gamma(link = "log"),
               data = ex,
               method = "REML")

newd <- expand.grid(year = c(1990, 2000, 2010, 2020),
                    # anychannel = c(0,1),
                    src_gdp = median(flow[year == yr, src_gdp]),
                    dst_gdp = seq(min(flow[year == yr, dst_gdp]),
                                  max(flow[year == yr, dst_gdp]),
                                  length.out = 1000)
)
setDT(newd)
newd <- cbind(
  newd,
  as.data.frame(predict(mod_dah, newd, type = "link", se.fit = TRUE))
)
newd[, `:=` (
  fit = mod_dah$family$linkinv(fit) / 1e6,
  lwr = mod_dah$family$linkinv(fit - 1.96 * se.fit) / 1e6,
  upr = mod_dah$family$linkinv(fit + 1.96 * se.fit) / 1e6
)]
# newd[, anychannel := as.factor(anychannel)]

ggplot(newd, aes(x = dst_gdp)) +
  geom_point(
    data = ex[year %in% c(1990, 2000, 2010, 2020)],
    aes(x = dst_gdp, y = dah / 1e6),
    alpha = 0.15,
    color = "grey50"
  ) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.3) +
  geom_line(aes(y = fit)) +
  coord_cartesian(ylim = c(0, 50)) +
  facet_wrap(~year, scales = "free",
             labeller = as_labeller(\(x) paste("Year:", x))) +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Recipient GDP and DAH disbursed from donor to recipient",
       x = "Recipient GDP",
       y = "DAH (millions of 2021 US$)") +
  hrbrthemes::theme_ipsum_pub() +
  theme(
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16)
  )




#
# dah level on n channels
#
ex <- flow[, .(year, source, src_gdp, dest_iso, dst_gdp, dah, nchannels)]
ex[, anychannel := as.numeric(nchannels > 0)]

mod_chann <- gam(dah ~ s(year, bs = "cr", k = -1) +
                 s(nchannels) +
                 s(src_gdp, bs = "cr", k = -1) +
                 s(dst_gdp, bs = "cr", k = -1),
               # family = Gamma(link = "log"),
               data = ex,
               method = "REML")

newd <- expand.grid(year = c(1990, 2000, 2010, 2020),
                    nchannels = seq(0, max(ex$nchannels), by = 1))
setDT(newd)
newd <- merge(
  newd,
  ex[, .(src_gdp = median(src_gdp), dst_gdp = median(dst_gdp)), by = year],
  by = "year", all.x = TRUE
)

newd <- cbind(
  newd,
  as.data.frame(predict(mod_chann, newd, type = "link", se.fit = TRUE))
)
newd[, `:=` (
  fit = mod_chann$family$linkinv(fit) / 1e6,
  lwr = mod_chann$family$linkinv(fit - 1.96 * se.fit) / 1e6,
  upr = mod_chann$family$linkinv(fit + 1.96 * se.fit) / 1e6
)]

ggplot(newd, aes(x = nchannels)) +
  geom_point(
    data = ex[year %in% c(1990, 2000, 2010, 2020)],
    aes(x = nchannels, y = dah / 1e6),
    alpha = 0.15,
    color = "grey50"
  ) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.3) +
  geom_line(aes(y = fit)) +
  coord_cartesian(ylim = c(0, 50)) +
  facet_wrap(~year, scales = "free",
             labeller = as_labeller(\(x) paste("Year:", x))) +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Recipient GDP and DAH disbursed from donor to recipient",
       x = "Recipient GDP",
       y = "DAH (millions of 2021 USD)") +
  hrbrthemes::theme_ipsum_pub() +
  theme(
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16)
  )



#########
ex <- unique(flow[, .(year, dest_iso, dst_degree, dst_gdp)])
mod_degree <- gam(dst_degree ~ s(year, bs = "cr", k = -1) +
                    s(dst_gdp, bs = "cr", k = -1),
                  family = nb(),
                  data = ex,
                  method = "REML")

yr <- 2020
newd <- data.table(year = yr,
                   dst_gdp = seq(min(flow[year == yr, dst_gdp]),
                                 max(flow[year == yr, dst_gdp]),
                                 length.out = 1000)
                   )
newd <- cbind(
  newd,
  as.data.frame(predict(mod_degree, newd, type = "link", se.fit = TRUE))
)
newd[, `:=` (
  fit = exp(fit),
  lwr = exp(fit - 1.96 * se.fit),
  upr = exp(fit + 1.96 * se.fit)
)]

ggplot(newd, aes(x = dst_gdp, y = fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
  geom_line() +
  geom_point(
    data = ex[year == yr, .(y = mean(dst_degree)), by = dst_gdp],
    aes(x = dst_gdp, y = y)
  )








                           # max 1
ggplot(flow[prop_dah_to_dst < 2],
       aes(x=(prop_dah_to_dst), y=year, fill=after_stat(x), group=year)) +
    geom_density_ridges_gradient(bandwidth=0.05) + coord_flip()

# - correlation w gdp - change over time
flow[, .(prop = mean(prop_dah_to_dst)), by = .(year, dst_gdp)] |>
    ggplot(aes(x = dst_gdp, y = prop, color = year, group = year)) +
    geom_smooth(se = FALSE, linewidth = .5) +
    scale_color_viridis_c() #+ facet_wrap(~year)

ggplot(flow, aes(x=dst_gdp, y=dst_dah/n_src, color=dst_gdp)) + geom_point() + facet_wrap(~year)


ggplot(flow, aes(x=dst_degree, y=dst_dah/n_src, color=dst_gdp)) + geom_point() + facet_wrap(~year)



# look at  the number of recipients for each donor, how many...? how concentrated?
flow[order(-prop_dah_to_dst), rank := 1:.N, by = .(year, source)]
flow[rank %in% 1:5 & source == "USA" & year == 2020, ][order(rank)]

# for each source-year, how many different recipients make up the top 50% of spending?
flow[, top50pct := sum_top_til(prop_dah_to_dst, 0.5), by = .(year, source)]
flow[order(year), (mean_top50pct = mean(top50pct)), by = year]# |> plot()
# grew a lot!


# sources w higher degree distribute to their aid to more recipients... duh, but
# still kind of cool
ggplot(flow, aes(x=src_degree, y=top50pct, color=year)) +
    geom_point()


# gdp
flow[, .(dah = mean(dah)), by = .(dst_gdp, year)] |>
    ggplot(aes(x = dst_gdp, y = dah, color = year)) + geom_point()


# ---- reg ---- #

glm(dst_degree ~ nchannels + as.factor(year),
    family = poisson(),
    data = flow) |> summary()

#fixest::feols(dst_degree ~ nchannels | year + source,
#              cluster = c("year", "source"),
#              data = flow)
#
#fixest::feols(src_degree ~ nchannels | year + source,
#              cluster = c("year", "source"),
#              data = flow)


# ---- reg ---- #


# dah by nchannels over time
flow[, .(dah = sum(dah)), by = .(year, nchannels)] |>
    ggplot(aes(x=year, y=dah, color=as.factor(nchannels), group=nchannels)) +
    geom_line()

# dah by bilateral vs channel over time
dah[, channel2 := ifelse(channel %like% "^BIL", "BIL", "INT")]
dah[, .(dah = sum(dah_21)), by = .(channel2, year)] |>
    ggplot(aes(x=year, y=dah, color=channel2)) +
    geom_line()


flow[, .(intermed = mean(nchannels >0)), by = .(year)]


# nchannels associated w degree of dst? - yes, def positive relationship
dsts <- flow[, .(nchannels = sum(nchannels)),
             by = .(dest_iso, dst_degree, year)]
fixest::feols(dst_degree ~ nchannels | year, data = dsts)

dah[, .(nchannel = length(unique(channel[! channel %like% "^BIL"])),
        dst_degree = length(unique(source))
        ), by = .(dest_iso, year)
    ][,
      .(nchannel = mean(nchannel), dst_degree = mean(dst_degree)),
      by = .(dest_iso)
      ] |>
    ggplot(aes(x = nchannel, y = dst_degree)) + geom_point()






new = 62.1
old = 11.4
(new - old) / old


