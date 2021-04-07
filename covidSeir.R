library(covidseir)
library(dplyr)
library(ggplot2)
library(readr)
ymd <- lubridate::ymd

cases <- read_csv('http://www.bccdc.ca/Health-Info-Site/Documents/BCCDC_COVID19_Dashboard_Case_Details.csv')

counts <- cases %>% filter (Reported_Date > ymd("2021-01-01")) %>% group_by(Reported_Date) %>% tally()
dat <- structure(list(value = counts$n,
                      date = counts$Reported_Date,
                      day = 1:94), row.names = c(NA, -94L), 
                 class = "data.frame")
dat <- dplyr::as_tibble(dat)
#dat$date <- ymd("2020-03-01") + dat$day - 1

ggplot(dat, aes(date, value)) + geom_line()

# Based on estimation with hospital data in other model:
samp_frac <- c(rep(0.14, 13), rep(0.21, 38))
samp_frac <- c(samp_frac, rep(0.37, nrow(dat) - length(samp_frac)))

f_seg <- c(0, rep(1, nrow(dat) - 1))
day_new_f <- which(dat$date == ymd("2020-11-01"))
f_seg[seq(day_new_f, length(f_seg))] <- 2
day_ch <- which(dat$date == ymd("2021-03-29"))
f_seg[seq(day_ch, length(f_seg))] <- 3
f_seg
#>   [1] 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#>  [38] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2
#>  [75] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
#> [112] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
#> [149] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3

fit <- covidseir::fit_seir(
  daily_cases = dat$value,
  samp_frac_fixed = samp_frac, 
  f_seg = f_seg,
  i0_prior = c(log(8), 1),
  e_prior = c(0.8, 0.05),
  start_decline_prior = c(log(15), 0.1),
  end_decline_prior = c(log(22), 0.1),
  f_prior = cbind(c(0.6, 0.5, 0.4), c(0.2, 0.2, 0.2)),
  R0_prior = c(log(2.6), 0.2),
  N_pop = 5.1e6, # BC population
  iter = 500, # number of posterior samples
  fit_type = "optimizing" # for speed only
)
#> Finding the MAP estimate.

print(fit)

future::plan(future::multisession)


proj <- covidseir::project_seir(fit, iter = 1:50)
proj

tidy_proj <- covidseir::tidy_seir(proj, resample_y_rep = 20)
tidy_proj


rt <- covidseir::get_rt(fit, iter = 1:50)
dplyr::glimpse(rt)

ggplot(rt, aes(time, Rt, group = .iteration)) + 
  geom_line(alpha = 0.2, na.rm = TRUE) +
  geom_hline(yintercept = 1, lty = 2)




first_day <- min(dat$date)
last_day <- 391 # how many days to create dates for
lut <- dplyr::tibble(
  day = seq_len(last_day),
  date = seq(first_day, first_day + length(day) - 1, by = "1 day")
)
tidy_proj <- dplyr::left_join(tidy_proj, lut, by = "day")
dplyr::glimpse(tidy_proj)

covidseir::plot_projection(tidy_proj, obs_dat = dat,
                           value_column = "value", date_column = "date")



plot_residuals(tidy_proj, obs_dat = dat, obj = fit)
set.seed(1)
plot_residuals(tidy_proj, obs_dat = dat, obj = fit, type = "quantile")


set.seed(1)
resid <- plot_residuals(tidy_proj, obs_dat = dat, obj = fit, type = "quantile",
                        return_residuals = TRUE)
hist(resid)

qqnorm(resid)
qqline(resid)







