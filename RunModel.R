# Intention Statement: To explore the Redoubt Lake sockeye weir data and interpolate missing days
# Written by Blake Toney, April, 2026
# Last update 5/19/26

# Package library
library(tidyverse)
library(lubridate)
library(MASS)
library(mgcv)
library(gratia)
library(mgcViz)
library(marginaleffects)
library(ggeffects)
library(gganimate)
library(gifski)
library(ggridges)

# setwd(C:/Users/bwton/OneDrive/Desktop/Forest Service/RedoubtModeling)
wd <- getwd()
dir.data <- file.path(wd, "data")
dir.output <- file.path(wd, "output")
# dir.create(dir.output)

# Read in dataset
dat <- read.csv(file.path(dir.data, "RedoubtRawCount.csv"), row.names = NULL)
age.dat <- read.csv(file.path(dir.data, "Age.Comp.csv"), row.names = NULL)

# Pivot longer for plotting
dat2 <- dat%>%
  pivot_longer(cols = starts_with("X"), names_to = "Year", values_to = "Count")%>%
  mutate(Year = as.numeric(sub("X", "", Year))) %>%
  rowwise() %>%
  mutate(Date = lubridate::dmy(paste(Date, Year)), doy=yday(Date), Year = factor(Year, levels = sort(unique(Year))))%>%
  ungroup()%>%
  group_by(Year) %>%
  arrange(Date, .by_group = TRUE) %>%
  mutate(cumsum = cumsum(replace_na(Count, 0))) %>%
  ungroup()

# Explore dataset
# Latest dates
endDay <- dat2  %>% 
  filter(!is.na(Count))%>%
  group_by(Year)%>%
  slice_max(Date, n = 1) %>%
  dplyr::select(Year, endDay = Date, Count)
# Earliest Dates
begDay <- dat2 %>% 
  filter(!is.na(Count))%>%
  group_by(Year)%>%
  slice_min(Date, n = 1) %>%
  dplyr::select(Year, begDay = Date, Count)

SampleTimeline <- left_join(begDay, endDay, by = "Year")%>%
  ungroup() %>%
  mutate(begdoy = yday(begDay), enddoy = yday(endDay))

# Setup labels for next plot
years <- dat2 %>%
  filter(!Year %in% c("1953", "1954", "1955")) %>%
  group_by(Year) %>%
  pull(Year) %>%
  unique() %>%
  sort()

every_other <- years[seq(1, length(years), by = 2)]

# Create a plot of observed run size
# dat2 %>%
#   filter(Date > as.Date("1980-01-01"), !is.na(Count)) %>%
#   group_by(Year) %>%
#   summarize(return = sum(Count), .groups = "drop") %>%
#   ggplot(aes(x = Year, y = return/1000, group = 1)) +
#   geom_point(alpha = 0.7, color = "black") +
#   geom_line(alpha = 0.6, color = "brown") +
#   scale_x_discrete(breaks = every_other)+
#   labs(x = "Year", y = "Annual Observed Return\n\ (thousands of fish)",
#     title = "Yearly Sockeye Weir Counts - Redoubt Lake") +
#   theme_bw(base_size = 18)+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


#Plot the changes in escapement across years sampled 
# dat2 %>%
#   filter(Date > as.Date("1980-01-01"), !is.na(Count)) %>%
#   group_by(Year) %>%
#   summarize(return = sum(Count), .groups = "drop") %>%
#   ggplot(aes(x = Year, y = return/1000, group = 1)) +
#   annotate("rect", xmin = -Inf, xmax = Inf,
#     ymin = 7, ymax = 25, fill = "darkgreen",
#     alpha = 0.3) +
#   geom_rect(aes(xmin = -Inf, xmax = Inf,
#       ymin = -Inf, ymax = -Inf,
#       fill = "Optimal Escapement Goal"),alpha = 0.15) +
#   geom_hline(aes(yintercept = 40, color = "Commercial Harvest Trigger"),
#     linetype = "dashed",
#     linewidth = 1) +
#   geom_hline(aes(yintercept = 7, fill = "Optimal Escapement Goal"),
#              linetype = "dotted",
#              linewidth = 1,
#              color ="grey") +
#   geom_hline(aes(yintercept = 25, fill = "Optimal Escapement Goal"),
#              linetype = "dotted",
#              linewidth = 1,
#              color = "grey") +
#   geom_line(alpha = 0.6, color = "brown") +
#   geom_point(alpha = 0.7, color = "black") +
#   scale_x_discrete(breaks = every_other) +
#   scale_fill_manual(name = "",
#     values = c("Optimal Escapement Goal" = "darkgreen")) +
#   scale_color_manual(name = "",
#     values = c("Commercial Harvest Trigger" = "blue",
#                "Optimal Escapement Goal" = "darkgreen")) + 
#   labs(x = "Year", y = "Annual Observed Return\n(thousands of fish)",
#     title = "Yearly Sockeye Weir Counts - Redoubt Lake") +
#   theme_bw(base_size = 18) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#     legend.position = "top")


# ggsave(file.path(dir.output, "YearlyWeirCount.png"), height = 5, width=9, units = "in")

# Plot of daily count data by year
# dat2 %>%
#   ggplot(aes(x = doy, y = Count, color = Year)) +
#   geom_point(alpha = 0.5, na.rm=F) +
#   geom_line(na.rm=F)+
#   labs(x = "Day of Year",
#        y = "Daily Count (individuals)",
#        title = "Daily Sockeye Return by Year") +
#   theme_minimal(base_size = 14) +
#   theme(legend.position = "right")

# Plot run timing by year in gg ridges form
# dat2 %>% filter(Date > as.Date("2004-01-01"))%>%
#   ggplot(aes(x = doy,
#              y = factor(Year),
#              height = Count,
#              group = Year,
#              fill = Year)) +
#   geom_density_ridges(stat = "identity",
#                       scale = 5,
#                       alpha = 0.8,
#                       color = "white") +
#   scale_fill_viridis_d(option = "D") +
#   labs(x = "Day of Year",
#        y = "Year",
#        title = "Sockeye Run Timing Across Years") +
#   theme_minimal(base_size = 16) +
#   theme(legend.position = "none")
# ggsave(file.path(dir.output, "RidgePlot_Count.png"), height = 5, width=9, units = "in")

# Lets identify the point at which 50% of the run has returned on an annual basis
# halfDay <- dat2 %>%
#   group_by(Year) %>% 
#   filter(cumsum >= max(cumsum) / 2) %>% 
#   slice_min(Date) %>% 
#   ungroup()
# hist(halfDay$doy, breaks = 20)
# 
# ggplot(halfDay, aes(x=doy, y=cumsum, color=Year))+
#   geom_point()+
#   scale_x_date()+
#   theme_bw()+
#   theme(legend.position = "none")+
#   labs(x= "Day of Year", y= "50% of run size", title = "Magnitude and Timing of 50% run completion")

# Lets create an animation of each year's run
# p <- dat2 %>%
#   filter(Date > as.Date("1980-01-01")) %>%
#   ggplot(aes(x = doy, y = Count, color = Year, group = Year)) +
#   geom_line(alpha = 0.8, linewidth = 1, na.rm=F) +
#   geom_point(alpha = 0.6, size = 1.5, na.rm=F) +
#   labs(x = "Day of Year", y = "Daily Count",
#     title = "Daily Sockeye Return — Year: {closest_state}") +
#   theme_bw(base_size = 16) +
#   theme(legend.position = "none") +
#   transition_states(Year,
#     transition_length = 2,
#     state_length = 1) +
#   enter_fade() +
#   exit_fade()
# 
# # animate( p, fps = 20, duration = 15, width = 800,
#   # height = 600, renderer = gifski_renderer("sockeye_returns.gif"))
# 
# 
# # Visualize sampling period of the weir
# SampleTimeline %>% 
#   ggplot(aes(y = Year,
#              x = begdoy,
#              xend = enddoy,
#              yend = Year,
#              color = Year)) + 
#   geom_segment(linewidth = 1.2, alpha = 0.9) +
#   scale_x_date(date_labels = "%m-%d")+
#   scale_y_discrete(breaks = every_other)+
#   scale_color_viridis_d(option = "D")+
#   labs(x = "Date", y = "Year",
#        title = "Annual Weir Operation Timeline") +
#   theme_bw(base_size = 18) +
#   theme(panel.grid.minor = element_blank(),
#         axis.text.y = element_text(size = 8), 
#         legend.position = "none")
# ggsave(file.path(dir.output, "WeirTimeline.png"), height = 5, width = 9, units = "in")



# Begin exploring the modeling possibilities
mod <- lm(Count~factor(Year)+doy+I(doy^2), data=dat2)

# summary(mod)
# residuals are bad, explore count dispersion
# hist(dat2$Count)
# hist(log(dat2$Count))


mod1 <- glm(Count~factor(Year)+doy+I(doy^2), data=dat2, family = "poisson")
# summary(mod1)
# plot(mod1)

# Test for dispersion
disp <- sum(residuals(mod1, type = "pearson")^2) / mod1$df.residual
disp

# Holy smokes dispersion is high, let's try nb but I think i have to go hurdle or zip
mod_nb <- glm.nb(Count~Year+poly(doy, 4), data=dat2)
summary(mod_nb)
# plot(mod_nb)

# Dispersion Test
disp <- sum(residuals(mod_nb, type = "pearson")^2) / mod_nb$df.residual
disp

dat2$pred <- predict(mod_nb, newdata = dat2, type = "response")
# dat2 %>%
#   ggplot(aes(x = doy)) +
#   geom_point(aes(y = pred),
#              alpha = 0.4,
#              color = "steelblue") +
#   labs(x = "Day of Year",
#        y = "Count",
#        title = "Model Predicted Sockeye Counts (NB GLM)") +
#   theme_minimal(base_size = 14)

# Model Interpretation:
# Mean day of peak timing = day 205, or July 24th
# peak_dates <- dat2 %>%
#   group_by(Year) %>%
#   slice_max(pred, n = 1, with_ties = FALSE) %>%
#   dplyr::select(Year, Date, doy, pred)
# Comparison of stable vs chaotic years =================
dat2_stable <- dat2 %>% filter(Date > as.Date("2008-01-01"), Date < as.Date("2014-01-01"))
mod_nb_stable <- glm.nb(Count~Year+poly(doy, 4), data=dat2_stable)
summary(mod_nb_stable)


dat2_chaotic <- dat2 %>% filter(Date > as.Date("2019-01-01"))
mod_nb_chaotic <- glm.nb(Count~Year+poly(doy, 4), data=dat2_chaotic)
summary(mod_nb_chaotic)

# 



# Lets try a gam to predict the peak run timing across years
gam.mod<-bam(Count~s(doy, by=Year), data=dat2, family = nb())
summary(gam.mod)
# gam.check(gam.mod)
# It apears as though the limited data before and after observations ended results in model entropy at upper and lower end. I need to constrain my prediction grid to first day sampled to last day sampled.
# draw(gam.mod)[[5]]

# create prediction grid
# pred_grid <- SampleTimeline %>%
#   dplyr::select(Year, begdoy, enddoy) %>%
#   rowwise() %>%
#   mutate(doy = list(seq(begdoy, enddoy, by = 1))) %>%
#   unnest(doy)
# table(pred_grid$doy)
# 
# pred_grid$pred <- predict(gam.mod,
#                           newdata = pred_grid,
#                           type = "response")
# 
# 
# pred_grid <- pred_grid %>%
#   mutate(
#     fit = predict(gam.mod, newdata = pred_grid, type = "link", se.fit = TRUE)$fit,
#     se  = predict(gam.mod, newdata = pred_grid, type = "link", se.fit = TRUE)$se.fit,
#     pred = exp(fit),                          # back-transform NB link
#     lower = exp(fit - 1.96 * se),
#     upper = exp(fit + 1.96 * se))
# 
# pred_grid %>% filter(as.numeric(as.character(Year)) > 2004) %>%       #note, 1983 removed because of erroneous early season prediction
#   ggplot(aes(x = doy, y = pred, color = Year)) +
#   geom_ribbon(aes(ymin = lower, ymax = upper, fill = Year),
#               alpha = 0.10,
#               linewidth = 0,
#               color = NA) +
#   geom_line() +
#   labs(x = "Day of Year",
#        y = "Predicted Count",
#        color = "Year",
#        title = "GAM predictions constrained to observed DOY ranges") +
#   theme_bw(base_size = 18)
# ggsave(file.path(dir.output, "GAM_yearSmooth.png"), height = 7.5, width = 13.5, units = "in")


gam.mod2 <-  bam(Count ~ Year + s(doy, k=20) + s(doy, Year, k=20, bs="fs", m=2), data=dat2, family = nb())
summary(gam.mod2)
# draw(gam.mod2)
# gam.check(gam.mod2)

# performance::compare_performance(gam.mod, gam.mod2)
# or

# Back predict values:
# pred_grid$pred2 <- predict(gam.mod2,
#                           newdata = pred_grid,
#                           type = "response")
# 
# pred_grid <- pred_grid %>%
#   mutate(fit2 = predict(gam.mod2, newdata = pred_grid, type = "link", se.fit = TRUE)$fit,
#          se2  = predict(gam.mod2, newdata = pred_grid, type = "link", se.fit = TRUE)$se.fit,,
#          lower2 = exp(fit2 - 1.96 * se2),
#          upper2 = exp(fit2 + 1.96 * se2))
# # plot the 
# pred_grid %>% filter(as.numeric(as.character(Year)) > 2004) %>%       #note, 1983 removed because of erroneous early season prediction
#   ggplot(aes(x = doy, y = pred2, color = Year)) +
#   geom_ribbon(aes(ymin = lower2, ymax = upper2, fill = Year),
#               alpha = 0.20,
#               linewidth = 0,
#               color = NA) +
#   geom_line() +
#   labs(x = "Day of Year",
#        y = "Predicted Count",
#        color = "Year",
#        fill = "Year",
#        title = "GAM predictions constrained to observed DOY ranges") +
#   scale_color_viridis_d(option = "B")+
#   theme_bw(base_size = 18)

# ggsave(file.path(dir.output, "GAM_yearSmooth2_fs.png"), height = 7.5, width = 13.5, units = "in")
# And finally, a simple single smoother model
gam.mod3 <- gam(Count ~ Year + s(doy, k=20), data=dat2, family = nb())
# summary(gam.mod3)
# draw(gam.mod3)
# gam.check(gam.mod3)
# performance::compare_performance(gam.mod, gam.mod2, gam.mod3)
# formula(gam.mod)

# The model below is intended to identify the "general shape" of the run across all years.
# Probably should exclude all years with commercial fishing to avoid misidentifying the peak
gam.mod4 <- gam(Count ~ s(doy, k=20), data=dat2, family = nb())
summary(gam.mod4)
# 
# pred_grid2 <- pred_grid %>% dplyr::select(Year, begdoy, enddoy, doy)
# pred_grid2$pred <- predict(gam.mod4,
#                           newdata = pred_grid2,
#                           type = "response")
# 
# 
# pred_grid2 <- pred_grid2 %>%
#   mutate(
#     fit = predict(gam.mod4, newdata = pred_grid2, type = "link", se.fit = TRUE)$fit,
#     se  = predict(gam.mod4, newdata = pred_grid2, type = "link", se.fit = TRUE)$se.fit,
#     pred = exp(fit),                          # back-transform NB link
#     lower = exp(fit - 1.96 * se),
#     upper = exp(fit + 1.96 * se))
# 
# pred_grid2 %>% filter(as.numeric(as.character(Year)) == 2004) %>%       #note, 1983 removed because of erroneous early season prediction
#   ggplot(aes(x = doy, y = pred, color = Year)) +
#   geom_ribbon(aes(ymin = lower, ymax = upper, fill = Year),
#               alpha = 0.10,
#               linewidth = 0,
#               color = NA) +
#   geom_line() +
#   labs(x = "Day of Year",
#        y = "Predicted Count",
#        color = "Year",
#        title = "Gam run-timing Predictions") +
#   theme_bw(base_size = 18)+
#   theme(legend.position = "none")

