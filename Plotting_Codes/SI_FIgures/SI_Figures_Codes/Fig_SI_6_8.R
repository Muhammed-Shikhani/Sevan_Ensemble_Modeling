library(lubridate)
library(reshape2)
library(dplyr)
library(patchwork)
library(ggplot2)
library(RColorBrewer)
library(lubridate)
setwd("/home/shikhani/Documents/Sevan_plotting_draft/")
output_daily <- get(load("CORDEX_LER_daily_named_sim.rda"))[1:2]




## my go at preparing the data using tidyr an plotting

# reshape all the data to a long format data.frame
dat_l <- setNames(reshape2::melt(output_daily, id.vars = c("DateTime")),
                  c("date", "gc_rc", "value", "period", "model", "variable"))
############

data <- dat_l %>% filter(period == "historical" & year(dat_l[,1]) ==1998) 

# define own colors
mycol <- c("grey42", rev(brewer.pal(3, "Set1")))
mycol2 <- alpha(mycol, 0.6)

# Reorder the levels of the 'variable' factor
data$variable <- factor(data$variable, levels = c("Surface_Temperature", "Bottom_Temperature" ))

p1 <- data %>%
  filter(model == "Ensemble_Mean") %>%
  ggplot() +
  geom_hline(yintercept = 4) +
  geom_line(aes(x = date, y = value, group = gc_rc, col = period), alpha = 0.25) +
  scale_y_continuous(limits = c(0, 24), breaks = seq(0, 24, 4), labels = seq(0, 24, 4)) +
  theme_bw(base_size = 17) +
  theme(plot.margin = margin(0, 0, 0, 0),
        legend.position = "none") +
  facet_wrap(variable ~ ., nrow = 2, dir = "v") +
  labs(y = "Temperature (°C)")

p1

ggsave('temp_1998.png', p1,  dpi = 300,width = 200,height = 200, units = 'mm')

##########


data <- dat_l %>% filter(period == "rcp85" & year(dat_l[,1]) ==2038) 

# define own colors
mycol <- c("grey42", rev(brewer.pal(3, "Set1")))
mycol2 <- alpha(mycol, 0.6)

# Reorder the levels of the 'variable' factor
data$variable <- factor(data$variable, levels = c("Surface_Temperature", "Bottom_Temperature" ))

p1 <- data %>%
  filter(model == "Ensemble_Mean") %>%
  ggplot() +
  geom_hline(yintercept = 4) +
  geom_line(aes(x = date, y = value, group = gc_rc, col = period), alpha = 0.25) +
  scale_y_continuous(limits = c(0, 24), breaks = seq(0, 24, 4), labels = seq(0, 24, 4)) +
  theme_bw(base_size = 17) +
  theme(plot.margin = margin(0, 0, 0, 0),
        legend.position = "none") +
  facet_wrap(variable ~ ., nrow = 2, dir = "v") +
  labs(y = "Temperature (°C)")

p1

ggsave('temp_2038.png', p1,  dpi = 300,width = 200,height = 200, units = 'mm')

#############


data <- dat_l %>% filter(period == "rcp85" & year(dat_l[,1]) ==2098) 

# define own colors
mycol <- c("grey42", rev(brewer.pal(3, "Set1")))
mycol2 <- alpha(mycol, 0.6)

# Reorder the levels of the 'variable' factor
data$variable <- factor(data$variable, levels = c("Surface_Temperature", "Bottom_Temperature" ))

p1 <- data %>%
  filter(model == "Ensemble_Mean") %>%
  ggplot() +
  geom_hline(yintercept = 4) +
  geom_line(aes(x = date, y = value, group = gc_rc, col = period), alpha = 0.25) +
  scale_y_continuous(limits = c(0, 24), breaks = seq(0, 24, 4), labels = seq(0, 24, 4)) +
  theme_bw(base_size = 17) +
  theme(plot.margin = margin(0, 0, 0, 0),
        legend.position = "none") +
  facet_wrap(variable ~ ., nrow = 2, dir = "v") +
  labs(y = "Temperature (°C)")

p1

ggsave('temp_2098.png', p1,  dpi = 300,width = 200,height = 200, units = 'mm')

