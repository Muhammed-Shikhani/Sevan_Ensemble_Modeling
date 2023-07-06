# Load required packages
library(Kendall)
library(trend)
library(dplyr)
library(lubridate)
library(reshape2)
library(ggplot2)
library(ggsci)          
library(patchwork)
library(xtable)

setwd("/home/shikhani/Documents/Sevan_plotting_draft/")

output_annual <- get(load("cordex_LER_annual_named.rda"))[1:2]
airtemp <- get(load("air_temp_annual.rda"))

dat_a <- setNames(reshape2::melt(airtemp, id.vars = c("date")),
                  c("date", "gc_rc", "value", "period"))
dat_a$variable <- "Air_Temperature"
dat_a$model <- "Climate_Model"
## my go at preparing the data using tidyr an plotting

# reshape all the data to a long format data.frame
dat_l <- setNames(reshape2::melt(output_annual, id.vars = c("date")),
                  c("date", "gc_rc", "value", "period", "model", "variable"))
dat_l <- rbind(dat_l, dat_a)

# Group the data by model, gc_rc, period, and variable
dat_grouped <- dat_l %>%
  group_by(variable)

# Define a function to calculate Kendall tau and Sen's slope for each group
calc_trend <- function(x) {
  # Calculate Kendall tau
  tau <- as.vector(Kendall::MannKendall(x$value)$tau)
  
  # Calculate Sen's slope
  slope <- as.vector(trend::sens.slope( x$value)$est[1])*10
  
  # Return the results as a data frame
  return(data.frame(tau = tau, slope = slope))
}

# Define your filter conditions
period_list <-unique(dat_l$period)
variable_list <- unique(dat_l$variable)

# Create an empty data frame to store the results
results_df <- data.frame(gc_rc = character(), 
                         period = character(), 
                         model = character(), 
                         variable = character(), 
                         tau = numeric(), 
                         slope = numeric(), 
                         stringsAsFactors = FALSE)

# Use nested loops to iterate through all combinations of filter conditions

for (period in period_list) {
  dat_p <- dat_l[which(dat_l$period== period),]
  for (variable in variable_list) {
    dat_v <- dat_p[which(dat_p$variable== variable),]
    
    
    model_list <-unique( dat_v$model)
    
    for (model in model_list) {
      dat_m <- dat_v[which(dat_v$model== model),]
      gc_rc_list <- unique( dat_m$gc_rc)
      for (gc_rc in gc_rc_list) {
        filtered_dat <- dat_m[which(dat_m$gc_rc==gc_rc),]
        # Apply the calc_trend function on the filtered data
        trend_results <- calc_trend(filtered_dat)
        
        # Add the filter conditions and results to the results data frame
        results_df <- rbind(results_df, 
                            data.frame(gc_rc = gc_rc, 
                                       period = period, 
                                       model = model, 
                                       variable = variable, 
                                       tau = trend_results$tau, 
                                       slope = trend_results$slope))
      }
    }
  }
}

str(results_df)

trend_all <- results_df

which(trend_all$tau < 0)
trend_all[(trend_all$tau < 0),]

trend_all[(trend_all$slope < 0),]
trend_all[which(trend_all$slope > 0.6),]

# Define your filter conditions
period_list <-unique(trend_all $period)
variable_list <- unique(trend_all $variable)

# Create an empty data frame to store the results
trends_mean <- data.frame(period = character(), 
                          model = character(), 
                          variable = character(), 
                          slope = numeric(), 
                          stringsAsFactors = FALSE)

# Use nested loops to iterate through all combinations of filter conditions

for (period in period_list) {
  dat_p <- trend_all [which(trend_all $period== period),]
  for (variable in variable_list) {
    dat_v <- dat_p[which(dat_p$variable== variable),]
    
    
    model_list <-unique( dat_v$model)
    
    for (model in model_list) {
      dat_m <- dat_v[which(dat_v$model== model),]
      
      
      trends_mean <- rbind(trends_mean, 
                           data.frame(period = period, 
                                      model = model, 
                                      variable = variable, 
                                      slope =mean(dat_m$slope)))
      
      
      
    }
  }
}

str(trends_mean)



########################################


# reshape all the data to a long format data.frame
dat_l <- setNames(reshape2::melt(output_annual, id.vars = c("date")),
                  c("date", "gc_rc", "value", "period", "model", "variable"))




air_df <- read.csv("airtemp_mdelted.csv")
names(air_df) <- names(dat_l)[1:4]
air_df$model <- "Climate_Model"
air_df$variable <- "Air_Temperature"
names(air_df)
names(air_df)[3] <- "anomaly"
head(air_df)



# calculate the mean of the historic period for each model, run, and variable
mean_hist <- dat_l %>% filter(period == "historical") %>%
  group_by(variable, gc_rc, model) %>% summarise(base_mean = mean(value))



dat_anomaly_withmean <- dat_l  %>%
  left_join(mean_hist) %>%
  group_by(date, period, model, variable, gc_rc) %>%
  summarise(anomaly = value - base_mean)

dat_anomaly_withmean <- rbind(dat_anomaly_withmean, air_df)


# calculate mean sd and quantiles for each variable and period

dat_an_ag_mean <- dat_anomaly_withmean %>% group_by(date, period, variable,model) %>%
  summarise(mean = mean(anomaly),
            median = median(anomaly),
            sd = sd(anomaly),
            q25 = quantile(anomaly, 0.25,na.rm=TRUE),
            q75 = quantile(anomaly, 0.75,na.rm=TRUE),
            q05 = quantile(anomaly, 0.05,na.rm=TRUE),
            q95 = quantile(anomaly, 0.95,na.rm=TRUE))




historical_period <- "1995-2005"
future_period <- "2089-2099"
desired_models <- c("rcp26", "rcp45", "rcp85")

filtered_table <- dat_an_ag_mean %>%
  filter(date %in% c(1995:2005) | date %in% c(2089:2099) )



# Calculate average quantities per model, variable, and period
averages <- filtered_table %>%
  group_by(model, variable, period) %>%
  summarise(mean = mean(mean),
            median = mean(median),
            sd = mean(sd),
            q25 = mean(q25),
            q75 = mean(q75),
            q05 = mean(q05),
            q95 = mean(q95))

# Print the resulting averages
print(averages)

# Define the custom order of variables
custom_order <- c("Air_Temperature", "Surface_Temperature", "Bottom_Temperature")

# Order the averages table by the period and custom order of variables
averages_ordered <- averages %>%
  arrange(period, factor(variable, levels = custom_order))

# Print the ordered averages table
print(averages_ordered)



# Merge the subset_table and subset_trends tables
merged_table <- merge(averages_ordered, trends_mean, by = c("model", "variable", "period"), all = TRUE)



# Print the merged table
print(merged_table)



merged_ordered <- merged_table %>%
  arrange(period, factor(variable, levels = custom_order))
# Remove underscore from the "variable" column in the merged data frame
merged_ordered$variable <- gsub("_", " ", merged_ordered$variable)


merged_ordered$anomaly_period <- ifelse(merged_ordered$period == "historical", "1995-2005", "2089-2099")


merged_ordered <- merged_ordered %>%
  select(period, variable, model, slope, anomaly_period, everything())


merged_ordered


# Convert merged_ordered to a table
table_merged <- xtable(merged_ordered)

# Print the Overleaf table code
print(xtable(table_merged, type = "latex", include.rownames = FALSE))
