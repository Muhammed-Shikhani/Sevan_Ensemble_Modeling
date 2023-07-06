calb <- read.csv("Final_plots_codes/Fig4_model_performance/calib_fit.csv")[,1:5]
vald <- read.csv("Final_plots_codes/Fig4_model_performance/valid_fit.csv")[,1:5]


my_table <- data.frame(model=calb$model)
for(i in 2:ncol(calb)){
  my_table[,i] <- paste(calb[,i], vald[,i], sep = "|")
}
names(my_table) <- names(calb)
my_table



# Convert merged_ordered to a table
my_table_tex <- xtable(my_table)

# Print the Overleaf table code
print(xtable(my_table_tex, type = "latex", include.rownames = FALSE))
