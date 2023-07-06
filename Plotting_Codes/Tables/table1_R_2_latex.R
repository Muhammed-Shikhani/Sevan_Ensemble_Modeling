data_list <- get(load("/home/shikhani/Documents/Cordex_rdata_final_output/cordex_LER_results_allmodels_allprojections_anual_swr.rda"))[[1]][[1]]
library(dplyr)
library(reshape2)
library(tidyr)
library(xtable)

model_df <- data.frame()

data_melted <- data_list

for(p in 1:length(data_list)){
  data_melted[[p]] <- melt( data_melted[[p]],id="date")
  data_melted[[p]]$Scenario <- names(data_melted)[p]
  model_df <- rbind(model_df, data_melted[[p]])
}
data_melted <- model_df 

data <- data_melted[which(data_melted$date %in% c(2000,2090)),]

data$rcm <- NA
data$gcm <- NA
data$domain <- NA
data$gcm_rcm <- NA
for (i in 1:length(data$variable)) {
  data$domain[i] <- unlist(strsplit(as.character(data$variable[i]) , split = "_"))[1]
  data$gcm[i] <- unlist(strsplit(as.character(data$variable[i]) , split = "_"))[2]
  data$rcm[i] <- unlist(strsplit(as.character(data$variable[i]) , split = "_"))[3]
  data$gcm_rcm[i] <- paste0(data$gcm[i] ,"_",data$rcm[i] )
  
}
head(data)
tail(data)


####################
table(data$variable, data$Scenario)
table(data$variable, data$Scenario, data$domain)

###########

str(data)


df <- data[, c("Scenario", "gcm_rcm")]

# Use the table function to count the occurrences of each combination of scenario and gcm_rcm
result <- as.data.frame(table(df$gcm_rcm, df$Scenario))

# Rename the columns for clarity
colnames(result) <- c("gcm_rcm", "Scenario", "Count")

# View the result
head(result)


unique_scenarios <- unique(result$Scenario)

# Create an empty list to store dataframes
my_list <- list()

for (i in unique_scenarios) {
  
  my_df <- result[which(result$Scenario==i),]
  
  # Extract GCM and RCM names into separate columns
  my_df$GCM <- sub("_.*", "", my_df$gcm_rcm)
  my_df$RCM <- sub(".*_", "", my_df$gcm_rcm)
  
  # Reshape dataframe using pivot_wider
  my_df <- pivot_wider(my_df, names_from = RCM, values_from = Count)
  
  # Remove unnecessary columns
  my_df <- my_df[,-c(1:2)]
  
  # Add dataframe to list
  my_list[[i]] <-   my_df %>% 
    group_by(GCM) %>% 
    summarize(
      `IITM-RegCM4-4` = sum(`IITM-RegCM4-4`, na.rm = TRUE),
      `SMHI-RCA4` = sum(`SMHI-RCA4`, na.rm = TRUE),
      `CLMcom-ETH-COSMO-crCLIM-v1-1` = sum(`CLMcom-ETH-COSMO-crCLIM-v1-1`, na.rm = TRUE),
      `ICTP-RegCM4-7` = sum(`ICTP-RegCM4-7`, na.rm = TRUE),
      `GERICS-REMO2015` = sum(`GERICS-REMO2015`, na.rm = TRUE)
    )
  
  
  
  
  
}

# View list of dataframes
my_list
#


list_reorder <- my_list

for(j in 1:length(list_reorder)){
  # reorder columns alphabetically using select()
  df_reorder <- as.data.frame(list_reorder[[j]])
  
  names(df_reorder )[1] <- "1GCM"
  df_reorder <-  df_reorder[, order(names(df_reorder))]
  list_reorder[[j]] <- df_reorder
  names(list_reorder[[j]] )[1] <- "GCM"
  
}



df_all <- list_reorder[[1]]
for (i in 2:ncol(list_reorder[[1]])) {
  new_df <- cbind(list_reorder[[1]][,i], list_reorder[[2]][,i], list_reorder[[3]][,i], list_reorder[[4]][,i])
  df_all[,i] <- paste(new_df[,1], new_df[,2], new_df[,3], new_df[,4], sep = "|")
}

df_all



table <- xtable(df_all, caption = "Table caption", label = "table:label")

print(table, booktabs = TRUE, include.rownames = TRUE, include.colnames = TRUE, caption.placement = "top")



# convert the dataframe to a matrix and then to numeric values
mat <- as.matrix(df_all[,-1])
mat <- apply(mat, c(1,2), function(x) as.numeric(strsplit(x, split = "|", fixed = TRUE)[[1]]))

sum(mat)
###############################
##############################
sum_cols <- NULL

for(i in 2:ncol(df_all)){
  
  sum_components <- colSums(matrix(as.numeric(unlist(strsplit(df_all[,i], "\\|"))), ncol = 4, byrow = TRUE))
  
  sum_cols <- c( sum_cols, paste(sum_components, collapse = "|"))  
}
sum_cols

colSums(matrix(as.numeric(unlist(strsplit( sum_cols , "\\|"))), ncol = 4, byrow = TRUE))


sum_rows <- NULL

for(i in 1:nrow(df_all)){
  
  sum_components <- colSums(matrix(as.numeric(unlist(strsplit(as.character(df_all[i,-1]), "\\|"))), ncol = 4, byrow = TRUE))
  
  sum_rows<- c( sum_rows, paste(sum_components, collapse = "|"))  
}
sum_rows

sum_all <- colSums(matrix(as.numeric(unlist(strsplit( sum_cols , "\\|"))), ncol = 4, byrow = TRUE))

sum(colSums(matrix(as.numeric(unlist(strsplit( sum_cols , "\\|"))), ncol = 4, byrow = TRUE)))
##################



df_sums <- df_all
df_sums <- rbind(df_sums,c("Sum",sum_cols))
df_sums$Sum <- c(sum_rows,paste(sum_all, collapse = "|"))

df_sums
table <- xtable(df_sums, caption = "Table caption", label = "table:label")

print(table, booktabs = TRUE, include.rownames = TRUE, include.colnames = TRUE, caption.placement = "top")

