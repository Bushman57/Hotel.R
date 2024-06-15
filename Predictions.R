# Code By: Joy Adul
#Created: 11/8/2023
# Last Edited: 
# Predictive Analysis of the Impact of Climate Change on Energy Generation in the USA and Kenya

# load libraries
library(stringr)            # for data loading
library(measurements)       # for converting units
library(gbm)                # for modeling
library(ggplot2)            # for plotting
library(dplyr)              # for data formatting
library(lubridate)
library(ggplot2)
library(stringr)
library(data.table)
library(randomForest) # for random forest
library(caret)
library(rlist)
library(purrr)
library(lubridate)
library(readr)
library(tidymodels)
library(tidyr)
library(zoo)
library(reshape2)

# Load CMIP6 files
file_list <- list.files(path = "./CMIP6_cvs", pattern = "\\.csv$", full.names = TRUE)

# Define the path to the directory containing the CSV files
path_to_files <- "./CMIP6_cvs"

# List all CSV files in the directory
file_list <- list.files(path = path_to_files, pattern = "\\.csv$", full.names = TRUE)

# Print the list of files to ensure they are being listed correctly
print(file_list)



#Separate files
index <- c(1:4)
gfdl_esm4_ssp126_list <- list()
for (filename in file_list[index]) {
  raw_df <- fread(filename)
  gfdl_esm4_ssp126_list <- list.append(gfdl_esm4_ssp126_list, raw_df)
}
index <- c(5:8)
gfdl_esm4_ssp585_list <- list()
for (filename in file_list[index]) {
  raw_df <- fread(filename)
  gfdl_esm4_ssp585_list <- list.append(gfdl_esm4_ssp585_list, raw_df)
}
index <- c(9:12)
ipsl_cm6a_lr_ssp126_list <- list()
for (filename in file_list[index]) {
  raw_df <- fread(filename)
  ipsl_cm6a_lr_ssp126_list <- list.append(ipsl_cm6a_lr_ssp126_list, raw_df)
}
index <- c(13:16)
ipsl_cm6a_lr_ssp585_list <- list()
for (filename in file_list[index]) {
  raw_df <- fread(filename)
  ipsl_cm6a_lr_ssp585_list <- list.append(ipsl_cm6a_lr_ssp585_list, raw_df)
}

index <- c(17:20)
mpi_esm1_2_hr_ssp126_list <- list()
for (filename in file_list[index]) {
  raw_df <- fread(filename)
  mpi_esm1_2_hr_ssp126_list <- list.append(mpi_esm1_2_hr_ssp126_list, raw_df)
}

index <- c(21:24)
mpi_esm1_2_hr_ssp585_list <- list()
for (filename in file_list[index]) {
  raw_df <- fread(filename)
  mpi_esm1_2_hr_ssp585_list <- list.append(mpi_esm1_2_hr_ssp585_list, raw_df)
}

index <- c(25:28)
mri_esm2_0_ssp126_list <- list()
for (filename in file_list[index]) {
  raw_df <- fread(filename)
  mri_esm2_0_ssp126_list <- list.append(mri_esm2_0_ssp126_list, raw_df)
}

index <- c(29:32)
mri_esm2_0_ssp585_list <- list()
for (filename in file_list[index]) {
  raw_df <- fread(filename)
  mri_esm2_0_ssp585_list <- list.append(mri_esm2_0_ssp585_list, raw_df)
}

index <- c(33:36)
ukesm1_0_ll_ssp126_list <- list()
for (filename in file_list[index]) {
  raw_df <- fread(filename)
  ukesm1_0_ll_ssp126_list <- list.append(ukesm1_0_ll_ssp126_list, raw_df)
}

index <- c(37:40)
ukesm1_0_ll_ssp585_list <- list()
for (filename in file_list[index]) {
  raw_df <- fread(filename)
  ukesm1_0_ll_ssp585_list <- list.append(ukesm1_0_ll_ssp585_list, raw_df)
}


STATE_IDS <- c(1,2,8,14,15,38)
cmip_list <- list(gfdl_esm4_ssp126_list, gfdl_esm4_ssp585_list, ipsl_cm6a_lr_ssp126_list, ipsl_cm6a_lr_ssp585_list, mpi_esm1_2_hr_ssp126_list,
                  mpi_esm1_2_hr_ssp585_list, mri_esm2_0_ssp126_list, mri_esm2_0_ssp585_list, ukesm1_0_ll_ssp126_list, ukesm1_0_ll_ssp585_list) 
cmip_min_list <- list() 
cmip_max_list <- list() 
cmip_avg_list <- list()
cmip_pr_total_list <- list()
for (cmip_condition in cmip_list) {   
  data_min <- list()   
  data_max <- list()   
  data_avg <- list()
  ind <- 0
  for (ent in cmip_condition) {
    ind <- ind + 1
    ent$date <- make_date(year = ent$YYYY, month = ent$MM, day = ent$DD)     
    ent_avg <- ent %>%       
      mutate(month = floor_date(date, "MM")) %>%       
      group_by(YYYY, MM) %>%       
      summarise(across(starts_with("ID"), mean, na.rm = TRUE))     
    ent_1 <- ent_avg [,STATE_IDS]          
    ent_max <- ent %>%       
      mutate(month = floor_date(date, "MM")) %>%       
      group_by(YYYY, MM) %>%       
      summarise(across(starts_with("ID"), max, na.rm = TRUE))     
    ent_2 <- ent_max [,STATE_IDS]          
    ent_min <- ent %>%       
      mutate(month = floor_date(date, "MM")) %>%       
      group_by(YYYY, MM) %>%       
      summarise(across(starts_with("ID"), min, na.rm = TRUE))     
    ent_3 <- ent_min [,STATE_IDS]     
    data_min <- list.append(data_min, ent_3)     
    data_max <- list.append(data_max, ent_2)     
    data_avg <- list.append(data_avg, ent_1)
    
    if (ind == 2) {
      ent_total <- ent %>%       
        mutate(month = floor_date(date, "MM")) %>%       
        group_by(YYYY, MM) %>%       
        summarise(across(starts_with("ID"), sum, na.rm = TRUE))     
      ent_total <- ent_total [,STATE_IDS]
      ent_total_cols <- colnames(ent_total)
      STATE_ID_COLUMN_NAMES_LIST <- ent_total_cols[c(3:length(ent_total_cols))]
      cmip_pr_total_list <- list.append(cmip_pr_total_list, ent_total)
    }
  }   
  cmip_min_list <- list.append(cmip_min_list, data_min)   
  cmip_max_list <- list.append(cmip_max_list, data_max)   
  cmip_avg_list <- list.append(cmip_avg_list, data_avg)   
}

#Combining predictors by state
nested_data_list <- list()

for (state_id in 3:length(STATE_IDS)-2) {
  cmip6_state_list <- list()
  for (cmip_cond_index in 1:length(cmip_list)) {
    cmip_cond_state_df <- cbind(cmip_avg_list[[cmip_cond_index]][[1]][STATE_ID_COLUMN_NAMES_LIST[state_id]],cmip_avg_list[[cmip_cond_index]][[2]][STATE_ID_COLUMN_NAMES_LIST[state_id]], cmip_avg_list[[cmip_cond_index]][[3]][STATE_ID_COLUMN_NAMES_LIST[state_id]],
                                cmip_avg_list[[cmip_cond_index]][[4]][STATE_ID_COLUMN_NAMES_LIST[state_id]],cmip_max_list[[cmip_cond_index]][[1]][STATE_ID_COLUMN_NAMES_LIST[state_id]],cmip_max_list[[cmip_cond_index]][[2]][STATE_ID_COLUMN_NAMES_LIST[state_id]],
                                cmip_max_list[[cmip_cond_index]][[3]][STATE_ID_COLUMN_NAMES_LIST[state_id]],cmip_max_list[[cmip_cond_index]][[4]][STATE_ID_COLUMN_NAMES_LIST[state_id]],cmip_min_list[[cmip_cond_index]][[1]][STATE_ID_COLUMN_NAMES_LIST[state_id]],
                                cmip_min_list[[cmip_cond_index]][[2]][STATE_ID_COLUMN_NAMES_LIST[state_id]],cmip_min_list[[cmip_cond_index]][[3]][STATE_ID_COLUMN_NAMES_LIST[state_id]],cmip_min_list[[cmip_cond_index]][[4]][STATE_ID_COLUMN_NAMES_LIST[state_id]],
                                cmip_pr_total_list[[cmip_cond_index]][STATE_ID_COLUMN_NAMES_LIST[state_id]])
    cmip_cond_state_df <- as.data.frame(cmip_cond_state_df)
    cmip_cond_state_df <- setnames(cmip_cond_state_df,
                                   # c("V1", "V2", "V3", "V4","V5", "V6", "V7", "V8","V9", "V10", "V11", "V12", "V13"),
                                   c("Ave_rh", "Ave_prep", "Ave_wind","Ave_temp",
                                     "Max_rh", "Max_prep", "Max_wind","Max_temp",
                                     "Min_rh", "Min_prep", "Min_wind","Min_temp",
                                     "Total_prep"))
    cmip6_state_list <- list.append(cmip6_state_list, cmip_cond_state_df)
  }
  nested_data_list <- list.append(nested_data_list, cmip6_state_list)
}

#loading the models
file_list <- list.files(path = "./Models", pattern = "\\.rds$", full.names = TRUE)

# For models indexed 1 to 3
index <- c(1:3)
model_CA <- list()
for (filename in file_list[index]) {
  loaded_model <- readRDS(filename)  # Load the .rds file
  model_CA <- append(model_CA, list(loaded_model))  # Append the loaded model to the list
}

# For models indexed 4 to 5
index <- c(4:5)
model_FL <- list()
for (filename in file_list[index]) {
  loaded_model <- readRDS(filename)  # Load the .rds file
  model_FL <- append(model_FL, list(loaded_model))  # Append the loaded model to the list
}

# For models indexed 6 to 7
index <- c(6:7)
model_GA <- list()
for (filename in file_list[index]) {
  loaded_model <- readRDS(filename)  # Load the .rds file
  model_GA <- append(model_GA, list(loaded_model))  # Append the loaded model to the list
}

# For models indexed 8 to 10
index <- c(8:10)
model_NY <- list()
for (filename in file_list[index]) {
  loaded_model <- readRDS(filename)  # Load the .rds file
  model_NY <- append(model_NY, list(loaded_model))  # Append the loaded model to the list
}

models_list <- list(model_CA,  model_FL, model_GA, model_NY) #combine models

# models_list
# nested_data_list
predictions_list <- vector("list", length(models_list))

for (i in seq_along(models_list)) {
  state_models <- models_list[[i]]
  state_data_list <- nested_data_list[[i]]
  predictions_list[[i]] <- vector("list", length(state_models))
  for (j in seq_along(state_models)) {
    # state_type_model is like model_ca_hydro
    state_type_model <- state_models[[j]]
    predictions_list[[i]][[j]] <- vector("list", length(state_data_list))
    for (k in seq_along(state_data_list)) {
      # state_type_data is like gfdl_esm4_ssp585_ca
      state_type_data <- state_data_list[[k]]
      pred_df <- predict(state_type_model, state_type_data)
      predictions_list[[i]][[j]][[k]] <- pred_df
      # strsplit(models_file_list, '/rf_')
    }
  }
}

predictions_df <- data.frame(
  model_CA_hydro = predictions_list[[1]][[1]],
  model_CA_solar = predictions_list[[1]][[2]],
  model_CA_wind = predictions_list[[1]][[3]],
  model_FL_hydro = predictions_list[[2]][[1]],
  model_FL_solar = predictions_list[[2]][[2]],
  model_GA_hydro = predictions_list[[3]][[1]],
  model_GA_solar = predictions_list[[3]][[2]],
  model_NY_hydro = predictions_list[[4]][[1]],
  model_NY_solar = predictions_list[[4]][[2]],
  model_NY_wind = predictions_list[[4]][[3]]
)


#Add date column
# Create a sequence of dates starting from a specific date
start_date <- as.Date("2001-01-01")
number_of_rows <- nrow(predictions_df)
dates <- seq.Date(from = start_date, by = "month", length.out = number_of_rows)

# Format the dates to keep only year and month
formatted_dates <- format(dates, "%Y-%m")

# Add the formatted dates as a new column
predictions_df$date <- formatted_dates

#Extracting 2040-2060 & 2021-2021)
future <- predictions_df[predictions_df$date >= "2040-01" & predictions_df$date <= "2060-12", ]
present <- predictions_df[predictions_df$date >= "2001-01" & predictions_df$date <= "2021-12", ]



#write.csv(predictions_df, "C:/Users/jaa6483/OneDrive - The Pennsylvania State University/Desktop/Class work/PROJECTS/Predictive_Analysis/Prediction\\Prediction.csv", row.names=TRUE)
#write.csv(future, "C:/Users/jaa6483/OneDrive - The Pennsylvania State University/Desktop/Class work/PROJECTS/Predictive_Analysis/Prediction\\Future_pred.csv", row.names=TRUE)
#write.csv(present, "C:/Users/jaa6483/OneDrive - The Pennsylvania State University/Desktop/Class work/PROJECTS/Predictive_Analysis/Prediction\\Present_pred.csv", row.names=TRUE)
#########################################################################################################################################################
#FUTURE
#California Results
gfdl_esm4_ssp126_CA_hydro_mean <- mean(future$model_CA_hydro.c..1....40.2200707630044...2....49.935081012511...3....41.6991993743741..)

gfdl_esm4_ssp585_CA_hydro_mean <- mean(future$model_CA_hydro.c..1....40.2200707630044...2....49.935081012511...3....41.6991993743741...1)

ipsl_cm6a_lr_ssp126_CA_hydro_mean <- mean(future$model_CA_hydro.c..1....43.6767493059273...2....39.8364782776058...3....46.0844176138097..)

ipsl_cm6a_lr_ssp585_CA_hydro_mean <- mean(future$model_CA_hydro.c..1....43.6767493059273...2....39.8364782776058...3....46.0844176138097...1)

mpi_esm1_2_hr_ssp126_CA_hydro_mean <- mean(future$model_CA_hydro.c..1....41.459795036014...2....41.1809313972826...3....48.4379164367442..)

mpi_esm1_2_hr_ssp585_CA_hydro_mean <- mean(future$model_CA_hydro.c..1....41.459795036014...2....41.1809313972826...3....48.4379164367442...1)

mri_esm2_0_ssp126_CA_hydro_mean <- mean(future$model_CA_hydro.c..1....41.8321530732576...2....44.7523393452629...3....42.2118139626244..)

mri_esm2_0_ssp585_CA_hydro_mean <- mean(future$model_CA_hydro.c..1....41.8321530732576...2....44.7523393452629...3....42.2118139626244...1)

ukesm1_0_ll_ssp126_CA_hydro_mean <- mean(future$model_CA_hydro.c..1....47.1225303720345...2....46.1459620384744...3....47.4991395542952..)

ukesm1_0_ll_ssp585_CA_hydro_mean <- mean(future$model_CA_hydro.c..1....47.1225303720345...2....46.1459620384744...3....47.4991395542952...1)

gfdl_esm4_ssp126_CA_solar_mean <- mean(future$model_CA_solar.c..1....5.62242389358436...2....8.73933917592614...3....10.3530850391603..)

gfdl_esm4_ssp585_CA_solar_mean <- mean(future$model_CA_solar.c..1....5.62242389358436...2....8.73933917592614...3....10.3530850391603...1)

ipsl_cm6a_lr_ssp126_CA_solar_mean <- mean(future$model_CA_solar.c..1....13.6496846427434...2....9.1207784084503...3....14.5541632364434..)

ipsl_cm6a_lr_ssp585_CA_solar_mean <- mean(future$model_CA_solar.c..1....13.6496846427434...2....9.1207784084503...3....14.5541632364434...1)

mpi_esm1_2_hr_ssp126_CA_solar_mean <- mean(future$model_CA_solar.c..1....8.00071401104046...2....8.01536946752824...3....18.3409079741164..)

mpi_esm1_2_hr_ssp585_CA_solar_mean <- mean(future$model_CA_solar.c..1....8.00071401104046...2....8.01536946752824...3....18.3409079741164...1)

mri_esm2_0_ssp126_CA_solar_mean <- mean(future$model_CA_solar.c..1....9.44516060707602...2....8.75308649665685...3....8.69778946373443..)

mri_esm2_0_ssp585_CA_solar_mean <- mean(future$model_CA_solar.c..1....9.44516060707602...2....8.75308649665685...3....8.69778946373443...1)

ukesm1_0_ll_ssp126_CA_solar_mean <- mean(future$model_CA_solar.c..1....10.508095315516...2....18.9783288541858...3....15.7529615828969..)

ukesm1_0_ll_ssp585_CA_solar_mean<- mean(future$model_CA_solar.c..1....10.508095315516...2....18.9783288541858...3....15.7529615828969...1)

gfdl_esm4_ssp126_CA_wind_mean <- mean(future$model_CA_wind.c..1....10.047464465374...2....15.6568653835754...3....12.0987858565097..)

gfdl_esm4_ssp585_CA_wind_mean <- mean(future$model_CA_wind.c..1....10.047464465374...2....15.6568653835754...3....12.0987858565097...1)

ipsl_cm6a_lr_ssp126_CA_wind_mean <- mean(future$model_CA_wind.c..1....10.6699424832778...2....9.04968350044599...3....17.232734774736..)

ipsl_cm6a_lr_ssp585_CA_wind_mean <- mean(future$model_CA_wind.c..1....10.6699424832778...2....9.04968350044599...3....17.232734774736...1)

mpi_esm1_2_hr_ssp126_CA_wind_mean <- mean(future$model_CA_wind.c..1....11.5735299478729...2....9.60120333129914...3....13.537847868794..)

mpi_esm1_2_hr_ssp585_CA_wind_mean <- mean(future$model_CA_wind.c..1....11.5735299478729...2....9.60120333129914...3....13.537847868794...1)

mri_esm2_0_ssp126_CA_wind_mean <- mean(future$model_CA_wind.c..1....11.785411598609...2....10.4008288574539...3....14.2044802123017..)

mri_esm2_0_ssp585_CA_wind_mean <- mean(future$model_CA_wind.c..1....11.785411598609...2....10.4008288574539...3....14.2044802123017...1)

ukesm1_0_ll_ssp126_CA_wind_mean <- mean(future$model_CA_wind.c..1....9.95483797853056...2....13.9491674974554...3....13.9437372276141..)

ukesm1_0_ll_ssp585_CA_wind_mean <- mean(future$model_CA_wind.c..1....9.95483797853056...2....13.9491674974554...3....13.9437372276141...1)


# Creating data frames for prediction results
# Hydroelectric
titles <- c("gfdl_esm4_ssp126_CA_hydro", "gfdl_esm4_ssp585_CA_hydro", "ipsl_cm6a_lr_ssp126_CA_hydro", "ipsl_cm6a_lr_ssp585_CA_hydro", "mpi_esm1_2_hr_ssp126_CA_hydro","mpi_esm1_2_hr_ssp585_CA_hydro",
            "mri_esm2_0_ssp126_CA_hydro", "mri_esm2_0_ssp585_CA_hydro","ukesm1_0_ll_ssp126_CA_hydro", "ukesm1_0_ll_ssp585_CA_hydro")

future_CA_hydro <- data.frame(
  Scenario = titles,
  Prediction = c(gfdl_esm4_ssp126_CA_hydro_mean, gfdl_esm4_ssp585_CA_hydro_mean, ipsl_cm6a_lr_ssp126_CA_hydro_mean, ipsl_cm6a_lr_ssp585_CA_hydro_mean, mpi_esm1_2_hr_ssp126_CA_hydro_mean,
                 mpi_esm1_2_hr_ssp585_CA_hydro_mean, mri_esm2_0_ssp126_CA_hydro_mean, mri_esm2_0_ssp585_CA_hydro_mean, ukesm1_0_ll_ssp126_CA_hydro_mean, ukesm1_0_ll_ssp585_CA_hydro_mean)
)
print(future_CA_hydro)

#Solar
titles <- c("gfdl_esm4_ssp126_CA_solar", "gfdl_esm4_ssp585_CA_solar", "ipsl_cm6a_lr_ssp126_CA_solar", "ipsl_cm6a_lr_ssp585_CA_solar", "mpi_esm1_2_hr_ssp126_CA_solar","mpi_esm1_2_hr_ssp585_CA_solar",
            "mri_esm2_0_ssp126_CA_solar", "mri_esm2_0_ssp585_CA_solar","ukesm1_0_ll_ssp126_CA_solar", "ukesm1_0_ll_ssp585_CA_solar")

future_CA_solar <- data.frame(
  Scenario = titles,
  Prediction = c(gfdl_esm4_ssp126_CA_solar_mean, gfdl_esm4_ssp585_CA_solar_mean, ipsl_cm6a_lr_ssp126_CA_solar_mean, ipsl_cm6a_lr_ssp585_CA_solar_mean, mpi_esm1_2_hr_ssp126_CA_solar_mean,
                 mpi_esm1_2_hr_ssp585_CA_solar_mean, mri_esm2_0_ssp126_CA_solar_mean, mri_esm2_0_ssp585_CA_solar_mean, ukesm1_0_ll_ssp126_CA_solar_mean, ukesm1_0_ll_ssp585_CA_solar_mean)
)
print(future_CA_solar)

#Wind
titles <- c("gfdl_esm4_ssp126_CA_wind", "gfdl_esm4_ssp585_CA_wind", "ipsl_cm6a_lr_ssp126_CA_wind", "ipsl_cm6a_lr_ssp585_CA_wind", "mpi_esm1_2_hr_ssp126_CA_wind","mpi_esm1_2_hr_ssp585_CA_wind",
            "mri_esm2_0_ssp126_CA_wind", "mri_esm2_0_ssp585_CA_wind","ukesm1_0_ll_ssp126_CA_wind", "ukesm1_0_ll_ssp585_CA_wind")

future_CA_wind <- data.frame(
  Scenario = titles,
  Prediction = c(gfdl_esm4_ssp126_CA_wind_mean, gfdl_esm4_ssp585_CA_wind_mean, ipsl_cm6a_lr_ssp126_CA_wind_mean, ipsl_cm6a_lr_ssp585_CA_wind_mean, mpi_esm1_2_hr_ssp126_CA_wind_mean,
                 mpi_esm1_2_hr_ssp585_CA_wind_mean, mri_esm2_0_ssp126_CA_wind_mean, mri_esm2_0_ssp585_CA_wind_mean, ukesm1_0_ll_ssp126_CA_wind_mean, ukesm1_0_ll_ssp585_CA_wind_mean)
)
print(future_CA_wind)



#Florida Results

gfdl_esm4_ssp126_FL_hydro_mean <- mean(future$model_FL_hydro.c..1....1.037143204051...2....1.10644623794212...3....1.10739759089575..)

gfdl_esm4_ssp585_FL_hydro_mean <- mean(future$model_FL_hydro.c..1....1.037143204051...2....1.10644623794212...3....1.10739759089575...1)

ipsl_cm6a_lr_ssp585_FL_hydro_mean <- mean(future_FL$model_FL_hydro.c..1....1.1277843782251...2....1.00467064954112...3....1.09634023795273..)

ipsl_cm6a_lr_ssp126_FL_hydro_mean <- mean(future$model_FL_hydro.c..1....1.1277843782251...2....1.00467064954112...3....1.09634023795273...1)

mpi_esm1_2_hr_ssp126_FL_hydro_mean <- mean(future$model_FL_hydro.c..1....1.16630053641878...2....1.19149757485573...3....1.21233434381287..)

mpi_esm1_2_hr_ssp585_FL_hydro_mean <- mean(future$model_FL_hydro.c..1....1.16630053641878...2....1.19149757485573...3....1.21233434381287...1)

mri_esm2_0_ssp126_FL_hydro_mean <- mean(future$model_FL_hydro.c..1....1.09718070455841...2....1.16947877764424...3....1.16206883649646..)

mri_esm2_0_ssp585_FL_hydro_mean <- mean(future$model_FL_hydro.c..1....1.09718070455841...2....1.16947877764424...3....1.16206883649646...1)

ukesm1_0_ll_ssp126_FL_hydro_mean <- mean(future$model_FL_hydro.c..1....1.10136153702534...2....1.05173516071977...3....1.10982751900638..)

ukesm1_0_ll_ssp585_FL_hydro_mean <- mean(future$model_FL_hydro.c..1....1.10136153702534...2....1.05173516071977...3....1.10982751900638...1)

#Solar

future_FL <- predictions_df[predictions_df$date >= "2040-01" & predictions_df$date <= "2051-12", ]


gfdl_esm4_ssp126_FL_solar_mean <- mean(future_FL$model_FL_solar.c..1....7.08270582248606...2....7.45271861822835...3....11.6071260691714..)

gfdl_esm4_ssp585_FL_solar_mean <- mean(future_FL$model_FL_solar.c..1....7.08270582248606...2....7.45271861822835...3....11.6071260691714...1)

ipsl_cm6a_lr_ssp126_FL_solar_mean <- mean(future_FL$model_FL_solar.c..1....7.63025276034013...2....9.95713484367658...3....9.82051319242905..)

ipsl_cm6a_lr_ssp585_FL_solar_mean <- mean(future_FL$model_FL_solar.c..1....7.63025276034013...2....9.95713484367658...3....9.82051319242905...1)

mpi_esm1_2_hr_ssp126_FL_solar_mean <- mean(future_FL$model_FL_solar.c..1....6.63289066220368...2....11.2881418777714...3....9.81591197879053..)

mpi_esm1_2_hr_ssp585_FL_solar_mean <- mean(future_FL$model_FL_solar.c..1....6.63289066220368...2....11.2881418777714...3....9.81591197879053...1)

mri_esm2_0_ssp126_FL_solar_mean <- mean(future_FL$model_FL_solar.c..1....7.47244866104792...2....6.95119471717215...3....6.91975974207416..)

mri_esm2_0_ssp585_FL_solar_mean <- mean(future_FL$model_FL_solar.c..1....7.47244866104792...2....6.95119471717215...3....6.91975974207416...1)

ukesm1_0_ll_ssp126_FL_solar_mean <- mean(future_FL$model_FL_solar.c..1....8.12504975035836...2....10.8768561992002...3....11.611798752919..)

ukesm1_0_ll_ssp585_FL_solar_mean <- mean(future_FL$model_FL_solar.c..1....8.12504975035836...2....10.8768561992002...3....11.611798752919...1)

# Creating data frames for prediction results
# Hydroelectric
titles <- c("gfdl_esm4_ssp126_FL_hydro", "gfdl_esm4_ssp585_FL_hydro", "ipsl_cm6a_lr_ssp126_FL_hydro", "ipsl_cm6a_lr_ssp585_FL_hydro", "mpi_esm1_2_hr_ssp126_FL_hydro","mpi_esm1_2_hr_ssp585_FL_hydro",
            "mri_esm2_0_ssp126_FL_hydro", "mri_esm2_0_ssp585_FL_hydro","ukesm1_0_ll_ssp126_FL_hydro", "ukesm1_0_ll_ssp585_FL_hydro")

future_FL_hydro <- data.frame(
  Scenario = titles,
  Prediction = c(gfdl_esm4_ssp126_FL_hydro_mean, gfdl_esm4_ssp585_FL_hydro_mean, ipsl_cm6a_lr_ssp126_FL_hydro_mean, ipsl_cm6a_lr_ssp585_FL_hydro_mean, mpi_esm1_2_hr_ssp126_FL_hydro_mean,
                 mpi_esm1_2_hr_ssp585_FL_hydro_mean, mri_esm2_0_ssp126_FL_hydro_mean, mri_esm2_0_ssp585_FL_hydro_mean, ukesm1_0_ll_ssp126_FL_hydro_mean, ukesm1_0_ll_ssp585_FL_hydro_mean)
)
print(future_FL_hydro)

#Solar
titles <- c("gfdl_esm4_ssp126_FL_solar", "gfdl_esm4_ssp585_FL_solar", "ipsl_cm6a_lr_ssp126_FL_solar", "ipsl_cm6a_lr_ssp585_FL_solar", "mpi_esm1_2_hr_ssp126_FL_solar","mpi_esm1_2_hr_ssp585_FL_solar",
            "mri_esm2_0_ssp126_FL_solar", "mri_esm2_0_ssp585_FL_solar","ukesm1_0_ll_ssp126_FL_solar", "ukesm1_0_ll_ssp585_FL_solar")

future_FL_solar <- data.frame(
  Scenario = titles,
  Prediction = c(gfdl_esm4_ssp126_FL_solar_mean, gfdl_esm4_ssp585_FL_solar_mean, ipsl_cm6a_lr_ssp126_FL_solar_mean, ipsl_cm6a_lr_ssp585_FL_solar_mean, mpi_esm1_2_hr_ssp126_FL_solar_mean,
                 mpi_esm1_2_hr_ssp585_FL_solar_mean, mri_esm2_0_ssp126_FL_solar_mean, mri_esm2_0_ssp585_FL_solar_mean, ukesm1_0_ll_ssp126_FL_solar_mean, ukesm1_0_ll_ssp585_FL_solar_mean)
)
print(future_FL_solar)

#Georgia Results

gfdl_esm4_ssp126_GA_hydro_mean <- mean(future$model_GA_hydro.c..1....32.7060809970714...2....35.4611890946338...3....39.7097744491182..)

gfdl_esm4_ssp585_GA_hydro_mean <- mean(future$model_GA_hydro.c..1....32.7060809970714...2....35.4611890946338...3....39.7097744491182...1)

ipsl_cm6a_lr_ssp585_GA_hydro_mean <- mean(future$model_GA_hydro.c..1....35.2145653934497...2....32.9703119987439...3....35.0051263125184..)

ipsl_cm6a_lr_ssp126_GA_hydro_mean <- mean(future$model_GA_hydro.c..1....35.2145653934497...2....32.9703119987439...3....35.0051263125184...1)

mpi_esm1_2_hr_ssp126_GA_hydro_mean <- mean(future$model_GA_hydro.c..1....37.4725908986836...2....36.110107331923...3....35.4044394228026..)

mpi_esm1_2_hr_ssp585_GA_hydro_mean <- mean(future$model_GA_hydro.c..1....37.4725908986836...2....36.110107331923...3....35.4044394228026...1)

mri_esm2_0_ssp126_GA_hydro_mean <- mean(future$model_GA_hydro.c..1....36.3934194245312...2....38.6197472687939...3....31.5384621791892..)

mri_esm2_0_ssp585_GA_hydro_mean <- mean(future$model_GA_hydro.c..1....36.3934194245312...2....38.6197472687939...3....31.5384621791892...1)

ukesm1_0_ll_ssp126_GA_hydro_mean <- mean(future$model_GA_hydro.c..1....34.5525201982353...2....35.5816694493585...3....36.928514423733..)

ukesm1_0_ll_ssp585_GA_hydro_mean <- mean(future$model_GA_hydro.c..1....34.5525201982353...2....35.5816694493585...3....36.928514423733...1)

#Solar
future_GA <- predictions_df[predictions_df$date >= "2042-01" & predictions_df$date <= "2051-12", ]

gfdl_esm4_ssp126_GA_solar_mean <- mean(future_GA$model_GA_solar.c..1....18.432193615314...2....20.7822859340486...3....33.6774234335268..)

gfdl_esm4_ssp585_GA_solar_mean <- mean(future_GA$model_GA_solar.c..1....18.432193615314...2....20.7822859340486...3....33.6774234335268...1)

ipsl_cm6a_lr_ssp126_GA_solar_mean <- mean(future_GA$model_GA_solar.c..1....12.3370515403035...2....18.2223344281539...3....32.3242298683883..)

ipsl_cm6a_lr_ssp585_GA_solar_mean <- mean(future_GA$model_GA_solar.c..1....12.3370515403035...2....18.2223344281539...3....32.3242298683883...1)

mpi_esm1_2_hr_ssp126_GA_solar_mean <- mean(future_GA$model_GA_solar.c..1....10.7982318000279...2....13.2767549630275...3....29.3751383364288..)

mpi_esm1_2_hr_ssp585_GA_solar_mean <- mean(future_GA$model_GA_solar.c..1....10.7982318000279...2....13.2767549630275...3....29.3751383364288...1)

mri_esm2_0_ssp126_GA_solar_mean <- mean(future_GA$model_GA_solar.c..1....9.08855637097832...2....17.609136534819...3....21.1254591309127..)

mri_esm2_0_ssp585_GA_solar_mean <- mean(future_GA$model_GA_solar.c..1....9.08855637097832...2....17.609136534819...3....21.1254591309127...1)

ukesm1_0_ll_ssp126_GA_solar_mean <- mean(future_GA$model_GA_solar.c..1....8.85819732368841...2....20.5803223266194...3....27.2958063794085..)

ukesm1_0_ll_ssp585_GA_solar_mean <- mean(future_GA$model_GA_solar.c..1....8.85819732368841...2....20.5803223266194...3....27.2958063794085...1)

# Creating data frames for prediction results
# Hydroelectric
titles <- c("gfdl_esm4_ssp126_GA_hydro", "gfdl_esm4_ssp585_GA_hydro", "ipsl_cm6a_lr_ssp126_GA_hydro", "ipsl_cm6a_lr_ssp585_GA_hydro", "mpi_esm1_2_hr_ssp126_GA_hydro","mpi_esm1_2_hr_ssp585_GA_hydro",
            "mri_esm2_0_ssp126_GA_hydro", "mri_esm2_0_ssp585_GA_hydro","ukesm1_0_ll_ssp126_GA_hydro", "ukesm1_0_ll_ssp585_GA_hydro")

future_GA_hydro <- data.frame(
  Scenario = titles,
  Prediction = c(gfdl_esm4_ssp126_GA_hydro_mean, gfdl_esm4_ssp585_GA_hydro_mean, ipsl_cm6a_lr_ssp126_GA_hydro_mean, ipsl_cm6a_lr_ssp585_GA_hydro_mean, mpi_esm1_2_hr_ssp126_GA_hydro_mean,
                 mpi_esm1_2_hr_ssp585_GA_hydro_mean, mri_esm2_0_ssp126_GA_hydro_mean, mri_esm2_0_ssp585_GA_hydro_mean, ukesm1_0_ll_ssp126_GA_hydro_mean, ukesm1_0_ll_ssp585_GA_hydro_mean)
)
print(future_GA_hydro)

#Solar
titles <- c("gfdl_esm4_ssp126_GA_solar", "gfdl_esm4_ssp585_GA_solar", "ipsl_cm6a_lr_ssp126_GA_solar", "ipsl_cm6a_lr_ssp585_GA_solar", "mpi_esm1_2_hr_ssp126_GA_solar","mpi_esm1_2_hr_ssp585_GA_solar",
            "mri_esm2_0_ssp126_GA_solar", "mri_esm2_0_ssp585_GA_solar","ukesm1_0_ll_ssp126_GA_solar", "ukesm1_0_ll_ssp585_GA_solar")

future_GA_solar <- data.frame(
  Scenario = titles,
  Prediction = c(gfdl_esm4_ssp126_GA_solar_mean, gfdl_esm4_ssp585_GA_solar_mean, ipsl_cm6a_lr_ssp126_GA_solar_mean, ipsl_cm6a_lr_ssp585_GA_solar_mean, mpi_esm1_2_hr_ssp126_GA_solar_mean,
                 mpi_esm1_2_hr_ssp585_GA_solar_mean, mri_esm2_0_ssp126_GA_solar_mean, mri_esm2_0_ssp585_GA_solar_mean, ukesm1_0_ll_ssp126_GA_solar_mean, ukesm1_0_ll_ssp585_GA_solar_mean)
)
print(future_GA_solar)


#New York Results

gfdl_esm4_ssp126_NY_hydro_mean <- mean(future$model_NY_hydro.c..1....118.502095035004...2....119.943683752703...3....120.20015503668..)

gfdl_esm4_ssp585_NY_hydro_mean <- mean(future$model_NY_hydro.c..1....118.502095035004...2....119.943683752703...3....120.20015503668...1)

ipsl_cm6a_lr_ssp126_NY_hydro_mean <- mean(future$model_NY_hydro.c..1....116.382872670918...2....118.660445081166...3....116.717662050271..)

ipsl_cm6a_lr_ssp585_NY_hydro_mean <- mean(future$model_NY_hydro.c..1....116.382872670918...2....118.660445081166...3....116.717662050271...1)

mpi_esm1_2_hr_ssp126_NY_hydro_mean <- mean(future$model_NY_hydro.c..1....113.332714624505...2....118.787330506921...3....116.554138855173..)

mpi_esm1_2_hr_ssp585_NY_hydro_mean <- mean(future$model_NY_hydro.c..1....113.332714624505...2....118.787330506921...3....116.554138855173...1)

mri_esm2_0_ssp126_NY_hydro_mean <- mean(future$model_NY_hydro.c..1....120.432639434581...2....120.695503479089...3....121.915149603391..)

mri_esm2_0_ssp585_NY_hydro_mean <- mean(future$model_NY_hydro.c..1....120.432639434581...2....120.695503479089...3....121.915149603391...1)

ukesm1_0_ll_ssp126_NY_hydro_mean <- mean(future$model_NY_hydro.c..1....117.781836572493...2....122.040958383782...3....118.948863877124..)

ukesm1_0_ll_ssp585_NY_hydro_mean <- mean(future$model_NY_hydro.c..1....117.781836572493...2....122.040958383782...3....118.948863877124...1)

#Solar
future_GA <- predictions_df[predictions_df$date >= "2042-01" & predictions_df$date <= "2051-12", ]

gfdl_esm4_ssp126_NY_solar_mean <- mean(future_GA$model_NY_solar.c..1....1.47022009373129...2....1.81998958511541...3....1.92129749089889..)

gfdl_esm4_ssp585_NY_solar_mean <- mean(future_GA$model_NY_solar.c..1....1.47022009373129...2....1.81998958511541...3....1.92129749089889...1)

ipsl_cm6a_lr_ssp126_NY_solar_mean <- mean(future_GA$model_NY_solar.c..1....1.66284761618979...2....1.75743912101142...3....1.757174776358..)

ipsl_cm6a_lr_ssp585_NY_solar_mean <- mean(future_GA$model_NY_solar.c..1....1.66284761618979...2....1.75743912101142...3....1.757174776358...1)

mpi_esm1_2_hr_ssp126_NY_solar_mean <- mean(future_GA$model_NY_solar.c..1....1.57115731092031...2....1.60852716515825...3....1.5648039383677..)

mpi_esm1_2_hr_ssp585_NY_solar_mean <- mean(future_GA$model_NY_solar.c..1....1.57115731092031...2....1.60852716515825...3....1.5648039383677...1)

mri_esm2_0_ssp126_NY_solar_mean <- mean(future_GA$model_NY_solar.c..1....1.22964638952271...2....1.65750819252978...3....1.91697102263458..)

mri_esm2_0_ssp585_NY_solar_mean <- mean(future_GA$model_NY_solar.c..1....1.22964638952271...2....1.65750819252978...3....1.91697102263458...1)

ukesm1_0_ll_ssp126_NY_solar_mean <- mean(future_GA$model_NY_solar.c..1....1.43642934478004...2....1.81115234544177...3....1.91125527835751..)

ukesm1_0_ll_ssp585_NY_solar_mean <- mean(future_GA$model_NY_solar.c..1....1.43642934478004...2....1.81115234544177...3....1.91125527835751...1)


#Wind
gfdl_esm4_ssp126_NY_wind_mean <- mean(future$model_NY_wind.c..1....9.79583459810618...2....10.3078248587467...3....9.32125917345974..)

gfdl_esm4_ssp585_NY_wind_mean <- mean(future$model_NY_wind.c..1....9.79583459810618...2....10.3078248587467...3....9.32125917345974...1)

ipsl_cm6a_lr_ssp126_NY_wind_mean <- mean(future$model_NY_wind.c..1....9.863532578271...2....13.4305590978724...3....9.93088104061013..)

ipsl_cm6a_lr_ssp585_NY_wind_mean <- mean(future$model_NY_wind.c..1....9.863532578271...2....13.4305590978724...3....9.93088104061013...1)

mpi_esm1_2_hr_ssp126_NY_wind_mean <- mean(future$model_NY_wind.c..1....11.4382649708818...2....9.88923344549994...3....10.1754947721102..)

mpi_esm1_2_hr_ssp585_NY_wind_mean <- mean(future$model_NY_wind.c..1....11.4382649708818...2....9.88923344549994...3....10.1754947721102...1)

mri_esm2_0_ssp126_NY_wind_mean <- mean(future$model_NY_wind.c..1....9.93054618529154...2....12.1142893192701...3....12.4760396357794..)

mri_esm2_0_ssp585_NY_wind_mean <- mean(future$model_NY_wind.c..1....9.93054618529154...2....12.1142893192701...3....12.4760396357794...1)

ukesm1_0_ll_ssp126_NY_wind_mean <- mean(future$model_NY_wind.c..1....9.74058654051555...2....10.0253947342467...3....10.6248577657507..) 

ukesm1_0_ll_ssp585_NY_wind_mean <- mean(future$model_NY_wind.c..1....9.74058654051555...2....10.0253947342467...3....10.6248577657507...1)

# Creating data frames for prediction results
# Hydroelectric
titles <- c("gfdl_esm4_ssp126_NY_hydro", "gfdl_esm4_ssp585_NY_hydro", "ipsl_cm6a_lr_ssp126_NY_hydro", "ipsl_cm6a_lr_ssp585_NY_hydro", "mpi_esm1_2_hr_ssp126_NY_hydro","mpi_esm1_2_hr_ssp585_NY_hydro",
            "mri_esm2_0_ssp126_NY_hydro", "mri_esm2_0_ssp585_NY_hydro","ukesm1_0_ll_ssp126_NY_hydro", "ukesm1_0_ll_ssp585_NY_hydro")

future_NY_hydro <- data.frame(
  Scenario = titles,
  Prediction = c(gfdl_esm4_ssp126_NY_hydro_mean, gfdl_esm4_ssp585_NY_hydro_mean, ipsl_cm6a_lr_ssp126_NY_hydro_mean, ipsl_cm6a_lr_ssp585_NY_hydro_mean, mpi_esm1_2_hr_ssp126_NY_hydro_mean,
                 mpi_esm1_2_hr_ssp585_NY_hydro_mean, mri_esm2_0_ssp126_NY_hydro_mean, mri_esm2_0_ssp585_NY_hydro_mean, ukesm1_0_ll_ssp126_NY_hydro_mean, ukesm1_0_ll_ssp585_NY_hydro_mean)
)
print(future_NY_hydro)

#Solar
titles <- c("gfdl_esm4_ssp126_NY_solar", "gfdl_esm4_ssp585_NY_solar", "ipsl_cm6a_lr_ssp126_NY_solar", "ipsl_cm6a_lr_ssp585_NY_solar", "mpi_esm1_2_hr_ssp126_NY_solar","mpi_esm1_2_hr_ssp585_NY_solar",
            "mri_esm2_0_ssp126_NY_solar", "mri_esm2_0_ssp585_NY_solar","ukesm1_0_ll_ssp126_NY_solar", "ukesm1_0_ll_ssp585_NY_solar")

future_NY_solar <- data.frame(
  Scenario = titles,
  Prediction = c(gfdl_esm4_ssp126_NY_solar_mean, gfdl_esm4_ssp585_NY_solar_mean, ipsl_cm6a_lr_ssp126_NY_solar_mean, ipsl_cm6a_lr_ssp585_NY_solar_mean, mpi_esm1_2_hr_ssp126_NY_solar_mean,
                 mpi_esm1_2_hr_ssp585_NY_solar_mean, mri_esm2_0_ssp126_NY_solar_mean, mri_esm2_0_ssp585_NY_solar_mean, ukesm1_0_ll_ssp126_NY_solar_mean, ukesm1_0_ll_ssp585_NY_solar_mean)
)
print(future_NY_solar)

#Wind
titles <- c("gfdl_esm4_ssp126_NY_wind", "gfdl_esm4_ssp585_NY_wind", "ipsl_cm6a_lr_ssp126_NY_wind", "ipsl_cm6a_lr_ssp585_NY_wind", "mpi_esm1_2_hr_ssp126_NY_wind","mpi_esm1_2_hr_ssp585_NY_wind",
            "mri_esm2_0_ssp126_NY_wind", "mri_esm2_0_ssp585_NY_wind","ukesm1_0_ll_ssp126_NY_wind", "ukesm1_0_ll_ssp585_NY_wind")

future_NY_wind <- data.frame(
  Scenario = titles,
  Prediction = c(gfdl_esm4_ssp126_NY_wind_mean, gfdl_esm4_ssp585_NY_wind_mean, ipsl_cm6a_lr_ssp126_NY_wind_mean, ipsl_cm6a_lr_ssp585_NY_wind_mean, mpi_esm1_2_hr_ssp126_NY_wind_mean,
                 mpi_esm1_2_hr_ssp585_NY_wind_mean, mri_esm2_0_ssp126_NY_wind_mean, mri_esm2_0_ssp585_NY_wind_mean, ukesm1_0_ll_ssp126_NY_wind_mean, ukesm1_0_ll_ssp585_NY_wind_mean)
)
print(future_NY_wind)

###################################################################################################################################################################

#PRESENT

gfdl_esm4_ssp126_CA_hydro_mean <- mean(present$model_CA_hydro.c..1....40.2200707630044...2....49.935081012511...3....41.6991993743741..)

gfdl_esm4_ssp585_CA_hydro_mean <- mean(present$model_CA_hydro.c..1....40.2200707630044...2....49.935081012511...3....41.6991993743741...1)

ipsl_cm6a_lr_ssp126_CA_hydro_mean <- mean(present$model_CA_hydro.c..1....43.6767493059273...2....39.8364782776058...3....46.0844176138097..)

ipsl_cm6a_lr_ssp585_CA_hydro_mean <- mean(present$model_CA_hydro.c..1....43.6767493059273...2....39.8364782776058...3....46.0844176138097...1)

mpi_esm1_2_hr_ssp126_CA_hydro_mean <- mean(present$model_CA_hydro.c..1....41.459795036014...2....41.1809313972826...3....48.4379164367442..)

mpi_esm1_2_hr_ssp585_CA_hydro_mean <- mean(present$model_CA_hydro.c..1....41.459795036014...2....41.1809313972826...3....48.4379164367442...1)

mri_esm2_0_ssp126_CA_hydro_mean <- mean(present$model_CA_hydro.c..1....41.8321530732576...2....44.7523393452629...3....42.2118139626244..)

mri_esm2_0_ssp585_CA_hydro_mean <- mean(present$model_CA_hydro.c..1....41.8321530732576...2....44.7523393452629...3....42.2118139626244...1)

ukesm1_0_ll_ssp126_CA_hydro_mean <- mean(present$model_CA_hydro.c..1....47.1225303720345...2....46.1459620384744...3....47.4991395542952..)

ukesm1_0_ll_ssp585_CA_hydro_mean <- mean(present$model_CA_hydro.c..1....47.1225303720345...2....46.1459620384744...3....47.4991395542952...1)

gfdl_esm4_ssp126_CA_solar_mean <- mean(present$model_CA_solar.c..1....5.62242389358436...2....8.73933917592614...3....10.3530850391603..)

gfdl_esm4_ssp585_CA_solar_mean <- mean(present$model_CA_solar.c..1....5.62242389358436...2....8.73933917592614...3....10.3530850391603...1)

ipsl_cm6a_lr_ssp126_CA_solar_mean <- mean(present$model_CA_solar.c..1....13.6496846427434...2....9.1207784084503...3....14.5541632364434..)

ipsl_cm6a_lr_ssp585_CA_solar_mean <- mean(present$model_CA_solar.c..1....13.6496846427434...2....9.1207784084503...3....14.5541632364434...1)

mpi_esm1_2_hr_ssp126_CA_solar_mean <- mean(present$model_CA_solar.c..1....8.00071401104046...2....8.01536946752824...3....18.3409079741164..)

mpi_esm1_2_hr_ssp585_CA_solar_mean <- mean(present$model_CA_solar.c..1....8.00071401104046...2....8.01536946752824...3....18.3409079741164...1)

mri_esm2_0_ssp126_CA_solar_mean <- mean(present$model_CA_solar.c..1....9.44516060707602...2....8.75308649665685...3....8.69778946373443..)

mri_esm2_0_ssp585_CA_solar_mean <- mean(present$model_CA_solar.c..1....9.44516060707602...2....8.75308649665685...3....8.69778946373443...1)

ukesm1_0_ll_ssp126_CA_solar_mean <- mean(present$model_CA_solar.c..1....10.508095315516...2....18.9783288541858...3....15.7529615828969..)

ukesm1_0_ll_ssp585_CA_solar_mean<- mean(present$model_CA_solar.c..1....10.508095315516...2....18.9783288541858...3....15.7529615828969...1)

gfdl_esm4_ssp126_CA_wind_mean <- mean(present$model_CA_wind.c..1....10.047464465374...2....15.6568653835754...3....12.0987858565097..)

gfdl_esm4_ssp585_CA_wind_mean <- mean(present$model_CA_wind.c..1....10.047464465374...2....15.6568653835754...3....12.0987858565097...1)

ipsl_cm6a_lr_ssp126_CA_wind_mean <- mean(present$model_CA_wind.c..1....10.6699424832778...2....9.04968350044599...3....17.232734774736..)

ipsl_cm6a_lr_ssp585_CA_wind_mean <- mean(present$model_CA_wind.c..1....10.6699424832778...2....9.04968350044599...3....17.232734774736...1)

mpi_esm1_2_hr_ssp126_CA_wind_mean <- mean(present$model_CA_wind.c..1....11.5735299478729...2....9.60120333129914...3....13.537847868794..)

mpi_esm1_2_hr_ssp585_CA_wind_mean <- mean(present$model_CA_wind.c..1....11.5735299478729...2....9.60120333129914...3....13.537847868794...1)

mri_esm2_0_ssp126_CA_wind_mean <- mean(present$model_CA_wind.c..1....11.785411598609...2....10.4008288574539...3....14.2044802123017..)

mri_esm2_0_ssp585_CA_wind_mean <- mean(present$model_CA_wind.c..1....11.785411598609...2....10.4008288574539...3....14.2044802123017...1)

ukesm1_0_ll_ssp126_CA_wind_mean <- mean(present$model_CA_wind.c..1....9.95483797853056...2....13.9491674974554...3....13.9437372276141..)

ukesm1_0_ll_ssp585_CA_wind_mean <- mean(present$model_CA_wind.c..1....9.95483797853056...2....13.9491674974554...3....13.9437372276141...1)


# Creating data frames for prediction results
# Hydroelectric
titles <- c("gfdl_esm4_ssp126_CA_hydro", "gfdl_esm4_ssp585_CA_hydro", "ipsl_cm6a_lr_ssp126_CA_hydro", "ipsl_cm6a_lr_ssp585_CA_hydro", "mpi_esm1_2_hr_ssp126_CA_hydro","mpi_esm1_2_hr_ssp585_CA_hydro",
            "mri_esm2_0_ssp126_CA_hydro", "mri_esm2_0_ssp585_CA_hydro","ukesm1_0_ll_ssp126_CA_hydro", "ukesm1_0_ll_ssp585_CA_hydro")

present_CA_hydro <- data.frame(
  Scenario = titles,
  Prediction = c(gfdl_esm4_ssp126_CA_hydro_mean, gfdl_esm4_ssp585_CA_hydro_mean, ipsl_cm6a_lr_ssp126_CA_hydro_mean, ipsl_cm6a_lr_ssp585_CA_hydro_mean, mpi_esm1_2_hr_ssp126_CA_hydro_mean,
                 mpi_esm1_2_hr_ssp585_CA_hydro_mean, mri_esm2_0_ssp126_CA_hydro_mean, mri_esm2_0_ssp585_CA_hydro_mean, ukesm1_0_ll_ssp126_CA_hydro_mean, ukesm1_0_ll_ssp585_CA_hydro_mean)
)
print(present_CA_hydro)

#Solar
titles <- c("gfdl_esm4_ssp126_CA_solar", "gfdl_esm4_ssp585_CA_solar", "ipsl_cm6a_lr_ssp126_CA_solar", "ipsl_cm6a_lr_ssp585_CA_solar", "mpi_esm1_2_hr_ssp126_CA_solar","mpi_esm1_2_hr_ssp585_CA_solar",
            "mri_esm2_0_ssp126_CA_solar", "mri_esm2_0_ssp585_CA_solar","ukesm1_0_ll_ssp126_CA_solar", "ukesm1_0_ll_ssp585_CA_solar")

present_CA_solar <- data.frame(
  Scenario = titles,
  Prediction = c(gfdl_esm4_ssp126_CA_solar_mean, gfdl_esm4_ssp585_CA_solar_mean, ipsl_cm6a_lr_ssp126_CA_solar_mean, ipsl_cm6a_lr_ssp585_CA_solar_mean, mpi_esm1_2_hr_ssp126_CA_solar_mean,
                 mpi_esm1_2_hr_ssp585_CA_solar_mean, mri_esm2_0_ssp126_CA_solar_mean, mri_esm2_0_ssp585_CA_solar_mean, ukesm1_0_ll_ssp126_CA_solar_mean, ukesm1_0_ll_ssp585_CA_solar_mean)
)
print(present_CA_solar)

#Wind
titles <- c("gfdl_esm4_ssp126_CA_wind", "gfdl_esm4_ssp585_CA_wind", "ipsl_cm6a_lr_ssp126_CA_wind", "ipsl_cm6a_lr_ssp585_CA_wind", "mpi_esm1_2_hr_ssp126_CA_wind","mpi_esm1_2_hr_ssp585_CA_wind",
            "mri_esm2_0_ssp126_CA_wind", "mri_esm2_0_ssp585_CA_wind","ukesm1_0_ll_ssp126_CA_wind", "ukesm1_0_ll_ssp585_CA_wind")

present_CA_wind <- data.frame(
  Scenario = titles,
  Prediction = c(gfdl_esm4_ssp126_CA_wind_mean, gfdl_esm4_ssp585_CA_wind_mean, ipsl_cm6a_lr_ssp126_CA_wind_mean, ipsl_cm6a_lr_ssp585_CA_wind_mean, mpi_esm1_2_hr_ssp126_CA_wind_mean,
                 mpi_esm1_2_hr_ssp585_CA_wind_mean, mri_esm2_0_ssp126_CA_wind_mean, mri_esm2_0_ssp585_CA_wind_mean, ukesm1_0_ll_ssp126_CA_wind_mean, ukesm1_0_ll_ssp585_CA_wind_mean)
)
print(present_CA_wind)

#Florida Results

gfdl_esm4_ssp126_FL_hydro_mean <- mean(present$model_FL_hydro.c..1....1.037143204051...2....1.10644623794212...3....1.10739759089575..)

gfdl_esm4_ssp585_FL_hydro_mean <- mean(present$model_FL_hydro.c..1....1.037143204051...2....1.10644623794212...3....1.10739759089575...1)

ipsl_cm6a_lr_ssp585_FL_hydro_mean <- mean(present$model_FL_hydro.c..1....1.1277843782251...2....1.00467064954112...3....1.09634023795273..)

ipsl_cm6a_lr_ssp126_FL_hydro_mean <- mean(present$model_FL_hydro.c..1....1.1277843782251...2....1.00467064954112...3....1.09634023795273...1)

mpi_esm1_2_hr_ssp126_FL_hydro_mean <- mean(present$model_FL_hydro.c..1....1.16630053641878...2....1.19149757485573...3....1.21233434381287..)

mpi_esm1_2_hr_ssp585_FL_hydro_mean <- mean(present$model_FL_hydro.c..1....1.16630053641878...2....1.19149757485573...3....1.21233434381287...1)

mri_esm2_0_ssp126_FL_hydro_mean <- mean(present$model_FL_hydro.c..1....1.09718070455841...2....1.16947877764424...3....1.16206883649646..)

mri_esm2_0_ssp585_FL_hydro_mean <- mean(present$model_FL_hydro.c..1....1.09718070455841...2....1.16947877764424...3....1.16206883649646...1)

ukesm1_0_ll_ssp126_FL_hydro_mean <- mean(present$model_FL_hydro.c..1....1.10136153702534...2....1.05173516071977...3....1.10982751900638..)

ukesm1_0_ll_ssp585_FL_hydro_mean <- mean(present$model_FL_hydro.c..1....1.10136153702534...2....1.05173516071977...3....1.10982751900638...1)

#Solar
present_FL <- predictions_df[predictions_df$date >= "2010-01" & predictions_df$date <= "2021-12", ]

gfdl_esm4_ssp126_FL_solar_mean <- mean(present_FL$model_FL_solar.c..1....7.08270582248606...2....7.45271861822835...3....11.6071260691714..)

gfdl_esm4_ssp585_FL_solar_mean <- mean(present_FL$model_FL_solar.c..1....7.08270582248606...2....7.45271861822835...3....11.6071260691714...1)

ipsl_cm6a_lr_ssp126_FL_solar_mean <- mean(present_FL$model_FL_solar.c..1....7.63025276034013...2....9.95713484367658...3....9.82051319242905..)

ipsl_cm6a_lr_ssp585_FL_solar_mean <- mean(present_FL$model_FL_solar.c..1....7.63025276034013...2....9.95713484367658...3....9.82051319242905...1)

mpi_esm1_2_hr_ssp126_FL_solar_mean <- mean(present_FL$model_FL_solar.c..1....6.63289066220368...2....11.2881418777714...3....9.81591197879053..)

mpi_esm1_2_hr_ssp585_FL_solar_mean <- mean(present_FL$model_FL_solar.c..1....6.63289066220368...2....11.2881418777714...3....9.81591197879053...1)

mri_esm2_0_ssp126_FL_solar_mean <- mean(present_FL$model_FL_solar.c..1....7.47244866104792...2....6.95119471717215...3....6.91975974207416..)

mri_esm2_0_ssp585_FL_solar_mean <- mean(present_FL$model_FL_solar.c..1....7.47244866104792...2....6.95119471717215...3....6.91975974207416...1)

ukesm1_0_ll_ssp126_FL_solar_mean <- mean(present_FL$model_FL_solar.c..1....8.12504975035836...2....10.8768561992002...3....11.611798752919..)

ukesm1_0_ll_ssp585_FL_solar_mean <- mean(present_FL$model_FL_solar.c..1....8.12504975035836...2....10.8768561992002...3....11.611798752919...1)

# Creating data frames for prediction results
# Hydroelectric
titles <- c("gfdl_esm4_ssp126_FL_hydro", "gfdl_esm4_ssp585_FL_hydro", "ipsl_cm6a_lr_ssp126_FL_hydro", "ipsl_cm6a_lr_ssp585_FL_hydro", "mpi_esm1_2_hr_ssp126_FL_hydro","mpi_esm1_2_hr_ssp585_FL_hydro",
            "mri_esm2_0_ssp126_FL_hydro", "mri_esm2_0_ssp585_FL_hydro","ukesm1_0_ll_ssp126_FL_hydro", "ukesm1_0_ll_ssp585_FL_hydro")

present_FL_hydro <- data.frame(
  Scenario = titles,
  Prediction = c(gfdl_esm4_ssp126_FL_hydro_mean, gfdl_esm4_ssp585_FL_hydro_mean, ipsl_cm6a_lr_ssp126_FL_hydro_mean, ipsl_cm6a_lr_ssp585_FL_hydro_mean, mpi_esm1_2_hr_ssp126_FL_hydro_mean,
                 mpi_esm1_2_hr_ssp585_FL_hydro_mean, mri_esm2_0_ssp126_FL_hydro_mean, mri_esm2_0_ssp585_FL_hydro_mean, ukesm1_0_ll_ssp126_FL_hydro_mean, ukesm1_0_ll_ssp585_FL_hydro_mean)
)
print(present_FL_hydro)

#Solar
write.csv(present_FL, "C:/Users/jaa6483/OneDrive - The Pennsylvania State University/Desktop/Class work/PROJECTS/Predictive_Analysis/Prediction\\Present_FL.csv", row.names=TRUE)
titles <- c("gfdl_esm4_ssp126_FL_solar", "gfdl_esm4_ssp585_FL_solar", "ipsl_cm6a_lr_ssp126_FL_solar", "ipsl_cm6a_lr_ssp585_FL_solar", "mpi_esm1_2_hr_ssp126_FL_solar","mpi_esm1_2_hr_ssp585_FL_solar",
            "mri_esm2_0_ssp126_FL_solar", "mri_esm2_0_ssp585_FL_solar","ukesm1_0_ll_ssp126_FL_solar", "ukesm1_0_ll_ssp585_FL_solar")

present_FL_solar <- data.frame(
  Scenario = titles,
  Prediction = c(gfdl_esm4_ssp126_FL_solar_mean, gfdl_esm4_ssp585_FL_solar_mean, ipsl_cm6a_lr_ssp126_FL_solar_mean, ipsl_cm6a_lr_ssp585_FL_solar_mean, mpi_esm1_2_hr_ssp126_FL_solar_mean,
                 mpi_esm1_2_hr_ssp585_FL_solar_mean, mri_esm2_0_ssp126_FL_solar_mean, mri_esm2_0_ssp585_FL_solar_mean, ukesm1_0_ll_ssp126_FL_solar_mean, ukesm1_0_ll_ssp585_FL_solar_mean)
)
print(present_FL_solar)


#Georgia
gfdl_esm4_ssp126_GA_hydro_mean <- mean(present$model_GA_hydro.c..1....32.7060809970714...2....35.4611890946338...3....39.7097744491182..)

gfdl_esm4_ssp585_GA_hydro_mean <- mean(present$model_GA_hydro.c..1....32.7060809970714...2....35.4611890946338...3....39.7097744491182...1)

ipsl_cm6a_lr_ssp585_GA_hydro_mean <- mean(present$model_GA_hydro.c..1....35.2145653934497...2....32.9703119987439...3....35.0051263125184..)

ipsl_cm6a_lr_ssp126_GA_hydro_mean <- mean(present$model_GA_hydro.c..1....35.2145653934497...2....32.9703119987439...3....35.0051263125184...1)

mpi_esm1_2_hr_ssp126_GA_hydro_mean <- mean(present$model_GA_hydro.c..1....37.4725908986836...2....36.110107331923...3....35.4044394228026..)

mpi_esm1_2_hr_ssp585_GA_hydro_mean <- mean(present$model_GA_hydro.c..1....37.4725908986836...2....36.110107331923...3....35.4044394228026...1)

mri_esm2_0_ssp126_GA_hydro_mean <- mean(present$model_GA_hydro.c..1....36.3934194245312...2....38.6197472687939...3....31.5384621791892..)

mri_esm2_0_ssp585_GA_hydro_mean <- mean(present$model_GA_hydro.c..1....36.3934194245312...2....38.6197472687939...3....31.5384621791892...1)

ukesm1_0_ll_ssp126_GA_hydro_mean <- mean(present$model_GA_hydro.c..1....34.5525201982353...2....35.5816694493585...3....36.928514423733..)

ukesm1_0_ll_ssp585_GA_hydro_mean <- mean(present$model_GA_hydro.c..1....34.5525201982353...2....35.5816694493585...3....36.928514423733...1)

#Solar
present_GA <- predictions_df[predictions_df$date >= "2012-01" & predictions_df$date <= "2021-12", ]

gfdl_esm4_ssp126_GA_solar_mean <- mean(present_GA$model_GA_solar.c..1....18.432193615314...2....20.7822859340486...3....33.6774234335268..)

gfdl_esm4_ssp585_GA_solar_mean <- mean(present_GA$model_GA_solar.c..1....18.432193615314...2....20.7822859340486...3....33.6774234335268...1)

ipsl_cm6a_lr_ssp126_GA_solar_mean <- mean(present_GA$model_GA_solar.c..1....12.3370515403035...2....18.2223344281539...3....32.3242298683883..)

ipsl_cm6a_lr_ssp585_GA_solar_mean <- mean(present_GA$model_GA_solar.c..1....12.3370515403035...2....18.2223344281539...3....32.3242298683883...1)

mpi_esm1_2_hr_ssp126_GA_solar_mean <- mean(present_GA$model_GA_solar.c..1....10.7982318000279...2....13.2767549630275...3....29.3751383364288..)

mpi_esm1_2_hr_ssp585_GA_solar_mean <- mean(present_GA$model_GA_solar.c..1....10.7982318000279...2....13.2767549630275...3....29.3751383364288...1)

mri_esm2_0_ssp126_GA_solar_mean <- mean(present_GA$model_GA_solar.c..1....9.08855637097832...2....17.609136534819...3....21.1254591309127..)

mri_esm2_0_ssp585_GA_solar_mean <- mean(present_GA$model_GA_solar.c..1....9.08855637097832...2....17.609136534819...3....21.1254591309127...1)

ukesm1_0_ll_ssp126_GA_solar_mean <- mean(present_GA$model_GA_solar.c..1....8.85819732368841...2....20.5803223266194...3....27.2958063794085..)

ukesm1_0_ll_ssp585_GA_solar_mean <- mean(present_GA$model_GA_solar.c..1....8.85819732368841...2....20.5803223266194...3....27.2958063794085...1)

# Creating data frames for prediction results
# Hydroelectric
titles <- c("gfdl_esm4_ssp126_GA_hydro", "gfdl_esm4_ssp585_GA_hydro", "ipsl_cm6a_lr_ssp126_GA_hydro", "ipsl_cm6a_lr_ssp585_GA_hydro", "mpi_esm1_2_hr_ssp126_GA_hydro","mpi_esm1_2_hr_ssp585_GA_hydro",
            "mri_esm2_0_ssp126_GA_hydro", "mri_esm2_0_ssp585_GA_hydro","ukesm1_0_ll_ssp126_GA_hydro", "ukesm1_0_ll_ssp585_GA_hydro")

present_GA_hydro <- data.frame(
  Scenario = titles,
  Prediction = c(gfdl_esm4_ssp126_GA_hydro_mean, gfdl_esm4_ssp585_GA_hydro_mean, ipsl_cm6a_lr_ssp126_GA_hydro_mean, ipsl_cm6a_lr_ssp585_GA_hydro_mean, mpi_esm1_2_hr_ssp126_GA_hydro_mean,
                 mpi_esm1_2_hr_ssp585_GA_hydro_mean, mri_esm2_0_ssp126_GA_hydro_mean, mri_esm2_0_ssp585_GA_hydro_mean, ukesm1_0_ll_ssp126_GA_hydro_mean, ukesm1_0_ll_ssp585_GA_hydro_mean)
)
print(present_GA_hydro)

#Solar
titles <- c("gfdl_esm4_ssp126_GA_solar", "gfdl_esm4_ssp585_GA_solar", "ipsl_cm6a_lr_ssp126_GA_solar", "ipsl_cm6a_lr_ssp585_GA_solar", "mpi_esm1_2_hr_ssp126_GA_solar","mpi_esm1_2_hr_ssp585_GA_solar",
            "mri_esm2_0_ssp126_GA_solar", "mri_esm2_0_ssp585_GA_solar","ukesm1_0_ll_ssp126_GA_solar", "ukesm1_0_ll_ssp585_GA_solar")

present_GA_solar <- data.frame(
  Scenario = titles,
  Prediction = c(gfdl_esm4_ssp126_GA_solar_mean, gfdl_esm4_ssp585_GA_solar_mean, ipsl_cm6a_lr_ssp126_GA_solar_mean, ipsl_cm6a_lr_ssp585_GA_solar_mean, mpi_esm1_2_hr_ssp126_GA_solar_mean,
                 mpi_esm1_2_hr_ssp585_GA_solar_mean, mri_esm2_0_ssp126_GA_solar_mean, mri_esm2_0_ssp585_GA_solar_mean, ukesm1_0_ll_ssp126_GA_solar_mean, ukesm1_0_ll_ssp585_GA_solar_mean)
)
print(present_GA_solar)


#New York
gfdl_esm4_ssp126_NY_hydro_mean <- mean(present$model_NY_hydro.c..1....118.502095035004...2....119.943683752703...3....120.20015503668..)

gfdl_esm4_ssp585_NY_hydro_mean <- mean(present$model_NY_hydro.c..1....118.502095035004...2....119.943683752703...3....120.20015503668...1)

ipsl_cm6a_lr_ssp126_NY_hydro_mean <- mean(present$model_NY_hydro.c..1....116.382872670918...2....118.660445081166...3....116.717662050271..)

ipsl_cm6a_lr_ssp585_NY_hydro_mean <- mean(present$model_NY_hydro.c..1....116.382872670918...2....118.660445081166...3....116.717662050271...1)

mpi_esm1_2_hr_ssp126_NY_hydro_mean <- mean(present$model_NY_hydro.c..1....113.332714624505...2....118.787330506921...3....116.554138855173..)

mpi_esm1_2_hr_ssp585_NY_hydro_mean <- mean(present$model_NY_hydro.c..1....113.332714624505...2....118.787330506921...3....116.554138855173...1)

mri_esm2_0_ssp126_NY_hydro_mean <- mean(present$model_NY_hydro.c..1....120.432639434581...2....120.695503479089...3....121.915149603391..)

mri_esm2_0_ssp585_NY_hydro_mean <- mean(present$model_NY_hydro.c..1....120.432639434581...2....120.695503479089...3....121.915149603391...1)

ukesm1_0_ll_ssp126_NY_hydro_mean <- mean(present$model_NY_hydro.c..1....117.781836572493...2....122.040958383782...3....118.948863877124..)

ukesm1_0_ll_ssp585_NY_hydro_mean <- mean(present$model_NY_hydro.c..1....117.781836572493...2....122.040958383782...3....118.948863877124...1)

#Solar
present_GA <- predictions_df[predictions_df$date >= "2012-01" & predictions_df$date <= "2021-12", ]

gfdl_esm4_ssp126_NY_solar_mean <- mean(present_GA$model_NY_solar.c..1....1.47022009373129...2....1.81998958511541...3....1.92129749089889..)

gfdl_esm4_ssp585_NY_solar_mean <- mean(present_GA$model_NY_solar.c..1....1.47022009373129...2....1.81998958511541...3....1.92129749089889...1)

ipsl_cm6a_lr_ssp126_NY_solar_mean <- mean(present_GA$model_NY_solar.c..1....1.66284761618979...2....1.75743912101142...3....1.757174776358..)

ipsl_cm6a_lr_ssp585_NY_solar_mean <- mean(present_GA$model_NY_solar.c..1....1.66284761618979...2....1.75743912101142...3....1.757174776358...1)

mpi_esm1_2_hr_ssp126_NY_solar_mean <- mean(present_GA$model_NY_solar.c..1....1.57115731092031...2....1.60852716515825...3....1.5648039383677..)

mpi_esm1_2_hr_ssp585_NY_solar_mean <- mean(present_GA$model_NY_solar.c..1....1.57115731092031...2....1.60852716515825...3....1.5648039383677...1)

mri_esm2_0_ssp126_NY_solar_mean <- mean(present_GA$model_NY_solar.c..1....1.22964638952271...2....1.65750819252978...3....1.91697102263458..)

mri_esm2_0_ssp585_NY_solar_mean <- mean(present_GA$model_NY_solar.c..1....1.22964638952271...2....1.65750819252978...3....1.91697102263458...1)

ukesm1_0_ll_ssp126_NY_solar_mean <- mean(present_GA$model_NY_solar.c..1....1.43642934478004...2....1.81115234544177...3....1.91125527835751..)

ukesm1_0_ll_ssp585_NY_solar_mean <- mean(present_GA$model_NY_solar.c..1....1.43642934478004...2....1.81115234544177...3....1.91125527835751...1)


#Wind
gfdl_esm4_ssp126_NY_wind_mean <- mean(present$model_NY_wind.c..1....9.79583459810618...2....10.3078248587467...3....9.32125917345974..)

gfdl_esm4_ssp585_NY_wind_mean <- mean(present$model_NY_wind.c..1....9.79583459810618...2....10.3078248587467...3....9.32125917345974...1)

ipsl_cm6a_lr_ssp126_NY_wind_mean <- mean(present$model_NY_wind.c..1....9.863532578271...2....13.4305590978724...3....9.93088104061013..)

ipsl_cm6a_lr_ssp585_NY_wind_mean <- mean(present$model_NY_wind.c..1....9.863532578271...2....13.4305590978724...3....9.93088104061013...1)

mpi_esm1_2_hr_ssp126_NY_wind_mean <- mean(present$model_NY_wind.c..1....11.4382649708818...2....9.88923344549994...3....10.1754947721102..)

mpi_esm1_2_hr_ssp585_NY_wind_mean <- mean(present$model_NY_wind.c..1....11.4382649708818...2....9.88923344549994...3....10.1754947721102...1)

mri_esm2_0_ssp126_NY_wind_mean <- mean(present$model_NY_wind.c..1....9.93054618529154...2....12.1142893192701...3....12.4760396357794..)

mri_esm2_0_ssp585_NY_wind_mean <- mean(present$model_NY_wind.c..1....9.93054618529154...2....12.1142893192701...3....12.4760396357794...1)

ukesm1_0_ll_ssp126_NY_wind_mean <- mean(present$model_NY_wind.c..1....9.74058654051555...2....10.0253947342467...3....10.6248577657507..) 

ukesm1_0_ll_ssp585_NY_wind_mean <- mean(present$model_NY_wind.c..1....9.74058654051555...2....10.0253947342467...3....10.6248577657507...1)

# Creating data frames for prediction results
# Hydroelectric
titles <- c("gfdl_esm4_ssp126_NY_hydro", "gfdl_esm4_ssp585_NY_hydro", "ipsl_cm6a_lr_ssp126_NY_hydro", "ipsl_cm6a_lr_ssp585_NY_hydro", "mpi_esm1_2_hr_ssp126_NY_hydro","mpi_esm1_2_hr_ssp585_NY_hydro",
            "mri_esm2_0_ssp126_NY_hydro", "mri_esm2_0_ssp585_NY_hydro","ukesm1_0_ll_ssp126_NY_hydro", "ukesm1_0_ll_ssp585_NY_hydro")

present_NY_hydro <- data.frame(
  Scenario = titles,
  Prediction = c(gfdl_esm4_ssp126_NY_hydro_mean, gfdl_esm4_ssp585_NY_hydro_mean, ipsl_cm6a_lr_ssp126_NY_hydro_mean, ipsl_cm6a_lr_ssp585_NY_hydro_mean, mpi_esm1_2_hr_ssp126_NY_hydro_mean,
                 mpi_esm1_2_hr_ssp585_NY_hydro_mean, mri_esm2_0_ssp126_NY_hydro_mean, mri_esm2_0_ssp585_NY_hydro_mean, ukesm1_0_ll_ssp126_NY_hydro_mean, ukesm1_0_ll_ssp585_NY_hydro_mean)
)
print(present_NY_hydro)


#Solar
titles <- c("gfdl_esm4_ssp126_NY_solar", "gfdl_esm4_ssp585_NY_solar", "ipsl_cm6a_lr_ssp126_NY_solar", "ipsl_cm6a_lr_ssp585_NY_solar", "mpi_esm1_2_hr_ssp126_NY_solar","mpi_esm1_2_hr_ssp585_NY_solar",
            "mri_esm2_0_ssp126_NY_solar", "mri_esm2_0_ssp585_NY_solar","ukesm1_0_ll_ssp126_NY_solar", "ukesm1_0_ll_ssp585_NY_solar")

present_NY_solar <- data.frame(
  Scenario = titles,
  Prediction = c(gfdl_esm4_ssp126_NY_solar_mean, gfdl_esm4_ssp585_NY_solar_mean, ipsl_cm6a_lr_ssp126_NY_solar_mean, ipsl_cm6a_lr_ssp585_NY_solar_mean, mpi_esm1_2_hr_ssp126_NY_solar_mean,
                 mpi_esm1_2_hr_ssp585_NY_solar_mean, mri_esm2_0_ssp126_NY_solar_mean, mri_esm2_0_ssp585_NY_solar_mean, ukesm1_0_ll_ssp126_NY_solar_mean, ukesm1_0_ll_ssp585_NY_solar_mean)
)
print(present_NY_solar)

#Wind
titles <- c("gfdl_esm4_ssp126_NY_wind", "gfdl_esm4_ssp585_NY_wind", "ipsl_cm6a_lr_ssp126_NY_wind", "ipsl_cm6a_lr_ssp585_NY_wind", "mpi_esm1_2_hr_ssp126_NY_wind","mpi_esm1_2_hr_ssp585_NY_wind",
            "mri_esm2_0_ssp126_NY_wind", "mri_esm2_0_ssp585_NY_wind","ukesm1_0_ll_ssp126_NY_wind", "ukesm1_0_ll_ssp585_NY_wind")

present_NY_wind <- data.frame(
  Scenario = titles,
  Prediction = c(gfdl_esm4_ssp126_NY_wind_mean, gfdl_esm4_ssp585_NY_wind_mean, ipsl_cm6a_lr_ssp126_NY_wind_mean, ipsl_cm6a_lr_ssp585_NY_wind_mean, mpi_esm1_2_hr_ssp126_NY_wind_mean,
                 mpi_esm1_2_hr_ssp585_NY_wind_mean, mri_esm2_0_ssp126_NY_wind_mean, mri_esm2_0_ssp585_NY_wind_mean, ukesm1_0_ll_ssp126_NY_wind_mean, ukesm1_0_ll_ssp585_NY_wind_mean)
)
print(present_NY_wind)

###################################################################################################################################################################
#Plots for results

#California
#Hydroelectric


CA <- read_csv("C:/Users/jaa6483/OneDrive - The Pennsylvania State University/Desktop/Class work/PROJECTS/Predictive_Analysis/Prediction/CSV_results/Hydro_CA.csv")
FL <- read_csv("C:/Users/jaa6483/OneDrive - The Pennsylvania State University/Desktop/Class work/PROJECTS/Predictive_Analysis/Prediction/CSV_results/FL.csv")
GA <- read_csv("C:/Users/jaa6483/OneDrive - The Pennsylvania State University/Desktop/Class work/PROJECTS/Predictive_Analysis/Prediction/CSV_results/GA.csv")
NY <-  read_csv("C:/Users/jaa6483/OneDrive - The Pennsylvania State University/Desktop/Class work/PROJECTS/Predictive_Analysis/Prediction/CSV_results/NY.csv")

#California
#boxplot(CA, ylab ="Relative Change (%)",las = 2, col = c("grey","palevioletred1","dark green","violet","orange","yellow"),
#at = c(1,2,3,4,5,6), par(mar = c(12, 5, 4, 2) + 0.05))
# Setting graphical parameters
par(mar = c(12, 5, 4, 2) + 0.1,  # Adjusting margins
    pin = c(6, 4))  # Making the plot area 6x4 inches
# Creating the boxplot without x-axis labels
boxplot(CA, 
        ylab ="Relative Change (%)",
        las = 1, 
        col = c("grey", "palevioletred1", "dark green", "violet", "orange", "yellow"),
        at = c(1,2,3,4,5,6),
        xaxt = 'n'  # Do not draw the x-axis
)
# Adding a legend
legend("topright",      # Position of the legend
       inset = 0.05,    # Margin between plot and legend
       legend = names(CA),  # Names of the groups
       fill = c("grey", "palevioletred1", "dark green", "violet", "orange", "yellow"),
       cex = 0.6)        # Font size of the legend text

#Florida
#boxplot(FL, ylab ="Relative Change (%)",las = 2, col = c("grey","palevioletred1","dark green","violet"),
#at = c(1,2,3,4), par(mar = c(12, 5, 4, 2) + 0.1))

par(mar = c(12, 5, 4, 2) + 0.1,  # Adjusting margins
    pin = c(4, 2))  # Making the plot area 6x4 inches
# Creating the boxplot without x-axis labels
boxplot(FL, 
        ylab ="Relative Change (%)",
        las = 1, 
        col = c("grey", "palevioletred1", "dark green", "violet"),
        at = c(1,2,3,4),
        xaxt = 'n'  # Do not draw the x-axis
)
# Adding a legend
legend("topleft",      # Position of the legend
       inset = 0.05,    # Margin between plot and legend
       legend = names(FL),  # Names of the groups
       fill = c("grey", "palevioletred1", "dark green", "violet", "orange", "yellow"),
       cex = 0.6)        # Font size of the legend text

#Georgia
#boxplot(GA, ylab ="Relative Change (%)",las = 2, col = c("grey","palevioletred1","dark green","violet"),
#at = c(1,2,3,4), par(mar = c(12, 5, 4, 2) + 0.1))

par(mar = c(12, 5, 4, 2) + 0.1,  # Adjusting margins
    pin = c(4, 2))  # Making the plot area 6x4 inches
# Creating the boxplot without x-axis labels
boxplot(GA, 
        ylab ="Relative Change (%)",
        las = 1, 
        col = c("grey", "palevioletred1", "dark green", "violet"),
        at = c(1,2,3,4),
        xaxt = 'n'  # Do not draw the x-axis
)
# Adding a legend
legend("topleft",      # Position of the legend
       inset = 0.07,    # Margin between plot and legend
       legend = names(GA),  # Names of the groups
       fill = c("grey", "palevioletred1", "dark green", "violet", "orange", "yellow"),
       cex = 0.6)        # Font size of the legend text


#New York
#boxplot(NY, ylab ="Relative Change (%)",las = 2, col = c("grey","palevioletred1","dark green","violet","orange","yellow"),
#at = c(1,2,3,4,5,6), par(mar = c(12, 5, 4, 2) + 0.1))

# Setting graphical parameters
par(mar = c(12, 5, 4, 2) + 0.1,  # Adjusting margins
    pin = c(6, 4))  # Making the plot area 6x4 inches
# Creating the boxplot without x-axis labels
boxplot(NY, 
        ylab ="Relative Change (%)",
        las = 1, 
        col = c("grey", "palevioletred1", "dark green", "violet", "orange", "yellow"),
        at = c(1,2,3,4,5,6),
        xaxt = 'n'  # Do not draw the x-axis
)
# Adding a legend
legend("topright",      # Position of the legend
       inset = 0.05,    # Margin between plot and legend
       legend = names(NY),  # Names of the groups
       fill = c("grey", "palevioletred1", "dark green", "violet", "orange", "yellow"),
       cex = 0.6)        # Font size of the legend text

#change in summer and winter per scenario w/ error bars
#California
CA_seasons <- read_csv("C:/Users/jaa6483/OneDrive - The Pennsylvania State University/Desktop/Class work/PROJECTS/Predictive_Analysis/Prediction/CSV_results/Seasons_CA.csv")
# Setting graphical parameters
par(mar = c(12, 5, 4, 2) + 0.1,  # Adjusting margins
    pin = c(4, 2))  # Making the plot area 6x4 inches
# Creating the boxplot without x-axis labels
boxplot(CA_seasons, 
        ylab ="Relative Change (%)",
        las = 1, 
        col = c("grey", "palevioletred1", "dark green", "violet"),
        at = c(1,2,3,4),
        xaxt = 'n'  # Do not draw the x-axis
)
# Adding a legend
legend("topright",      # Position of the legend
       inset = 0.07,    # Margin between plot and legend
       legend = names(CA_seasons),  # Names of the groups
       fill = c("grey", "palevioletred1", "dark green", "violet", "orange", "yellow"),
       cex = 0.6)        # Font size of the legend text


#New York
NY_seasons <- read_csv("C:/Users/jaa6483/OneDrive - The Pennsylvania State University/Desktop/Class work/PROJECTS/Predictive_Analysis/Prediction/CSV_results/Seasons_NY.csv")
# Setting graphical parameters
par(mar = c(12, 5, 4, 2) + 0.1,  # Adjusting margins
    pin = c(4, 2))  # Making the plot area 6x4 inches
# Creating the boxplot without x-axis labels
boxplot(NY_seasons, 
        ylab ="Relative Change (%)",
        las = 1, 
        col = c("grey", "palevioletred1", "dark green", "violet"),
        at = c(1,2,3,4),
        xaxt = 'n'  # Do not draw the x-axis
)
# Adding a legend
legend("topright",      # Position of the legend
       inset = 0.07,    # Margin between plot and legend
       legend = names(NY_seasons),  # Names of the groups
       fill = c("grey", "palevioletred1", "dark green", "violet", "orange", "yellow"),
       cex = 0.6)        # Font size of the legend text


#Florida
FL_seasons <- read_csv("C:/Users/jaa6483/OneDrive - The Pennsylvania State University/Desktop/Class work/PROJECTS/Predictive_Analysis/Prediction/CSV_results/Seasons_FL.csv")
# Setting graphical parameters
par(mar = c(12, 5, 4, 2) + 0.1,  # Adjusting margins
    pin = c(4, 2))  # Making the plot area 6x4 inches
# Creating the boxplot without x-axis labels
boxplot(FL_seasons, 
        ylab ="Relative Change (%)",
        las = 1, 
        col = c("grey", "palevioletred1", "dark green", "violet"),
        at = c(1,2,3,4),
        xaxt = 'n'  # Do not draw the x-axis
)
# Adding a legend
legend("topleft",      # Position of the legend
       inset = 0.07,    # Margin between plot and legend
       legend = names(FL_seasons),  # Names of the groups
       fill = c("grey", "palevioletred1", "dark green", "violet", "orange", "yellow"),
       cex = 0.6)        # Font size of the legend text

#Georgia
GA_seasons <- read_csv("C:/Users/jaa6483/OneDrive - The Pennsylvania State University/Desktop/Class work/PROJECTS/Predictive_Analysis/Prediction/CSV_results/Seasons_GA.csv")
# Setting graphical parameters
par(mar = c(12, 5, 4, 2) + 0.1,  # Adjusting margins
    pin = c(4, 2))  # Making the plot area 6x4 inches
# Creating the boxplot without x-axis labels
boxplot(GA_seasons, 
        ylab ="Relative Change (%)",
        las = 1, 
        col = c("grey", "palevioletred1", "dark green", "violet"),
        at = c(1,2,3,4),
        xaxt = 'n'  # Do not draw the x-axis
)
# Adding a legend
legend("topright",      # Position of the legend
       inset = 0.07,    # Margin between plot and legend
       legend = names(GA_seasons),  # Names of the groups
       fill = c("grey", "palevioletred1", "dark green", "violet", "orange", "yellow"),
       cex = 0.6)        # Font size of the legend text


# Changes in precipitation form the cmip6 models for CA and GA
#ipsl_cm6a_lr_ssp585
ipsl_cm6a_lr_ssp585 <- read_csv("C:/Users/jaa6483/OneDrive - The Pennsylvania State University/Desktop/Class work/PROJECTS/Predictive_Analysis/Prediction/Prediction/CMIP6_cvs/ipsl_cm6a_lr_ssp585_daily_pr.csv")
ipsl_cm6a_lr_ssp585$date <- as.Date(paste(ipsl_cm6a_lr_ssp585$YYYY, ipsl_cm6a_lr_ssp585$MM, ipsl_cm6a_lr_ssp585$DD, sep = "-"), format = "%Y-%m-%d")
filtered1_ipsl_cm6a_lr_ssp585 <- subset(ipsl_cm6a_lr_ssp585, date >= as.Date("2001-01-01") & date <= as.Date("2022-12-31"))
mean(filtered1_ipsl_cm6a_lr_ssp585$ID_06)
mean(filtered1_ipsl_cm6a_lr_ssp585$ID_13)

filtered2_ipsl_cm6a_lr_ssp585 <- subset(ipsl_cm6a_lr_ssp585, date >= as.Date("2040-01-01") & date <= as.Date("2060-12-31"))
mean(filtered2_ipsl_cm6a_lr_ssp585$ID_06)
mean(filtered2_ipsl_cm6a_lr_ssp585$ID_13)

#ipsl_cm6a_lr_ssp126
ipsl_cm6a_lr_ssp126 <- read_csv("C:/Users/jaa6483/OneDrive - The Pennsylvania State University/Desktop/Class work/PROJECTS/Predictive_Analysis/Prediction/Prediction/CMIP6_cvs/ipsl_cm6a_lr_ssp126_daily_pr.csv")
ipsl_cm6a_lr_ssp126$date <- as.Date(paste(ipsl_cm6a_lr_ssp126$YYYY, ipsl_cm6a_lr_ssp126$MM, ipsl_cm6a_lr_ssp126$DD, sep = "-"), format = "%Y-%m-%d")
filtered1_ipsl_cm6a_lr_ssp126 <- subset(ipsl_cm6a_lr_ssp126, date >= as.Date("2001-01-01") & date <= as.Date("2022-12-31"))
mean(filtered1_ipsl_cm6a_lr_ssp126$ID_06)
mean(filtered1_ipsl_cm6a_lr_ssp126$ID_13)

filtered2_ipsl_cm6a_lr_ssp126 <- subset(ipsl_cm6a_lr_ssp126, date >= as.Date("2040-01-01") & date <= as.Date("2060-12-31"))
mean(filtered2_ipsl_cm6a_lr_ssp126$ID_06)
mean(filtered2_ipsl_cm6a_lr_ssp126$ID_13)

#ukesm1_0_ll_ssp585
ukesm1_0_ll_ssp585 <- read_csv("C:/Users/jaa6483/OneDrive - The Pennsylvania State University/Desktop/Class work/PROJECTS/Predictive_Analysis/Prediction/Prediction/CMIP6_cvs/ukesm1_0_ll_ssp585_daily_pr.csv")
ukesm1_0_ll_ssp585$date <- as.Date(paste(ukesm1_0_ll_ssp585$YYYY, ukesm1_0_ll_ssp585$MM, ukesm1_0_ll_ssp585$DD, sep = "-"), format = "%Y-%m-%d")
filtered1_ukesm1_0_ll_ssp585 <- subset(ukesm1_0_ll_ssp585, date >= as.Date("2001-01-01") & date <= as.Date("2022-12-31"))
mean(filtered1_ukesm1_0_ll_ssp585$ID_06)
mean(filtered1_ukesm1_0_ll_ssp585$ID_13)

filtered2_ukesm1_0_ll_ssp585 <- subset(ukesm1_0_ll_ssp585, date >= as.Date("2040-01-01") & date <= as.Date("2060-12-31"))
mean(filtered2_ukesm1_0_ll_ssp585$ID_06)
mean(filtered2_ukesm1_0_ll_ssp585$ID_13)

#ukesm1_0_ll_ssp126
ukesm1_0_ll_ssp126 <- read_csv("C:/Users/jaa6483/OneDrive - The Pennsylvania State University/Desktop/Class work/PROJECTS/Predictive_Analysis/Prediction/Prediction/CMIP6_cvs/ukesm1_0_ll_ssp126_daily_pr.csv")
ukesm1_0_ll_ssp126$date <- as.Date(paste(ukesm1_0_ll_ssp126$YYYY, ukesm1_0_ll_ssp126$MM, ukesm1_0_ll_ssp126$DD, sep = "-"), format = "%Y-%m-%d")
filtered1_ukesm1_0_ll_ssp126 <- subset(ukesm1_0_ll_ssp126, date >= as.Date("2001-01-01") & date <= as.Date("2022-12-31"))
mean(filtered1_ukesm1_0_ll_ssp126$ID_06)
mean(filtered1_ukesm1_0_ll_ssp126$ID_13)

filtered2_ukesm1_0_ll_ssp126 <- subset(ukesm1_0_ll_ssp126, date >= as.Date("2040-01-01") & date <= as.Date("2060-12-31"))
mean(filtered2_ukesm1_0_ll_ssp126$ID_06)
mean(filtered2_ukesm1_0_ll_ssp126$ID_13)

#gfdl_esm4_ssp585
gfdl_esm4_ssp585 <- read_csv("C:/Users/jaa6483/OneDrive - The Pennsylvania State University/Desktop/Class work/PROJECTS/Predictive_Analysis/Prediction/Prediction/CMIP6_cvs/gfdl_esm4_ssp585_daily_pr.csv")
gfdl_esm4_ssp585$date <- as.Date(paste(gfdl_esm4_ssp585$YYYY, gfdl_esm4_ssp585$MM, gfdl_esm4_ssp585$DD, sep = "-"), format = "%Y-%m-%d")
filtered1_gfdl_esm4_ssp585 <- subset(gfdl_esm4_ssp585, date >= as.Date("2001-01-01") & date <= as.Date("2022-12-31"))
mean(filtered1_gfdl_esm4_ssp585$ID_06)
mean(filtered1_gfdl_esm4_ssp585$ID_13)

filtered2_gfdl_esm4_ssp585 <- subset(gfdl_esm4_ssp585, date >= as.Date("2040-01-01") & date <= as.Date("2060-12-31"))
mean(filtered2_gfdl_esm4_ssp585$ID_06)
mean(filtered2_gfdl_esm4_ssp585$ID_13)


#gfdl_esm4_ssp126
gfdl_esm4_ssp126 <- read_csv("C:/Users/jaa6483/OneDrive - The Pennsylvania State University/Desktop/Class work/PROJECTS/Predictive_Analysis/Prediction/Prediction/CMIP6_cvs/gfdl_esm4_ssp126_daily_pr.csv")
gfdl_esm4_ssp126$date <- as.Date(paste(gfdl_esm4_ssp126$YYYY, gfdl_esm4_ssp126$MM, gfdl_esm4_ssp126$DD, sep = "-"), format = "%Y-%m-%d")
filtered1_gfdl_esm4_ssp126 <- subset(gfdl_esm4_ssp126, date >= as.Date("2001-01-01") & date <= as.Date("2022-12-31"))
mean(filtered1_gfdl_esm4_ssp126$ID_06)
mean(filtered1_gfdl_esm4_ssp126$ID_13)

filtered2_gfdl_esm4_ssp126 <- subset(gfdl_esm4_ssp126, date >= as.Date("2040-01-01") & date <= as.Date("2060-12-31"))
mean(filtered2_gfdl_esm4_ssp126$ID_06)
mean(filtered2_gfdl_esm4_ssp126$ID_13)

#mri_esm2_0_ssp585
mri_esm2_0_ssp585 <- read_csv("C:/Users/jaa6483/OneDrive - The Pennsylvania State University/Desktop/Class work/PROJECTS/Predictive_Analysis/Prediction/Prediction/CMIP6_cvs/mri_esm2_0_ssp585_daily_pr.csv")
mri_esm2_0_ssp585$date <- as.Date(paste(mri_esm2_0_ssp585$YYYY, mri_esm2_0_ssp585$MM, mri_esm2_0_ssp585$DD, sep = "-"), format = "%Y-%m-%d")
filtered1_mri_esm2_0_ssp585 <- subset(mri_esm2_0_ssp585, date >= as.Date("2001-01-01") & date <= as.Date("2022-12-31"))
mean(filtered1_mri_esm2_0_ssp585$ID_06)
mean(filtered1_mri_esm2_0_ssp585$ID_13)

filtered2_mri_esm2_0_ssp585 <- subset(mri_esm2_0_ssp585, date >= as.Date("2040-01-01") & date <= as.Date("2060-12-31"))
mean(filtered2_mri_esm2_0_ssp585$ID_06)
mean(filtered2_mri_esm2_0_ssp585$ID_13)

#mri_esm2_0_ssp126
mri_esm2_0_ssp126 <- read_csv("C:/Users/jaa6483/OneDrive - The Pennsylvania State University/Desktop/Class work/PROJECTS/Predictive_Analysis/Prediction/Prediction/CMIP6_cvs/mri_esm2_0_ssp126_daily_pr.csv")
mri_esm2_0_ssp126$date <- as.Date(paste(mri_esm2_0_ssp126$YYYY, mri_esm2_0_ssp126$MM, mri_esm2_0_ssp126$DD, sep = "-"), format = "%Y-%m-%d")
filtered1_mri_esm2_0_ssp126 <- subset(mri_esm2_0_ssp126, date >= as.Date("2001-01-01") & date <= as.Date("2022-12-31"))
mean(filtered1_mri_esm2_0_ssp126$ID_06)
mean(filtered1_mri_esm2_0_ssp126$ID_13)

filtered2_mri_esm2_0_ssp126 <- subset(mri_esm2_0_ssp126, date >= as.Date("2040-01-01") & date <= as.Date("2060-12-31"))
mean(filtered2_mri_esm2_0_ssp126$ID_06)
mean(filtered2_mri_esm2_0_ssp126$ID_13)

#mpi_esm1_2_hr_ssp585
mpi_esm1_2_hr_ssp585 <- read_csv("C:/Users/jaa6483/OneDrive - The Pennsylvania State University/Desktop/Class work/PROJECTS/Predictive_Analysis/Prediction/Prediction/CMIP6_cvs/mpi_esm1_2_hr_ssp585_daily_pr.csv")
mpi_esm1_2_hr_ssp585$date <- as.Date(paste(mpi_esm1_2_hr_ssp585$YYYY, mpi_esm1_2_hr_ssp585$MM, mpi_esm1_2_hr_ssp585$DD, sep = "-"), format = "%Y-%m-%d")
filtered1_mpi_esm1_2_hr_ssp585 <- subset(mpi_esm1_2_hr_ssp585, date >= as.Date("2001-01-01") & date <= as.Date("2022-12-31"))
mean(filtered1_mpi_esm1_2_hr_ssp585$ID_06)
mean(filtered1_mpi_esm1_2_hr_ssp585$ID_13)

filtered2_mpi_esm1_2_hr_ssp585 <- subset(mpi_esm1_2_hr_ssp585, date >= as.Date("2040-01-01") & date <= as.Date("2060-12-31"))
mean(filtered2_mpi_esm1_2_hr_ssp585$ID_06)
mean(filtered2_mpi_esm1_2_hr_ssp585$ID_13)

#mpi_esm1_2_hr_ssp126
mpi_esm1_2_hr_ssp126 <- read_csv("C:/Users/jaa6483/OneDrive - The Pennsylvania State University/Desktop/Class work/PROJECTS/Predictive_Analysis/Prediction/Prediction/CMIP6_cvs/mpi_esm1_2_hr_ssp126_daily_pr.csv")
mpi_esm1_2_hr_ssp126$date <- as.Date(paste(mpi_esm1_2_hr_ssp126$YYYY, mpi_esm1_2_hr_ssp126$MM, mpi_esm1_2_hr_ssp126$DD, sep = "-"), format = "%Y-%m-%d")
filtered1_mpi_esm1_2_hr_ssp126 <- subset(mpi_esm1_2_hr_ssp126, date >= as.Date("2001-01-01") & date <= as.Date("2022-12-31"))
mean(filtered1_mpi_esm1_2_hr_ssp126$ID_06)
mean(filtered1_mpi_esm1_2_hr_ssp126$ID_13)

filtered2_mpi_esm1_2_hr_ssp126 <- subset(mpi_esm1_2_hr_ssp126, date >= as.Date("2040-01-01") & date <= as.Date("2060-12-31"))
mean(filtered2_mpi_esm1_2_hr_ssp126$ID_06)
mean(filtered2_mpi_esm1_2_hr_ssp126$ID_13)

#California Time Series for Hydroelectric generation
#FUTURE
#SUMMER
#Add date column
# Create a sequence of dates starting from a specific date
future <- read_csv("C:/Users/jaa6483/OneDrive - The Pennsylvania State University/Desktop/Class work/PROJECTS/Predictive_Analysis/Prediction/Results/Future_pred.csv")
start_date <- as.Date("2040-01-01")
number_of_rows <- nrow(future)
dates <- seq.Date(from = start_date, by = "month", length.out = number_of_rows)

# Format the dates to keep only year and month
formatted_dates <- format(dates, "%Y-%m-%d")

# Add the formatted dates as a new column
future$date <- formatted_dates
#Extracting 2040-2060)

# Define summer months (replace with your desired months)
summer_months <- c(6, 7, 8, 9)  # June, July, August

# Convert 'date' column to a date object (assuming year and month format)
future$date <- ymd(future$date)

# Filter data for summer months using month function within square brackets

# Filter data for the years 2040-2060 during summer months
summer_data <- future[month(future$date) %in% summer_months & 
                        year(future$date) >= 2040 & 
                        year(future$date) <= 2060, ]

write.csv(summer_data, "C:/Users/jaa6483/OneDrive - The Pennsylvania State University/Desktop/Class work/PROJECTS/Predictive_Analysis/Prediction\\summer_future_revised.csv", row.names=TRUE)

#INTERMEDIATE

#Add date column
# Create a sequence of dates starting from a specific date
future <- read_csv("C:/Users/jaa6483/OneDrive - The Pennsylvania State University/Desktop/Class work/PROJECTS/Predictive_Analysis/Prediction/Results/Future_pred.csv")
start_date <- as.Date("2040-01-01")
number_of_rows <- nrow(future)
dates <- seq.Date(from = start_date, by = "month", length.out = number_of_rows)

# Format the dates to keep only year and month
formatted_dates <- format(dates, "%Y-%m-%d")

# Add the formatted dates as a new column
future$date <- formatted_dates
#Extracting 2040-2060)

# Define summer months (replace with your desired months)
int_months <- c(10, 11, 4, 5)  # June, July, August

# Convert 'date' column to a date object (assuming year and month format)
future$date <- ymd(future$date)

# Filter data for summer months using month function within square brackets

# Filter data for the years 2040-2060 during summer months
int_data <- future[month(future$date) %in% int_months & 
                     year(future$date) >= 2040 & 
                     year(future$date) <= 2060, ]

write.csv(int_data, "C:/Users/jaa6483/OneDrive - The Pennsylvania State University/Desktop/Class work/PROJECTS/Predictive_Analysis/Prediction\\int_future_revised.csv", row.names=TRUE)

#WINTER
# Create a sequence of dates starting from a specific date
future <- read_csv("C:/Users/jaa6483/OneDrive - The Pennsylvania State University/Desktop/Class work/PROJECTS/Predictive_Analysis/Prediction/Results/Future_pred.csv")
start_date <- as.Date("2040-01-01")
number_of_rows <- nrow(future)
dates <- seq.Date(from = start_date, by = "month", length.out = number_of_rows)

# Format the dates to keep only year and month
formatted_dates <- format(dates, "%Y-%m-%d")

# Add the formatted dates as a new column
future$date <- formatted_dates
#Extracting 2040-2060)

# Define winter months (replace with your desired months)
winter_months <- c(12, 1, 2, 3)  # June, July, August

# Convert 'date' column to a date object (assuming year and month format)
future$date <- ymd(future$date)

# Filter data for summer months using month function within square brackets

# Filter data for the years 2040-2060 during summer months
winter_data <- future[month(future$date) %in% winter_months & 
                        year(future$date) >= 2040 & 
                        year(future$date) <= 2060, ]

write.csv(winter_data, "C:/Users/jaa6483/OneDrive - The Pennsylvania State University/Desktop/Class work/PROJECTS/Predictive_Analysis/Prediction\\winter_future_revised.csv", row.names=TRUE)

#PRESENT
#SUMMER
#Add date column
# Create a sequence of dates starting from a specific date
present <- read_csv("C:/Users/jaa6483/OneDrive - The Pennsylvania State University/Desktop/Class work/PROJECTS/Predictive_Analysis/Prediction/Results/Present_pred.csv")
start_date <- as.Date("2001-01-01")
number_of_rows <- nrow(present)
dates <- seq.Date(from = start_date, by = "month", length.out = number_of_rows)

# Format the dates to keep only year and month
formatted_dates <- format(dates, "%Y-%m-%d")

# Add the formatted dates as a new column
present$date <- formatted_dates

# Define summer months (replace with your desired months)
summer_months <- c(6, 7, 8, 9)  # June, July, August, September

# Convert 'date' column to a date object (assuming year and month format)
present$date <- ymd(present$date)

# Filter data for the years 2001-2021 during summer months
present_data <- present[month(present$date) %in% summer_months & 
                          year(present$date) >= 2001 & 
                          year(present$date) <= 2021, ]

write.csv(present_data, "C:/Users/jaa6483/OneDrive - The Pennsylvania State University/Desktop/Class work/PROJECTS/Predictive_Analysis/Prediction\\summer_present_revised.csv", row.names=TRUE)

#INTERMEDIATE

#Add date column
# Create a sequence of dates starting from a specific date
present <- read_csv("C:/Users/jaa6483/OneDrive - The Pennsylvania State University/Desktop/Class work/PROJECTS/Predictive_Analysis/Prediction/Results/Present_pred.csv")
start_date <- as.Date("2001-01-01")
number_of_rows <- nrow(present)
dates <- seq.Date(from = start_date, by = "month", length.out = number_of_rows)

# Format the dates to keep only year and month
formatted_dates <- format(dates, "%Y-%m-%d")

# Add the formatted dates as a new column
present$date <- formatted_dates

# Define intermediate months (replace with your desired months)
int_months <- c(10, 11, 4, 5)  # October, November, ApriL, May

# Convert 'date' column to a date object (assuming year and month format)
present$date <- ymd(present$date)

# Filter data for the years 2001-2021 during summer months
present_data <- present[month(present$date) %in% int_months & 
                          year(present$date) >= 2001 & 
                          year(present$date) <= 2021, ]

write.csv(present_data, "C:/Users/jaa6483/OneDrive - The Pennsylvania State University/Desktop/Class work/PROJECTS/Predictive_Analysis/Prediction\\int_present_revised.csv", row.names=TRUE)

#WINTER
#Add date column
# Create a sequence of dates starting from a specific date
present <- read_csv("C:/Users/jaa6483/OneDrive - The Pennsylvania State University/Desktop/Class work/PROJECTS/Predictive_Analysis/Prediction/Results/Present_pred.csv")
start_date <- as.Date("2001-01-01")
number_of_rows <- nrow(present)
dates <- seq.Date(from = start_date, by = "month", length.out = number_of_rows)

# Format the dates to keep only year and month
formatted_dates <- format(dates, "%Y-%m-%d")

# Add the formatted dates as a new column
present$date <- formatted_dates

# Define winter months (replace with your desired months)
winter_months <- c(12, 1, 2, 3)  # December, January, Febuary, March

# Convert 'date' column to a date object (assuming year and month format)
present$date <- ymd(present$date)

# Filter data for the years 2040-2060 during summer months
winter_data <- present[month(present$date) %in% winter_months & 
                         year(present$date) >= 2001 & 
                         year(present$date) <= 2021, ]

write.csv(winter_data, "C:/Users/jaa6483/OneDrive - The Pennsylvania State University/Desktop/Class work/PROJECTS/Predictive_Analysis/Prediction\\winter_present_revised.csv", row.names=TRUE)

# Changes in wind speeds form the cmip6 models for CA and NY
#ipsl_cm6a_lr_ssp585
ipsl_cm6a_lr_ssp585 <- read_csv("C:/Users/jaa6483/OneDrive - The Pennsylvania State University/Desktop/Class work/PROJECTS/Predictive_Analysis/Prediction/Prediction/CMIP6_cvs/ipsl_cm6a_lr_ssp585_daily_sfcwind.csv")
ipsl_cm6a_lr_ssp585$date <- as.Date(paste(ipsl_cm6a_lr_ssp585$YYYY, ipsl_cm6a_lr_ssp585$MM, ipsl_cm6a_lr_ssp585$DD, sep = "-"), format = "%Y-%m-%d")
filtered1_ipsl_cm6a_lr_ssp585 <- subset(ipsl_cm6a_lr_ssp585, date >= as.Date("2001-01-01") & date <= as.Date("2022-12-31"))
mean(filtered1_ipsl_cm6a_lr_ssp585$ID_06)
mean(filtered1_ipsl_cm6a_lr_ssp585$ID_36)

filtered2_ipsl_cm6a_lr_ssp585 <- subset(ipsl_cm6a_lr_ssp585, date >= as.Date("2040-01-01") & date <= as.Date("2060-12-31"))
mean(filtered2_ipsl_cm6a_lr_ssp585$ID_06)
mean(filtered2_ipsl_cm6a_lr_ssp585$ID_36)

#ipsl_cm6a_lr_ssp126
ipsl_cm6a_lr_ssp126 <- read_csv("C:/Users/jaa6483/OneDrive - The Pennsylvania State University/Desktop/Class work/PROJECTS/Predictive_Analysis/Prediction/Prediction/CMIP6_cvs/ipsl_cm6a_lr_ssp126_daily_sfcwind.csv")
ipsl_cm6a_lr_ssp126$date <- as.Date(paste(ipsl_cm6a_lr_ssp126$YYYY, ipsl_cm6a_lr_ssp126$MM, ipsl_cm6a_lr_ssp126$DD, sep = "-"), format = "%Y-%m-%d")
filtered1_ipsl_cm6a_lr_ssp126 <- subset(ipsl_cm6a_lr_ssp126, date >= as.Date("2001-01-01") & date <= as.Date("2022-12-31"))
mean(filtered1_ipsl_cm6a_lr_ssp126$ID_06)
mean(filtered1_ipsl_cm6a_lr_ssp126$ID_36)

filtered2_ipsl_cm6a_lr_ssp126 <- subset(ipsl_cm6a_lr_ssp126, date >= as.Date("2040-01-01") & date <= as.Date("2060-12-31"))
mean(filtered2_ipsl_cm6a_lr_ssp126$ID_06)
mean(filtered2_ipsl_cm6a_lr_ssp126$ID_36)

#ukesm1_0_ll_ssp585
ukesm1_0_ll_ssp585 <- read_csv("C:/Users/jaa6483/OneDrive - The Pennsylvania State University/Desktop/Class work/PROJECTS/Predictive_Analysis/Prediction/Prediction/CMIP6_cvs/ukesm1_0_ll_ssp585_daily_sfcwind.csv")
ukesm1_0_ll_ssp585$date <- as.Date(paste(ukesm1_0_ll_ssp585$YYYY, ukesm1_0_ll_ssp585$MM, ukesm1_0_ll_ssp585$DD, sep = "-"), format = "%Y-%m-%d")
filtered1_ukesm1_0_ll_ssp585 <- subset(ukesm1_0_ll_ssp585, date >= as.Date("2001-01-01") & date <= as.Date("2022-12-31"))
mean(filtered1_ukesm1_0_ll_ssp585$ID_06)
mean(filtered1_ukesm1_0_ll_ssp585$ID_36)

filtered2_ukesm1_0_ll_ssp585 <- subset(ukesm1_0_ll_ssp585, date >= as.Date("2040-01-01") & date <= as.Date("2060-12-31"))
mean(filtered2_ukesm1_0_ll_ssp585$ID_06)
mean(filtered2_ukesm1_0_ll_ssp585$ID_36)

#ukesm1_0_ll_ssp126
ukesm1_0_ll_ssp126 <- read_csv("C:/Users/jaa6483/OneDrive - The Pennsylvania State University/Desktop/Class work/PROJECTS/Predictive_Analysis/Prediction/Prediction/CMIP6_cvs/ukesm1_0_ll_ssp126_daily_sfcwind.csv")
ukesm1_0_ll_ssp126$date <- as.Date(paste(ukesm1_0_ll_ssp126$YYYY, ukesm1_0_ll_ssp126$MM, ukesm1_0_ll_ssp126$DD, sep = "-"), format = "%Y-%m-%d")
filtered1_ukesm1_0_ll_ssp126 <- subset(ukesm1_0_ll_ssp126, date >= as.Date("2001-01-01") & date <= as.Date("2022-12-31"))
mean(filtered1_ukesm1_0_ll_ssp126$ID_06)
mean(filtered1_ukesm1_0_ll_ssp126$ID_36)

filtered2_ukesm1_0_ll_ssp126 <- subset(ukesm1_0_ll_ssp126, date >= as.Date("2040-01-01") & date <= as.Date("2060-12-31"))
mean(filtered2_ukesm1_0_ll_ssp126$ID_06)
mean(filtered2_ukesm1_0_ll_ssp126$ID_36)

#gfdl_esm4_ssp585
gfdl_esm4_ssp585 <- read_csv("C:/Users/jaa6483/OneDrive - The Pennsylvania State University/Desktop/Class work/PROJECTS/Predictive_Analysis/Prediction/Prediction/CMIP6_cvs/gfdl_esm4_ssp585_daily_sfcwind.csv")
gfdl_esm4_ssp585$date <- as.Date(paste(gfdl_esm4_ssp585$YYYY, gfdl_esm4_ssp585$MM, gfdl_esm4_ssp585$DD, sep = "-"), format = "%Y-%m-%d")
filtered1_gfdl_esm4_ssp585 <- subset(gfdl_esm4_ssp585, date >= as.Date("2001-01-01") & date <= as.Date("2022-12-31"))
mean(filtered1_gfdl_esm4_ssp585$ID_06)
mean(filtered1_gfdl_esm4_ssp585$ID_36)

filtered2_gfdl_esm4_ssp585 <- subset(gfdl_esm4_ssp585, date >= as.Date("2040-01-01") & date <= as.Date("2060-12-31"))
mean(filtered2_gfdl_esm4_ssp585$ID_06)
mean(filtered2_gfdl_esm4_ssp585$ID_36)


#gfdl_esm4_ssp126
gfdl_esm4_ssp126 <- read_csv("C:/Users/jaa6483/OneDrive - The Pennsylvania State University/Desktop/Class work/PROJECTS/Predictive_Analysis/Prediction/Prediction/CMIP6_cvs/gfdl_esm4_ssp126_daily_sfcwind.csv")
gfdl_esm4_ssp126$date <- as.Date(paste(gfdl_esm4_ssp126$YYYY, gfdl_esm4_ssp126$MM, gfdl_esm4_ssp126$DD, sep = "-"), format = "%Y-%m-%d")
filtered1_gfdl_esm4_ssp126 <- subset(gfdl_esm4_ssp126, date >= as.Date("2001-01-01") & date <= as.Date("2022-12-31"))
mean(filtered1_gfdl_esm4_ssp126$ID_06)
mean(filtered1_gfdl_esm4_ssp126$ID_36)

filtered2_gfdl_esm4_ssp126 <- subset(gfdl_esm4_ssp126, date >= as.Date("2040-01-01") & date <= as.Date("2060-12-31"))
mean(filtered2_gfdl_esm4_ssp126$ID_06)
mean(filtered2_gfdl_esm4_ssp126$ID_36)

#mri_esm2_0_ssp585
mri_esm2_0_ssp585 <- read_csv("C:/Users/jaa6483/OneDrive - The Pennsylvania State University/Desktop/Class work/PROJECTS/Predictive_Analysis/Prediction/Prediction/CMIP6_cvs/mri_esm2_0_ssp585_daily_sfcwind.csv")
mri_esm2_0_ssp585$date <- as.Date(paste(mri_esm2_0_ssp585$YYYY, mri_esm2_0_ssp585$MM, mri_esm2_0_ssp585$DD, sep = "-"), format = "%Y-%m-%d")
filtered1_mri_esm2_0_ssp585 <- subset(mri_esm2_0_ssp585, date >= as.Date("2001-01-01") & date <= as.Date("2022-12-31"))
mean(filtered1_mri_esm2_0_ssp585$ID_06)
mean(filtered1_mri_esm2_0_ssp585$ID_36)

filtered2_mri_esm2_0_ssp585 <- subset(mri_esm2_0_ssp585, date >= as.Date("2040-01-01") & date <= as.Date("2060-12-31"))
mean(filtered2_mri_esm2_0_ssp585$ID_06)
mean(filtered2_mri_esm2_0_ssp585$ID_36)

#mri_esm2_0_ssp126
mri_esm2_0_ssp126 <- read_csv("C:/Users/jaa6483/OneDrive - The Pennsylvania State University/Desktop/Class work/PROJECTS/Predictive_Analysis/Prediction/Prediction/CMIP6_cvs/mri_esm2_0_ssp126_daily_sfcwind.csv")
mri_esm2_0_ssp126$date <- as.Date(paste(mri_esm2_0_ssp126$YYYY, mri_esm2_0_ssp126$MM, mri_esm2_0_ssp126$DD, sep = "-"), format = "%Y-%m-%d")
filtered1_mri_esm2_0_ssp126 <- subset(mri_esm2_0_ssp126, date >= as.Date("2001-01-01") & date <= as.Date("2022-12-31"))
mean(filtered1_mri_esm2_0_ssp126$ID_06)
mean(filtered1_mri_esm2_0_ssp126$ID_36)

filtered2_mri_esm2_0_ssp126 <- subset(mri_esm2_0_ssp126, date >= as.Date("2040-01-01") & date <= as.Date("2060-12-31"))
mean(filtered2_mri_esm2_0_ssp126$ID_06)
mean(filtered2_mri_esm2_0_ssp126$ID_36)

#mpi_esm1_2_hr_ssp585
mpi_esm1_2_hr_ssp585 <- read_csv("C:/Users/jaa6483/OneDrive - The Pennsylvania State University/Desktop/Class work/PROJECTS/Predictive_Analysis/Prediction/Prediction/CMIP6_cvs/mpi_esm1_2_hr_ssp585_daily_sfcwind.csv")
mpi_esm1_2_hr_ssp585$date <- as.Date(paste(mpi_esm1_2_hr_ssp585$YYYY, mpi_esm1_2_hr_ssp585$MM, mpi_esm1_2_hr_ssp585$DD, sep = "-"), format = "%Y-%m-%d")
filtered1_mpi_esm1_2_hr_ssp585 <- subset(mpi_esm1_2_hr_ssp585, date >= as.Date("2001-01-01") & date <= as.Date("2022-12-31"))
mean(filtered1_mpi_esm1_2_hr_ssp585$ID_06)
mean(filtered1_mpi_esm1_2_hr_ssp585$ID_36)

filtered2_mpi_esm1_2_hr_ssp585 <- subset(mpi_esm1_2_hr_ssp585, date >= as.Date("2040-01-01") & date <= as.Date("2060-12-31"))
mean(filtered2_mpi_esm1_2_hr_ssp585$ID_06)
mean(filtered2_mpi_esm1_2_hr_ssp585$ID_36)

#mpi_esm1_2_hr_ssp126
mpi_esm1_2_hr_ssp126 <- read_csv("C:/Users/jaa6483/OneDrive - The Pennsylvania State University/Desktop/Class work/PROJECTS/Predictive_Analysis/Prediction/Prediction/CMIP6_cvs/mpi_esm1_2_hr_ssp126_daily_sfcwind.csv")
mpi_esm1_2_hr_ssp126$date <- as.Date(paste(mpi_esm1_2_hr_ssp126$YYYY, mpi_esm1_2_hr_ssp126$MM, mpi_esm1_2_hr_ssp126$DD, sep = "-"), format = "%Y-%m-%d")
filtered1_mpi_esm1_2_hr_ssp126 <- subset(mpi_esm1_2_hr_ssp126, date >= as.Date("2001-01-01") & date <= as.Date("2022-12-31"))
mean(filtered1_mpi_esm1_2_hr_ssp126$ID_06)
mean(filtered1_mpi_esm1_2_hr_ssp126$ID_36)

filtered2_mpi_esm1_2_hr_ssp126 <- subset(mpi_esm1_2_hr_ssp126, date >= as.Date("2040-01-01") & date <= as.Date("2060-12-31"))
mean(filtered2_mpi_esm1_2_hr_ssp126$ID_06)
mean(filtered2_mpi_esm1_2_hr_ssp126$ID_36)


#California Time Series for Wind generation

#SUMMER
#Add date column
# Create a sequence of dates starting from a specific date
start_date <- as.Date("2001-01-01")
number_of_rows <- nrow(predictions_df)
dates <- seq.Date(from = start_date, by = "month", length.out = number_of_rows)

# Format the dates to keep only year and month
formatted_dates <- format(dates, "%Y-%m-%d")

# Add the formatted dates as a new column
predictions_df$date <- formatted_dates
#Extracting 2040-2060 & 2021-2021)

# Define summer months (replace with your desired months)
summer_months <- c(6, 7, 8, 9)  # June, July, August

# Convert 'date' column to a date object (assuming year and month format)
future$date <- ymd(future$date)

# Filter data for summer months using month function within square brackets

summer_data <- future[month(future[, "date"]) %in% summer_months, ]

write.csv(summer_data, "C:/Users/jaa6483/OneDrive - The Pennsylvania State University/Desktop/Class work/PROJECTS/Predictive_Analysis/Prediction\\CA_wind_summer_future.csv", row.names=TRUE)
summer_future <- read_csv("C:/Users/jaa6483/OneDrive - The Pennsylvania State University/Desktop/Class work/PROJECTS/Predictive_Analysis/Prediction/CA_wind_summer_future.csv")


meltdf <- melt(summer_future ,id="date")
meltdf$date <- as.Date(meltdf$date, format = "%m/%d/%Y")
ggplot(meltdf, aes(x = date, y = value,colour=variable,group=variable)) +
  geom_line() +
  scale_x_date(date_breaks = "4 year", date_labels = "%Y") +
  labs(title = "Monthly Wind Generation Changes in Summer", x = "Year", y = "Value") +
  theme_minimal()


#WINTER
#Add date column
# Create a sequence of dates starting from a specific date
start_date <- as.Date("2001-01-01")
number_of_rows <- nrow(predictions_df)
dates <- seq.Date(from = start_date, by = "month", length.out = number_of_rows)

# Format the dates to keep only year and month
formatted_dates <- format(dates, "%Y-%m-%d")

# Add the formatted dates as a new column
predictions_df$date <- formatted_dates
#Extracting 2040-2060 & 2021-2021)
future <- predictions_df[predictions_df$date >= "2040-01" & predictions_df$date <= "2060-12", ]
# Define summer months (replace with your desired months)
winter_months <- c(12, 1, 2, 3)  # June, July, August

# Convert 'date' column to a date object (assuming year and month format)
future$date <- ymd(future$date)

# Filter data for summer months using month function within square brackets

winter_data <- future[month(future[, "date"]) %in% winter_months, ]

#write.csv(winter_data, "C:/Users/jaa6483/OneDrive - The Pennsylvania State University/Desktop/Class work/PROJECTS/Predictive_Analysis/Prediction\\CA_wind_winter_future.csv", row.names=TRUE)
winter_future <- read_csv("C:/Users/jaa6483/OneDrive - The Pennsylvania State University/Desktop/Class work/PROJECTS/Predictive_Analysis/Prediction/CA_wind_winter_future.csv")

meltdf <- melt(winter_future ,id="date")
meltdf$date <- as.Date(meltdf$date, format = "%m/%d/%Y")
ggplot(meltdf, aes(x = date, y = value,colour=variable,group=variable)) +
  geom_line() +
  scale_x_date(date_breaks = "4 year", date_labels = "%Y") +
  labs(title = "Montly Wind Generation Changes in Winter", x = "Year", y = "Value") +
  theme_minimal()


summer_future <- read_csv("C:/Users/jaa6483/OneDrive - The Pennsylvania State University/Desktop/Class work/PROJECTS/Predictive_Analysis/Prediction/CA_hydro_summer_future.csv")

meltdf <- melt(summer_future ,id="date")
meltdf$date <- as.Date(meltdf$date, format = "%m/%d/%Y")
ggplot(meltdf, aes(x = date, y = value,colour=variable,group=variable)) +
  geom_point() +
  scale_x_date(date_breaks = "3 year", date_labels = "%Y") +
  labs(title = "Hydroelectric Generation Changes in Summer", x = "Year", y = "Value") +
  theme_minimal() +
  scale_color_brewer(palette = "Dark2") +
  theme(
    panel.grid.major = element_line(color = "gray", size = 0.5, linetype = "solid"),  # Solid major grid lines
    panel.grid.minor = element_blank(),  # Hide minor grid lines, if desired
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)  # Solid border around the plot
  )
#Statistical Significance
nrmse_bart <- c(14.9, 12.2, 11.5,	14.8,	12.2,	8.5,	17,	24.1,	14,	13.8)  
nrmse_rf <- c(15.8,	12.5,	12.5,	14.8,	12.2,	7.8,	16.8,	24.6,	13.4,	14) 

# Perform the paired t-test
test_result <- t.test(nrmse_rf,nrmse_bart,  paired = TRUE)
print(test_result)

#RF_SVM
#CA_Hydro
rmse_rf <- c(13.38, 13.76, 12.39, 14.80, 13.30)
rmse_svm <- c(12.46, 13.15, 10.44, 13.34, 12.41)

# Perform the paired t-test
test_result <- t.test(rmse_rf,rmse_svm,  paired = TRUE)
print(test_result)

# Calculate the variance for each model
variance_rf <- var(rmse_rf)
variance_svm <- var(rmse_svm)

# Print the variances
print(paste("Variance of RMSE for RF: ", variance_rf))
print(paste("Variance of RMSE for SVM: ", variance_svm))


#CA_Solar
rmse_rf <- c(5.44, 5.50, 6.29, 6.23, 6.08)
rmse_svm <- c(5.54, 5.57, 5.88, 5.54, 6.58)

# Perform the paired t-test
test_result <- t.test(rmse_rf,rmse_svm,  paired = TRUE)
print(test_result)  

#CA_Wind
rmse_rf <- c( 4.64, 3.79, 4.03, 4.51, 4.42)  
rmse_svm <- c(4.28, 3.13, 3.71, 3.68, 3.58)
# Perform the paired t-test
test_result <- t.test(rmse_rf,rmse_svm,  paired = TRUE)
print(test_result)  

# Calculate the variance for each model
variance_rf <- var(rmse_rf)
variance_svm <- var(rmse_svm)

# Print the variances
print(paste("Variance of RMSE for RF: ", variance_rf))
print(paste("Variance of RMSE for SVM: ", variance_svm))

#FL_Hydro
rmse_rf <- c(0.23, 0.20, 0.25, 0.32, 0.24)
rmse_svm <- c(0.24, 0.21, 0.27, 0.31, 0.24)
# Perform the paired t-test
test_result <- t.test(rmse_rf,rmse_svm,  paired = TRUE)
print(test_result)  

#FL_Solar
rmse_rf <- c(1.90, 2.41, 2.02, 2.42, 2.37)
rmse_svm <- c(1.68, 2.54, 2.17, 2.33, 2.14)
# Perform the paired t-test
test_result <- t.test(rmse_rf,rmse_svm,  paired = TRUE)
print(test_result)  

#GA_Hydro
rmse_rf <- c(10.25, 13.00, 12.30, 7.91, 11.44)
rmse_svm <- c(9.53, 13.12, 12.39, 7.85, 12.26)
# Perform the paired t-test
test_result <- t.test(rmse_rf,rmse_svm,  paired = TRUE)
print(test_result)  

#GA_Solar
rmse_rf <- c(19.16, 21.14, 16.23, 22.66, 13.81)
rmse_svm <- c(18.74, 22.38, 15.72, 21.96, 13.51)
# Perform the paired t-test
test_result <- t.test(rmse_rf,rmse_svm,  paired = TRUE)
print(test_result)  

#NY_Hydro
rmse_rf <- c(7.90, 9.32, 8.57, 8.48, 6.63)
rmse_svm <- c(7.86, 9.55, 8.86, 8.61, 6.77)

# Perform the paired t-test
test_result <- t.test(rmse_rf,rmse_svm,  paired = TRUE)
print(test_result)

#NY_Solar
rmse_rf <- c(0.29, 0.35, 0.30, 0.27, 0.32)
rmse_svm <- c(0.29, 0.37, 0.25, 0.32, 0.28)
# Perform the paired t-test
test_result <- t.test(rmse_rf,rmse_svm,  paired = TRUE)
print(test_result)

#NY_Wind
rmse_rf <- c(2.48, 2.13, 3.94, 1.80, 2.35)  
rmse_svm <- c(2.31, 2.19, 4.13, 1.81, 2.43)
# Perform the paired t-test
test_result <- t.test(rmse_rf,rmse_svm,  paired = TRUE)
print(test_result)  

#SVM_BART
#CA_Hydro
rmse_bart <- c(12.85, 12.96, 12.04, 14.01, 12.19)
rmse_svm <- c(2.31, 2.19, 4.13, 1.81, 2.43)

# Perform the paired t-test
test_result <- t.test(rmse_bart,rmse_svm,  paired = TRUE)
print(test_result)


#CA_Solar
rmse_bart <- c(5.13, 5.20, 6.46, 6.04, 6.41)
rmse_svm <- c(5.54, 5.57, 5.88, 5.54, 6.58)

# Perform the paired t-test
test_result <- t.test(rmse_bart,rmse_svm,  paired = TRUE)
print(test_result)  

#CA_Wind
rmse_bart <- c( 4.53, 3.32, 3.79, 4.35, 4.01)  
rmse_svm <- c(4.28, 3.13, 3.71, 3.68, 3.58)
# Perform the paired t-test
test_result <- t.test(rmse_bart,rmse_svm,  paired = TRUE)
print(test_result)  

# Calculate the variance for each model
variance_rf <- var(rmse_rf)
variance_svm <- var(rmse_svm)

# Print the variances
print(paste("Variance of RMSE for RF: ", variance_rf))
print(paste("Variance of RMSE for SVM: ", variance_svm))

#FL_Hydro
rmse_bart <- c( 0.24, 0.21, 0.25, 0.32, 0.24)
rmse_svm <- c(0.24, 0.21, 0.27, 0.31, 0.24)
# Perform the paired t-test
test_result <- t.test(rmse_bart,rmse_svm,  paired = TRUE)
print(test_result)  

#FL_Solar
rmse_bart <- c(2.10, 2.35, 1.99, 2.39, 2.19)
rmse_svm <- c(1.68, 2.54, 2.17, 2.33, 2.14)
# Perform the paired t-test
test_result <- t.test(rmse_bart,rmse_svm,  paired = TRUE)
print(test_result)  

#GA_Hydro
rmse_bart <- c(11.48, 13.01, 11.98,  7.79, 11.47)
rmse_svm <- c(9.53, 13.12, 12.39, 7.85, 12.26)
# Perform the paired t-test
test_result <- t.test(rmse_bart,rmse_svm,  paired = TRUE)
print(test_result)  

#GA_Solar
rmse_bart <- c(18.43, 20.99, 16.19, 21.09, 14.12)
rmse_svm <- c(18.74, 22.38, 15.72, 21.96, 13.51)
# Perform the paired t-test
test_result <- t.test(rmse_bart,rmse_svm,  paired = TRUE)
print(test_result)  

#NY_Hydro
rmse_bart <- c(7.63, 9.23, 8.55, 8.36, 6.75)
rmse_svm <- c(7.86, 9.55, 8.86, 8.61, 6.77)

# Perform the paired t-test
test_result <- t.test(rmse_bart,rmse_svm,  paired = TRUE)
print(test_result)

#NY_Solar
rmse_bart <- c(0.31, 0.37, 0.27, 0.27, 0.32)
rmse_svm <- c(0.29, 0.37, 0.25, 0.32, 0.28)
# Perform the paired t-test
test_result <- t.test(rmse_bart,rmse_svm,  paired = TRUE)
print(test_result)

#NY_Wind
rmse_bart <- c( 2.68, 2.36, 3.95, 2.11, 2.67)  
rmse_svm <- c(2.31, 2.19, 4.13, 1.81, 2.43)
# Perform the paired t-test
test_result <- t.test(rmse_bart,rmse_svm,  paired = TRUE)
print(test_result)  


save.image('Prediction.rdata')
