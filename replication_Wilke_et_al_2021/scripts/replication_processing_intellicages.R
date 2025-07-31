## Last modified: 15.08.2024
#### Loading files #####
set.seed(0)
options(digits = 5,scipen = 20)

#### Place Learning ####
read.delim(paste(project_folder,"/",project,"/","Challenge_1/Visit.txt",sep="")) -> PL_visits
read.delim(paste(project_folder,"/",project,"/","Challenge_1/Nosepoke.txt",sep="")) -> PL_nosepokes

#### Reversal Learning ####
read.delim(paste(project_folder,"/",project,"/","Challenge_2/Visit.txt",sep="")) -> RL_visits
read.delim(paste(project_folder,"/",project,"/","Challenge_2/Nosepoke.txt",sep="")) -> RL_nosepokes

#### Creating the dataframe for the results ####
unblinding[,c(1,2)] -> unblinding
unblinding -> results_df
unblinding$ID -> all_animals

#### Going to the results folder ####
dir.create(paste(results_folder,project,sep = "/"))
setwd(paste(results_folder,project,sep = "/"))

#### Fixing/preparing time columns #####
# Getting all visits objects
visits_objects <- ls(pattern = "_visits$")

# Iterate through the visits objects
for (visits_object in visits_objects) {
  # Get the current visits data frame
  visits_df <- get(visits_object)
  
  # Sometimes there are dates in the StartTime and EndTime columns and they are not supposed to be there
  gsub("1900-01-01 ","",visits_df$StartTime) -> visits_df$StartTime
  gsub("1900-01-01 ","",visits_df$EndTime) -> visits_df$EndTime
  gsub("1899-12-31 ","",visits_df$StartTime) -> visits_df$StartTime
  gsub("1899-12-31 ","",visits_df$EndTime) -> visits_df$EndTime
  
  # The time in these columns is coded in a way that makes working with it more difficult. This solves it.
  cbind(visits_df,t(as.data.frame(strsplit(visits_df$StartTime,split = ":")))) -> visits_df
  colnames(visits_df)[28:30] <- c("Start_Hour","Start_Minute","Start_SecondMS")
  cbind(visits_df,t(as.data.frame(strsplit(visits_df$EndTime,split = ":")))) -> visits_df
  colnames(visits_df)[31:33] <- c("End_Hour","End_Minute","End_SecondMS")
  visits_df[,c(28,29,30,31,32,33)] <- sapply(visits_df[,c(28,29,30,31,32,33)],as.numeric)
  
  # We need to differentiate between Light and Dark for several of the measurements
  visits_df$Light_Status <- NA
  
  visits_df <- visits_df %>% mutate(Light_Status = case_when(
    (Cage == 1 | Cage == 2) & (Start_Hour >= 6 & Start_Hour <= 17) ~ "Light")) 
  
  visits_df[is.na(visits_df$Light_Status),ncol(visits_df)] <- "Dark"
  
  # Reset the rownames
  row.names(visits_df) <- NULL
  
  # Return to the original dataframe
  assign(visits_object,visits_df)
}
rm(visits_df,visits_object,visits_objects)

# Getting all nosepokes objects
nosepokes_objects <- ls(pattern = "_nosepokes$")

# Iterate through the nosepokes objects
for (nosepokes_object in nosepokes_objects) {
  # Get the current nosepokes data frame
  nosepokes_df <- get(nosepokes_object)
  
  # Sometimes there are dates in the StartTime and EndTime columns and they are not supposed to be there
  gsub("1900-01-01 ","",nosepokes_df$StartTime) -> nosepokes_df$StartTime
  gsub("1900-01-01 ","",nosepokes_df$EndTime) -> nosepokes_df$EndTime
  gsub("1899-12-31 ","",nosepokes_df$StartTime) -> nosepokes_df$StartTime
  gsub("1899-12-31 ","",nosepokes_df$EndTime) -> nosepokes_df$EndTime
  
  # The time in these columns is coded in a way that makes working with it more difficult. This solves it.
  cbind(nosepokes_df,t(as.data.frame(strsplit(nosepokes_df$StartTime,split = ":")))) -> nosepokes_df
  colnames(nosepokes_df)[25:27] <- c("Start_Hour","Start_Minute","Start_SecondMS")
  cbind(nosepokes_df,t(as.data.frame(strsplit(nosepokes_df$EndTime,split = ":")))) -> nosepokes_df
  colnames(nosepokes_df)[28:30] <- c("End_Hour","End_Minute","End_SecondMS")
  nosepokes_df[,c(25:30)] <- sapply(nosepokes_df[,c(25:30)],as.numeric)
  
  # We need to differentiate between Light and Dark for several of the measurements
  nosepokes_df$Light_Status <- NA
  
  nosepokes_df <- nosepokes_df %>% mutate(Light_Status = case_when(
    (Cage == 1 | Cage == 2) & (Start_Hour >= 6 & Start_Hour <= 17) ~ "Light")) 
  
  nosepokes_df[is.na(nosepokes_df$Light_Status),ncol(nosepokes_df)] <- "Dark"
  
  # Reset the rownames
  row.names(nosepokes_df) < NULL
  
  # Return to the original dataframe
  assign(nosepokes_object,nosepokes_df)
}
rm(nosepokes_df,nosepokes_object,nosepokes_objects)

#### Getting total visits, nosepokes, and licks ####
# Get the names of all objects that will be used for these variables
visits_objects <- ls(pattern = "_visits$")
# Reorder to follow the challenges
#visits_objects <- visits_objects[c(15,17,18,16,13,6,5,8,7,10,9,12,11,14,3,4,1,2)]
visits_objects[!is.na(visits_objects)] -> visits_objects  
# Create a vector of column names for the results data frame
column_names <- c("Animal", paste0(rep(gsub("_visits","",visits_objects), each = 3), c("_Total_Licks","_Total_Visits", "_Total_Nosepokes")))

# Create the results data frame
results <- setNames(data.frame(matrix(ncol = length(column_names), nrow = 0)), column_names)

# Iterate through all the animals
for (animal in all_animals) {
  # Create a list to store the counts for each visits object
  counts <- list(animal)
  
  # Iterate through all the challenges
  for (visits_object in visits_objects) {
    # Get the current challenge
    visits_df <- get(visits_object)
    
    # Subset the data frame based on each individual animal
    subset_df <- subset(visits_df, Animal == animal)
    
    # Calculate the total number of visits, nosepokes, and licks
    total_visits <- ifelse(nrow(subset_df) == 0, NA, nrow(subset_df))
    total_nosepokes <- ifelse(nrow(subset_df) == 0, NA, sum(subset_df$NosepokeNumber, na.rm = TRUE))
    total_licks <- ifelse(nrow(subset_df) == 0, NA, sum(subset_df$LickNumber, na.rm = TRUE))
    
    # Add the counts to the counts list
    counts[[length(counts) + 1]] <- total_licks
    counts[[length(counts) + 1]] <- total_visits
    counts[[length(counts) + 1]] <- total_nosepokes
  }
  
  # Add a row with the counts to the results data frame
  results[nrow(results) + 1,] <- counts
}

# Clean up intermediate files
rm(animal,total_visits,total_nosepokes,total_licks,counts,visits_df,subset_df,visits_object,visits_objects)

# Merge with the actual results dataframe
results_df <- merge(results_df,results,by.x = 1,by.y = 1,all.x = T)

#### Getting visits, nosepokes, and licks in the Dark period ####
# Get the names of all objects that will be used for these variables
visits_objects <- ls(pattern = "_visits$")
# Reorder to follow the challenges
#visits_objects <- visits_objects[c(15,17,18,16,13,6,5,8,7,10,9,12,11,14,3,4,1,2)]
visits_objects[!is.na(visits_objects)] -> visits_objects  
# Create a vector of column names for the results data frame
column_names <- c("Animal", paste0(rep(gsub("_visits","",visits_objects), each = 3), c("_Dark_Licks","_Dark_Visits", "_Dark_Nosepokes")))

# Create the results data frame
results <- setNames(data.frame(matrix(ncol = length(column_names), nrow = 0)), column_names)

# Iterate through all the animals
for (animal in all_animals) {
  # Create a list to store the counts for each challenge object
  counts <- list(animal)
  
  # Iterate through all the challenges
  for (visits_object in visits_objects) {
    # Get the current challenge
    visits_df <- get(visits_object)
    
    # Subset the data frame based on each individual animal
    subset_df <- subset(visits_df, Animal == animal & Light_Status == "Dark")
    
    # Calculate the total number of visits, nosepokes, and licks
    dark_visits <- ifelse(nrow(subset_df) == 0, NA, nrow(subset_df))
    dark_nosepokes <- ifelse(nrow(subset_df) == 0, NA, sum(subset_df$NosepokeNumber, na.rm = TRUE))
    dark_licks <- ifelse(nrow(subset_df) == 0, NA, sum(subset_df$LickNumber, na.rm = TRUE))
    
    # Add the counts to the counts list
    counts[[length(counts) + 1]] <- dark_licks
    counts[[length(counts) + 1]] <- dark_visits
    counts[[length(counts) + 1]] <- dark_nosepokes
  }
  
  # Add a row with the counts to the results data frame
  results[nrow(results) + 1,] <- counts
}

# Clean up intermediate files
rm(animal,dark_visits,dark_nosepokes,dark_licks,counts,visits_df,subset_df,visits_object,visits_objects)

# Merge with the actual results dataframe
results_df <- merge(results_df,results,by.x = 1,by.y = 1,all.x = T)

#### Exploratory visits and drinking attempts - Total ####
# Get the names of all objects that will be used for these variables
visits_objects <- ls(pattern = "_visits$")
# Reorder to follow the challenges
visits_objects <- visits_objects[c(15,17,18,16,13,6,5,8,7,10,9,12,11,14,3,4,1,2)]
visits_objects[!is.na(visits_objects)] -> visits_objects  

# Create a vector of column names for the results data frame
column_names <- c("Animal", paste0(rep(gsub("_visits","",visits_objects), each = 2), c("_Total_ExploratoryVisits", "_Total_DrinkingAttempts")))

# Create the results data frame
results <- setNames(data.frame(matrix(ncol = length(column_names), nrow = 0)), column_names)

# Iterate through all the animals
for (animal in all_animals) {
  # Create a list to store the counts for each challenge object
  counts <- list(animal)
  
  # Iterate through all the challenges
  for (visits_object in visits_objects) {
    # Get the current challenge
    visits_df <- get(visits_object)
    
    # Subset the data frame based on each individual animal
    subset_df <- subset(visits_df, Animal == animal)
    
    # Calculate the exploratory visits and drinking attempts    
    total_exploratory_visits <- ifelse(nrow(subset_df) == 0, NA, nrow(subset_df[which(subset_df$NosepokeNumber == 0),]))
    total_drinking_attempts <- ifelse(nrow(subset_df) == 0, NA, nrow(subset_df[which(subset_df$NosepokeNumber != 0),]))
    
    # Add the counts to the counts list
    counts[[length(counts) + 1]] <- total_exploratory_visits
    counts[[length(counts) + 1]] <- total_drinking_attempts
  }
  
  # Add a row with the counts to the results data frame
  results[nrow(results) + 1,] <- counts
}
rm(animal,total_exploratory_visits,total_drinking_attempts,counts,visits_df,subset_df,visits_object,visits_objects)

# Merge with the actual results dataframe
results_df <- merge(results_df,results,by.x = 1,by.y = 1,all.x = T)

#### Exploratory visits and drinking attempts - Dark ####
# Get the names of all objects that will be used for these variables
visits_objects <- ls(pattern = "_visits$")
# Reorder to follow the challenges
#visits_objects <- visits_objects[c(15,17,18,16,13,6,5,8,7,10,9,12,11,14,3,4,1,2)]
visits_objects[!is.na(visits_objects)] -> visits_objects  

# Create a vector of column names for the results data frame
column_names <- c("Animal", paste0(rep(gsub("_visits","",visits_objects), each = 2), c("_Dark_ExploratoryVisits", "_Dark_DrinkingAttempts")))

# Create the results data frame
results <- setNames(data.frame(matrix(ncol = length(column_names), nrow = 0)), column_names)

# Iterate through all the animals
for (animal in all_animals) {
  # Create a list to store the counts for each challenge object
  counts <- list(animal)
  
  # Iterate through all the challenges
  for (visits_object in visits_objects) {
    # Get the current challenge
    visits_df <- get(visits_object)
    
    # Subset the data frame based on each individual animal
    subset_df <- subset(visits_df, Animal == animal & Light_Status == "Dark")
    
    # Calculate the exploratory visits and drinking attempts
    dark_exploratory_visits <- ifelse(nrow(subset_df) == 0, NA, nrow(subset_df[which(subset_df$NosepokeNumber == 0),]))
    dark_drinking_attempts <- ifelse(nrow(subset_df) == 0, NA, nrow(subset_df[which(subset_df$NosepokeNumber != 0),]))
    
    # Add the counts to the counts list
    counts[[length(counts) + 1]] <- dark_exploratory_visits
    counts[[length(counts) + 1]] <- dark_drinking_attempts
  }
  
  # Add a row with the counts to the results data frame
  results[nrow(results) + 1,] <- counts
}
rm(animal,dark_exploratory_visits,dark_drinking_attempts,counts,visits_df,subset_df,visits_object,visits_objects)

# Merge with the actual results dataframe
results_df <- merge(results_df,results,by.x = 1,by.y = 1,all.x = T)

#### Correct Visits vs Incorrect Visits - Total ####
# This was how PL and RL were evaluated in Wilke et al 2021
# Get the names of all objects that will be used for these variables
visits_objects <- ls(pattern = "_visits$")
# Reorder to follow the challenges
#visits_objects <- visits_objects[c(15,17,18,16,13,6,5,8,7,10,9,12,11,14,3,4,1,2)]
visits_objects[!is.na(visits_objects)] -> visits_objects  

# Create a vector of column names for the results data frame
column_names <- c("Animal", paste0(rep(gsub("_visits","",visits_objects), each = 4), c("_Total_CorrectVisits", "_Total_IncorrectVisits","_Total_ProportionCorrectVisits","_Total_ProportionIncorrectVisits")))

# Create the results data frame
results <- setNames(data.frame(matrix(ncol = length(column_names), nrow = 0)), column_names)

# Iterate through all the animals
for (animal in all_animals) {
  # Create a list to store the counts for each challenge object
  counts <- list(animal)
  
  # Iterate through all the challenges
  for (visits_object in visits_objects) {
    # Get the current challenge
    visits_df <- get(visits_object)
    
    # Subset the data frame based on each individual animal
    subset_df <- subset(visits_df, Animal == animal)
    
    # Calculate the proportion of visits to the correct corner vs incorrect corner
    total_correct_visit <- ifelse(nrow(subset_df) == 0, NA, nrow(subset_df[which(subset_df$CornerCondition == "Correct"),]))
    total_incorrect_visit <- ifelse(nrow(subset_df) == 0, NA, nrow(subset_df[which(subset_df$CornerCondition == "Incorrect"),]))
    total_proportion_correct_visit <- ifelse(nrow(subset_df) == 0, NA, total_correct_visit/nrow(subset_df))
    total_proportion_incorrect_visit <- ifelse(nrow(subset_df) == 0, NA, total_incorrect_visit/nrow(subset_df))
    
    # Add the counts to the counts list
    counts[[length(counts) + 1]] <- total_correct_visit
    counts[[length(counts) + 1]] <- total_incorrect_visit
    counts[[length(counts) + 1]] <- total_proportion_correct_visit
    counts[[length(counts) + 1]] <- total_proportion_incorrect_visit
  }
  
  # Add a row with the counts to the results data frame
  results[nrow(results) + 1,] <- counts
}
rm(animal,total_correct_visit,total_incorrect_visit,total_proportion_correct_visit,total_proportion_incorrect_visit,counts,visits_df,subset_df,visits_object,visits_objects)

# Merge with the actual results dataframe
results_df <- merge(results_df,results,by.x = 1,by.y = 1,all.x = T)

#### Correct Visits vs Incorrect Visits - Dark ####
# This was how PL and RL were evaluated in Wilke et al 2021
# Get the names of all objects that will be used for these variables
visits_objects <- ls(pattern = "_visits$")
# Reorder to follow the challenges
#visits_objects <- visits_objects[c(15,17,18,16,13,6,5,8,7,10,9,12,11,14,3,4,1,2)]
visits_objects[!is.na(visits_objects)] -> visits_objects  

# Create a vector of column names for the results data frame
column_names <- c("Animal", paste0(rep(gsub("_visits","",visits_objects), each = 4), c("_Dark_CorrectVisits", "_Dark_IncorrectVisits","_Dark_ProportionCorrectVisits","_Dark_ProportionIncorrectVisits")))

# Create the results data frame
results <- setNames(data.frame(matrix(ncol = length(column_names), nrow = 0)), column_names)

# Iterate through all the animals
for (animal in all_animals) {
  # Create a list to store the counts for each challenge object
  counts <- list(animal)
  
  # Iterate through all the challenges
  for (visits_object in visits_objects) {
    # Get the current challenge
    visits_df <- get(visits_object)
    
    # Subset the data frame based on each individual animal
    subset_df <- subset(visits_df, Animal == animal & Light_Status == "Dark")
    
    # Calculate the proportion of visits to the correct corner vs incorrect corner
    dark_correct_visit <- ifelse(nrow(subset_df) == 0, NA, nrow(subset_df[which(subset_df$CornerCondition == "Correct"),]))
    dark_incorrect_visit <- ifelse(nrow(subset_df) == 0, NA, nrow(subset_df[which(subset_df$CornerCondition == "Incorrect"),]))
    dark_proportion_correct_visit <- ifelse(nrow(subset_df) == 0, NA, dark_correct_visit/nrow(subset_df))
    dark_proportion_incorrect_visit <- ifelse(nrow(subset_df) == 0, NA, dark_incorrect_visit/nrow(subset_df))
    
    # Add the counts to the counts list
    counts[[length(counts) + 1]] <- dark_correct_visit
    counts[[length(counts) + 1]] <- dark_incorrect_visit
    counts[[length(counts) + 1]] <- dark_proportion_correct_visit
    counts[[length(counts) + 1]] <- dark_proportion_incorrect_visit
  }
  
  # Add a row with the counts to the results data frame
  results[nrow(results) + 1,] <- counts
}
rm(animal,dark_correct_visit,dark_incorrect_visit,dark_proportion_correct_visit,dark_proportion_incorrect_visit,counts,visits_df,subset_df,visits_object,visits_objects)

# Merge with the actual results dataframe
results_df <- merge(results_df,results,by.x = 1,by.y = 1,all.x = T)

#### Repetitive Behavior - Frequency and number - Total ####
# Get the names of all objects that will be used for this variable
nosepokes_objects <- ls(pattern = "_nosepokes$")
# Reorder to follow the challenges
#nosepokes_objects <- nosepokes_objects[c(15,17,18,16,13,6,5,8,7,10,9,12,11,14,3,4,1,2)]
nosepokes_objects[!is.na(nosepokes_objects)] -> nosepokes_objects  

# Create a vector of column names for the results data frame
column_names <- c("Animal", paste0(rep(gsub("_nosepokes","",nosepokes_objects), each = 2), c("_Total_NumberRepetitiveBehavior", "_Total_FrequencyRepetitiveBehavior")))

# Create the results data frame
results <- setNames(data.frame(matrix(ncol = length(column_names), nrow = 0)), column_names)

# Iterate through all the animals
for (animal in all_animals) {
  # Create a list to store the counts for each challenge object
  counts <- list(animal)
  
  # Iterate through all the challenges
  for (nosepokes_object in nosepokes_objects) {
    # Get the current challenge
    nosepokes_df <- get(nosepokes_object)
    
    # Subset the data frame based on each individual animal
    subset_df <- subset(nosepokes_df, Animal == animal)
    if (nrow(subset_df) < 1){
      total_number_repetitive <- NA
      total_frequency_repetitive <- NA
    } else {
      total_number_repetitive <- 0
      total_frequency_repetitive <- 0
      
      # Calculate number and frequency of repetitive behaviors
      for (visit in unique(subset_df$VisitID)){
        visit_np_df <- subset(subset_df,VisitID == visit)
        if (nrow(visit_np_df) < 2){
          next()
        } else if (visit_np_df[1,"LickNumber"] > 0){
          total_number_repetitive <- total_number_repetitive+sum(visit_np_df$LickNumber == 0)
          total_frequency_repetitive <- total_frequency_repetitive+ifelse(sum(visit_np_df$LickNumber == 0) > 0,1,0)
        } else {
          total_number_repetitive <- total_number_repetitive+(sum(visit_np_df$LickNumber == 0)-1)
          total_frequency_repetitive <- total_frequency_repetitive+ifelse(sum(visit_np_df$LickNumber == 0) > 0,1,0)
        }
      }
    }
    # Add the counts to the counts list
    counts[[length(counts) + 1]] <- total_number_repetitive
    counts[[length(counts) + 1]] <- total_frequency_repetitive
  }
  # Add a row with the counts to the results data frame
  results[nrow(results) + 1,] <- counts
}
rm(animal,total_number_repetitive,total_frequency_repetitive,counts,nosepokes_df,subset_df,nosepokes_object,nosepokes_objects,visit)

# Merge with the actual results dataframe
results_df <- merge(results_df,results,by.x = 1,by.y = 1,all.x = T)

#### Repetitive Behavior - Frequency and number - Dark ####
# Get the names of all objects that will be used for this variable
nosepokes_objects <- ls(pattern = "_nosepokes$")
# Reorder to follow the challenges
#nosepokes_objects <- nosepokes_objects[c(15,17,18,16,13,6,5,8,7,10,9,12,11,14,3,4,1,2)]
nosepokes_objects[!is.na(nosepokes_objects)] -> nosepokes_objects  

# Create a vector of column names for the results data frame
column_names <- c("Animal", paste0(rep(gsub("_nosepokes","",nosepokes_objects), each = 2), c("_Dark_NumberRepetitiveBehavior", "_Dark_FrequencyRepetitiveBehavior")))

# Create the results data frame
results <- setNames(data.frame(matrix(ncol = length(column_names), nrow = 0)), column_names)

# Iterate through all the animals
for (animal in all_animals) {
  # Create a list to store the counts for each challenge object
  counts <- list(animal)
  
  # Iterate through all the challenges
  for (nosepokes_object in nosepokes_objects) {
    # Get the current challenge
    nosepokes_df <- get(nosepokes_object)
    
    # Subset the data frame based on each individual animal
    subset_df <- subset(nosepokes_df, Animal == animal & Light_Status == "Dark")
    if (nrow(subset_df) < 1){
      dark_number_repetitive <- NA
      dark_frequency_repetitive <- NA
    } else {
      dark_number_repetitive <- 0
      dark_frequency_repetitive <- 0
      
      # Calculate number and frequency of repetitive behaviors
      for (visit in unique(subset_df$VisitID)){
        visit_np_df <- subset(subset_df,VisitID == visit)
        if (nrow(visit_np_df) < 2){
          next()
        } else if (visit_np_df[1,"LickNumber"] > 0){
          dark_number_repetitive <- dark_number_repetitive+sum(visit_np_df$LickNumber == 0)
          dark_frequency_repetitive <- dark_frequency_repetitive+ifelse(sum(visit_np_df$LickNumber == 0) > 0,1,0)
        } else {
          dark_number_repetitive <- dark_number_repetitive+(sum(visit_np_df$LickNumber == 0)-1)
          dark_frequency_repetitive <- dark_frequency_repetitive+ifelse(sum(visit_np_df$LickNumber == 0) > 0,1,0)
        }
      }
    }
    # Add the counts to the counts list
    counts[[length(counts) + 1]] <- dark_number_repetitive
    counts[[length(counts) + 1]] <- dark_frequency_repetitive
  }
  # Add a row with the counts to the results data frame
  results[nrow(results) + 1,] <- counts
}
rm(animal,dark_number_repetitive,dark_frequency_repetitive,counts,nosepokes_df,subset_df,nosepokes_object,nosepokes_objects,visit)

# Merge with the actual results dataframe
results_df <- merge(results_df,results,by.x = 1,by.y = 1,all.x = T)

#### Place Error - Total ####
# Get the names of all objects that will be used for this variable
visits_objects <- ls(pattern = "_visits$")
# Reorder to follow the challenges
#visits_objects <- visits_objects[c(3,4)]
visits_objects[!is.na(visits_objects)] -> visits_objects  

# Create a vector of column names for the results data frame
column_names <- c("Animal", paste0(rep(gsub("_visits","",visits_objects), each = 1), c("_Total_PlaceErrorNonAdjusted")))

# Create the results data frame
results <- setNames(data.frame(matrix(ncol = length(column_names), nrow = 0)), column_names)

# Iterate through all the animals
for (animal in all_animals) {
  # Create a list to store the counts for each challenge object
  counts <- list(animal)
  
  # Iterate through all the challenges
  for (visits_object in visits_objects) {
    # Get the current challenge
    visits_df <- get(visits_object)
    
    # Subset the data frame based on each individual animal
    subset_df <- subset(visits_df, Animal == animal)
    row.names(subset_df) <- NULL
    # Clean dataframe for Patrolling 1
    if (visits_object == "CW1_visits"){
      subset_df[subset_df$LickNumber > 0,] -> holding
      subset_df[which(subset_df$VisitID >= holding[1,"VisitID"]),] -> subset_df
      subset_df[grepl("start corner",subset_df$Module),"CornerCondition"] <- "Correct"
    }
    # Calculate Place Error
    total_place_error <- nrow(subset_df[which(subset_df$NosepokeNumber > 0 & subset_df$CornerCondition == "Incorrect"),])/nrow(subset_df[which(subset_df$NosepokeNumber > 0),])
    if (nrow(subset_df) < 1){
      total_place_error <- NA
    }
    # Add the counts to the counts list
    counts[[length(counts) + 1]] <- total_place_error
  }
  # Add a row with the counts to the results data frame
  results[nrow(results) + 1,] <- counts
}
rm(total_place_error,counts,visits_df,subset_df,visits_object,visits_objects,holding)

# Merge with the actual results dataframe
results_df <- merge(results_df,results,by.x = 1,by.y = 1,all.x = T)

#### Place Error - Dark ####
# Get the names of all objects that will be used for this variable
visits_objects <- ls(pattern = "_visits$")
# Reorder to follow the challenges
#visits_objects <- visits_objects[c(15,17,18,16,13,5,6,7,8,9,10,11,12,14,3,4,1,2)]
# Create a vector of column names for the results data frame
column_names <- c("Animal", paste0(rep(gsub("_visits","",visits_objects), each = 1), c("_Dark_PlaceErrorNonAdjusted")))

# Create the results data frame
results <- setNames(data.frame(matrix(ncol = length(column_names), nrow = 0)), column_names)

# Iterate through all the animals
for (animal in all_animals) {
  # Create a list to store the counts for each challenge object
  counts <- list(animal)
  
  # Iterate through all the challenges
  for (visits_object in visits_objects) {
    # Get the current challenge
    visits_df <- get(visits_object)
    # Subset the data frame based on each individual animal
    subset_df <- subset(visits_df, Animal == animal)
    row.names(subset_df) <- NULL
    # Clean dataframe for Patrolling 1
    if (visits_object == "CW1_visits"){
      subset_df[subset_df$LickNumber > 0,] -> holding
      subset_df[which(subset_df$VisitID >= holding[1,"VisitID"]),] -> subset_df
      subset_df[grepl("start corner",subset_df$Module),"CornerCondition"] <- "Correct"
    }
    # Calculate Place Error
    dark_place_error <- nrow(subset_df[which(subset_df$NosepokeNumber > 0 & subset_df$CornerCondition == "Incorrect"),])/nrow(subset_df[which(subset_df$NosepokeNumber > 0),])
    if (nrow(subset_df) < 1){
      dark_place_error <- NA
    }
    # Add the counts to the counts list
    counts[[length(counts) + 1]] <- dark_place_error
  }
  # Add a row with the counts to the results data frame
  results[nrow(results) + 1,] <- counts
}
rm(animal,dark_place_error,counts,visits_df,subset_df,visits_object,visits_objects,holding)

# Merge with the actual results dataframe
results_df <- merge(results_df,results,by.x = 1,by.y = 1,all.x = T)

write.xlsx(results_df,paste("processedICs_allData_",project,".xlsx",sep=""),colNames = T,rowNames = F,keepNA = F)

#### Statistical Analysis ####
# The IntelliR script currently supports analysis for 2, 3 or 4 groups
# It detects how many groups are being used through the variables group"NUMBER"_name
# The script doesn't require any special formatting and can be applied to different variables without issues
# You only need the table to start with a column named ID and second named Group.

if(length(grep("^group[0-9]+_name$", ls(envir = globalenv()))) == 2){
  # Create a vector of column names for the results data frame
  column_names <- c("Variable",paste("Number_Animals_",group1_name,sep = ""),paste("Number_Animals_",group2_name,sep = ""),paste("Mean_",group1_name,sep = ""),paste("SD_",group1_name,sep = ""),paste("Mean_",group2_name,sep = ""),paste("SD_",group2_name,sep = ""),paste("Shapiro_",group1_name,sep = ""),paste("Shapiro_",group2_name,sep = ""),"Levene","Statistical test","p-value","Statistic name","Statistic Value","Effect size test","Effect size","CI High","CI Low")
  factor(results_df[,"Group"],levels = c(group1_name,group2_name)) -> group
  factor(results_df[,"Group"],levels = c(group1_name,group2_name)) -> results_df[,"Group"]
  # Create the results data frame
  results_stats <- setNames(data.frame(matrix(ncol = length(column_names), nrow = 0)), column_names)
  for (variable in 3:ncol(results_df)){
    counts <- list(colnames(results_df)[variable])
    # Taking into account when there is no variability for all groups
    if (all(results_df[,variable][!is.na(results_df[,variable])] == results_df[,variable][!is.na(results_df[,variable])][1]) == TRUE){
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
    } else {
      # For when a group is not represented at all for a specific variable
      if(all(is.na(results_df[which(results_df$Group == group1_name),variable]))){
        counts <- list(colnames(results_df)[variable])
        counts[[length(counts) + 1]] <- "Please check this variable, there is an issue with your groups"
        results_stats[nrow(results_stats) + 1,] <- counts
        rm(counts)
        next()
      }
      if(all(is.na(results_df[which(results_df$Group == group2_name),variable]))){
        counts <- list(colnames(results_df)[variable])
        counts[[length(counts) + 1]] <- "Please check this variable, there is an issue with your groups"
        results_stats[nrow(results_stats) + 1,] <- counts
        rm(counts)
        next()
      }
      
      # Number of animals group A
      counts[[length(counts) + 1]] <- nrow(results_df[which(results_df$Group == group1_name & !is.na(results_df[,variable])),])
      # Number of animals group B
      counts[[length(counts) + 1]] <- nrow(results_df[which(results_df$Group == group2_name & !is.na(results_df[,variable])),])
      # Mean group A
      counts[[length(counts) + 1]] <- mean(results_df[which(results_df$Group == group1_name & !is.na(results_df[,variable])),variable])
      # SD group A
      counts[[length(counts) + 1]] <- sd(results_df[which(results_df$Group == group1_name & !is.na(results_df[,variable])),variable])
      # Mean group B
      counts[[length(counts) + 1]] <- mean(results_df[which(results_df$Group == group2_name & !is.na(results_df[,variable])),variable])
      # SD group B
      counts[[length(counts) + 1]] <- sd(results_df[which(results_df$Group == group2_name & !is.na(results_df[,variable])),variable])
      
      # Taking into account when there is no variability, but group by group
      # Also tests normality for each group and variance for the variable of interest
      if(all(results_df[which(results_df$Group == group1_name),variable] == results_df[which(results_df$Group == group1_name & !is.na(results_df[,variable])),variable][1])){
        counts[[length(counts) + 1]] <- 1
      } else {
        counts[[length(counts) + 1]] <- shapiro.test(results_df[which(results_df$Group == group1_name),variable])$p.value
      }
      if(all(results_df[which(results_df$Group == group2_name),variable] == results_df[which(results_df$Group == group2_name & !is.na(results_df[,variable])),variable][1])){
        counts[[length(counts) + 1]] <- 1
      } else {
        counts[[length(counts) + 1]] <- shapiro.test(results_df[which(results_df$Group == group2_name),variable])$p.value
      }  
      counts[[length(counts) + 1]] <- leveneTest(results_df[,variable] ~ results_df[,"Group"])$`Pr(>F)`[1]
      # Choosing the appropriate statistical test
      # In addition to the p-value, test statistic and effect sizes are also registered
      if (counts[[8]] > 0.05 & counts [[9]] > 0.05){
        if  (counts[[10]] > 0.05){
          # The variable is normally distributed and the variance homogeneous
          counts[[length(counts) + 1]] <- "Two Sample t-test"
          counts[[length(counts) + 1]] <- t.test(results_df[,variable] ~ results_df[,"Group"],var.equal = T)$p.value
          counts[[length(counts) + 1]] <- "T statistic"
          counts[[length(counts) + 1]] <- t.test(results_df[,variable] ~ results_df[,"Group"],var.equal = T)$statistic
          counts[[length(counts) + 1]] <- "Cohen's d"
          counts[[length(counts) + 1]] <- effectsize::cohens_d(results_df[,variable] ~ results_df[,"Group"], data = results_df)$Cohens_d
          counts[[length(counts) + 1]] <- effectsize::cohens_d(results_df[,variable] ~ results_df[,"Group"], data = results_df)$CI_high
          counts[[length(counts) + 1]] <- effectsize::cohens_d(results_df[,variable] ~ results_df[,"Group"], data = results_df)$CI_low
        } else {
          # The variable is normally distributed and the variance heterogeneous
          counts[[length(counts) + 1]] <- "Welch Two Sample t-test"
          counts[[length(counts) + 1]] <- t.test(results_df[,variable] ~ results_df[,"Group"],var.equal = F)$p.value
          counts[[length(counts) + 1]] <- "T statistic"
          counts[[length(counts) + 1]] <- t.test(results_df[,variable] ~ results_df[,"Group"],var.equal = T)$statistic
          counts[[length(counts) + 1]] <- "Cohen's d"
          counts[[length(counts) + 1]] <- effectsize::cohens_d(results_df[,variable] ~ results_df[,"Group"], data = results_df,pooled_sd = F)$Cohens_d
          counts[[length(counts) + 1]] <- effectsize::cohens_d(results_df[,variable] ~ results_df[,"Group"], data = results_df,pooled_sd = F)$CI_high
          counts[[length(counts) + 1]] <- effectsize::cohens_d(results_df[,variable] ~ results_df[,"Group"], data = results_df,pooled_sd = F)$CI_low
        }
      } else {
        if (counts[[10]] > 0.05){
          # The variable is not normally distributed and the variance homogeneous
          counts[[length(counts) + 1]] <- "Wilcoxon rank sum test with continuity correction"
          counts[[length(counts) + 1]] <- wilcox.test(results_df[,variable] ~ results_df[,"Group"])$p.value
          counts[[length(counts) + 1]] <- "W statistic"
          counts[[length(counts) + 1]] <- wilcox.test(results_df[,variable] ~ results_df[,"Group"])$statistic
          counts[[length(counts) + 1]] <- "Cliff's delta"
          effectsize::cliffs_delta(results_df[,variable] ~ results_df[,"Group"]) -> hold_wc
          counts[[length(counts) + 1]] <- hold_wc$r_rank_biserial       
          counts[[length(counts) + 1]] <- hold_wc$CI_high
          counts[[length(counts) + 1]] <- hold_wc$CI_low
        } else {
          # The variable is not normally distributed and the variance heterogeneous
          counts[[length(counts) + 1]] <- "Wilcoxon rank sum test with continuity correction - Heterogeneous variance"
          counts[[length(counts) + 1]] <- wilcox.test(results_df[,variable] ~ results_df[,"Group"])$p.value
          counts[[length(counts) + 1]] <- "W statistic"
          counts[[length(counts) + 1]] <- wilcox.test(results_df[,variable] ~ results_df[,"Group"])$statistic
          counts[[length(counts) + 1]] <- "Cliff's delta"
          effectsize::cliffs_delta(results_df[,variable] ~ results_df[,"Group"]) -> hold_wc
          counts[[length(counts) + 1]] <- hold_wc$r_rank_biserial       
          counts[[length(counts) + 1]] <- hold_wc$CI_high
          counts[[length(counts) + 1]] <- hold_wc$CI_low
        }
      }
    }
    results_stats[nrow(results_stats) + 1,] <- counts
    rm(counts,hold_wc)
  }
} else if (length(grep("^group[0-9]+_name$", ls(envir = globalenv()))) == 3){
  # 3 groups
  # Create a vector of column names for the results data frame
  column_names <- c("Variable",paste("Number_Animals_",group1_name,sep = ""),paste("Number_Animals_",group2_name,sep = ""),paste("Number_Animals_",group3_name,sep = ""),paste("Mean_",group1_name,sep = ""),paste("SD_",group1_name,sep = ""),paste("Mean_",group2_name,sep = ""),paste("SD_",group2_name,sep = ""),paste("Mean_",group3_name,sep = ""),paste("SD_",group3_name,sep = ""),paste("Shapiro_",group1_name,sep = ""),paste("Shapiro_",group2_name,sep = ""),paste("Shapiro_",group3_name,sep = ""),"Levene","Statistical test","p-value","Statistic name","Statistic Value","Effect size test","Effect size","CI High","CI Low","Post-hoc test","Post-hoc statistic",paste(group1_name," vs ",group2_name," p-value",sep = ""),paste(group1_name," vs ",group2_name," statistic",sep = ""),paste(group1_name," vs ",group3_name," p-value",sep = ""),paste(group1_name," vs ",group3_name," statistic",sep = ""),paste(group2_name," vs ",group3_name," p-value",sep = ""),paste(group2_name," vs ",group3_name," statistic",sep = ""),"Effect size test post-hoc",paste("Effect size ",group1_name," vs ",group2_name,sep = ""),paste("CI High ",group1_name," vs ",group2_name,sep = ""),paste("CI Low ",group1_name," vs ",group2_name,sep = ""),paste("Effect size ",group1_name," vs ",group3_name,sep = ""),paste("CI High ",group1_name," vs ",group3_name,sep=""),paste("CI Low ",group1_name," vs ",group3_name,sep = ""),paste("Effect size ",group2_name," vs ",group3_name,sep = ""),paste("CI High ",group2_name," vs ",group3_name,sep = ""),paste("CI Low ",group2_name," vs ",group3_name,sep = ""))
  # Create the results data frame
  results_stats <- setNames(data.frame(matrix(ncol = length(column_names), nrow = 0)), column_names)
  for (variable in 3:ncol(results_df)){
    ## Unfortunately the function for Conover-Iman test ignores the names of groups, so a workaround is required for the names of the groups
    # First section guarantees that the comparisons are done in the correct order
    results_df -> bckp
    results_df[which(results_df$Group == group1_name),"Group"] <- "A"
    results_df[which(results_df$Group == group2_name),"Group"] <- "B"
    results_df[which(results_df$Group == group3_name),"Group"] <- "C"
    factor(results_df[,"Group"]) -> group
    factor(results_df[,"Group"]) -> results_df[,"Group"]
    counts <- list(colnames(results_df)[variable])
    # Taking into account when there is no variability for all groups
    if (all(results_df[,variable][!is.na(results_df[,variable])] == results_df[,variable][!is.na(results_df[,variable])][1]) == TRUE){
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
    } else {
      # General availability check to ensure we can proceed
      if(all(is.na(results_df[which(results_df$Group == "A"),variable]))){
        counts <- list(colnames(results_df)[variable])
        counts[[length(counts) + 1]] <- "Please check this variable, there is an issue with your groups"
        results_stats[nrow(results_stats) + 1,] <- counts
        rm(counts)
        next()
      }
      if(all(is.na(results_df[which(results_df$Group == "B"),variable]))){
        counts <- list(colnames(results_df)[variable])
        counts[[length(counts) + 1]] <- "Please check this variable, there is an issue with your groups"
        results_stats[nrow(results_stats) + 1,] <- counts
        rm(counts)
        next()
      }
      if(all(is.na(results_df[which(results_df$Group == "C"),variable]))){
        counts <- list(colnames(results_df)[variable])
        counts[[length(counts) + 1]] <- "Please check this variable, there is an issue with your groups"
        results_stats[nrow(results_stats) + 1,] <- counts
        rm(counts)
        next()
      }
      
      # Number of animals group A
      counts[[length(counts) + 1]] <- nrow(results_df[which(results_df$Group == "A" & !is.na(results_df[,variable])),])
      # Number of animals group B
      counts[[length(counts) + 1]] <- nrow(results_df[which(results_df$Group == "B" & !is.na(results_df[,variable])),])
      # Number of animals group C
      counts[[length(counts) + 1]] <- nrow(results_df[which(results_df$Group == "C" & !is.na(results_df[,variable])),])
      # Mean group A
      counts[[length(counts) + 1]] <- mean(results_df[which(results_df$Group == "A" & !is.na(results_df[,variable])),variable])
      # SD group A
      counts[[length(counts) + 1]] <- sd(results_df[which(results_df$Group == "A" & !is.na(results_df[,variable])),variable])
      # Mean group B
      counts[[length(counts) + 1]] <- mean(results_df[which(results_df$Group == "B" & !is.na(results_df[,variable])),variable])
      # SD group B
      counts[[length(counts) + 1]] <- sd(results_df[which(results_df$Group == "B" & !is.na(results_df[,variable])),variable])
      # Mean group C
      counts[[length(counts) + 1]] <- mean(results_df[which(results_df$Group == "C" & !is.na(results_df[,variable])),variable])
      # SD group C
      counts[[length(counts) + 1]] <- sd(results_df[which(results_df$Group == "C" & !is.na(results_df[,variable])),variable])
      
      # Testing normality for each group  and variance
      if(all(results_df[which(results_df$Group == "A"),variable] == results_df[which(results_df$Group == "A" & !is.na(results_df[,variable])),variable][1])){
        counts[[length(counts) + 1]] <- 1
      } else {
        counts[[length(counts) + 1]] <- shapiro.test(results_df[which(results_df$Group == "A"),variable])$p.value
      }
      if(all(results_df[which(results_df$Group == "B"),variable] == results_df[which(results_df$Group == "B" & !is.na(results_df[,variable])),variable][1])){
        counts[[length(counts) + 1]] <- 1
      } else {
        counts[[length(counts) + 1]] <- shapiro.test(results_df[which(results_df$Group == "B"),variable])$p.value
      }
      if(all(results_df[which(results_df$Group == "C"),variable] == results_df[which(results_df$Group == "C" & !is.na(results_df[,variable])),variable][1])){
        counts[[length(counts) + 1]] <- 1
      } else {
        counts[[length(counts) + 1]] <- shapiro.test(results_df[which(results_df$Group == "C"),variable])$p.value
      }
      # Testing equality of data variance
      counts[[length(counts) + 1]] <- leveneTest(results_df[,variable] ~ results_df[,"Group"])$`Pr(>F)`[1]
      # Choosing the appropriate statistical test
      # In addition to the p-value, test statistic and effect sizes are also registered whenever possible
      # Same applies to the post-hoc tests
      if (counts[[11]] > 0.05 & counts [[12]] > 0.05 & counts [[13]] > 0.05){
        if  (counts[[14]] > 0.05){
          # If all groups are normally distributed and the variance equal
          counts[[length(counts) + 1]] <- "One-way ANOVA with equal variances"
          model <- aov(results_df[,variable] ~ Group, data = results_df)
          model_summary <- summary(model)  
          counts[[length(counts) + 1]] <- model_summary[[1]][["Pr(>F)"]][1]
          counts[[length(counts) + 1]] <- "F value"
          counts[[length(counts) + 1]] <- model_summary[[1]][["F value"]][1]
          # Effect size for the group comparison test
          counts[[length(counts) + 1]] <- "Omega Squared"
          counts[[length(counts) + 1]] <-  omega_squared(oneway.test(results_df[,variable]~results_df[,"Group"], data=results_df, na.action=na.omit, var.equal=TRUE))$Omega2
          counts[[length(counts) + 1]] <- omega_squared(oneway.test(results_df[,variable]~results_df[,"Group"], data=results_df, na.action=na.omit, var.equal=TRUE))$CI_high
          counts[[length(counts) + 1]] <- omega_squared(oneway.test(results_df[,variable]~results_df[,"Group"], data=results_df, na.action=na.omit, var.equal=TRUE))$CI_low
          # Perform Tukey's HSD as the post-hoc test extracting p-values and HSD statistic
          tukey_results <- TukeyHSD(model)
          counts[[length(counts) + 1]] <- "Tukey's HSD"
          counts[[length(counts) + 1]] <- "HSD statistic"
          counts[[length(counts) + 1]] <- tukey_results$Group[, "p adj"][1]
          counts[[length(counts) + 1]] <- tukey_results$Group[, "diff"][1]
          counts[[length(counts) + 1]] <- tukey_results$Group[, "p adj"][2]
          counts[[length(counts) + 1]] <- tukey_results$Group[, "diff"][2]
          counts[[length(counts) + 1]] <- tukey_results$Group[, "p adj"][3]
          counts[[length(counts) + 1]] <- tukey_results$Group[, "diff"][3]
          # Obtain effect sizes for the post-hoc test
          counts[[length(counts) + 1]] <- "Cohen's d"
          results_df[grepl("A|B",results_df$Group),] -> ef_df
          counts[[length(counts) + 1]] <- effectsize::cohens_d(ef_df[,variable], ef_df[,"Group"], data = results_df, paired = F)$Cohens_d
          counts[[length(counts) + 1]] <- effectsize::cohens_d(ef_df[,variable], ef_df[,"Group"], data = results_df, paired = F)$CI_high
          counts[[length(counts) + 1]] <- effectsize::cohens_d(ef_df[,variable], ef_df[,"Group"], data = results_df, paired = F)$CI_low
          results_df[grepl("A|C",results_df$Group),] -> ef_df
          counts[[length(counts) + 1]] <- effectsize::cohens_d(ef_df[,variable], ef_df[,"Group"], data = results_df, paired = F)$Cohens_d
          counts[[length(counts) + 1]] <- effectsize::cohens_d(ef_df[,variable], ef_df[,"Group"], data = results_df, paired = F)$CI_high
          counts[[length(counts) + 1]] <- effectsize::cohens_d(ef_df[,variable], ef_df[,"Group"], data = results_df, paired = F)$CI_low
          results_df[grepl("B|C",results_df$Group),] -> ef_df
          counts[[length(counts) + 1]] <- effectsize::cohens_d(ef_df[,variable], ef_df[,"Group"], data = results_df, paired = F)$Cohens_d
          counts[[length(counts) + 1]] <- effectsize::cohens_d(ef_df[,variable], ef_df[,"Group"], data = results_df, paired = F)$CI_high
          counts[[length(counts) + 1]] <- effectsize::cohens_d(ef_df[,variable], ef_df[,"Group"], data = results_df, paired = F)$CI_low
        } else {
          # If all groups are normally distributed and the variance unequal
          counts[[length(counts) + 1]] <- "One-way ANOVA with unequal variances"
          counts[[length(counts) + 1]] <- oneway.test(results_df[,variable]~results_df[,"Group"], data=results_df, na.action=na.omit, var.equal=FALSE)$p.value
          counts[[length(counts) + 1]] <- "F value"
          counts[[length(counts) + 1]] <- as.numeric(oneway.test(results_df[,variable]~results_df[,"Group"], data=results_df, na.action=na.omit, var.equal=FALSE)$statistic)
          # Effect size for the group comparison test
          counts[[length(counts) + 1]] <- "Omega Squared"
          counts[[length(counts) + 1]] <-  omega_squared(oneway.test(results_df[,variable]~results_df[,"Group"], data=results_df, na.action=na.omit, var.equal=TRUE))$Omega2
          counts[[length(counts) + 1]] <- omega_squared(oneway.test(results_df[,variable]~results_df[,"Group"], data=results_df, na.action=na.omit, var.equal=TRUE))$CI_high
          counts[[length(counts) + 1]] <- omega_squared(oneway.test(results_df[,variable]~results_df[,"Group"], data=results_df, na.action=na.omit, var.equal=TRUE))$CI_low
          # Perform Games Howell test as the post-hoc test extracting p-values and T-value statistic
          counts[[length(counts) + 1]] <- "Games Howell Test"
          counts[[length(counts) + 1]] <- "T-value"
          formula <- as.formula(paste(colnames(results_df)[variable], "~ Group"))
          results_df %>% games_howell_test(formula) -> comparison
          counts[[length(counts) + 1]] <- comparison$p.adj[1]
          counts[[length(counts) + 1]] <- comparison$estimate[1]
          counts[[length(counts) + 1]] <- comparison$p.adj[2]
          counts[[length(counts) + 1]] <- comparison$estimate[2]
          counts[[length(counts) + 1]] <- comparison$p.adj[3]
          counts[[length(counts) + 1]] <- comparison$estimate[3]
          # Obtain effect sizes for the post-hoc test
          counts[[length(counts) + 1]] <- "Cohen's d"
          results_df[grepl("A|B",results_df$Group),] -> ef_df
          counts[[length(counts) + 1]] <- effectsize::cohens_d(ef_df[,variable], ef_df[,"Group"], data = results_df, paired = F)$Cohens_d
          counts[[length(counts) + 1]] <- effectsize::cohens_d(ef_df[,variable], ef_df[,"Group"], data = results_df, paired = F)$CI_high
          counts[[length(counts) + 1]] <- effectsize::cohens_d(ef_df[,variable], ef_df[,"Group"], data = results_df, paired = F)$CI_low
          results_df[grepl("A|C",results_df$Group),] -> ef_df
          counts[[length(counts) + 1]] <- effectsize::cohens_d(ef_df[,variable], ef_df[,"Group"], data = results_df, paired = F)$Cohens_d
          counts[[length(counts) + 1]] <- effectsize::cohens_d(ef_df[,variable], ef_df[,"Group"], data = results_df, paired = F)$CI_high
          counts[[length(counts) + 1]] <- effectsize::cohens_d(ef_df[,variable], ef_df[,"Group"], data = results_df, paired = F)$CI_low
          results_df[grepl("B|C",results_df$Group),] -> ef_df
          counts[[length(counts) + 1]] <- effectsize::cohens_d(ef_df[,variable], ef_df[,"Group"], data = results_df, paired = F)$Cohens_d
          counts[[length(counts) + 1]] <- effectsize::cohens_d(ef_df[,variable], ef_df[,"Group"], data = results_df, paired = F)$CI_high
          counts[[length(counts) + 1]] <- effectsize::cohens_d(ef_df[,variable], ef_df[,"Group"], data = results_df, paired = F)$CI_low
        }
      } else {
        # If at least one group is not normally distributed but the variances are equal
        if (counts[[14]] > 0.05){
          counts[[length(counts) + 1]] <- "Kruskal-Wallis rank sum test"
          counts[[length(counts) + 1]] <- kruskal.test(results_df[,variable]~results_df[,"Group"], data=results_df, na.action=na.omit)$p.value
          counts[[length(counts) + 1]] <- "Kruskal-Wallis chi-square"
          counts[[length(counts) + 1]] <- as.numeric(kruskal.test(results_df[,variable]~results_df[,"Group"], data=results_df, na.action=na.omit)$statistic)
          counts[[length(counts) + 1]] <- "Epsilon-squared"
          
          # Extract the test statistic
          H <- kruskal.test(results_df[,variable]~results_df[,"Group"], data=results_df, na.action=na.omit)$statistic
          
          # Calculate the sample size
          n <- sum(table(results_df[, "Group"]),na.rm = T)
          
          # Calculate Epsilon-squared
          epsilon_squared_calc <- 1 - (n - 1) * H / (n * (n + 1))
          
          counts[[length(counts) + 1]] <- epsilon_squared_calc <- 1 - (n - 1) * H / (n * (n + 1))
          epsilon_squared_func <- function(data, indices) {
            # Extract the resampled data
            data_resampled <- data[indices, ]
            
            # Perform a Kruskal-Wallis test
            result <- kruskal.test(as.numeric(data_resampled[, variable]) ~ data_resampled[, "Group"])
            
            # Extract the test statistic
            H <- result$statistic
            
            # Calculate the sample size
            n <- sum(table(data_resampled[, "Group"]))
            
            # Calculate Epsilon-squared
            1 - (n - 1) * H / (n * (n + 1))
          }
          calculated_ep_squared <- boot(results_df,epsilon_squared_func, R = 1000)
          ci <- boot.ci(calculated_ep_squared, type = "perc")
          counts[[length(counts) + 1]] <- ci$percent[5]
          counts[[length(counts) + 1]] <- ci$percent[4]
          # Perform Conover-Iman test as the post-hoc test extracting p-values and T-value statistic
          conover.test(results_df[, variable], results_df[, "Group"], method = "bonferroni") -> comparison
          counts[[length(counts) + 1]] <- "Conover-Iman Test"
          counts[[length(counts) + 1]] <- "T-values"
          counts[[length(counts) + 1]] <- comparison$P.adjusted[1]
          counts[[length(counts) + 1]] <- comparison$T[1]
          counts[[length(counts) + 1]] <- comparison$P.adjusted[2]
          counts[[length(counts) + 1]] <- comparison$T[2]
          counts[[length(counts) + 1]] <- comparison$P.adjusted[3]
          counts[[length(counts) + 1]] <- comparison$T[3]
          # Obtain effect sizes for the post-hoc test
          counts[[length(counts) + 1]] <- "Cliff's delta"
          results_df[grepl("A|B",results_df$Group),] -> ef_df
          ef_df$Group <- droplevels(ef_df$Group)
          effectsize::cliffs_delta(ef_df[,variable] ~ ef_df[,"Group"]) -> hold_wc
          counts[[length(counts) + 1]] <- hold_wc$r_rank_biserial       
          counts[[length(counts) + 1]] <- hold_wc$CI_high
          counts[[length(counts) + 1]] <- hold_wc$CI_low
          results_df[grepl("A|C",results_df$Group),] -> ef_df
          ef_df$Group <- droplevels(ef_df$Group)
          effectsize::cliffs_delta(ef_df[,variable] ~ ef_df[,"Group"]) -> hold_wc
          counts[[length(counts) + 1]] <- hold_wc$r_rank_biserial       
          counts[[length(counts) + 1]] <- hold_wc$CI_high
          counts[[length(counts) + 1]] <- hold_wc$CI_low
          results_df[grepl("B|C",results_df$Group),] -> ef_df
          ef_df$Group <- droplevels(ef_df$Group)
          effectsize::cliffs_delta(ef_df[,variable] ~ ef_df[,"Group"]) -> hold_wc
          counts[[length(counts) + 1]] <- hold_wc$r_rank_biserial       
          counts[[length(counts) + 1]] <- hold_wc$CI_high
          counts[[length(counts) + 1]] <- hold_wc$CI_low
        } else {
          # If at least one group is not normally distributed and the variances are unequal
          counts[[length(counts) + 1]] <- "Asymptotic K-Sample Fisher-Pitman Permutation Test"
          counts[[length(counts) + 1]] <- pvalue(oneway_test(results_df[,variable]~as.factor(results_df[,"Group"])))
          counts[[length(counts) + 1]] <- "Chi-squared"
          counts[[length(counts) + 1]] <- statistic(oneway_test(results_df[,variable]~as.factor(results_df[,"Group"])))
          counts[[length(counts) + 1]] <- "No global effect size test"
          counts[[length(counts) + 1]] <- NA
          counts[[length(counts) + 1]] <- NA
          counts[[length(counts) + 1]] <- NA
          # fit a linear model
          factor(results_df[,"Group"]) -> group
          fit <- lm(results_df[,variable]~ group, data=results_df)
          # specify the contrasts
          contrasts <- rbind(c(-1, 1, 0),
                             c(-1, 0, 1),
                             c(0, -1, 1))
          
          # Assign row names
          rownames(contrasts) <- c(paste0("A"," vs ","B"),
                                   paste0("A"," vs ","C"),
                                   paste0("B"," vs ","C"))
          
          # Perform a contrast test as the post-hoc test extracting p-values and T-value statistic
          summary(glht(fit, linfct = mcp(group = contrasts), alternative = "two.sided"), test = adjusted(type = "bonferroni")) -> comparison
          counts[[length(counts) + 1]] <- "Simultaneous Tests for General Linear Hypotheses - Multiple Comparisons of Means"
          counts[[length(counts) + 1]] <- "T-values"
          counts[[length(counts) + 1]] <- comparison$test$pvalues[1]
          counts[[length(counts) + 1]] <- comparison$test$tstat[1]
          counts[[length(counts) + 1]] <- comparison$test$pvalues[2]
          counts[[length(counts) + 1]] <- comparison$test$tstat[2]
          counts[[length(counts) + 1]] <- comparison$test$pvalues[3]
          counts[[length(counts) + 1]] <- comparison$test$tstat[3]
          # Calculating effect sizes for the post-hoc comparisons
          (em <- emmeans(fit, specs = ~ group))
          eff_size(em, sigma = sigma(fit), edf = df.residual(fit)) -> hold_eff
          hold_eff <- summary(hold_eff)
          counts[[length(counts) + 1]] <- "Standardized mean difference between groups - Hedges'g"
          counts[[length(counts) + 1]] <- hold_eff[1,2]
          counts[[length(counts) + 1]] <- hold_eff[1,6]
          counts[[length(counts) + 1]] <- hold_eff[1,5]
          counts[[length(counts) + 1]] <- hold_eff[2,2]
          counts[[length(counts) + 1]] <- hold_eff[2,6]
          counts[[length(counts) + 1]] <- hold_eff[2,5]
          counts[[length(counts) + 1]] <- hold_eff[3,2]
          counts[[length(counts) + 1]] <- hold_eff[3,6]
          counts[[length(counts) + 1]] <- hold_eff[3,5]
        }
      }
    }
    results_stats[nrow(results_stats) + 1,] <- counts
    bckp -> results_df
    rm(counts,fit,contrasts,comparison)
  }
} else {
  # 4 groups
  # Create a vector of column names for the results data frame
  column_names <- c("Variable",paste("Number_Animals_",group1_name,sep = ""),paste("Number_Animals_",group2_name,sep = ""),paste("Number_Animals_",group3_name,sep = ""),paste("Number_Animals_",group4_name,sep = ""),paste("Mean_",group1_name,sep = ""),paste("SD_",group1_name,sep = ""),paste("Mean_",group2_name,sep = ""),paste("SD_",group2_name,sep = ""),paste("Mean_",group3_name,sep = ""),paste("SD_",group3_name,sep = ""),paste("Mean_",group4_name,sep = ""),paste("SD_",group4_name,sep = ""),paste("Shapiro_",group1_name,sep = ""),paste("Shapiro_",group2_name,sep = ""),paste("Shapiro_",group3_name,sep = ""),paste("Shapiro_",group4_name,sep = ""),"Levene","Statistical test","p-value","Statistic name","Statistic Value","Effect size test","Effect size","CI High","CI Low","Post-hoc test","Post-hoc statistic",paste(group1_name," vs ",group2_name," p-value",sep = ""),paste(group1_name," vs ",group2_name," statistic",sep = ""),paste(group1_name," vs ",group3_name," p-value",sep = ""),paste(group1_name," vs ",group3_name," statistic",sep = ""),paste(group1_name," vs ",group4_name," p-value",sep = ""),paste(group1_name," vs ",group4_name," statistic",sep = ""),paste(group2_name," vs ",group3_name," p-value",sep = ""),paste(group2_name," vs ",group3_name," statistic",sep = ""),paste(group2_name," vs ",group4_name," p-value",sep = ""),paste(group2_name," vs ",group4_name," statistic",sep = ""),paste(group3_name," vs ",group4_name," p-value",sep = ""),paste(group3_name," vs ",group4_name," statistic",sep = ""),paste("CI High ",group1_name," vs ",group3_name,sep=""),paste("CI Low ",group1_name," vs ",group3_name,sep = ""),paste("Effect size ",group1_name," vs ",group4_name,sep = ""),paste("CI High ",group1_name," vs ",group4_name,sep = ""),paste("CI Low ",group1_name," vs ",group4_name,sep = ""),paste("Effect size ",group2_name," vs ",group3_name,sep = ""),paste("CI High ",group2_name," vs ",group3_name,sep = ""),paste("CI Low ",group2_name," vs ",group3_name,sep = ""),paste("Effect size ",group2_name," vs ",group4_name,sep = ""),paste("CI High ",group2_name," vs ",group4_name,sep = ""),paste("CI Low ",group2_name," vs ",group4_name,sep = ""),paste("Effect size ",group3_name," vs ",group4_name,sep = ""),paste("CI High ",group3_name," vs ",group4_name,sep = ""),paste("CI Low ",group3_name," vs ",group4_name,sep = ""))
  # Create the results data frame
  results_stats <- setNames(data.frame(matrix(ncol = length(column_names), nrow = 0)), column_names)
  for (variable in 3:ncol(results_df)){
    ## Unfortunately the function for Conover-Iman test ignores the names of groups, so a workaround is required for the names of the groups
    # First section guarantees that the comparisons are done in the correct order
    results_df -> bckp
    results_df[which(results_df$Group == group1_name),"Group"] <- "A"
    results_df[which(results_df$Group == group2_name),"Group"] <- "B"
    results_df[which(results_df$Group == group3_name),"Group"] <- "C"
    results_df[which(results_df$Group == group4_name),"Group"] <- "D"
    factor(results_df[,"Group"]) -> group
    factor(results_df[,"Group"]) -> results_df[,"Group"]
    counts <- list(colnames(results_df)[variable])
    # Taking into account when there is no variability for all groups
    if (all(results_df[,variable][!is.na(results_df[,variable])] == results_df[,variable][!is.na(results_df[,variable])][1]) == TRUE){
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
      counts[[length(counts) + 1]] <- NA
    } else {
      # General availability check to ensure we can proceed
      if(all(is.na(results_df[which(results_df$Group == "A"),variable]))){
        counts <- list(colnames(results_df)[variable])
        counts[[length(counts) + 1]] <- "Please check this variable, there is an issue with your groups"
        results_stats[nrow(results_stats) + 1,] <- counts
        rm(counts)
        next()
      }
      if(all(is.na(results_df[which(results_df$Group == "B"),variable]))){
        counts <- list(colnames(results_df)[variable])
        counts[[length(counts) + 1]] <- "Please check this variable, there is an issue with your groups"
        results_stats[nrow(results_stats) + 1,] <- counts
        rm(counts)
        next()
      }
      if(all(is.na(results_df[which(results_df$Group == "C"),variable]))){
        counts <- list(colnames(results_df)[variable])
        counts[[length(counts) + 1]] <- "Please check this variable, there is an issue with your groups"
        results_stats[nrow(results_stats) + 1,] <- counts
        rm(counts)
        next()
      }
      if(all(is.na(results_df[which(results_df$Group == "D"),variable]))){
        counts <- list(colnames(results_df)[variable])
        counts[[length(counts) + 1]] <- "Please check this variable, there is an issue with your groups"
        results_stats[nrow(results_stats) + 1,] <- counts
        rm(counts)
        next()
      }
      
      # Number of animals group A
      counts[[length(counts) + 1]] <- nrow(results_df[which(results_df$Group == "A" & !is.na(results_df[,variable])),])
      # Number of animals group B
      counts[[length(counts) + 1]] <- nrow(results_df[which(results_df$Group == "B" & !is.na(results_df[,variable])),])
      # Number of animals group C
      counts[[length(counts) + 1]] <- nrow(results_df[which(results_df$Group == "C" & !is.na(results_df[,variable])),])
      # Number of animals group D
      counts[[length(counts) + 1]] <- nrow(results_df[which(results_df$Group == "D" & !is.na(results_df[,variable])),])
      # Mean group A
      counts[[length(counts) + 1]] <- mean(results_df[which(results_df$Group == "A" & !is.na(results_df[,variable])),variable])
      # SD group A
      counts[[length(counts) + 1]] <- sd(results_df[which(results_df$Group == "A" & !is.na(results_df[,variable])),variable])
      # Mean group B
      counts[[length(counts) + 1]] <- mean(results_df[which(results_df$Group == "B" & !is.na(results_df[,variable])),variable])
      # SD group B
      counts[[length(counts) + 1]] <- sd(results_df[which(results_df$Group == "B" & !is.na(results_df[,variable])),variable])
      # Mean group C
      counts[[length(counts) + 1]] <- mean(results_df[which(results_df$Group == "C" & !is.na(results_df[,variable])),variable])
      # SD group C
      counts[[length(counts) + 1]] <- sd(results_df[which(results_df$Group == "C" & !is.na(results_df[,variable])),variable])
      # Mean group D
      counts[[length(counts) + 1]] <- mean(results_df[which(results_df$Group == "D" & !is.na(results_df[,variable])),variable])
      # SD group D
      counts[[length(counts) + 1]] <- sd(results_df[which(results_df$Group == "D" & !is.na(results_df[,variable])),variable])
      
      # Testing normality for each group and variance
      if(all(results_df[which(results_df$Group == "A"),variable] == results_df[which(results_df$Group == "A" & !is.na(results_df[,variable])),variable][1])){
        counts[[length(counts) + 1]] <- 1
      } else {
        counts[[length(counts) + 1]] <- shapiro.test(results_df[which(results_df$Group == "A"),variable])$p.value
      }
      if(all(results_df[which(results_df$Group == "B"),variable] == results_df[which(results_df$Group == "B" & !is.na(results_df[,variable])),variable][1])){
        counts[[length(counts) + 1]] <- 1
      } else {
        counts[[length(counts) + 1]] <- shapiro.test(results_df[which(results_df$Group == "B"),variable])$p.value
      }
      if(all(results_df[which(results_df$Group == "C"),variable] == results_df[which(results_df$Group == "C" & !is.na(results_df[,variable])),variable][1])){
        counts[[length(counts) + 1]] <- 1
      } else {
        counts[[length(counts) + 1]] <- shapiro.test(results_df[which(results_df$Group == "C"),variable])$p.value
      }
      if(all(results_df[which(results_df$Group == "D"),variable] == results_df[which(results_df$Group == "D" & !is.na(results_df[,variable])),variable][1])){
        counts[[length(counts) + 1]] <- 1
      } else {
        counts[[length(counts) + 1]] <- shapiro.test(results_df[which(results_df$Group == "D"),variable])$p.value
      }
      # Testing equality of data variance
      counts[[length(counts) + 1]] <- leveneTest(results_df[,variable] ~ results_df[,"Group"])$`Pr(>F)`[1]
      # Choosing the appropriate statistical test
      # In addition to the p-value, test statistic and effect sizes are also registered whenever possible
      # Same applies to the post-hoc tests
      if (counts[[14]] > 0.05 & counts [[15]] > 0.05 & counts [[16]] > 0.05 & counts [[17]] > 0.05){
        if  (counts[[18]] > 0.05){
          # If all groups are normally distributed and the variance equal
          
          counts[[length(counts) + 1]] <- "One-way ANOVA with equal variances"
          model <- aov(results_df[,variable] ~ Group, data = results_df)
          model_summary <- summary(model)  
          counts[[length(counts) + 1]] <- model_summary[[1]][["Pr(>F)"]][1]
          counts[[length(counts) + 1]] <- "F value"
          counts[[length(counts) + 1]] <- model_summary[[1]][["F value"]][1]
          # Effect size for the group comparison test
          counts[[length(counts) + 1]] <- "Omega Squared"
          counts[[length(counts) + 1]] <-  omega_squared(oneway.test(results_df[,variable]~results_df[,"Group"], data=results_df, na.action=na.omit, var.equal=TRUE))$Omega2
          counts[[length(counts) + 1]] <- omega_squared(oneway.test(results_df[,variable]~results_df[,"Group"], data=results_df, na.action=na.omit, var.equal=TRUE))$CI_high
          counts[[length(counts) + 1]] <- omega_squared(oneway.test(results_df[,variable]~results_df[,"Group"], data=results_df, na.action=na.omit, var.equal=TRUE))$CI_low
          # Perform Tukey's HSD test extracting p-values and HSD statistic
          tukey_results <- TukeyHSD(model)
          counts[[length(counts) + 1]] <- "Tukey's HSD"
          counts[[length(counts) + 1]] <- "HSD statistic"
          counts[[length(counts) + 1]] <- tukey_results$Group[, "p adj"][1]
          counts[[length(counts) + 1]] <- tukey_results$Group[, "diff"][1]
          counts[[length(counts) + 1]] <- tukey_results$Group[, "p adj"][2]
          counts[[length(counts) + 1]] <- tukey_results$Group[, "diff"][2]
          counts[[length(counts) + 1]] <- tukey_results$Group[, "p adj"][3]
          counts[[length(counts) + 1]] <- tukey_results$Group[, "diff"][3]
          counts[[length(counts) + 1]] <- tukey_results$Group[, "p adj"][4]
          counts[[length(counts) + 1]] <- tukey_results$Group[, "diff"][4]
          counts[[length(counts) + 1]] <- tukey_results$Group[, "p adj"][5]
          counts[[length(counts) + 1]] <- tukey_results$Group[, "diff"][5]
          counts[[length(counts) + 1]] <- tukey_results$Group[, "p adj"][6]
          counts[[length(counts) + 1]] <- tukey_results$Group[, "diff"][6]
          # Obtain effect sizes for the post-hoc test
          counts[[length(counts) + 1]] <- "Cohen's d"
          results_df[grepl("A|B",results_df$Group),] -> ef_df
          counts[[length(counts) + 1]] <- effectsize::cohens_d(ef_df[,variable], ef_df[,"Group"], data = results_df, paired = F)$Cohens_d
          counts[[length(counts) + 1]] <- effectsize::cohens_d(ef_df[,variable], ef_df[,"Group"], data = results_df, paired = F)$CI_high
          counts[[length(counts) + 1]] <- effectsize::cohens_d(ef_df[,variable], ef_df[,"Group"], data = results_df, paired = F)$CI_low
          results_df[grepl("A|C",results_df$Group),] -> ef_df
          counts[[length(counts) + 1]] <- effectsize::cohens_d(ef_df[,variable], ef_df[,"Group"], data = results_df, paired = F)$Cohens_d
          counts[[length(counts) + 1]] <- effectsize::cohens_d(ef_df[,variable], ef_df[,"Group"], data = results_df, paired = F)$CI_high
          counts[[length(counts) + 1]] <- effectsize::cohens_d(ef_df[,variable], ef_df[,"Group"], data = results_df, paired = F)$CI_low
          results_df[grepl("A|D",results_df$Group),] -> ef_df
          counts[[length(counts) + 1]] <- effectsize::cohens_d(ef_df[,variable], ef_df[,"Group"], data = results_df, paired = F)$Cohens_d
          counts[[length(counts) + 1]] <- effectsize::cohens_d(ef_df[,variable], ef_df[,"Group"], data = results_df, paired = F)$CI_high
          counts[[length(counts) + 1]] <- effectsize::cohens_d(ef_df[,variable], ef_df[,"Group"], data = results_df, paired = F)$CI_low
          results_df[grepl("B|C",results_df$Group),] -> ef_df
          counts[[length(counts) + 1]] <- effectsize::cohens_d(ef_df[,variable], ef_df[,"Group"], data = results_df, paired = F)$Cohens_d
          counts[[length(counts) + 1]] <- effectsize::cohens_d(ef_df[,variable], ef_df[,"Group"], data = results_df, paired = F)$CI_high
          counts[[length(counts) + 1]] <- effectsize::cohens_d(ef_df[,variable], ef_df[,"Group"], data = results_df, paired = F)$CI_low
          results_df[grepl("B|D",results_df$Group),] -> ef_df
          counts[[length(counts) + 1]] <- effectsize::cohens_d(ef_df[,variable], ef_df[,"Group"], data = results_df, paired = F)$Cohens_d
          counts[[length(counts) + 1]] <- effectsize::cohens_d(ef_df[,variable], ef_df[,"Group"], data = results_df, paired = F)$CI_high
          counts[[length(counts) + 1]] <- effectsize::cohens_d(ef_df[,variable], ef_df[,"Group"], data = results_df, paired = F)$CI_low
          results_df[grepl("C|D",results_df$Group),] -> ef_df
          counts[[length(counts) + 1]] <- effectsize::cohens_d(ef_df[,variable], ef_df[,"Group"], data = results_df, paired = F)$Cohens_d
          counts[[length(counts) + 1]] <- effectsize::cohens_d(ef_df[,variable], ef_df[,"Group"], data = results_df, paired = F)$CI_high
          counts[[length(counts) + 1]] <- effectsize::cohens_d(ef_df[,variable], ef_df[,"Group"], data = results_df, paired = F)$CI_low
        } else {
          # If all groups are normally distributed and the variance unequal
          counts[[length(counts) + 1]] <- "One-way ANOVA with unequal variances"
          counts[[length(counts) + 1]] <- oneway.test(results_df[,variable]~results_df[,"Group"], data=results_df, na.action=na.omit, var.equal=FALSE)$p.value
          counts[[length(counts) + 1]] <- "F value"
          counts[[length(counts) + 1]] <- as.numeric(oneway.test(results_df[,variable]~results_df[,"Group"], data=results_df, na.action=na.omit, var.equal=FALSE)$statistic)
          # Effect size for the group comparison test
          counts[[length(counts) + 1]] <- "Omega Squared"
          counts[[length(counts) + 1]] <-  omega_squared(oneway.test(results_df[,variable]~results_df[,"Group"], data=results_df, na.action=na.omit, var.equal=TRUE))$Omega2
          counts[[length(counts) + 1]] <- omega_squared(oneway.test(results_df[,variable]~results_df[,"Group"], data=results_df, na.action=na.omit, var.equal=TRUE))$CI_high
          counts[[length(counts) + 1]] <- omega_squared(oneway.test(results_df[,variable]~results_df[,"Group"], data=results_df, na.action=na.omit, var.equal=TRUE))$CI_low
          # Perform Games Howell test as the post-hoc test extracting p-values and T-value statistic
          counts[[length(counts) + 1]] <- "Games Howell Test"
          counts[[length(counts) + 1]] <- "T-value"
          formula <- as.formula(paste(colnames(results_df)[variable], "~ Group"))
          results_df %>% games_howell_test(formula) -> comparison
          counts[[length(counts) + 1]] <- comparison$p.adj[1]
          counts[[length(counts) + 1]] <- comparison$estimate[1]
          counts[[length(counts) + 1]] <- comparison$p.adj[2]
          counts[[length(counts) + 1]] <- comparison$estimate[2]
          counts[[length(counts) + 1]] <- comparison$p.adj[3]
          counts[[length(counts) + 1]] <- comparison$estimate[3]
          counts[[length(counts) + 1]] <- comparison$p.adj[4]
          counts[[length(counts) + 1]] <- comparison$estimate[4]
          counts[[length(counts) + 1]] <- comparison$p.adj[5]
          counts[[length(counts) + 1]] <- comparison$estimate[5]
          counts[[length(counts) + 1]] <- comparison$p.adj[6]
          counts[[length(counts) + 1]] <- comparison$estimate[7]
          # Obtain effect sizes for the post-hoc test
          counts[[length(counts) + 1]] <- "Cohen's d"
          results_df[grepl("A|B",results_df$Group),] -> ef_df
          counts[[length(counts) + 1]] <- effectsize::cohens_d(ef_df[,variable], ef_df[,"Group"], data = results_df, paired = F)$Cohens_d
          counts[[length(counts) + 1]] <- effectsize::cohens_d(ef_df[,variable], ef_df[,"Group"], data = results_df, paired = F)$CI_high
          counts[[length(counts) + 1]] <- effectsize::cohens_d(ef_df[,variable], ef_df[,"Group"], data = results_df, paired = F)$CI_low
          results_df[grepl("A|C",results_df$Group),] -> ef_df
          counts[[length(counts) + 1]] <- effectsize::cohens_d(ef_df[,variable], ef_df[,"Group"], data = results_df, paired = F)$Cohens_d
          counts[[length(counts) + 1]] <- effectsize::cohens_d(ef_df[,variable], ef_df[,"Group"], data = results_df, paired = F)$CI_high
          counts[[length(counts) + 1]] <- effectsize::cohens_d(ef_df[,variable], ef_df[,"Group"], data = results_df, paired = F)$CI_low
          results_df[grepl("A|D",results_df$Group),] -> ef_df
          counts[[length(counts) + 1]] <- effectsize::cohens_d(ef_df[,variable], ef_df[,"Group"], data = results_df, paired = F)$Cohens_d
          counts[[length(counts) + 1]] <- effectsize::cohens_d(ef_df[,variable], ef_df[,"Group"], data = results_df, paired = F)$CI_high
          counts[[length(counts) + 1]] <- effectsize::cohens_d(ef_df[,variable], ef_df[,"Group"], data = results_df, paired = F)$CI_low
          results_df[grepl("B|C",results_df$Group),] -> ef_df
          counts[[length(counts) + 1]] <- effectsize::cohens_d(ef_df[,variable], ef_df[,"Group"], data = results_df, paired = F)$Cohens_d
          counts[[length(counts) + 1]] <- effectsize::cohens_d(ef_df[,variable], ef_df[,"Group"], data = results_df, paired = F)$CI_high
          counts[[length(counts) + 1]] <- effectsize::cohens_d(ef_df[,variable], ef_df[,"Group"], data = results_df, paired = F)$CI_low
          results_df[grepl("B|D",results_df$Group),] -> ef_df
          counts[[length(counts) + 1]] <- effectsize::cohens_d(ef_df[,variable], ef_df[,"Group"], data = results_df, paired = F)$Cohens_d
          counts[[length(counts) + 1]] <- effectsize::cohens_d(ef_df[,variable], ef_df[,"Group"], data = results_df, paired = F)$CI_high
          counts[[length(counts) + 1]] <- effectsize::cohens_d(ef_df[,variable], ef_df[,"Group"], data = results_df, paired = F)$CI_low
          results_df[grepl("C|D",results_df$Group),] -> ef_df
          counts[[length(counts) + 1]] <- effectsize::cohens_d(ef_df[,variable], ef_df[,"Group"], data = results_df, paired = F)$Cohens_d
          counts[[length(counts) + 1]] <- effectsize::cohens_d(ef_df[,variable], ef_df[,"Group"], data = results_df, paired = F)$CI_high
          counts[[length(counts) + 1]] <- effectsize::cohens_d(ef_df[,variable], ef_df[,"Group"], data = results_df, paired = F)$CI_low
        }
      } else {
        # If at least one group is not normally distributed but the variances are equal
        if (counts[[18]] > 0.05){
          counts[[length(counts) + 1]] <- "Kruskal-Wallis rank sum test"
          counts[[length(counts) + 1]] <- kruskal.test(results_df[,variable]~results_df[,"Group"], data=results_df, na.action=na.omit)$p.value
          counts[[length(counts) + 1]] <- "Kruskal-Wallis chi-square"
          counts[[length(counts) + 1]] <- as.numeric(kruskal.test(results_df[,variable]~results_df[,"Group"], data=results_df, na.action=na.omit)$statistic)
          counts[[length(counts) + 1]] <- "Epsilon-squared"
          
          # Extract the test statistic
          H <- kruskal.test(results_df[,variable]~results_df[,"Group"], data=results_df, na.action=na.omit)$statistic
          
          # Calculate the sample size
          n <- sum(table(results_df[, "Group"]),na.rm = T)
          
          # Calculate Epsilon-squared
          epsilon_squared_calc <- 1 - (n - 1) * H / (n * (n + 1))
          
          counts[[length(counts) + 1]] <- epsilon_squared_calc <- 1 - (n - 1) * H / (n * (n + 1))
          epsilon_squared_func <- function(data, indices) {
            # Extract the resampled data
            data_resampled <- data[indices, ]
            
            # Perform a Kruskal-Wallis test
            result <- kruskal.test(as.numeric(data_resampled[, variable]) ~ data_resampled[, "Group"])
            
            # Extract the test statistic
            H <- result$statistic
            
            # Calculate the sample size
            n <- sum(table(data_resampled[, "Group"]))
            
            # Calculate Epsilon-squared
            1 - (n - 1) * H / (n * (n + 1))
          }
          calculated_ep_squared <- boot(results_df,epsilon_squared_func, R = 1000)
          ci <- boot.ci(calculated_ep_squared, type = "perc")
          counts[[length(counts) + 1]] <- ci$percent[5]
          counts[[length(counts) + 1]] <- ci$percent[4]
          # Perform Conover-Iman test as the post-hoc test extracting p-values and T-value statistic
          conover.test(results_df[,variable],results_df[,"Group"],method="bonferroni") -> comparison
          counts[[length(counts) + 1]] <- "Conover-Iman Test"
          counts[[length(counts) + 1]] <- "T-values"
          counts[[length(counts) + 1]] <- comparison$P.adjusted[1]
          counts[[length(counts) + 1]] <- comparison$T[1]
          counts[[length(counts) + 1]] <- comparison$P.adjusted[2]
          counts[[length(counts) + 1]] <- comparison$T[2]
          counts[[length(counts) + 1]] <- comparison$P.adjusted[4]
          counts[[length(counts) + 1]] <- comparison$T[4]
          counts[[length(counts) + 1]] <- comparison$P.adjusted[3]
          counts[[length(counts) + 1]] <- comparison$T[3]
          counts[[length(counts) + 1]] <- comparison$P.adjusted[5]
          counts[[length(counts) + 1]] <- comparison$T[5]
          counts[[length(counts) + 1]] <- comparison$P.adjusted[6]
          counts[[length(counts) + 1]] <- comparison$T[6]
          # Obtain effect sizes for the post-hoc test
          counts[[length(counts) + 1]] <- "Cliff's delta"
          results_df[grepl("A|B",results_df$Group),] -> ef_df
          ef_df$Group <- droplevels(ef_df$Group)
          effectsize::cliffs_delta(ef_df[,variable] ~ ef_df[,"Group"]) -> hold_wc
          counts[[length(counts) + 1]] <- hold_wc$r_rank_biserial       
          counts[[length(counts) + 1]] <- hold_wc$CI_high
          counts[[length(counts) + 1]] <- hold_wc$CI_low
          results_df[grepl("A|C",results_df$Group),] -> ef_df
          ef_df$Group <- droplevels(ef_df$Group)
          effectsize::cliffs_delta(ef_df[,variable] ~ ef_df[,"Group"]) -> hold_wc
          counts[[length(counts) + 1]] <- hold_wc$r_rank_biserial       
          counts[[length(counts) + 1]] <- hold_wc$CI_high
          counts[[length(counts) + 1]] <- hold_wc$CI_low
          results_df[grepl("A|D",results_df$Group),] -> ef_df
          ef_df$Group <- droplevels(ef_df$Group)
          effectsize::cliffs_delta(ef_df[,variable] ~ ef_df[,"Group"]) -> hold_wc
          counts[[length(counts) + 1]] <- hold_wc$r_rank_biserial       
          counts[[length(counts) + 1]] <- hold_wc$CI_high
          counts[[length(counts) + 1]] <- hold_wc$CI_low
          results_df[grepl("B|C",results_df$Group),] -> ef_df
          ef_df$Group <- droplevels(ef_df$Group)
          effectsize::cliffs_delta(ef_df[,variable] ~ ef_df[,"Group"]) -> hold_wc
          counts[[length(counts) + 1]] <- hold_wc$r_rank_biserial       
          counts[[length(counts) + 1]] <- hold_wc$CI_high
          counts[[length(counts) + 1]] <- hold_wc$CI_low
          results_df[grepl("B|D",results_df$Group),] -> ef_df
          ef_df$Group <- droplevels(ef_df$Group)
          effectsize::cliffs_delta(ef_df[,variable] ~ ef_df[,"Group"]) -> hold_wc
          counts[[length(counts) + 1]] <- hold_wc$r_rank_biserial       
          counts[[length(counts) + 1]] <- hold_wc$CI_high
          counts[[length(counts) + 1]] <- hold_wc$CI_low
          results_df[grepl("C|D",results_df$Group),] -> ef_df
          ef_df$Group <- droplevels(ef_df$Group)
          effectsize::cliffs_delta(ef_df[,variable] ~ ef_df[,"Group"]) -> hold_wc
          counts[[length(counts) + 1]] <- hold_wc$r_rank_biserial       
          counts[[length(counts) + 1]] <- hold_wc$CI_high
          counts[[length(counts) + 1]] <- hold_wc$CI_low
        } else {
          # If at least one group is not normally distributed and the variances are unequal
          counts[[length(counts) + 1]] <- "Asymptotic K-Sample Fisher-Pitman Permutation Test"
          counts[[length(counts) + 1]] <- pvalue(oneway_test(results_df[,variable]~as.factor(results_df[,"Group"])))
          counts[[length(counts) + 1]] <- "Chi-squared"
          counts[[length(counts) + 1]] <- statistic(oneway_test(results_df[,variable]~as.factor(results_df[,"Group"])))
          counts[[length(counts) + 1]] <- "No global effect size test"
          counts[[length(counts) + 1]] <- NA
          counts[[length(counts) + 1]] <- NA
          counts[[length(counts) + 1]] <- NA
          # fit a linear model
          factor(results_df[,"Group"]) -> group
          fit <- lm(results_df[,variable]~ group, data=results_df)
          # specify the contrasts
          contrasts <- rbind(c(-1, 1, 0, 0),
                             c(-1, 0, 1, 0),
                             c(-1, 0, 0, 1),
                             c(0, -1, 1, 0),
                             c(0, -1, 0, 1),
                             c(0, 0, -1, 1))
          
          # Assign row names
          rownames(contrasts) <- c(paste0("A"," vs ","B"),
                                   paste0("A"," vs ","C"),
                                   paste0("A"," vs ","D"),
                                   paste0("B"," vs ","C"),
                                   paste0("B"," vs ","D"),
                                   paste0("C"," vs ","D"))
          
          # Perform a contrast test as the post-hoc test extracting p-values and T-value statistic
          summary(glht(fit, linfct = mcp(group = contrasts), alternative = "two.sided"), test = adjusted(type = "bonferroni")) -> comparison
          counts[[length(counts) + 1]] <- "Simultaneous Tests for General Linear Hypotheses - Multiple Comparisons of Means"
          counts[[length(counts) + 1]] <- "T-values"
          counts[[length(counts) + 1]] <- comparison$test$pvalues[1]
          counts[[length(counts) + 1]] <- comparison$test$tstat[1]
          counts[[length(counts) + 1]] <- comparison$test$pvalues[2]
          counts[[length(counts) + 1]] <- comparison$test$tstat[2]
          counts[[length(counts) + 1]] <- comparison$test$pvalues[3]
          counts[[length(counts) + 1]] <- comparison$test$tstat[3]
          counts[[length(counts) + 1]] <- comparison$test$pvalues[4]
          counts[[length(counts) + 1]] <- comparison$test$tstat[4]
          counts[[length(counts) + 1]] <- comparison$test$pvalues[5]
          counts[[length(counts) + 1]] <- comparison$test$tstat[5]
          counts[[length(counts) + 1]] <- comparison$test$pvalues[6]
          counts[[length(counts) + 1]] <- comparison$test$tstat[6]
          # Calculating effect sizes for the post-hoc comparisons
          (em <- emmeans(fit, specs = ~ group))
          eff_size(em, sigma = sigma(fit), edf = df.residual(fit)) -> hold_eff
          hold_eff <- summary(hold_eff)
          counts[[length(counts) + 1]] <- "Standardized mean difference between groups - Hedges'g"
          counts[[length(counts) + 1]] <- hold_eff[1,2]
          counts[[length(counts) + 1]] <- hold_eff[1,6]
          counts[[length(counts) + 1]] <- hold_eff[1,5]
          counts[[length(counts) + 1]] <- hold_eff[2,2]
          counts[[length(counts) + 1]] <- hold_eff[2,6]
          counts[[length(counts) + 1]] <- hold_eff[2,5]
          counts[[length(counts) + 1]] <- hold_eff[3,2]
          counts[[length(counts) + 1]] <- hold_eff[3,6]
          counts[[length(counts) + 1]] <- hold_eff[3,5]
          counts[[length(counts) + 1]] <- hold_eff[4,2]
          counts[[length(counts) + 1]] <- hold_eff[4,6]
          counts[[length(counts) + 1]] <- hold_eff[4,5]
          counts[[length(counts) + 1]] <- hold_eff[5,2]
          counts[[length(counts) + 1]] <- hold_eff[5,6]
          counts[[length(counts) + 1]] <- hold_eff[5,5]
          counts[[length(counts) + 1]] <- hold_eff[6,2]
          counts[[length(counts) + 1]] <- hold_eff[6,6]
          counts[[length(counts) + 1]] <- hold_eff[6,5]
        }
      }
    }
    results_stats[nrow(results_stats) + 1,] <- counts
    bckp -> results_df
    rm(counts,fit,contrasts,comparison)
  }
}
write.xlsx(results_stats,paste("statistics_",project,".xlsx",sep=""),colNames = T,rowNames = F,keepNA = F)

#### Learning Curves ####
DA_progression <- NULL
visits_objects <- ls(pattern = "_visits$")

for (animal in all_animals) {
  # Iterate through all the challenges
  for (visits_object in visits_objects) {
    # Get the current challenge
    visits_df <- get(visits_object)
    
    # Subset the data frame based on each individual animal
    subset_df <- subset(visits_df, Animal == animal & NosepokeNumber > 0 & Light_Status == "Dark")
    
    # Progression
    if(nrow(subset_df) > 0){
      row.names(subset_df) <- NULL
      df <- data.frame(ID = animal, Number_DAs = 1, Success_DAs = 0, Challenge = gsub("_visits","",visits_object))
      if(subset_df[which(row.names(subset_df) == 1),"CornerCondition"] == "Correct"){
        df$Success_DAs <- 1
      }
      for (row in 2:nrow(subset_df)){
        df[nrow(df)+1,] <- c(animal,df[nrow(df),2],df[nrow(df),3],gsub("_visits","",visits_object))
        df[c(2,3)] <- lapply(df[c(2,3)], as.numeric)
        df[nrow(df),"Number_DAs"] <- df[nrow(df),"Number_DAs"] + 1
        if(subset_df[which(row.names(subset_df) == row),"CornerCondition"] == "Correct"){
          df[nrow(df),"Success_DAs"] <- df[nrow(df),"Success_DAs"]+1
        }
      }
      rbind(DA_progression,df) -> DA_progression
    } else {
      df <- data.frame(ID = animal, Number_DAs = 0, Success_DAs = 0, Challenge = gsub("_visits","",visits_object))
      rbind(DA_progression,df) -> DA_progression
    }
  }
}

DA_progression$Success_DAs/DA_progression$Number_DAs -> DA_progression$Ongoing_Success
DA_progression[!is.nan(DA_progression$Ongoing_Success),] -> DA_progression
DA_progression[!grepl("Extinction",DA_progression$Challenge),] -> DA_progression
DA_progression[!grepl("Habituation",DA_progression$Challenge),] -> DA_progression
DA_progression[!is.nan(DA_progression$Ongoing_Success),] -> DA_progression
merge(DA_progression,unblinding,by.x = 1,by.y = 1) -> DA_progression

slope <- 0.25
intercept <- 0

all_comparisons_set <- NULL
all_comparisons_set_EM <- NULL

for (i in unique(DA_progression$Challenge)){
  DA_progression[which(DA_progression$Challenge == i),] -> df
  df[which(df$Number_DAs <= 100),] -> df
  # Generate the y-values for the artificial line directly using the desired slope and intercept
  artificial_data <- data.frame(ID = "dummy", Number_DAs = seq(1, max(df$Number_DAs), length.out = max(df$Number_DAs)))
  artificial_data$Success_DAs <- intercept + slope * artificial_data$Number_DAs
  artificial_data$Challenge <- i
  artificial_data$Ongoing_Success <- artificial_data$Success_DAs/artificial_data$Number_DAs
  artificial_data$Group <- "dummy"
  rbind(df,artificial_data) -> df
  as.factor(df$Group) -> df$Group
  # Perform an ANCOVA
  ancova <- aov(Success_DAs ~ Number_DAs * Group, data = df)
  summary(emmeans(ancova, pairwise ~ Number_DAs:Group, at = list(Number_DAs = c(100)))) -> posthoc
  posthoc_df <- posthoc$contrasts[, c("contrast","estimate", "p.value")]
  posthoc_df$Challenge <- i
  posthoc$emmeans -> emmeans_df
  emmeans_df$Challenge <- i
  rbind(all_comparisons_set,posthoc_df) -> all_comparisons_set
  rbind(all_comparisons_set_EM,emmeans_df) -> all_comparisons_set_EM
}
gsub("Number_DAs100 ","",all_comparisons_set$contrast) -> all_comparisons_set$contrast
as.character(all_comparisons_set$p.value) -> all_comparisons_set$p.value
all_comparisons_set$p.value[all_comparisons_set$p.value == "0"] <- "<2e-16"
openxlsx::write.xlsx(all_comparisons_set,"all_comparisons_set.xlsx")
openxlsx::write.xlsx(all_comparisons_set_EM,"all_comparisons_set_EM.xlsx")

#### Plotting ####
## Going to the correct folder
dir.create(paste(results_folder,project,"figures",sep = "/"))
setwd(paste(results_folder,project,"figures",sep = "/"))

if(length(grep("^group[0-9]+_name$", ls(envir = globalenv()))) == 4){
  colors <- c(group1_color,group2_color,group3_color,group4_color)
} else {
  colors <- c(group1_color,group2_color)
}

#### General statistics ####
if(length(grep("^group[0-9]+_name$", ls(envir = globalenv()))) == 4){
  for (variable in 3:ncol(results_df)){
    results_df[,c(1,2,variable)] -> df
    colnames(df)[3] <- "Variable1"
    measurement_plotted <- colnames(results_df)[variable]
    # Remove rows with NAs in Variable1
    df <- df %>% filter(!is.na(Variable1))
    if(nrow(df) == 0){
      next()
    } else {
      # Create a named vector with the desired mapping
      group_map <- setNames(c(1, 2, 3, 4), c(get("group1_name"), get("group2_name"), get("group3_name"), get("group4_name")))
      
      # Use the match function to find the index of the Group column values in the group_map vector
      df$Group <- match(df$Group, names(group_map))
      
      # Use the as.numeric function to convert the index values into numeric values
      df$Group <- as.numeric(df$Group)
      
      # Create a new column for group name
      df <- df %>%
        rowwise() %>%
        mutate(name = get(paste0("group", as.numeric(Group), "_name"))) %>%
        ungroup()
      
      # Group by Group and calculate mean and standard deviation of Variable1
      df_summary <- df %>%
        group_by(Group) %>%
        summarize(mean = mean(Variable1), sd = sd(Variable1))
      
      # Create a new column for group color
      df_summary <- df_summary %>%
        rowwise() %>%
        mutate(color = get(paste0("group", as.numeric(Group), "_color"))) %>%
        ungroup()
      
      # Create a new column for group name
      df_summary <- df_summary %>%
        rowwise() %>%
        mutate(name = get(paste0("group", as.numeric(Group), "_name"))) %>%
        ungroup()
      
      # Calculate the y position of the comparison bars
      y_pos <- max(df_summary$mean + df_summary$sd) * 1.05 # Set y position of comparison bars to 10% above maximum value
      
      # Set the spacing between the comparison bars
      spacing <- max(df_summary$mean + df_summary$sd) * 0.04 # Set spacing between comparison bars to 15% of maximum value
      
      # Get the p-values from the stats table
      as.numeric(results_stats[which(results_stats$Variable == measurement_plotted),c(29,31,33,35,37,39)]) -> p_values
      if (!is.na(p_values)[1]){
        # Define a function to format the p-value annotations
        format_pvalue <- function(p) {
          if (p >= 0.001 & p <= 0.05) {
            return(paste0("bold('p = ", round(p, digits = 3), "')"))
          } else if (p < 0.001){
            return(paste0("bold('p < 0.001')"))
          } else if (p < 1 & p > 0.05) {
            return(paste0("'p = ", round(p, digits = 3), "'"))
          } else {
            return("'p > 0.999'")
          }
        }
        
        df_summary$name <- factor(df_summary$name,levels = c(group1_name,group2_name,group3_name,group4_name))
        # Plot the data using ggplot
        p <- ggplot(df_summary, aes(x = name, y = mean, fill = name)) +
          # Add the bars
          geom_bar(stat = "identity", position = position_dodge(), color="black") + # Add black outline around bars
          # Add the error bars
          geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .2, position = position_dodge(.9)) +
          # Add the dots representing the individual observations
          geom_point(data=df,aes(x=name,y=Variable1),position=position_jitter(width=0.2,height=0),size=3)+
          # Set the fill scale manually
          scale_fill_manual(values = unique(df_summary$color), labels = unique(df_summary$name)) +
          scale_color_manual(values = unique(df_summary$color), labels = unique(df_summary$name)) +
          # Add comparison bars and p-value annotations for each pair of groups you want to compare
          geom_segment(aes(x=1,xend=2,y=y_pos,yend=y_pos))+
          annotate("text",x=mean(c(1,2)),y=y_pos+spacing/2,label=format_pvalue(p_values[1]),parse=T,size=4)+ # Increase size of p-value text
          geom_segment(aes(x=1,xend=3,y=y_pos+spacing,yend=y_pos+spacing))+
          annotate("text",x=mean(c(1,3)),y=y_pos+spacing+spacing/2,label=format_pvalue(p_values[2]),parse=T,size=4)+ # Increase size of p-value text
          geom_segment(aes(x=1,xend=4,y=y_pos+spacing*2,yend=y_pos+spacing*2))+
          annotate("text",x=mean(c(1,4)),y=y_pos+spacing*2+spacing/2,label=format_pvalue(p_values[3]),parse=T,size=4)+ # Increase size of p-value text
          geom_segment(aes(x=2,xend=3,y=y_pos+spacing*3,yend=y_pos+spacing*3))+
          annotate("text",x=mean(c(2,3)),y=y_pos+spacing*3+spacing/2,label=format_pvalue(p_values[4]),parse=T,size=4)+ # Increase size of p-value text
          geom_segment(aes(x=2,xend=4,y=y_pos+spacing*4,yend=y_pos+spacing*4))+
          annotate("text",x=mean(c(2,4)),y=y_pos+spacing*4+spacing/2,label=format_pvalue(p_values[5]),parse=T,size=4)+ # Increase size of p-value text
          geom_segment(aes(x=3,xend=4,y=y_pos+spacing*5,yend=y_pos+spacing*5))+
          annotate("text",x=mean(c(3,4)),y=y_pos+spacing*5+spacing/2,label=format_pvalue(p_values[6]),parse=T,size=4)+ # Increase size of p-value text
          theme_minimal()+ # Apply theme_minimal()
          theme(axis.title.x=element_blank(), # Suppress the X axis label
                axis.title.y=element_text(size=14,face="bold"), # Increase size of Y axis label and make it bold
                axis.text=element_text(size=12), # Increase size of axis text
                axis.text.x = element_text(face = "bold"), # Make the group names bold
                legend.position="none", # Suppress the legend
                panel.grid.major.x = element_blank()) + # Remove background vertical lines
          ylab(measurement_plotted) # Set the Y axis label to the value of measurement_plotted
        ggsave(paste(measurement_plotted,".png",sep = ""), p, width = 10, height = 10, dpi = 600)
      } else {
        next()
      }
    }
  }
} else if (length(grep("^group[0-9]+_name$", ls(envir = globalenv()))) == 3) {
  for (variable in 3:ncol(results_df)){
    results_df[,c(1,2,variable)] -> df
    colnames(df)[3] <- "Variable1"
    measurement_plotted <- colnames(results_df)[variable]
    # Remove rows with NAs in Variable1
    df <- df %>% filter(!is.na(Variable1))
    if(nrow(df) == 0){
      next()
    } else {
      # Create a named vector with the desired mapping
      group_map <- setNames(c(1, 2, 3), c(get("group1_name"), get("group2_name"), get("group3_name")))
      
      # Use the match function to find the index of the Group column values in the group_map vector
      df$Group <- match(df$Group, names(group_map))
      
      # Use the as.numeric function to convert the index values into numeric values
      df$Group <- as.numeric(df$Group)
      
      # Create a new column for group name
      df <- df %>%
        rowwise() %>%
        mutate(name = get(paste0("group", as.numeric(Group), "_name"))) %>%
        ungroup()
      
      # Group by Group and calculate mean and standard deviation of Variable1
      df_summary <- df %>%
        group_by(Group) %>%
        summarize(mean = mean(Variable1), sd = sd(Variable1))
      
      # Create a new column for group color
      df_summary <- df_summary %>%
        rowwise() %>%
        mutate(color = get(paste0("group", as.numeric(Group), "_color"))) %>%
        ungroup()
      
      # Create a new column for group name
      df_summary <- df_summary %>%
        rowwise() %>%
        mutate(name = get(paste0("group", as.numeric(Group), "_name"))) %>%
        ungroup()
      
      # Calculate the y position of the comparison bars
      y_pos <- max(df_summary$mean + df_summary$sd) * 1.05 # Set y position of comparison bars to 10% above maximum value
      
      # Set the spacing between the comparison bars
      spacing <- max(df_summary$mean + df_summary$sd) * 0.04 # Set spacing between comparison bars to 15% of maximum value
      
      # Get the p-values from the stats table
      as.numeric(results_stats[which(results_stats$Variable == measurement_plotted),c(25,27,29)]) -> p_values
      if (!is.na(p_values)[1]){
        # Define a function to format the p-value annotations
        format_pvalue <- function(p) {
          if (p >= 0.001 & p <= 0.05) {
            return(paste0("bold('p = ", round(p, digits = 3), "')"))
          } else if (p < 0.001){
            return(paste0("bold('p < 0.001')"))
          } else if (p < 1 & p > 0.05) {
            return(paste0("'p = ", round(p, digits = 3), "'"))
          } else {
            return("'p > 0.999'")
          }
        }
        
        df_summary$name <- factor(df_summary$name,levels = c(group1_name,group2_name,group3_name))
        # Plot the data using ggplot
        p <- ggplot(df_summary, aes(x = name, y = mean, fill = name)) +
          # Add the bars
          geom_bar(stat = "identity", position = position_dodge(), color="black") + # Add black outline around bars
          # Add the error bars
          geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .2, position = position_dodge(.9)) +
          # Add the dots representing the individual observations
          geom_point(data=df,aes(x=name,y=Variable1),position=position_jitter(width=0.2,height=0),size=3)+
          # Set the fill scale manually
          scale_fill_manual(values = unique(df_summary$color), labels = unique(df_summary$name)) +
          scale_color_manual(values = unique(df_summary$color), labels = unique(df_summary$name)) +
          # Add comparison bars and p-value annotations for each pair of groups you want to compare
          geom_segment(aes(x=1,xend=2,y=y_pos,yend=y_pos))+
          annotate("text",x=mean(c(1,2)),y=y_pos+spacing/2,label=format_pvalue(p_values[1]),parse=T,size=4)+ # Increase size of p-value text
          geom_segment(aes(x=1,xend=3,y=y_pos+spacing,yend=y_pos+spacing))+
          annotate("text",x=mean(c(1,3)),y=y_pos+spacing+spacing/2,label=format_pvalue(p_values[2]),parse=T,size=4)+ # Increase size of p-value text
          geom_segment(aes(x=1,xend=4,y=y_pos+spacing*2,yend=y_pos+spacing*2))+
          annotate("text",x=mean(c(1,4)),y=y_pos+spacing*2+spacing/2,label=format_pvalue(p_values[3]),parse=T,size=4)+ # Increase size of p-value text
          theme_minimal()+ # Apply theme_minimal()
          theme(axis.title.x=element_blank(), # Suppress the X axis label
                axis.title.y=element_text(size=14,face="bold"), # Increase size of Y axis label and make it bold
                axis.text=element_text(size=12), # Increase size of axis text
                axis.text.x = element_text(face = "bold"), # Make the group names bold
                legend.position="none", # Suppress the legend
                panel.grid.major.x = element_blank()) + # Remove background vertical lines
          ylab(measurement_plotted) # Set the Y axis label to the value of measurement_plotted
        ggsave(paste(measurement_plotted,".png",sep = ""), p, width = 10, height = 10, dpi = 600)
      } else {
        next()
      }
    }
  }
} else {
  for (variable in 3:ncol(results_df)){
    results_df[,c(1,2,variable)] -> df
    colnames(df)[3] <- "Variable1"
    measurement_plotted <- colnames(results_df)[variable]
    # Assuming your data frame is called df
    # Remove rows with NAs in Variable1
    df <- df %>% filter(!is.na(Variable1))
    
    if (nrow(df) == 0){
      next()
    } else {
      # Create a named vector with the desired mapping
      group_map <- setNames(c(1, 2), c(get("group1_name"), get("group2_name")))
      
      # Use the match function to find the index of the Group column values in the group_map vector
      df$Group <- match(df$Group, names(group_map))
      
      # Use the as.numeric function to convert the index values into numeric values
      df$Group <- as.numeric(df$Group)
      
      # Create a new column for group name
      df <- df %>%
        rowwise() %>%
        mutate(name = get(paste0("group", as.numeric(Group), "_name"))) %>%
        ungroup()
      
      # Group by Group and calculate mean and standard deviation of Variable1
      df_summary <- df %>%
        group_by(Group) %>%
        summarize(mean = mean(Variable1), sd = sd(Variable1))
      
      # Create a new column for group color
      df_summary <- df_summary %>%
        rowwise() %>%
        mutate(color = get(paste0("group", as.numeric(Group), "_color"))) %>%
        ungroup()
      
      # Create a new column for group name
      df_summary <- df_summary %>%
        rowwise() %>%
        mutate(name = get(paste0("group", as.numeric(Group), "_name"))) %>%
        ungroup()
      
      # Calculate the y position of the comparison bars
      y_pos <- max(df_summary$mean + df_summary$sd) * 1.05 # Set y position of comparison bars to 10% above maximum value
      
      # Set the spacing between the comparison bars
      spacing <- max(df_summary$mean + df_summary$sd) * 0.04 # Set spacing between comparison bars to 15% of maximum value
      
      # Get the p-values from the stats table
      as.numeric(results_stats[which(results_stats$Variable == measurement_plotted),12]) -> p_values
      if (!is.na(p_values)){
        # Define a function to format the p-value annotations
        format_pvalue <- function(p) {
          if (p >= 0.001 & p <= 0.05) {
            return(paste0("bold('p = ", round(p, digits = 3), "')"))
          } else if (p < 0.001){
            return(paste0("bold('p < 0.001')"))
          } else if (p < 1 & p > 0.05) {
            return(paste0("'p = ", round(p, digits = 3), "'"))
          } else {
            return("'p > 0.999'")
          }
        }
        
        df_summary$name <- factor(df_summary$name,levels = c(group1_name,group2_name))
        # Plot the data using ggplot
        p <- ggplot(df_summary, aes(x = name, y = mean, fill = name)) +
          # Add the bars
          geom_bar(stat = "identity", position = position_dodge(), color="black") + # Add black outline around bars
          # Add the error bars
          geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .2, position = position_dodge(.9)) +
          # Add the dots representing the individual observations
          geom_point(data=df,aes(x=name,y=Variable1),position=position_jitter(width=0.2,height=0),size=4)+
          # Set the fill scale manually
          scale_fill_manual(values = setNames(unique(df_summary$color), unique(df_summary$name))) +
          scale_color_manual(values = setNames(unique(df_summary$color), unique(df_summary$name))) +
          # Add comparison bars and p-value annotations for each pair of groups you want to compare
          geom_segment(aes(x=1,xend=2,y=y_pos,yend=y_pos))+
          annotate("text",x=mean(c(1,2)),y=y_pos+spacing/2,label=format_pvalue(p_values[1]),parse=T,size=10)+ # Increase size of p-value text
          theme_minimal()+ # Apply theme_minimal()
          theme(axis.title.x=element_blank(), # Suppress the X axis label
                axis.title.y=element_text(size=22,face="bold"), # Increase size of Y axis label and make it bold
                axis.text=element_text(size=20), # Increase size of axis text
                axis.text.x = element_text(face = "bold"), # Make the group names bold
                legend.position="none", # Suppress the legend
                panel.grid.major.x = element_blank()) + # Remove background vertical lines
          ylab(measurement_plotted) # Set the Y axis label to the value of measurement_plotted
        ggsave(paste(measurement_plotted,".png",sep = ""), p, width = 10, height = 10, dpi = 600) # Save the plot  
      }
    }
  }
}

#### Plotting Learning curves ####
for (i in unique(DA_progression$Challenge)){
  DA_progression[which(DA_progression$Challenge == i),] -> df
  
  df[which(df$Number_DAs <= 100),] -> df
  
  # Set the desired slope and intercept for the artificial line
  slope <- 0.25
  intercept <- 0 # You can change this value to set a different intercept
  
  # Create a new data frame with the x-values for the artificial line
  x_range <- range(df$Number_DAs)
  artificial_data <- data.frame(Number_DAs = seq(x_range[1], x_range[2], length.out = max(df$Number_DAs)))
  
  # Generate the y-values for the artificial line directly using the desired slope and intercept
  artificial_data$Success_DAs <- intercept + slope * artificial_data$Number_DAs
  
  # Create a line plot of Success_DAs vs Number_DAs, colored by Project and grouped by ID
  p <- ggplot(df, aes(x = Number_DAs, y = Success_DAs, color = Group, group = ID)) +
    geom_line(alpha = 0.65) +
    geom_smooth(aes(group = Group),method = "lm", formula = y ~ x - 1, se = TRUE, level = 0.95, linewidth = 1.5, alpha = 0.8) + # aes(group = Project, fill = Project)
    scale_color_manual(values = colors) +
    geom_line(data = artificial_data, aes(x = Number_DAs, y = Success_DAs, group = NULL), color = "black", linetype = "dashed", linewidth = 1.5) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 102.5),breaks = seq(50, max(df$Number_DAs), by = 50)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA),breaks = seq(10, max(df$Success_DAs), by = 10)) +
    theme_minimal() + 
    theme(
      axis.title = element_text(size = 30, face = "bold"),
      axis.text = element_text(size = 26),
      plot.title = element_text(size = 1),
      axis.line = element_line(),
      axis.ticks = element_line(color = "black", linewidth = 0.5),
      axis.ticks.length = unit(0.1, "cm")
    ) +
    xlab("Drinking Attempts") +
    ylab("Successful Drinking Attempts") +
    labs(title = i) + theme(legend.position = "none")
  ggsave(paste("learning_curve_",i,".png",sep = ""), p, width = 30, height = 25, units = "cm")
}