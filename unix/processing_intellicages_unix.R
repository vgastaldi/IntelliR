## Last modified: 27.12.2023
#### Loading files #####
set.seed(0)
options(digits = 5,scipen = 20)

#### Challenge 1 ####
read.delim(paste(project_folder,"/",project,"/","Challenge_1/Visit.txt",sep="")) -> C1_visits
read.delim(paste(project_folder,"/",project,"/","Challenge_1/Nosepoke.txt",sep="")) -> C1_nosepokes
#### Challenge 2 ####
read.delim(paste(project_folder,"/",project,"/","Challenge_2/Visit.txt",sep="")) -> C2_visits
read.delim(paste(project_folder,"/",project,"/","Challenge_2/Nosepoke.txt",sep="")) -> C2_nosepokes
#### Challenge 3 ####
read.delim(paste(project_folder,"/",project,"/","Challenge_3/Visit.txt",sep="")) -> C3_visits
read.delim(paste(project_folder,"/",project,"/","Challenge_3/Nosepoke.txt",sep="")) -> C3_nosepokes

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

#### Dividing into the individual challenges ####
rbind(C1_visits[which(C1_visits$StartDate == unique(C1_visits$StartDate)[1]),],C1_visits[which(C1_visits$StartDate == unique(C1_visits$StartDate)[2] & C1_visits$Start_Hour <= 5),]) -> Habituation_visits

rbind(C1_visits[which(C1_visits$StartDate == unique(C1_visits$StartDate)[2] & C1_visits$Start_Hour >= 6),],C1_visits[which(C1_visits$StartDate == unique(C1_visits$StartDate)[3] & C1_visits$Start_Hour <= 5),]) -> PL_visits

rbind(C1_visits[which(C1_visits$StartDate == unique(C1_visits$StartDate)[3] & C1_visits$Start_Hour >= 6),],C1_visits[which(C1_visits$StartDate == unique(C1_visits$StartDate)[4] & C1_visits$Start_Hour <= 5),]) -> RL_visits

rbind(C1_visits[which(C1_visits$StartDate == unique(C1_visits$StartDate)[4] & C1_visits$Start_Hour >= 6),],C1_visits[which(C1_visits$StartDate == unique(C1_visits$StartDate)[5] & C1_visits$Start_Hour <= 5),]) -> MRL_visits

C1_nosepokes[which(C1_nosepokes$VisitID %in% Habituation_visits$VisitID),] -> Habituation_nosepokes
C1_nosepokes[which(C1_nosepokes$VisitID %in% PL_visits$VisitID),] -> PL_nosepokes
C1_nosepokes[which(C1_nosepokes$VisitID %in% PL_visits$VisitID),] -> RL_nosepokes
C1_nosepokes[which(C1_nosepokes$VisitID %in% PL_visits$VisitID),] -> MRL_nosepokes

rbind(C1_visits[which(C1_visits$StartDate == unique(C1_visits$StartDate)[5] & C1_visits$Start_Hour >= 6),],C2_visits[which(C2_visits$StartDate == unique(C2_visits$StartDate)[1]),],C2_visits[which(C2_visits$StartDate == unique(C2_visits$StartDate)[2] & C2_visits$Start_Hour <= 5),]) -> ExtinctionCh2_visits

rbind(C2_visits[which(C2_visits$StartDate == unique(C2_visits$StartDate)[2] & C2_visits$Start_Hour >= 6),],C2_visits[which(C2_visits$StartDate == unique(C2_visits$StartDate)[3] & C2_visits$Start_Hour <= 5),]) -> ELM1A_visits

rbind(C2_visits[which(C2_visits$StartDate == unique(C2_visits$StartDate)[3] & C2_visits$Start_Hour >= 6),],C2_visits[which(C2_visits$StartDate == unique(C2_visits$StartDate)[4] & C2_visits$Start_Hour <= 5),]) -> ELM1R_visits

rbind(C2_visits[which(C2_visits$StartDate == unique(C2_visits$StartDate)[4] & C2_visits$Start_Hour >= 6),],C2_visits[which(C2_visits$StartDate == unique(C2_visits$StartDate)[5] & C2_visits$Start_Hour <= 5),]) -> ELM2A_visits

rbind(C2_visits[which(C2_visits$StartDate == unique(C2_visits$StartDate)[5] & C2_visits$Start_Hour >= 6),],C2_visits[which(C2_visits$StartDate == unique(C2_visits$StartDate)[6] & C2_visits$Start_Hour <= 5),]) -> ELM2R_visits

rbind(C1_nosepokes[which(C1_nosepokes$VisitID %in% ExtinctionCh2_visits$VisitID),],C2_nosepokes[which(C2_nosepokes$VisitID %in% ExtinctionCh2_visits$VisitID),]) -> ExtinctionCh2_nosepokes
C2_nosepokes[which(C2_nosepokes$VisitID %in% ELM1A_visits$VisitID),] -> ELM1A_nosepokes
C2_nosepokes[which(C2_nosepokes$VisitID %in% ELM1R_visits$VisitID),] -> ELM1R_nosepokes
C2_nosepokes[which(C2_nosepokes$VisitID %in% ELM2A_visits$VisitID),] -> ELM2A_nosepokes
C2_nosepokes[which(C2_nosepokes$VisitID %in% ELM2R_visits$VisitID),] -> ELM2R_nosepokes

unique(C3_visits$StartDate)

rbind(C2_visits[which(C2_visits$StartDate == unique(C2_visits$StartDate)[5] & C2_visits$Start_Hour >= 6),],C3_visits[which(C3_visits$StartDate == unique(C3_visits$StartDate)[1]),],C3_visits[which(C3_visits$StartDate == unique(C3_visits$StartDate)[2] & C3_visits$Start_Hour <= 17),]) -> ExtinctionCh3_visits

rbind(C3_visits[which(C3_visits$StartDate == unique(C3_visits$StartDate)[2] & C3_visits$Start_Hour >= 18),],C3_visits[which(C3_visits$StartDate == unique(C3_visits$StartDate)[3] & C3_visits$Start_Hour <= 17),]) -> CW1_visits

rbind(C3_visits[which(C3_visits$StartDate == unique(C3_visits$StartDate)[3] & C3_visits$Start_Hour >= 18),],C3_visits[which(C3_visits$StartDate == unique(C3_visits$StartDate)[4] & C3_visits$Start_Hour <= 17),]) -> CW2_visits

rbind(C3_visits[which(C3_visits$StartDate == unique(C3_visits$StartDate)[4] & C3_visits$Start_Hour >= 18),],C3_visits[which(C3_visits$StartDate == unique(C3_visits$StartDate)[5] & C3_visits$Start_Hour <= 17),]) -> CCW1_visits

rbind(C3_visits[which(C3_visits$StartDate == unique(C3_visits$StartDate)[5] & C3_visits$Start_Hour >= 18),],C3_visits[which(C3_visits$StartDate == unique(C3_visits$StartDate)[6] & C3_visits$Start_Hour <= 17),]) -> CCW2_visits


rbind(C2_nosepokes[which(C2_nosepokes$VisitID %in% ExtinctionCh3_visits$VisitID),],C3_nosepokes[which(C3_nosepokes$VisitID %in% ExtinctionCh3_visits$VisitID),]) -> ExtinctionCh3_nosepokes
C3_nosepokes[which(C3_nosepokes$VisitID %in% CW1_visits$VisitID),] -> CW1_nosepokes
C3_nosepokes[which(C3_nosepokes$VisitID %in% CW2_visits$VisitID),] -> CW2_nosepokes
C3_nosepokes[which(C3_nosepokes$VisitID %in% CCW1_visits$VisitID),] -> CCW1_nosepokes
C3_nosepokes[which(C3_nosepokes$VisitID %in% CCW2_visits$VisitID),] -> CCW2_nosepokes

rm(C1_visits,C2_visits,C3_visits,C1_nosepokes,C2_nosepokes,C3_nosepokes)

#### ELM has the specific feature that the active time is only two hours ####
subset(ELM1A_visits,Start_Hour >= 22 & Start_Hour <= 23) -> ELM1A_active_visits
subset(ELM1A_nosepokes,Start_Hour >= 22 & Start_Hour <= 23) -> ELM1A_active_nosepokes
subset(ELM1R_visits,Start_Hour >= 22 & Start_Hour <= 23) -> ELM1R_active_visits
subset(ELM1R_nosepokes,Start_Hour >= 22 & Start_Hour <= 23) -> ELM1R_active_nosepokes
subset(ELM2A_visits,Start_Hour >= 20 & Start_Hour <= 22) -> ELM2A_active_visits
subset(ELM2A_nosepokes,Start_Hour >= 20 & Start_Hour <= 22) -> ELM2A_active_nosepokes
subset(ELM2R_visits,Start_Hour >= 20 & Start_Hour <= 22) -> ELM2R_active_visits
subset(ELM2R_nosepokes,Start_Hour >= 20 & Start_Hour <= 22) -> ELM2R_active_nosepokes

#### Getting total visits, nosepokes, and licks ####
# Get the names of all objects that will be used for these variables
visits_objects <- ls(pattern = "_visits$")
# Reorder to follow the challenges
visits_objects <- visits_objects[c(15,17,18,16,13,6,5,8,7,10,9,12,11,14,3,4,1,2)]
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

#### Getting visits, nosepokes, and licks in the Light period ####
# Get the names of all objects that will be used for these variables
visits_objects <- ls(pattern = "_visits$")
# Reorder to follow the challenges
visits_objects <- visits_objects[c(15,17,18,16,13,6,5,8,7,10,9,12,11,14,3,4,1,2)]
visits_objects[!is.na(visits_objects)] -> visits_objects  
# Create a vector of column names for the results data frame
column_names <- c("Animal", paste0(rep(gsub("_visits","",visits_objects), each = 3), c("_Light_Licks","_Light_Visits", "_Light_Nosepokes")))

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
    subset_df <- subset(visits_df, Animal == animal & Light_Status == "Light")
    
    # Calculate the total number of visits, nosepokes, and licks
    light_visits <- ifelse(nrow(subset_df) == 0, NA, nrow(subset_df))
    light_nosepokes <- ifelse(nrow(subset_df) == 0, NA, sum(subset_df$NosepokeNumber, na.rm = TRUE))
    light_licks <- ifelse(nrow(subset_df) == 0, NA, sum(subset_df$LickNumber, na.rm = TRUE))
    
    # Add the counts to the counts list
    counts[[length(counts) + 1]] <- light_licks
    counts[[length(counts) + 1]] <- light_visits
    counts[[length(counts) + 1]] <- light_nosepokes
  }
  
  # Add a row with the counts to the results data frame
  results[nrow(results) + 1,] <- counts
}

# Clean up intermediate files
rm(animal,light_visits,light_nosepokes,light_licks,counts,visits_df,subset_df,visits_object,visits_objects)

# Merge with the actual results dataframe
results_df <- merge(results_df,results,by.x = 1,by.y = 1,all.x = T)

#### Getting visits, nosepokes, and licks in the Dark period ####
# Get the names of all objects that will be used for these variables
visits_objects <- ls(pattern = "_visits$")
# Reorder to follow the challenges
visits_objects <- visits_objects[c(15,17,18,16,13,6,5,8,7,10,9,12,11,14,3,4,1,2)]
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
    
    # Calculate the total number of visits, nosepokes, and licks
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

#### Exploratory visits and drinking attempts - Light ####
# Get the names of all objects that will be used for these variables
visits_objects <- ls(pattern = "_visits$")
# Reorder to follow the challenges
visits_objects <- visits_objects[c(15,17,18,16,13,6,5,8,7,10,9,12,11,14,3,4,1,2)]
visits_objects[!is.na(visits_objects)] -> visits_objects  

# Create a vector of column names for the results data frame
column_names <- c("Animal", paste0(rep(gsub("_visits","",visits_objects), each = 2), c("_Light_ExploratoryVisits", "_Light_DrinkingAttempts")))

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
    subset_df <- subset(visits_df, Animal == animal & Light_Status == "Light")
    
    # Calculate the total number of visits, nosepokes, and licks
    light_exploratory_visits <- ifelse(nrow(subset_df) == 0, NA, nrow(subset_df[which(subset_df$NosepokeNumber == 0),]))
    light_drinking_attempts <- ifelse(nrow(subset_df) == 0, NA, nrow(subset_df[which(subset_df$NosepokeNumber != 0),]))
    
    # Add the counts to the counts list
    counts[[length(counts) + 1]] <- light_exploratory_visits
    counts[[length(counts) + 1]] <- light_drinking_attempts
  }
  
  # Add a row with the counts to the results data frame
  results[nrow(results) + 1,] <- counts
}
rm(animal,light_exploratory_visits,light_drinking_attempts,counts,visits_df,subset_df,visits_object,visits_objects)

# Merge with the actual results dataframe
results_df <- merge(results_df,results,by.x = 1,by.y = 1,all.x = T)

#### Exploratory visits and drinking attempts - Dark ####
# Get the names of all objects that will be used for these variables
visits_objects <- ls(pattern = "_visits$")
# Reorder to follow the challenges
visits_objects <- visits_objects[c(15,17,18,16,13,6,5,8,7,10,9,12,11,14,3,4,1,2)]
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
    
    # Calculate the total number of visits, nosepokes, and licks
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

#### Repetitive Behavior - Frequency and number - Total ####
# Get the names of all objects that will be used for this variable
nosepokes_objects <- ls(pattern = "_nosepokes$")
# Reorder to follow the challenges
nosepokes_objects <- nosepokes_objects[c(15,17,18,16,13,6,5,8,7,10,9,12,11,14,3,4,1,2)]
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

#### Repetitive Behavior - Frequency and number - Light ####
# Get the names of all objects that will be used for this variable
nosepokes_objects <- ls(pattern = "_nosepokes$")
# Reorder to follow the challenges
nosepokes_objects <- nosepokes_objects[c(15,17,18,16,13,6,5,8,7,10,9,12,11,14,3,4,1,2)]
nosepokes_objects[!is.na(nosepokes_objects)] -> nosepokes_objects  

# Create a vector of column names for the results data frame
column_names <- c("Animal", paste0(rep(gsub("_nosepokes","",nosepokes_objects), each = 2), c("_Light_NumberRepetitiveBehavior", "_Light_FrequencyRepetitiveBehavior")))

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
    subset_df <- subset(nosepokes_df, Animal == animal & Light_Status == "Light")
    if (nrow(subset_df) < 1){
      light_number_repetitive <- NA
      light_frequency_repetitive <- NA
    } else {
      light_number_repetitive <- 0
      light_frequency_repetitive <- 0
      
      # Calculate number and frequency of repetitive behaviors
      for (visit in unique(subset_df$VisitID)){
        visit_np_df <- subset(subset_df,VisitID == visit)
        if (nrow(visit_np_df) < 2){
          next()
        } else if (visit_np_df[1,"LickNumber"] > 0){
          light_number_repetitive <- light_number_repetitive+sum(visit_np_df$LickNumber == 0)
          light_frequency_repetitive <- light_frequency_repetitive+ifelse(sum(visit_np_df$LickNumber == 0) > 0,1,0)
        } else {
          light_number_repetitive <- light_number_repetitive+(sum(visit_np_df$LickNumber == 0)-1)
          light_frequency_repetitive <- light_frequency_repetitive+ifelse(sum(visit_np_df$LickNumber == 0) > 0,1,0)
        }
      }
    }
    # Add the counts to the counts list
    counts[[length(counts) + 1]] <- light_number_repetitive
    counts[[length(counts) + 1]] <- light_frequency_repetitive
  }
  # Add a row with the counts to the results data frame
  results[nrow(results) + 1,] <- counts
}
rm(animal,light_number_repetitive,light_frequency_repetitive,counts,nosepokes_df,subset_df,nosepokes_object,nosepokes_objects,visit)

# Merge with the actual results dataframe
results_df <- merge(results_df,results,by.x = 1,by.y = 1,all.x = T)

#### Repetitive Behavior - Frequency and number - Dark ####
# Get the names of all objects that will be used for this variable
nosepokes_objects <- ls(pattern = "_nosepokes$")
# Reorder to follow the challenges
nosepokes_objects <- nosepokes_objects[c(15,17,18,16,13,6,5,8,7,10,9,12,11,14,3,4,1,2)]
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

#### Old Performance Index - Total ####
# Get the names of all objects that will be used for this variable
nosepokes_objects <- ls(pattern = "_nosepokes$")
# Reorder to follow the challenges
nosepokes_objects <- nosepokes_objects[c(15,17,18,16,13,6,5,8,7,10,9,12,11,14,3,4,1,2)]
nosepokes_objects[!is.na(nosepokes_objects)] -> nosepokes_objects  

# Create a vector of column names for the results data frame
column_names <- c("Animal", paste0(rep(gsub("_nosepokes","",nosepokes_objects), each = 1), c("_Total_PerformanceIndex")))

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
    gsub("_nosepokes","_visits",nosepokes_object) -> visits_object
    visits_df <- get(visits_object)
    
    # Subset the data frame based on each individual animal
    subset_df <- subset(nosepokes_df, Animal == animal)
    subset_df2 <- subset(visits_df, Animal == animal)
    
    # Calculate the Performance Index
    total_performance_index <- nrow(subset_df[subset_df$LickNumber > 0 ,])/((nrow(subset_df)-nrow(subset_df[subset_df$LickNumber > 0 ,])+1)*(nrow(subset_df2)-nrow(subset_df2[subset_df2$NosepokeNumber > 0,])+1))
    # Add the counts to the counts list
    counts[[length(counts) + 1]] <- total_performance_index
  }
  # Add a row with the counts to the results data frame
  results[nrow(results) + 1,] <- counts
}
rm(animal,total_performance_index,counts,nosepokes_df,subset_df,nosepokes_object,nosepokes_objects,subset_df2,visits_objects,visit_object)

# Merge with the actual results dataframe
results_df <- merge(results_df,results,by.x = 1,by.y = 1,all.x = T)

#### Old Performance Index - Light ####
# Get the names of all objects that will be used for this variable
nosepokes_objects <- ls(pattern = "_nosepokes$")
# Reorder to follow the challenges
nosepokes_objects <- nosepokes_objects[c(15,17,18,16,13,6,5,8,7,10,9,12,11,14,3,4,1,2)]
nosepokes_objects[!is.na(nosepokes_objects)] -> nosepokes_objects  

# Create a vector of column names for the results data frame
column_names <- c("Animal", paste0(rep(gsub("_nosepokes","",nosepokes_objects), each = 1), c("_Light_PerformanceIndex")))

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
    gsub("_nosepokes","_visits",nosepokes_object) -> visits_object
    visits_df <- get(visits_object)
    
    # Subset the data frame based on each individual animal
    subset_df <- subset(nosepokes_df, Animal == animal & Light_Status == "Light")
    subset_df2 <- subset(visits_df, Animal == animal & Light_Status == "Light")
    
    # Calculate the Performance Index
    light_performance_index <- nrow(subset_df[subset_df$LickNumber > 0 ,])/((nrow(subset_df)-nrow(subset_df[subset_df$LickNumber > 0 ,])+1)*(nrow(subset_df2)-nrow(subset_df2[subset_df2$NosepokeNumber > 0,])+1))
    # Add the counts to the counts list
    counts[[length(counts) + 1]] <- light_performance_index
  }
  # Add a row with the counts to the results data frame
  results[nrow(results) + 1,] <- counts
}
rm(animal,light_performance_index,counts,nosepokes_df,subset_df,nosepokes_object,nosepokes_objects,subset_df2,visits_object)

# Merge with the actual results dataframe
results_df <- merge(results_df,results,by.x = 1,by.y = 1,all.x = T)

#### Old Performance Index - Dark ####
# Get the names of all objects that will be used for this variable
nosepokes_objects <- ls(pattern = "_nosepokes$")
# Reorder to follow the challenges
nosepokes_objects <- nosepokes_objects[c(15,17,18,16,13,6,5,8,7,10,9,12,11,14,3,4,1,2)]
nosepokes_objects[!is.na(nosepokes_objects)] -> nosepokes_objects  

# Create a vector of column names for the results data frame
column_names <- c("Animal", paste0(rep(gsub("_nosepokes","",nosepokes_objects), each = 1), c("_Dark_PerformanceIndex")))

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
    gsub("_nosepokes","_visits",nosepokes_object) -> visits_object
    visits_df <- get(visits_object)
    
    # Subset the data frame based on each individual animal
    subset_df <- subset(nosepokes_df, Animal == animal & Light_Status == "Dark")
    subset_df2 <- subset(visits_df, Animal == animal & Light_Status == "Dark")
    
    # Calculate the Performance Index
    dark_performance_index <- nrow(subset_df[subset_df$LickNumber > 0 ,])/((nrow(subset_df)-nrow(subset_df[subset_df$LickNumber > 0 ,])+1)*(nrow(subset_df2)-nrow(subset_df2[subset_df2$NosepokeNumber > 0,])+1))
    # Add the counts to the counts list
    counts[[length(counts) + 1]] <- dark_performance_index
  }
  # Add a row with the counts to the results data frame
  results[nrow(results) + 1,] <- counts
}
rm(animal,dark_performance_index,counts,nosepokes_df,subset_df,nosepokes_object,nosepokes_objects,subset_df2,visits_object,visits_df)

# Merge with the actual results dataframe
results_df <- merge(results_df,results,by.x = 1,by.y = 1,all.x = T)

#### Place Error - Total ####
# Get the names of all objects that will be used for this variable
visits_objects <- ls(pattern = "_visits$")
# Reorder to follow the challenges
visits_objects <- visits_objects[c(3,4)]
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

#### Place Error - Light ####
# Get the names of all objects that will be used for this variable
visits_objects <- ls(pattern = "_visits$")
# Reorder to follow the challenges
visits_objects <- visits_objects[c(3,4)]
visits_objects[!is.na(visits_objects)] -> visits_objects  
# Create a vector of column names for the results data frame
column_names <- c("Animal", paste0(rep(gsub("_visits","",visits_objects), each = 1), c("_Light_PlaceErrorNonAdjusted")))

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
    light_place_error <- nrow(subset_df[which(subset_df$NosepokeNumber > 0 & subset_df$CornerCondition == "Incorrect"),])/nrow(subset_df[which(subset_df$NosepokeNumber > 0),])
    if (nrow(subset_df) < 1){
      light_place_error <- NA
    }
    # Add the counts to the counts list
    counts[[length(counts) + 1]] <- light_place_error
  }
  # Add a row with the counts to the results data frame
  results[nrow(results) + 1,] <- counts
}
rm(light_place_error,counts,visits_df,subset_df,visits_object,visits_objects,holding)

# Merge with the actual results dataframe
results_df <- merge(results_df,results,by.x = 1,by.y = 1,all.x = T)

#### Place Error - Dark ####
# Get the names of all objects that will be used for this variable
visits_objects <- ls(pattern = "_visits$")
# Reorder to follow the challenges
visits_objects <- visits_objects[c(15,17,18,16,13,5,6,7,8,9,10,11,12,14,3,4,1,2)]
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

#### Challenge Error - Dark (only) ####
# Get the names of all objects that will be used for this variable
visits_objects <- ls(pattern = "_visits$")

# Reorder to follow the challenges
visits_objects <- c("RL_visits","MRL1_visits","MRL2_visits","MRL3_visits","MRL4_visits","MRL_visits","ELM2A_visits","ELM2A_active_visits","ELM2R_visits","ELM2R_active_visits") # Reversal, Multiple, ELM2A, ELM2R

# Create a vector of column names for the results data frame
column_names <- c("Animal", paste0(rep(gsub("_visits","",visits_objects), each = 2), c("_Dark_PlaceErrorAdjusted","_Dark_ChallengeError")))

# Create the results data frame
results <- setNames(data.frame(matrix(ncol = length(column_names), nrow = 0)), column_names)

# Iterate through all the animals
for (animal in all_animals) {
  # Create a list to store the counts for each challenge object
  counts <- list(animal)
  get("PL_visits") -> previous_correctRL
  subset(previous_correctRL,Animal == animal & Module == "1 Place learning 1" & CornerCondition == "Correct") -> previous_correctRL
  if(nrow(previous_correctRL) < 1){
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
    previous_correctRL[1,"Corner"] -> previous_correctRL
    if(previous_correctRL == 1){
      previous_correctMRL1 <- 3
      previous_correctMRL2 <- 1
      previous_correctMRL3 <- 3
      previous_correctMRL4 <- 1
      previous_correctELM <- 1
    } else if(previous_correctRL == 3){
      previous_correctMRL1 <- 1
      previous_correctMRL2 <- 3
      previous_correctMRL3 <- 1
      previous_correctMRL4 <- 3
      previous_correctELM <- 3
    } else if(previous_correctRL == 2){
      previous_correctMRL1 <- 4
      previous_correctMRL2 <- 2
      previous_correctMRL3 <- 4
      previous_correctMRL4 <- 2
      previous_correctELM <- 2
    } else if(previous_correctRL == 4){
      previous_correctMRL1 <- 2
      previous_correctMRL2 <- 4
      previous_correctMRL3 <- 2
      previous_correctMRL4 <- 4
      previous_correctELM <- 4
    }
    # Previous for Reversal Learning
    subset(RL_visits,Animal == animal & Light_Status == "Dark") -> subset_df
    counts[[length(counts) + 1]] <- (nrow(subset_df[which(subset_df$CornerCondition == "Incorrect" & subset_df$NosepokeNumber > 0),])-nrow(subset_df[which(subset_df$NosepokeNumber > 0 & subset_df$Corner == previous_correctRL),]))/(nrow(subset_df[which(subset_df$NosepokeNumber > 0),])-nrow(subset_df[which(subset_df$NosepokeNumber > 0 & subset_df$Corner == previous_correctRL),]))
    counts[[length(counts) + 1]] <- nrow(subset_df[which(subset_df$NosepokeNumber > 0 & subset_df$Corner == previous_correctRL),])/nrow(subset_df[which(subset_df$NosepokeNumber > 0),])
    # Previous for MRL Time 1
    subset(MRL_visits,Animal == animal & (Start_Hour >= 18 & Start_Hour <= 20) & Light_Status == "Dark") -> subset_df
    counts[[length(counts) + 1]] <- (nrow(subset_df[which(subset_df$CornerCondition == "Incorrect" & subset_df$NosepokeNumber > 0),])-nrow(subset_df[which(subset_df$NosepokeNumber > 0 & subset_df$Corner == previous_correctMRL1),]))/(nrow(subset_df[which(subset_df$NosepokeNumber > 0),])-nrow(subset_df[which(subset_df$NosepokeNumber > 0 & subset_df$Corner == previous_correctMRL1),]))
    counts[[length(counts) + 1]] <- nrow(subset_df[which(subset_df$NosepokeNumber > 0 & subset_df$Corner == previous_correctMRL1),])/nrow(subset_df[which(subset_df$NosepokeNumber > 0),])
    DA_MRL1 <- nrow(subset_df[which(subset_df$NosepokeNumber > 0),])
    FC_MRL1 <- nrow(subset_df[which(subset_df$NosepokeNumber > 0 & subset_df$Corner == previous_correctMRL1),])
    PEP1_MRL1 <-  (nrow(subset_df[which(subset_df$CornerCondition == "Incorrect" & subset_df$NosepokeNumber > 0),])-nrow(subset_df[which(subset_df$NosepokeNumber > 0 & subset_df$Corner == previous_correctMRL1),]))
    PEP2_MRL1 <- (nrow(subset_df[which(subset_df$NosepokeNumber > 0),])-nrow(subset_df[which(subset_df$NosepokeNumber > 0 & subset_df$Corner == previous_correctMRL1),]))
    # Previous for MRL Time 2
    subset(MRL_visits,Animal == animal & (Start_Hour >= 21 & Start_Hour <= 23) & Light_Status == "Dark") -> subset_df
    counts[[length(counts) + 1]] <- (nrow(subset_df[which(subset_df$CornerCondition == "Incorrect" & subset_df$NosepokeNumber > 0),])-nrow(subset_df[which(subset_df$NosepokeNumber > 0 & subset_df$Corner == previous_correctMRL2),]))/(nrow(subset_df[which(subset_df$NosepokeNumber > 0),])-nrow(subset_df[which(subset_df$NosepokeNumber > 0 & subset_df$Corner == previous_correctMRL2),]))
    counts[[length(counts) + 1]] <- nrow(subset_df[which(subset_df$NosepokeNumber > 0 & subset_df$Corner == previous_correctMRL2),])/nrow(subset_df[which(subset_df$NosepokeNumber > 0),])
    DA_MRL2 <- nrow(subset_df[which(subset_df$NosepokeNumber > 0),])
    FC_MRL2 <- nrow(subset_df[which(subset_df$NosepokeNumber > 0 & subset_df$Corner == previous_correctMRL2),])
    PEP1_MRL2 <-  (nrow(subset_df[which(subset_df$CornerCondition == "Incorrect" & subset_df$NosepokeNumber > 0),])-nrow(subset_df[which(subset_df$NosepokeNumber > 0 & subset_df$Corner == previous_correctMRL2),]))
    PEP2_MRL2 <- (nrow(subset_df[which(subset_df$NosepokeNumber > 0),])-nrow(subset_df[which(subset_df$NosepokeNumber > 0 & subset_df$Corner == previous_correctMRL2),]))
    # Previous for MRL Time 3
    subset(MRL_visits,Animal == animal & (Start_Hour >= 00 & Start_Hour <= 02) & Light_Status == "Dark") -> subset_df
    counts[[length(counts) + 1]] <- (nrow(subset_df[which(subset_df$CornerCondition == "Incorrect" & subset_df$NosepokeNumber > 0),])-nrow(subset_df[which(subset_df$NosepokeNumber > 0 & subset_df$Corner == previous_correctMRL3),]))/(nrow(subset_df[which(subset_df$NosepokeNumber > 0),])-nrow(subset_df[which(subset_df$NosepokeNumber > 0 & subset_df$Corner == previous_correctMRL3),]))
    counts[[length(counts) + 1]] <- nrow(subset_df[which(subset_df$NosepokeNumber > 0 & subset_df$Corner == previous_correctMRL3),])/nrow(subset_df[which(subset_df$NosepokeNumber > 0),])
    DA_MRL3 <- nrow(subset_df[which(subset_df$NosepokeNumber > 0),])
    FC_MRL3 <- nrow(subset_df[which(subset_df$NosepokeNumber > 0 & subset_df$Corner == previous_correctMRL3),])
    PEP1_MRL3 <-  (nrow(subset_df[which(subset_df$CornerCondition == "Incorrect" & subset_df$NosepokeNumber > 0),])-nrow(subset_df[which(subset_df$NosepokeNumber > 0 & subset_df$Corner == previous_correctMRL3),]))
    PEP2_MRL3 <- (nrow(subset_df[which(subset_df$NosepokeNumber > 0),])-nrow(subset_df[which(subset_df$NosepokeNumber > 0 & subset_df$Corner == previous_correctMRL3),]))
    # Previous for MRL Time 4
    subset(MRL_visits,Animal == animal & (Start_Hour >= 03 & Start_Hour <= 05) & Light_Status == "Dark") -> subset_df
    counts[[length(counts) + 1]] <- (nrow(subset_df[which(subset_df$CornerCondition == "Incorrect" & subset_df$NosepokeNumber > 0),])-nrow(subset_df[which(subset_df$NosepokeNumber > 0 & subset_df$Corner == previous_correctMRL4),]))/(nrow(subset_df[which(subset_df$NosepokeNumber > 0),])-nrow(subset_df[which(subset_df$NosepokeNumber > 0 & subset_df$Corner == previous_correctMRL4),]))
    counts[[length(counts) + 1]] <- nrow(subset_df[which(subset_df$NosepokeNumber > 0 & subset_df$Corner == previous_correctMRL4),])/nrow(subset_df[which(subset_df$NosepokeNumber > 0),])
    DA_MRL4 <- nrow(subset_df[which(subset_df$NosepokeNumber > 0),])
    FC_MRL4 <- nrow(subset_df[which(subset_df$NosepokeNumber > 0 & subset_df$Corner == previous_correctMRL4),])
    PEP1_MRL4 <-  (nrow(subset_df[which(subset_df$CornerCondition == "Incorrect" & subset_df$NosepokeNumber > 0),])-nrow(subset_df[which(subset_df$NosepokeNumber > 0 & subset_df$Corner == previous_correctMRL4),]))
    PEP2_MRL4 <- (nrow(subset_df[which(subset_df$NosepokeNumber > 0),])-nrow(subset_df[which(subset_df$NosepokeNumber > 0 & subset_df$Corner == previous_correctMRL4),]))
    # Calculate for Total MRL
    counts[[length(counts) + 1]] <- (PEP1_MRL1+PEP1_MRL2+PEP1_MRL3+PEP1_MRL4)/(PEP2_MRL1+PEP2_MRL2+PEP2_MRL3+PEP2_MRL4)
    counts[[length(counts) + 1]] <- (FC_MRL1+FC_MRL2+FC_MRL3+FC_MRL4)/(DA_MRL1+DA_MRL2+DA_MRL3+DA_MRL4)
    # Calculate for ELM2A
    subset(ELM2A_visits,Animal == animal & Light_Status == "Dark") -> subset_df
    counts[[length(counts) + 1]] <- (nrow(subset_df[which(subset_df$CornerCondition == "Incorrect" & subset_df$NosepokeNumber > 0),])-nrow(subset_df[which(subset_df$NosepokeNumber > 0 & subset_df$Corner == previous_correctELM),]))/(nrow(subset_df[which(subset_df$NosepokeNumber > 0),])-nrow(subset_df[which(subset_df$NosepokeNumber > 0 & subset_df$Corner == previous_correctELM),]))
    counts[[length(counts) + 1]] <- nrow(subset_df[which(subset_df$NosepokeNumber > 0 & subset_df$Corner == previous_correctELM),])/nrow(subset_df[which(subset_df$NosepokeNumber > 0),])
    # Calculate for ELM2A Active
    subset(ELM2A_active_visits,Animal == animal & Light_Status == "Dark") -> subset_df
    counts[[length(counts) + 1]] <- (nrow(subset_df[which(subset_df$CornerCondition == "Incorrect" & subset_df$NosepokeNumber > 0),])-nrow(subset_df[which(subset_df$NosepokeNumber > 0 & subset_df$Corner == previous_correctELM),]))/(nrow(subset_df[which(subset_df$NosepokeNumber > 0),])-nrow(subset_df[which(subset_df$NosepokeNumber > 0 & subset_df$Corner == previous_correctELM),]))
    counts[[length(counts) + 1]] <- nrow(subset_df[which(subset_df$NosepokeNumber > 0 & subset_df$Corner == previous_correctELM),])/nrow(subset_df[which(subset_df$NosepokeNumber > 0),])
    # Calculate for ELM2R
    subset(ELM2R_visits,Animal == animal & Light_Status == "Dark") -> subset_df
    counts[[length(counts) + 1]] <- (nrow(subset_df[which(subset_df$CornerCondition == "Incorrect" & subset_df$NosepokeNumber > 0),])-nrow(subset_df[which(subset_df$NosepokeNumber > 0 & subset_df$Corner == previous_correctELM),]))/(nrow(subset_df[which(subset_df$NosepokeNumber > 0),])-nrow(subset_df[which(subset_df$NosepokeNumber > 0 & subset_df$Corner == previous_correctELM),]))
    counts[[length(counts) + 1]] <- nrow(subset_df[which(subset_df$NosepokeNumber > 0 & subset_df$Corner == previous_correctELM),])/nrow(subset_df[which(subset_df$NosepokeNumber > 0),])
    # Calculate for ELM2R Active
    subset(ELM2R_active_visits,Animal == animal & Light_Status == "Dark") -> subset_df
    counts[[length(counts) + 1]] <- (nrow(subset_df[which(subset_df$CornerCondition == "Incorrect" & subset_df$NosepokeNumber > 0),])-nrow(subset_df[which(subset_df$NosepokeNumber > 0 & subset_df$Corner == previous_correctELM),]))/(nrow(subset_df[which(subset_df$NosepokeNumber > 0),])-nrow(subset_df[which(subset_df$NosepokeNumber > 0 & subset_df$Corner == previous_correctELM),]))
    counts[[length(counts) + 1]] <- nrow(subset_df[which(subset_df$NosepokeNumber > 0 & subset_df$Corner == previous_correctELM),])/nrow(subset_df[which(subset_df$NosepokeNumber > 0),])  
    
  }
  
  # Add a row with the counts to the results data frame
  results[nrow(results) + 1,] <- counts
}
rm(animal,DA_MRL1,FC_MRL1,DA_MRL2,FC_MRL2,DA_MRL3,FC_MRL3,DA_MRL4,FC_MRL4,counts,subset_df,visits_objects,previous_correctELM,previous_correctMRL1,previous_correctMRL2,previous_correctMRL3,previous_correctMRL4,previous_correctRL,PEP1_MRL1,PEP1_MRL2,PEP1_MRL3,PEP1_MRL4,PEP2_MRL1,PEP2_MRL2,PEP2_MRL3,PEP2_MRL4)

# Merge with the actual results dataframe
results_df <- merge(results_df,results,by.x = 1,by.y = 1,all.x = T)

#### Time Error - Dark Phase ####
# Get the names of all objects that will be used for this variable
visits_objects <- c("ELM1A_visits","ELM1R_visits","ELM2A_visits","ELM2R_visits")

# Create a vector of column names for the results data frame
column_names <- c("Animal", paste0(rep(gsub("_visits","",visits_objects), each = 1), c("_Dark_TimeError")))

# Create the results data frame
results <- setNames(data.frame(matrix(ncol = length(column_names), nrow = 0)), column_names)

# Iterate through all the animals
for (animal in all_animals) {
  # Create a list to store the counts for each challenge object
  counts <- list(animal)
  
  # ELM1 Acquisition
  visits_df <- get("ELM1A_visits")
  
  # Subset the data frame based on each individual animal
  subset_df <- subset(visits_df, Animal == animal)
  
  counts[[length(counts) + 1]] <- nrow(subset_df[which(subset_df$NosepokeNumber >0 & subset_df$Module != "2 ELM 1 - Acquisition"),])/nrow(subset_df[which(subset_df$NosepokeNumber >0),])
  
  # ELM1 Retrieval
  visits_df <- get("ELM1R_visits")
  
  # Subset the data frame based on each individual animal
  subset_df <- subset(visits_df, Animal == animal)
  
  counts[[length(counts) + 1]] <- nrow(subset_df[which(subset_df$NosepokeNumber >0 & subset_df$Module != "2 ELM 1 - Retrieval"),])/nrow(subset_df[which(subset_df$NosepokeNumber >0),])
  
  # ELM2 Acquisition
  visits_df <- get("ELM2A_visits")
  
  # Subset the data frame based on each individual animal
  subset_df <- subset(visits_df, Animal == animal)
  
  counts[[length(counts) + 1]] <- nrow(subset_df[which(subset_df$NosepokeNumber >0 & subset_df$Module != "2 ELM 2 - Acquisition"),])/nrow(subset_df[which(subset_df$NosepokeNumber >0),])
  
  # ELM2 Retrieval
  visits_df <- get("ELM2R_visits")
  
  # Subset the data frame based on each individual animal
  subset_df <- subset(visits_df, Animal == animal)
  
  counts[[length(counts) + 1]] <- nrow(subset_df[which(subset_df$NosepokeNumber >0 & subset_df$Module != "2 ELM 2 - Retrieval"),])/nrow(subset_df[which(subset_df$NosepokeNumber >0),])
  
  # Add to the results object
  results[nrow(results) + 1,] <- counts
}
rm(animal,counts,visits_df,subset_df,visits_objects)

# Merge with the actual results dataframe
results_df <- merge(results_df,results,by.x = 1,by.y = 1,all.x = T)

#### Direction Error and Place Error for CCW - Total ####
# Get the names of all objects that will be used for this variable
visits_objects <- c("CCW1_visits","CCW2_visits")

# Create a vector of column names for the results data frame
column_names <- c("Animal", paste0(rep(gsub("_visits","",visits_objects), each = 2), c("_Total_PlaceErrorAdjusted","_Total_DirectionError")))

# Create the results data frame
results <- setNames(data.frame(matrix(ncol = length(column_names), nrow = 0)), column_names)

# Iterate through all the animals
for (animal in all_animals) {
  # Create a list to store the counts for each challenge object
  counts <- list(animal)
  
  # The first correct corner in CCW is still a reflection from CW, so it is the CW corner. We count after it finishes the first correct behavior
  visits_df <- get("CCW1_visits")
  
  subset_df <- subset(visits_df, Animal == animal)
  
  #subset_df <- visits_df[which(visits_df$Animal == animal & visits_df$Light_Status == "Light"),]
  
  if (nrow(subset_df) < 2){
    counts[[length(counts) + 1]] <- NA
    counts[[length(counts) + 1]] <- NA
  } else {
    #### Step only required if files are not fixed ####
    #### Cannot activate for Light ####
    #subset_df[which(subset_df$Module != "4 Patrolling 1 clockwise"),] -> subset_df
    
    #### Continuing as normal ####
    row.names(subset_df) <- NULL
    if(sum(subset_df[,"LickNumber"]) == 0){
      # In this case everything revolves around the only correct corner
      subset_df[which(subset_df$CornerCondition == "Correct"),9][1] -> correct_corner_CCW1_nolicks
      if (correct_corner_CCW1_nolicks == 1){
        DE_CCW1 <- nrow(subset_df[which(subset_df$Corner == 2 & subset_df$NosepokeNumber > 0),])
      } else if (correct_corner_CCW1_nolicks == 2){
        DE_CCW1 <- nrow(subset_df[which(subset_df$Corner == 3 & subset_df$NosepokeNumber > 0),])
      } else if (correct_corner_CCW1_nolicks == 3){
        DE_CCW1 <- nrow(subset_df[which(subset_df$Corner == 4 & subset_df$NosepokeNumber > 0),])
      } else if (correct_corner_CCW1_nolicks == 4){
        DE_CCW1 <- nrow(subset_df[which(subset_df$Corner == 1 & subset_df$NosepokeNumber > 0),])
      }
      # Place Error - CCW1
      counts[[length(counts) + 1]] <- (nrow(subset_df[which(subset_df$CornerCondition == "Incorrect" & subset_df$NosepokeNumber > 0),])-DE_CCW1)/nrow(subset_df[which(subset_df$NosepokeNumber > 0),])
      # Direction Error - CCW1
      counts[[length(counts) + 1]] <- (DE_CCW1)/nrow(subset_df[which(subset_df$NosepokeNumber > 0),])
    } else {
      # Saving the very last correct corner for CCW2
      subset_df[which(subset_df$LickNumber > 0),"Corner"][length(subset_df[which(subset_df$LickNumber > 0),"Corner"])] -> last_licked_corner_CCW1 
      
      # This is the first moment where the mice see it can lick a CCW corner
      # Two options, first lick or first time a CCW corner opens?
      # Lick done in a CCW corner
      #subset_df[as.numeric(row.names(subset_df)) >= as.numeric(row.names(subset_df[which(subset_df$LickNumber > 0),][2,])),] -> subset_df
      #row.names(subset_df) <- NULL
      
      # Corner simply opening
      if(subset_df[which(subset_df$LickNumber > 0),][1,"Corner"] == 1){
        next_corner <- 4
      } else if (subset_df[which(subset_df$LickNumber > 0),][1,"Corner"] == 2){
        next_corner <- 1
      } else if (subset_df[which(subset_df$LickNumber > 0),][1,"Corner"] == 3){
        next_corner <- 2
      } else if (subset_df[which(subset_df$LickNumber > 0),][1,"Corner"] == 4){
        next_corner <- 3
      }
      
      subset_df[as.numeric(row.names(subset_df)) > as.numeric(row.names(subset_df[which(subset_df$LickNumber > 0),][1,])),] -> subset_df
      subset_df[as.numeric(row.names(subset_df)) >= as.numeric(row.names(subset_df[which(subset_df$Corner == next_corner),][1,])),] -> subset_df
      row.names(subset_df) <- NULL
      
      c(1,as.numeric(row.names(subset_df[which(subset_df$LickNumber > 0),])),row.names(subset_df[nrow(subset_df),])) -> DE_corners
      if(DE_corners[1] == DE_corners[2]){
        DE_corners[-1] -> DE_corners
      }
      if(DE_corners[length(DE_corners)] == DE_corners[length(DE_corners)-1]){
        DE_corners[-length(DE_corners)] -> DE_corners
      }
      as.numeric(DE_corners) -> DE_corners
      
      DE_CCW1 <- 0
      
      for (x in 1:(length(DE_corners)-1)){
        subset_df[as.numeric(row.names(subset_df)) >= DE_corners[x] & as.numeric(row.names(subset_df)) < DE_corners[x+1],] -> DE_evaluate
        if (DE_evaluate[1,"Corner"] == 1){
          DE_CCW1 <- DE_CCW1+nrow(DE_evaluate[which(DE_evaluate$Corner == 2 & DE_evaluate$NosepokeNumber > 0),])
        } else if (DE_evaluate[1,"Corner"] == 2){
          DE_CCW1 <- DE_CCW1+nrow(DE_evaluate[which(DE_evaluate$Corner == 3 & DE_evaluate$NosepokeNumber > 0),])
        } else if (DE_evaluate[1,"Corner"] == 3){
          DE_CCW1 <- DE_CCW1+nrow(DE_evaluate[which(DE_evaluate$Corner == 4 & DE_evaluate$NosepokeNumber > 0),])
        } else if (DE_evaluate[1,"Corner"] == 4){
          DE_CCW1 <- DE_CCW1+nrow(DE_evaluate[which(DE_evaluate$Corner == 1 & DE_evaluate$NosepokeNumber > 0),])
        }
      }
      # Now to calculate the scores, including the place error
      # Place Error - CCW1
      counts[[length(counts) + 1]] <- (nrow(subset_df[which(subset_df$CornerCondition == "Incorrect" & subset_df$NosepokeNumber > 0),])-DE_CCW1)/nrow(subset_df[which(subset_df$NosepokeNumber > 0),])
      # Direction Error - CCW1
      counts[[length(counts) + 1]] <- (DE_CCW1)/nrow(subset_df[which(subset_df$NosepokeNumber > 0),])
    }
  }
  
  #### For CCW2 ####
  visits_dfCCW2 <- get("CCW2_visits")
  
  subset_dfCCW2 <- subset(visits_dfCCW2, Animal == animal)
  
  if (nrow(subset_dfCCW2) < 2){
    counts[[length(counts) + 1]] <- NA
    counts[[length(counts) + 1]] <- NA
  } else if (sum(subset_dfCCW2$LickNumber) == 0) {
    subset_dfCCW2[which(subset_dfCCW2$CornerCondition == "Correct"),9][1] -> correct_corner_CCW2_nolicks
    if (correct_corner_CCW2_nolicks == 1){
      DE_CCW2 <- nrow(subset_dfCCW2[which(subset_dfCCW2$Corner == 2 & subset_dfCCW2$NosepokeNumber > 0),])
    } else if (correct_corner_CCW2_nolicks == 2){
      DE_CCW2 <- nrow(subset_dfCCW2[which(subset_dfCCW2$Corner == 3 & subset_dfCCW2$NosepokeNumber > 0),])
    } else if (correct_corner_CCW2_nolicks == 3){
      DE_CCW2 <- nrow(subset_dfCCW2[which(subset_dfCCW2$Corner == 4 & subset_dfCCW2$NosepokeNumber > 0),])
    } else if (correct_corner_CCW2_nolicks == 4){
      DE_CCW2 <- nrow(subset_dfCCW2[which(subset_dfCCW2$Corner == 1 & subset_dfCCW2$NosepokeNumber > 0),])
    }
    # Place Error - CCW2
    counts[[length(counts) + 1]] <- (nrow(subset_dfCCW2[which(subset_dfCCW2$CornerCondition == "Incorrect" & subset_dfCCW2$NosepokeNumber > 0),])-DE_CCW2)/nrow(subset_dfCCW2[which(subset_dfCCW2$NosepokeNumber > 0),])
    # Direction Error - CCW2
    counts[[length(counts) + 1]] <- (DE_CCW2)/nrow(subset_dfCCW2[which(subset_dfCCW2$NosepokeNumber > 0),])
  } else {
    if(exists("last_licked_corner_CCW1") == T){
      rbind(subset_dfCCW2[1,],subset_dfCCW2) -> subset_dfCCW2
      subset_dfCCW2[1,] <- NA
      subset_dfCCW2[1,"Corner"] <- last_licked_corner_CCW1
      
      row.names(subset_dfCCW2) <- NULL
      
      # Getting all rows where there was a lick
      c(1,as.numeric(row.names(subset_dfCCW2[which(subset_dfCCW2$LickNumber > 0),])),row.names(subset_dfCCW2[nrow(subset_dfCCW2),])) -> DECCW2_corners
      if(DECCW2_corners[1] == DECCW2_corners[2]){
        DECCW2_corners[-1] -> DECCW2_corners
      }
      if(DECCW2_corners[length(DECCW2_corners)] == DECCW2_corners[length(DECCW2_corners)-1]){
        DECCW2_corners[-length(DECCW2_corners)] -> DECCW2_corners
      }
      as.numeric(DECCW2_corners) -> DECCW2_corners
      
      DE_CCW2 <- 0
      
      for (x in 1:(length(DECCW2_corners)-1)){
        subset_dfCCW2[as.numeric(row.names(subset_dfCCW2)) >= DECCW2_corners[x] & as.numeric(row.names(subset_dfCCW2)) < DECCW2_corners[x+1],] -> DECCW2_evaluate
        if (DECCW2_evaluate[1,"Corner"] == 1){
          DE_CCW2 <- DE_CCW2+nrow(DECCW2_evaluate[which(DECCW2_evaluate$Corner == 2 & DECCW2_evaluate$NosepokeNumber > 0),])
        } else if (DECCW2_evaluate[1,"Corner"] == 2){
          DE_CCW2 <- DE_CCW2+nrow(DECCW2_evaluate[which(DECCW2_evaluate$Corner == 3 & DECCW2_evaluate$NosepokeNumber > 0),])
        } else if (DECCW2_evaluate[1,"Corner"] == 3){
          DE_CCW2 <- DE_CCW2+nrow(DECCW2_evaluate[which(DECCW2_evaluate$Corner == 4 & DECCW2_evaluate$NosepokeNumber > 0),])
        } else if (DECCW2_evaluate[1,"Corner"] == 4){
          DE_CCW2 <- DE_CCW2+nrow(DECCW2_evaluate[which(DECCW2_evaluate$Corner == 1 & DECCW2_evaluate$NosepokeNumber > 0),])
        }
      }
      
      # Now to calculate the scores, including the place error
      # Place Error - CCW2
      counts[[length(counts) + 1]] <- (nrow(subset_dfCCW2[which(subset_dfCCW2$CornerCondition == "Incorrect" & subset_dfCCW2$NosepokeNumber > 0),])-DE_CCW2)/nrow(subset_dfCCW2[which(subset_dfCCW2$NosepokeNumber > 0),])
      # Direction Error - CCW2
      counts[[length(counts) + 1]] <- (DE_CCW2)/nrow(subset_dfCCW2[which(subset_dfCCW2$NosepokeNumber > 0),])
    } else {
      # When there were no licks in CCW1
      row.names(subset_dfCCW2) <- NULL
      
      # Getting all rows where there was a lick
      c(as.numeric(row.names(subset_dfCCW2[which(subset_dfCCW2$LickNumber > 0),])),row.names(subset_dfCCW2[nrow(subset_dfCCW2),])) -> DECCW2_corners
      DECCW2_corners[-1] -> DECCW2_corners
      as.numeric(DECCW2_corners) -> DECCW2_corners
      
      DE_CCW2 <- 0
      
      if(length(DECCW2_corners) > 1){
        for (x in 1:(length(DECCW2_corners)-1)){
          subset_dfCCW2[as.numeric(row.names(subset_dfCCW2)) >= DECCW2_corners[x] & as.numeric(row.names(subset_dfCCW2)) < DECCW2_corners[x+1],] -> DECCW2_evaluate
          if (DECCW2_evaluate[1,"Corner"] == 1){
            DE_CCW2 <- DE_CCW2+nrow(DECCW2_evaluate[which(DECCW2_evaluate$Corner == 2 & DECCW2_evaluate$NosepokeNumber > 0),])
          } else if (DECCW2_evaluate[1,"Corner"] == 2){
            DE_CCW2 <- DE_CCW2+nrow(DECCW2_evaluate[which(DECCW2_evaluate$Corner == 3 & DECCW2_evaluate$NosepokeNumber > 0),])
          } else if (DECCW2_evaluate[1,"Corner"] == 3){
            DE_CCW2 <- DE_CCW2+nrow(DECCW2_evaluate[which(DECCW2_evaluate$Corner == 4 & DECCW2_evaluate$NosepokeNumber > 0),])
          } else if (DECCW2_evaluate[1,"Corner"] == 4){
            DE_CCW2 <- DE_CCW2+nrow(DECCW2_evaluate[which(DECCW2_evaluate$Corner == 1 & DECCW2_evaluate$NosepokeNumber > 0),])
          }
        }
        # Now to calculate the scores, including the place error
        subset_dfCCW2[DECCW2_corners[1]+1:nrow(subset_dfCCW2),] -> subset_dfCCW2
        # Place Error - CCW2
        counts[[length(counts) + 1]] <- (nrow(subset_dfCCW2[which(subset_dfCCW2$CornerCondition == "Incorrect" & subset_dfCCW2$NosepokeNumber > 0),])-DE_CCW2)/nrow(subset_dfCCW2[which(subset_dfCCW2$NosepokeNumber > 0),])
        # Direction Error - CCW2
        counts[[length(counts) + 1]] <- (DE_CCW2)/nrow(subset_dfCCW2[which(subset_dfCCW2$NosepokeNumber > 0),])
      } else {
        c(as.numeric(row.names(subset_dfCCW2[which(subset_dfCCW2$LickNumber > 0),])),row.names(subset_dfCCW2[nrow(subset_dfCCW2),])) -> DECCW2_corners
        subset_dfCCW2[as.numeric(row.names(subset_dfCCW2)) >= DECCW2_corners[x],] -> DECCW2_evaluate
        if (DECCW2_evaluate[1,"Corner"] == 1){
          DE_CCW2 <- DE_CCW2+nrow(DECCW2_evaluate[which(DECCW2_evaluate$Corner == 2 & DECCW2_evaluate$NosepokeNumber > 0),])
        } else if (DECCW2_evaluate[1,"Corner"] == 2){
          DE_CCW2 <- DE_CCW2+nrow(DECCW2_evaluate[which(DECCW2_evaluate$Corner == 3 & DECCW2_evaluate$NosepokeNumber > 0),])
        } else if (DECCW2_evaluate[1,"Corner"] == 3){
          DE_CCW2 <- DE_CCW2+nrow(DECCW2_evaluate[which(DECCW2_evaluate$Corner == 4 & DECCW2_evaluate$NosepokeNumber > 0),])
        } else if (DECCW2_evaluate[1,"Corner"] == 4){
          DE_CCW2 <- DE_CCW2+nrow(DECCW2_evaluate[which(DECCW2_evaluate$Corner == 1 & DECCW2_evaluate$NosepokeNumber > 0),])
        }
        # Place Error - CCW2
        counts[[length(counts) + 1]] <- (nrow(subset_dfCCW2[which(subset_dfCCW2$CornerCondition == "Incorrect" & subset_dfCCW2$NosepokeNumber > 0),])-DE_CCW2)/nrow(subset_dfCCW2[which(subset_dfCCW2$NosepokeNumber > 0),])
        # Direction Error - CCW2
        counts[[length(counts) + 1]] <- (DE_CCW2)/nrow(subset_dfCCW2[which(subset_dfCCW2$NosepokeNumber > 0),])
      }
    }
  }
  # Add to the results object
  results[nrow(results) + 1,] <- counts
  rm(last_licked_corner_CCW1,next_corner,DE_corners,DE_CCW1,DE_evaluate,subset_df,subset_dfCCW2,DECCW2_corners,DE_CCW2,DECCW2_evaluate,counts,x,correct_corner_CCW1_nolicks,correct_corner_CCW2_nolicks)
}
rm(animal,visits_df,visits_objects,column_names)

# Merge with the actual results dataframe
results_df <- merge(results_df,results,by.x = 1,by.y = 1,all.x = T)

#### Direction Error and Place Error for CCW - Dark ####
# Get the names of all objects that will be used for this variable
visits_objects <- c("CCW1_visits","CCW2_visits")

# Create a vector of column names for the results data frame
column_names <- c("Animal", paste0(rep(gsub("_visits","",visits_objects), each = 2), c("_Dark_PlaceErrorAdjusted","_Dark_DirectionError")))

# Create the results data frame
results <- setNames(data.frame(matrix(ncol = length(column_names), nrow = 0)), column_names)

# Iterate through all the animals
for (animal in all_animals) {
  # Create a list to store the counts for each challenge object
  counts <- list(animal)
  
  # The first correct corner in CCW is still a reflection from CW, so it is the CW corner. We count after it finishes the first correct behavior
  visits_df <- get("CCW1_visits")
  
  subset_df <- subset(visits_df, Animal == animal)
  
  #subset_df <- visits_df[which(visits_df$Animal == animal & visits_df$Light_Status == "Light"),]
  
  if (nrow(subset_df[which(subset_df$Light_Status == "Dark"),]) < 2){
    counts[[length(counts) + 1]] <- NA
    counts[[length(counts) + 1]] <- NA
  } else {
    #### Step only required if files are not fixed ####
    #### Cannot activate for Light ####
    #subset_df[which(subset_df$Module != "4 Patrolling 1 clockwise"),] -> subset_df
    
    #### Continuing as normal ####
    row.names(subset_df) <- NULL
    if(sum(subset_df[which(subset_df$Light_Status == "Dark"),"LickNumber"]) == 0){
      # In this case everything revolves around the only correct corner
      subset_df[which(subset_df$CornerCondition == "Correct" & subset_df$Light_Status == "Dark"),9][1] -> correct_corner_CCW1_nolicks
      if (correct_corner_CCW1_nolicks == 1){
        DE_CCW1 <- nrow(subset_df[which(subset_df$Corner == 2 & subset_df$NosepokeNumber > 0 & subset_df$Light_Status == "Dark"),])
      } else if (correct_corner_CCW1_nolicks == 2){
        DE_CCW1 <- nrow(subset_df[which(subset_df$Corner == 3 & subset_df$NosepokeNumber > 0 & subset_df$Light_Status == "Dark"),])
      } else if (correct_corner_CCW1_nolicks == 3){
        DE_CCW1 <- nrow(subset_df[which(subset_df$Corner == 4 & subset_df$NosepokeNumber > 0 & subset_df$Light_Status == "Dark"),])
      } else if (correct_corner_CCW1_nolicks == 4){
        DE_CCW1 <- nrow(subset_df[which(subset_df$Corner == 1 & subset_df$NosepokeNumber > 0 & subset_df$Light_Status == "Dark"),])
      }
      # Place Error - CCW1
      counts[[length(counts) + 1]] <- (nrow(subset_df[which(subset_df$CornerCondition == "Incorrect" & subset_df$NosepokeNumber > 0 & subset_df$Light_Status == "Dark"),])-DE_CCW1)/nrow(subset_df[which(subset_df$NosepokeNumber > 0 & subset_df$Light_Status == "Dark"),])
      # Direction Error - CCW1
      counts[[length(counts) + 1]] <- (DE_CCW1)/nrow(subset_df[which(subset_df$NosepokeNumber > 0 & subset_df$Light_Status == "Dark"),])
    } else {
      # Saving the very last correct corner for CCW2
      subset_df[which(subset_df$LickNumber > 0 & subset_df$Light_Status == "Dark"),"Corner"][length(subset_df[which(subset_df$LickNumber > 0 & subset_df$Light_Status == "Dark"),"Corner"])] -> last_licked_corner_CCW1 
      
      subset_df <- subset(subset_df, Animal == animal & Light_Status == "Dark")
      
      # This is the first moment where the mice see it can lick a CCW corner
      # Two options, first lick or first time a CCW corner opens?
      # Lick done in a CCW corner
      #subset_df[as.numeric(row.names(subset_df)) >= as.numeric(row.names(subset_df[which(subset_df$LickNumber > 0),][2,])),] -> subset_df
      #row.names(subset_df) <- NULL
      
      # Corner simply opening
      if(subset_df[which(subset_df$LickNumber > 0),][1,"Corner"] == 1){
        next_corner <- 4
      } else if (subset_df[which(subset_df$LickNumber > 0),][1,"Corner"] == 2){
        next_corner <- 1
      } else if (subset_df[which(subset_df$LickNumber > 0),][1,"Corner"] == 3){
        next_corner <- 2
      } else if (subset_df[which(subset_df$LickNumber > 0),][1,"Corner"] == 4){
        next_corner <- 3
      }
      
      subset_df[as.numeric(row.names(subset_df)) > as.numeric(row.names(subset_df[which(subset_df$LickNumber > 0),][1,])),] -> subset_df
      subset_df[as.numeric(row.names(subset_df)) >= as.numeric(row.names(subset_df[which(subset_df$Corner == next_corner),][1,])),] -> subset_df
      row.names(subset_df) <- NULL
      
      c(1,as.numeric(row.names(subset_df[which(subset_df$LickNumber > 0),])),row.names(subset_df[nrow(subset_df),])) -> DE_corners
      if(DE_corners[1] == DE_corners[2]){
        DE_corners[-1] -> DE_corners
      }
      if(DE_corners[length(DE_corners)] == DE_corners[length(DE_corners)-1]){
        DE_corners[-length(DE_corners)] -> DE_corners
      }
      as.numeric(DE_corners) -> DE_corners
      
      DE_CCW1 <- 0
      
      for (x in 1:(length(DE_corners)-1)){
        subset_df[as.numeric(row.names(subset_df)) >= DE_corners[x] & as.numeric(row.names(subset_df)) < DE_corners[x+1],] -> DE_evaluate
        if (DE_evaluate[1,"Corner"] == 1){
          DE_CCW1 <- DE_CCW1+nrow(DE_evaluate[which(DE_evaluate$Corner == 2 & DE_evaluate$NosepokeNumber > 0),])
        } else if (DE_evaluate[1,"Corner"] == 2){
          DE_CCW1 <- DE_CCW1+nrow(DE_evaluate[which(DE_evaluate$Corner == 3 & DE_evaluate$NosepokeNumber > 0),])
        } else if (DE_evaluate[1,"Corner"] == 3){
          DE_CCW1 <- DE_CCW1+nrow(DE_evaluate[which(DE_evaluate$Corner == 4 & DE_evaluate$NosepokeNumber > 0),])
        } else if (DE_evaluate[1,"Corner"] == 4){
          DE_CCW1 <- DE_CCW1+nrow(DE_evaluate[which(DE_evaluate$Corner == 1 & DE_evaluate$NosepokeNumber > 0),])
        }
      }
      # Now to calculate the scores, including the place error
      # Place Error - CCW1
      counts[[length(counts) + 1]] <- (nrow(subset_df[which(subset_df$CornerCondition == "Incorrect" & subset_df$NosepokeNumber > 0),])-DE_CCW1)/nrow(subset_df[which(subset_df$NosepokeNumber > 0),])
      # Direction Error - CCW1
      counts[[length(counts) + 1]] <- (DE_CCW1)/nrow(subset_df[which(subset_df$NosepokeNumber > 0),])
    }
  }
  
  #### For CCW2 ####
  visits_dfCCW2 <- get("CCW2_visits")
  
  subset_dfCCW2 <- subset(visits_dfCCW2, Animal == animal & Light_Status == "Dark")
  
  if (nrow(subset_dfCCW2) < 2){
    counts[[length(counts) + 1]] <- NA
    counts[[length(counts) + 1]] <- NA
  } else if (sum(subset_dfCCW2$LickNumber) == 0) {
    subset_dfCCW2[which(subset_dfCCW2$CornerCondition == "Correct"),9][1] -> correct_corner_CCW2_nolicks
    if (correct_corner_CCW2_nolicks == 1){
      DE_CCW2 <- nrow(subset_dfCCW2[which(subset_dfCCW2$Corner == 2 & subset_dfCCW2$NosepokeNumber > 0),])
    } else if (correct_corner_CCW2_nolicks == 2){
      DE_CCW2 <- nrow(subset_dfCCW2[which(subset_dfCCW2$Corner == 3 & subset_dfCCW2$NosepokeNumber > 0),])
    } else if (correct_corner_CCW2_nolicks == 3){
      DE_CCW2 <- nrow(subset_dfCCW2[which(subset_dfCCW2$Corner == 4 & subset_dfCCW2$NosepokeNumber > 0),])
    } else if (correct_corner_CCW2_nolicks == 4){
      DE_CCW2 <- nrow(subset_dfCCW2[which(subset_dfCCW2$Corner == 1 & subset_dfCCW2$NosepokeNumber > 0),])
    }
    # Place Error - CCW2
    counts[[length(counts) + 1]] <- (nrow(subset_dfCCW2[which(subset_dfCCW2$CornerCondition == "Incorrect" & subset_dfCCW2$NosepokeNumber > 0),])-DE_CCW2)/nrow(subset_dfCCW2[which(subset_dfCCW2$NosepokeNumber > 0),])
    # Direction Error - CCW2
    counts[[length(counts) + 1]] <- (DE_CCW2)/nrow(subset_dfCCW2[which(subset_dfCCW2$NosepokeNumber > 0),])
  } else {
    if(exists("last_licked_corner_CCW1") == T){
      rbind(subset_dfCCW2[1,],subset_dfCCW2) -> subset_dfCCW2
      subset_dfCCW2[1,] <- NA
      subset_dfCCW2[1,"Corner"] <- last_licked_corner_CCW1
      
      row.names(subset_dfCCW2) <- NULL
      
      # Getting all rows where there was a lick
      c(1,as.numeric(row.names(subset_dfCCW2[which(subset_dfCCW2$LickNumber > 0),])),row.names(subset_dfCCW2[nrow(subset_dfCCW2),])) -> DECCW2_corners
      if(DECCW2_corners[1] == DECCW2_corners[2]){
        DECCW2_corners[-1] -> DECCW2_corners
      }
      if(DECCW2_corners[length(DECCW2_corners)] == DECCW2_corners[length(DECCW2_corners)-1]){
        DECCW2_corners[-length(DECCW2_corners)] -> DECCW2_corners
      }
      as.numeric(DECCW2_corners) -> DECCW2_corners
      
      DE_CCW2 <- 0
      
      for (x in 1:(length(DECCW2_corners)-1)){
        subset_dfCCW2[as.numeric(row.names(subset_dfCCW2)) >= DECCW2_corners[x] & as.numeric(row.names(subset_dfCCW2)) < DECCW2_corners[x+1],] -> DECCW2_evaluate
        if (DECCW2_evaluate[1,"Corner"] == 1){
          DE_CCW2 <- DE_CCW2+nrow(DECCW2_evaluate[which(DECCW2_evaluate$Corner == 2 & DECCW2_evaluate$NosepokeNumber > 0),])
        } else if (DECCW2_evaluate[1,"Corner"] == 2){
          DE_CCW2 <- DE_CCW2+nrow(DECCW2_evaluate[which(DECCW2_evaluate$Corner == 3 & DECCW2_evaluate$NosepokeNumber > 0),])
        } else if (DECCW2_evaluate[1,"Corner"] == 3){
          DE_CCW2 <- DE_CCW2+nrow(DECCW2_evaluate[which(DECCW2_evaluate$Corner == 4 & DECCW2_evaluate$NosepokeNumber > 0),])
        } else if (DECCW2_evaluate[1,"Corner"] == 4){
          DE_CCW2 <- DE_CCW2+nrow(DECCW2_evaluate[which(DECCW2_evaluate$Corner == 1 & DECCW2_evaluate$NosepokeNumber > 0),])
        }
      }
      
      # Now to calculate the scores, including the place error
      # Place Error - CCW2
      counts[[length(counts) + 1]] <- (nrow(subset_dfCCW2[which(subset_dfCCW2$CornerCondition == "Incorrect" & subset_dfCCW2$NosepokeNumber > 0),])-DE_CCW2)/nrow(subset_dfCCW2[which(subset_dfCCW2$NosepokeNumber > 0),])
      # Direction Error - CCW2
      counts[[length(counts) + 1]] <- (DE_CCW2)/nrow(subset_dfCCW2[which(subset_dfCCW2$NosepokeNumber > 0),])
    } else {
      # When there were no licks in CCW1
      row.names(subset_dfCCW2) <- NULL
      
      # Getting all rows where there was a lick
      c(as.numeric(row.names(subset_dfCCW2[which(subset_dfCCW2$LickNumber > 0),])),row.names(subset_dfCCW2[nrow(subset_dfCCW2),])) -> DECCW2_corners
      DECCW2_corners[-1] -> DECCW2_corners
      as.numeric(DECCW2_corners) -> DECCW2_corners
      
      DE_CCW2 <- 0
      
      if(length(DECCW2_corners) > 1){
        for (x in 1:(length(DECCW2_corners)-1)){
          subset_dfCCW2[as.numeric(row.names(subset_dfCCW2)) >= DECCW2_corners[x] & as.numeric(row.names(subset_dfCCW2)) < DECCW2_corners[x+1],] -> DECCW2_evaluate
          if (DECCW2_evaluate[1,"Corner"] == 1){
            DE_CCW2 <- DE_CCW2+nrow(DECCW2_evaluate[which(DECCW2_evaluate$Corner == 2 & DECCW2_evaluate$NosepokeNumber > 0),])
          } else if (DECCW2_evaluate[1,"Corner"] == 2){
            DE_CCW2 <- DE_CCW2+nrow(DECCW2_evaluate[which(DECCW2_evaluate$Corner == 3 & DECCW2_evaluate$NosepokeNumber > 0),])
          } else if (DECCW2_evaluate[1,"Corner"] == 3){
            DE_CCW2 <- DE_CCW2+nrow(DECCW2_evaluate[which(DECCW2_evaluate$Corner == 4 & DECCW2_evaluate$NosepokeNumber > 0),])
          } else if (DECCW2_evaluate[1,"Corner"] == 4){
            DE_CCW2 <- DE_CCW2+nrow(DECCW2_evaluate[which(DECCW2_evaluate$Corner == 1 & DECCW2_evaluate$NosepokeNumber > 0),])
          }
        }
        # Now to calculate the scores, including the place error
        subset_dfCCW2[DECCW2_corners[1]+1:nrow(subset_dfCCW2),] -> subset_dfCCW2
        # Place Error - CCW2
        counts[[length(counts) + 1]] <- (nrow(subset_dfCCW2[which(subset_dfCCW2$CornerCondition == "Incorrect" & subset_dfCCW2$NosepokeNumber > 0),])-DE_CCW2)/nrow(subset_dfCCW2[which(subset_dfCCW2$NosepokeNumber > 0),])
        # Direction Error - CCW2
        counts[[length(counts) + 1]] <- (DE_CCW2)/nrow(subset_dfCCW2[which(subset_dfCCW2$NosepokeNumber > 0),])
      } else {
        c(as.numeric(row.names(subset_dfCCW2[which(subset_dfCCW2$LickNumber > 0),])),row.names(subset_dfCCW2[nrow(subset_dfCCW2),])) -> DECCW2_corners
        subset_dfCCW2[as.numeric(row.names(subset_dfCCW2)) >= DECCW2_corners[1],] -> DECCW2_evaluate
        if (DECCW2_evaluate[1,"Corner"] == 1){
          DE_CCW2 <- DE_CCW2+nrow(DECCW2_evaluate[which(DECCW2_evaluate$Corner == 2 & DECCW2_evaluate$NosepokeNumber > 0),])
        } else if (DECCW2_evaluate[1,"Corner"] == 2){
          DE_CCW2 <- DE_CCW2+nrow(DECCW2_evaluate[which(DECCW2_evaluate$Corner == 3 & DECCW2_evaluate$NosepokeNumber > 0),])
        } else if (DECCW2_evaluate[1,"Corner"] == 3){
          DE_CCW2 <- DE_CCW2+nrow(DECCW2_evaluate[which(DECCW2_evaluate$Corner == 4 & DECCW2_evaluate$NosepokeNumber > 0),])
        } else if (DECCW2_evaluate[1,"Corner"] == 4){
          DE_CCW2 <- DE_CCW2+nrow(DECCW2_evaluate[which(DECCW2_evaluate$Corner == 1 & DECCW2_evaluate$NosepokeNumber > 0),])
        }
        # Place Error - CCW2
        counts[[length(counts) + 1]] <- (nrow(subset_dfCCW2[which(subset_dfCCW2$CornerCondition == "Incorrect" & subset_dfCCW2$NosepokeNumber > 0),])-DE_CCW2)/nrow(subset_dfCCW2[which(subset_dfCCW2$NosepokeNumber > 0),])
        # Direction Error - CCW2
        counts[[length(counts) + 1]] <- (DE_CCW2)/nrow(subset_dfCCW2[which(subset_dfCCW2$NosepokeNumber > 0),])
      }
    }
  }
  # Add to the results object
  results[nrow(results) + 1,] <- counts
  rm(last_licked_corner_CCW1,next_corner,DE_corners,DE_CCW1,DE_evaluate,subset_df,subset_dfCCW2,DECCW2_corners,DE_CCW2,counts,x,correct_corner_CCW1_nolicks,correct_corner_CCW2_nolicks)
}
rm(animal,visits_df,visits_objects,column_names)

# Merge with the actual results dataframe
results_df <- merge(results_df,results,by.x = 1,by.y = 1,all.x = T)

#### Overall Error Index ####
# For Place Learning we use Place Error
results_df$Overall_Error_Index_PL <- results_df$PL_Dark_PlaceErrorNonAdjusted

# For Reversal Learning we use 3/4 Place Error and 1/4 Challenge Error
results_df$Overall_Error_Index_RL <- (3*(results_df$RL_Dark_PlaceErrorAdjusted/4))+(1*(results_df$RL_Dark_ChallengeError/4))

# For Multiple Reversal Learning we use 3/4 Place Error and 1/4 Challenge Error
results_df$Overall_Error_Index_MRL <- (3*(results_df$MRL_Dark_PlaceErrorAdjusted/4))+(1*(results_df$MRL_Dark_ChallengeError/4))

# For ELM1A we use Place Error
results_df$Overall_Error_Index_ELM1A <- (3*(results_df$ELM1A_active_Dark_PlaceErrorNonAdjusted)/4)+(1*(results_df$ELM1A_Dark_TimeError)/4)

# For ELM1A active we use Place Error
#results_df$Overall_Error_Index_ELM1A_active <- results_df$ELM1A_active_Dark_PlaceError

# For ELM1R we use 3/4 Place Error and 1/4 Time Error
results_df$Overall_Error_Index_ELM1R <- (3*(results_df$ELM1R_active_Dark_PlaceErrorNonAdjusted)/4)+(1*(results_df$ELM1R_Dark_TimeError)/4)

# For ELM1R active we cannot calculate it as there is no Time Error calculated

# For ELM2A we use 2/4 Place Error, 1/4 Challenge Error, and 1/4 Time Error
results_df$Overall_Error_Index_ELM2A <- (2*(results_df$ELM2A_active_Dark_PlaceErrorAdjusted)/4)+(1*(results_df$ELM2A_active_Dark_ChallengeError)/4)+(1*(results_df$ELM2A_Dark_TimeError)/4)

# For ELM2A active we cannot calculate it as there is no Time Error calculated

# For ELM2R we use 2/4 Place Error, 1/4 Challenge Error, and 1/4 Time Error
results_df$Overall_Error_Index_ELM2R <- (2*(results_df$ELM2R_active_Dark_PlaceErrorAdjusted)/4)+(1*(results_df$ELM2R_active_Dark_ChallengeError)/4)+(1*(results_df$ELM2R_Dark_TimeError)/4)

# For ELM2R active we cannot calculate it as there is no Time Error calculated

# Working Memory is the only one where it makes sense to calculate for the different daytimes

# For CW1 we use Place Error
results_df$Overall_Error_Index_CW1_Total <- results_df$CW1_Total_PlaceErrorNonAdjusted

# For CW2 we use Place Error
results_df$Overall_Error_Index_CW2_Total <- results_df$CW2_Total_PlaceErrorNonAdjusted

# For CCW1 we use 3/4 Place Error and 1/4 Direction Error
results_df$Overall_Error_Index_CCW1_Total <- (3*(results_df$CCW1_Total_PlaceErrorAdjusted/4))+(1*(results_df$CCW1_Total_DirectionError/4))

# For CCW2 we use 3/4 Place Error and 1/4 Direction Error
results_df$Overall_Error_Index_CCW2_Total <- (3*(results_df$CCW2_Total_PlaceErrorAdjusted/4))+(1*(results_df$CCW2_Total_DirectionError/4))

### Not using Light
# For CW1 we use Place Error
#results_df$Overall_Error_Index_CW1_Light <- results_df$CW1_Light_PlaceError

# For CW2 we use Place Error
#results_df$Overall_Error_Index_CW2_Light <- results_df$CW2_Light_PlaceError

# For CCW1 we use 3/4 Place Error and 1/4 Direction Error
#results_df$Overall_Error_Index_CCW1_Light <- (2*(results_df$CCW1_Light_PlaceError/4))+(1*(results_df$CCW1_Light_DirectionError/4))

# For CCW2 we use 3/4 Place Error and 1/4 Direction Error
#results_df$Overall_Error_Index_CCW2_Light <- (2*(results_df$CCW2_Light_PlaceError/4))+(1*(results_df$CCW2_Light_DirectionError/4))

# For CW1 we use Place Error
results_df$Overall_Error_Index_CW1_Dark <- results_df$CW1_Dark_PlaceErrorNonAdjusted

# For CW2 we use Place Error
results_df$Overall_Error_Index_CW2_Dark <- results_df$CW2_Dark_PlaceErrorNonAdjusted

# For CCW1 we use 3/4 Place Error and 1/4 Direction Error
results_df$Overall_Error_Index_CCW1_Dark <- (3*(results_df$CCW1_Dark_PlaceErrorAdjusted/4))+(1*(results_df$CCW1_Dark_DirectionError/4))

# For CCW2 we use 3/4 Place Error and 1/4 Direction Error
results_df$Overall_Error_Index_CCW2_Dark <- (3*(results_df$CCW2_Dark_PlaceErrorAdjusted/4))+(1*(results_df$CCW2_Dark_DirectionError/4))

#### Cognition Index ####
# Same as Error Index, but higher the better
results_df$Cognition_Index_PL <- 1-results_df$Overall_Error_Index_PL
results_df$Cognition_Index_RL <- 1-results_df$Overall_Error_Index_RL
results_df$Cognition_Index_MRL <- 1-results_df$Overall_Error_Index_MRL
results_df$Cognition_Index_ELM1A <- 1-results_df$Overall_Error_Index_ELM1A
results_df$Cognition_Index_ELM1R <- 1-results_df$Overall_Error_Index_ELM1R
results_df$Cognition_Index_ELM2A <- 1-results_df$Overall_Error_Index_ELM2A
results_df$Cognition_Index_ELM2R <- 1-results_df$Overall_Error_Index_ELM2R
results_df$Cognition_Index_CW1_Total <- 1-results_df$Overall_Error_Index_CW1_Total
results_df$Cognition_Index_CW2_Total <- 1-results_df$Overall_Error_Index_CW2_Total
results_df$Cognition_Index_CCW1_Total <- 1-results_df$Overall_Error_Index_CCW1_Total
results_df$Cognition_Index_CCW2_Total <- 1-results_df$Overall_Error_Index_CCW2_Total
#results_df$Cognition_Index_CW1_Light <- 1-results_df$Overall_Error_Index_CW1_Light
#results_df$Cognition_Index_CW2_Light <- 1-results_df$Overall_Error_Index_CW2_Light
#results_df$Cognition_Index_CCW1_Light <- 1-results_df$Overall_Error_Index_CCW1_Light
#results_df$Cognition_Index_CCW2_Light <- 1-results_df$Overall_Error_Index_CCW2_Light
results_df$Cognition_Index_CW1_Dark <- 1-results_df$Overall_Error_Index_CW1_Dark
results_df$Cognition_Index_CW2_Dark <- 1-results_df$Overall_Error_Index_CW2_Dark
results_df$Cognition_Index_CCW1_Dark <- 1-results_df$Overall_Error_Index_CCW1_Dark
results_df$Cognition_Index_CCW2_Dark <- 1-results_df$Overall_Error_Index_CCW2_Dark

#### Reorganizing columns ####
## Due to how the challenges are structured, the order of the columns is not perfect. Here we fix this organizing all columns by the challenges.
# define the column order
column_order <- c("ID", "Group")

# find columns that match the pattern and add them to the column order
column_order <- c(column_order, grep("^Habituation_Total", colnames(results_df), value = TRUE))
column_order <- c(column_order, grep("^Habituation_Dark", colnames(results_df), value = TRUE))
column_order <- c(column_order, grep("^Habituation_Light", colnames(results_df), value = TRUE))
column_order <- c(column_order, grep("^PL_Total", colnames(results_df), value = TRUE))
column_order <- c(column_order, grep("^PL_Dark", colnames(results_df), value = TRUE))
column_order <- c(column_order, grep("^PL_Light", colnames(results_df), value = TRUE))
column_order <- c(column_order, "Overall_Error_Index_PL","Cognition_Index_PL")
column_order <- c(column_order, grep("^RL_Total", colnames(results_df), value = TRUE))
column_order <- c(column_order, grep("^RL_Dark", colnames(results_df), value = TRUE))
column_order <- c(column_order, grep("^RL_Light", colnames(results_df), value = TRUE))
column_order <- c(column_order, "Overall_Error_Index_RL","Cognition_Index_RL")
column_order <- c(column_order, grep("^MRL_Total", colnames(results_df), value = TRUE))
column_order <- c(column_order, grep("^MRL_Dark", colnames(results_df), value = TRUE))
column_order <- c(column_order, grep("^MRL1_Dark", colnames(results_df), value = TRUE))
column_order <- c(column_order, grep("^MRL2_Dark", colnames(results_df), value = TRUE))
column_order <- c(column_order, grep("^MRL3_Dark", colnames(results_df), value = TRUE))
column_order <- c(column_order, grep("^MRL4_Dark", colnames(results_df), value = TRUE))
column_order <- c(column_order, grep("^MRL_Light", colnames(results_df), value = TRUE))
column_order <- c(column_order, "Overall_Error_Index_MRL","Cognition_Index_MRL")
column_order <- c(column_order, grep("^ExtinctionCh2_Total", colnames(results_df), value = TRUE))
column_order <- c(column_order, grep("^ExtinctionCh2_Dark", colnames(results_df), value = TRUE))
column_order <- c(column_order, grep("^ExtinctionCh2_Light", colnames(results_df), value = TRUE))
column_order <- c(column_order, grep("^ELM1A_Total", colnames(results_df), value = TRUE))
column_order <- c(column_order, grep("^ELM1A_Dark", colnames(results_df), value = TRUE))
column_order <- c(column_order, grep("^ELM1A_Light", colnames(results_df), value = TRUE))
column_order <- c(column_order, grep("^ELM1A_active_Total", colnames(results_df), value = TRUE))
column_order <- c(column_order, grep("^ELM1A_active_Dark", colnames(results_df), value = TRUE))
column_order <- c(column_order, grep("^ELM1A_active_Light", colnames(results_df), value = TRUE))
column_order <- c(column_order, "Overall_Error_Index_ELM1A","Cognition_Index_ELM1A")
column_order <- c(column_order, grep("^ELM1R_Total", colnames(results_df), value = TRUE))
column_order <- c(column_order, grep("^ELM1R_Dark", colnames(results_df), value = TRUE))
column_order <- c(column_order, grep("^ELM1R_Light", colnames(results_df), value = TRUE))
column_order <- c(column_order, grep("^ELM1R_active_Total", colnames(results_df), value = TRUE))
column_order <- c(column_order, grep("^ELM1R_active_Dark", colnames(results_df), value = TRUE))
column_order <- c(column_order, grep("^ELM1R_active_Light", colnames(results_df), value = TRUE))
column_order <- c(column_order, "Overall_Error_Index_ELM1R","Cognition_Index_ELM1R")
column_order <- c(column_order, grep("^ELM2A_Total", colnames(results_df), value = TRUE))
column_order <- c(column_order, grep("^ELM2A_Dark", colnames(results_df), value = TRUE))
column_order <- c(column_order, grep("^ELM2A_Light", colnames(results_df), value = TRUE))
column_order <- c(column_order, grep("^ELM2A_active_Total", colnames(results_df), value = TRUE))
column_order <- c(column_order, grep("^ELM2A_active_Dark", colnames(results_df), value = TRUE))
column_order <- c(column_order, grep("^ELM2A_active_Light", colnames(results_df), value = TRUE))
column_order <- c(column_order, "Overall_Error_Index_ELM2A","Cognition_Index_ELM2A")
column_order <- c(column_order, grep("^ELM2R_Total", colnames(results_df), value = TRUE))
column_order <- c(column_order, grep("^ELM2R_Dark", colnames(results_df), value = TRUE))
column_order <- c(column_order, grep("^ELM2R_Light", colnames(results_df), value = TRUE))
column_order <- c(column_order, grep("^ELM2R_active_Total", colnames(results_df), value = TRUE))
column_order <- c(column_order, grep("^ELM2R_active_Dark", colnames(results_df), value = TRUE))
column_order <- c(column_order, grep("^ELM2R_active_Light", colnames(results_df), value = TRUE))
column_order <- c(column_order, "Overall_Error_Index_ELM2R","Cognition_Index_ELM2R")
column_order <- c(column_order, grep("^ExtinctionCh3_Total", colnames(results_df), value = TRUE))
column_order <- c(column_order, grep("^ExtinctionCh3_Dark", colnames(results_df), value = TRUE))
column_order <- c(column_order, grep("^ExtinctionCh3_Light", colnames(results_df), value = TRUE))
column_order <- c(column_order, grep("^CW1_Total", colnames(results_df), value = TRUE))
column_order <- c(column_order, grep("^CW1_Dark", colnames(results_df), value = TRUE))
column_order <- c(column_order, grep("^CW1_Light", colnames(results_df), value = TRUE))
column_order <- c(column_order, "Overall_Error_Index_CW1_Total","Cognition_Index_CW1_Total", "Overall_Error_Index_CW1_Dark","Cognition_Index_CW1_Dark")
column_order <- c(column_order, grep("^CW2_Total", colnames(results_df), value = TRUE))
column_order <- c(column_order, grep("^CW2_Dark", colnames(results_df), value = TRUE))
column_order <- c(column_order, grep("^CW2_Light", colnames(results_df), value = TRUE))
column_order <- c(column_order, "Overall_Error_Index_CW2_Total","Cognition_Index_CW2_Total", "Overall_Error_Index_CW2_Dark","Cognition_Index_CW2_Dark")
column_order <- c(column_order, grep("^CCW1_Total", colnames(results_df), value = TRUE))
column_order <- c(column_order, grep("^CCW1_Dark", colnames(results_df), value = TRUE))
column_order <- c(column_order, grep("^CCW1_Light", colnames(results_df), value = TRUE))
column_order <- c(column_order, "Overall_Error_Index_CCW1_Total","Cognition_Index_CCW1_Total", "Overall_Error_Index_CCW1_Dark","Cognition_Index_CCW1_Dark")
column_order <- c(column_order, grep("^CCW2_Total", colnames(results_df), value = TRUE))
column_order <- c(column_order, grep("^CCW2_Dark", colnames(results_df), value = TRUE))
column_order <- c(column_order, grep("^CCW2_Light", colnames(results_df), value = TRUE))
column_order <- c(column_order, "Overall_Error_Index_CCW2_Total","Cognition_Index_CCW2_Total", "Overall_Error_Index_CCW2_Dark","Cognition_Index_CCW2_Dark")
# reorder the columns
results_df <- results_df[, column_order]

write.xlsx(results_df,paste("processedICs_allData_",project,".xlsx",sep=""),colNames = T,rowNames = F,keepNA = F)

#### Removing data for animals that did not drink ####
total_licks_cols <- grep("Total_Licks$", colnames(results_df))
total_licks_cols <- total_licks_cols[-c(1,5,14)] # remove habituation and extinction
findings_df <- data.frame(column = character(0), ID = character(0), stringsAsFactors = FALSE)
reported_IDs <- character(0)
for (i in seq_along(total_licks_cols)) {
  zero_na_rows <- which(results_df[[total_licks_cols[i]]] == 0 | is.na(results_df[[total_licks_cols[i]]]))
  if (length(zero_na_rows) > 0) {
    new_IDs <- setdiff(as.character(results_df[zero_na_rows, 1]), reported_IDs)
    if (length(new_IDs) > 0) {
      new_findings <- data.frame(column = colnames(results_df)[total_licks_cols[i]], ID = new_IDs, stringsAsFactors = FALSE)
      findings_df <- rbind(findings_df, new_findings)
      reported_IDs <- c(reported_IDs, new_IDs)
    }
    results_df[zero_na_rows, seq(from = total_licks_cols[i], to = ncol(results_df))] <- NA
  }
}

write.xlsx(results_df,paste("processedICs_removedNonDrinkers_",project,".xlsx",sep=""),colNames = T,rowNames = F,keepNA = F)
write.xlsx(findings_df,paste("processedICs_droppedIndividuals_",project,".xlsx",sep=""),colNames = T,rowNames = F,keepNA = F)

#### Corners ####
unblinding[,c(1,2)] -> results_corner

#### Corner preference calculation - Total ####
# Get the names of all objects that will be used for these variables
visits_objects <- c("Habituation_visits","ExtinctionCh2_visits","ExtinctionCh3_visits")

# Create a vector of column names for the results data frame
column_names <- c("Animal", paste0(rep(gsub("_visits","",visits_objects), each = 12), c("_Total_Corner1Preference_visits", "_Total_Corner2Preference_visits", "_Total_Corner3Preference_visits","_Total_Corner4Preference_visits","_Total_Corner1Preference_nosepokes", "_Total_Corner2Preference_nosepokes", "_Total_Corner3Preference_nosepokes","_Total_Corner4Preference_nosepokes","_Total_Corner1Preference_licks", "_Total_Corner2Preference_licks", "_Total_Corner3Preference_licks","_Total_Corner4Preference_licks")))

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
    
    # Calculate the total number of visits, nosepokes, and licks by corner preference
    total_c1preference_v <- ifelse(nrow(subset_df) == 0, NA, nrow(subset_df[subset_df$Corner == 1,])/nrow(subset_df))
    total_c1preference_n <- ifelse(nrow(subset_df) == 0, NA, sum(subset_df[subset_df$Corner == 1,15],na.rm = T)/sum(subset_df$NosepokeNumber,na.rm =T))
    total_c1preference_l <- ifelse(nrow(subset_df) == 0, NA, sum(subset_df[subset_df$Corner == 1,17],na.rm = T)/sum(subset_df$LickNumber,na.rm =T))
    total_c2preference_v <- ifelse(nrow(subset_df) == 0, NA, nrow(subset_df[subset_df$Corner == 2,])/nrow(subset_df))
    total_c2preference_n <- ifelse(nrow(subset_df) == 0, NA, sum(subset_df[subset_df$Corner == 2,15],na.rm = T)/sum(subset_df$NosepokeNumber,na.rm =T))
    total_c2preference_l <- ifelse(nrow(subset_df) == 0, NA, sum(subset_df[subset_df$Corner == 2,17],na.rm = T)/sum(subset_df$LickNumber,na.rm =T))
    total_c3preference_v <- ifelse(nrow(subset_df) == 0, NA, nrow(subset_df[subset_df$Corner == 3,])/nrow(subset_df))
    total_c3preference_n <- ifelse(nrow(subset_df) == 0, NA, sum(subset_df[subset_df$Corner == 3,15],na.rm = T)/sum(subset_df$NosepokeNumber,na.rm =T))
    total_c3preference_l <- ifelse(nrow(subset_df) == 0, NA, sum(subset_df[subset_df$Corner == 3,17],na.rm = T)/sum(subset_df$LickNumber,na.rm =T))
    total_c4preference_v <- ifelse(nrow(subset_df) == 0, NA, nrow(subset_df[subset_df$Corner == 4,])/nrow(subset_df))
    total_c4preference_n <- ifelse(nrow(subset_df) == 0, NA, sum(subset_df[subset_df$Corner == 4,15],na.rm = T)/sum(subset_df$NosepokeNumber,na.rm =T))
    total_c4preference_l <- ifelse(nrow(subset_df) == 0, NA, sum(subset_df[subset_df$Corner == 4,17],na.rm = T)/sum(subset_df$LickNumber,na.rm =T))
    
    # Add the counts to the counts list
    counts[[length(counts) + 1]] <- total_c1preference_v
    counts[[length(counts) + 1]] <- total_c2preference_v
    counts[[length(counts) + 1]] <- total_c3preference_v
    counts[[length(counts) + 1]] <- total_c4preference_v
    counts[[length(counts) + 1]] <- total_c1preference_n
    counts[[length(counts) + 1]] <- total_c2preference_n
    counts[[length(counts) + 1]] <- total_c3preference_n
    counts[[length(counts) + 1]] <- total_c4preference_n
    counts[[length(counts) + 1]] <- total_c1preference_l
    counts[[length(counts) + 1]] <- total_c2preference_l
    counts[[length(counts) + 1]] <- total_c3preference_l
    counts[[length(counts) + 1]] <- total_c4preference_l
  }
  
  # Add a row with the counts to the results data frame
  results[nrow(results) + 1,] <- counts
}

# Clean up intermediate files
rm(animal,counts,visits_df,subset_df,visits_object,visits_objects,total_c1preference_v,total_c2preference_v,total_c3preference_v,total_c4preference_v,total_c1preference_n,total_c2preference_n,total_c3preference_n,total_c4preference_n,total_c1preference_l,total_c2preference_l,total_c3preference_l,total_c4preference_l)

# Merge with the actual results dataframe
results_corner <- merge(results_corner,results,by.x = 1,by.y = 1,all.x = T)

#### Corner preference calculation - Light ####
# Get the names of all objects that will be used for these variables
visits_objects <- c("Habituation_visits","ExtinctionCh2_visits","ExtinctionCh3_visits")

# Create a vector of column names for the results data frame
column_names <- c("Animal", paste0(rep(gsub("_visits","",visits_objects), each = 12), c("_Light_Corner1Preference_visits", "_Light_Corner2Preference_visits", "_Light_Corner3Preference_visits","_Light_Corner4Preference_visits","_Light_Corner1Preference_nosepokes", "_Light_Corner2Preference_nosepokes", "_Light_Corner3Preference_nosepokes","_Light_Corner4Preference_nosepokes","_Light_Corner1Preference_licks", "_Light_Corner2Preference_licks", "_Light_Corner3Preference_licks","_Light_Corner4Preference_licks")))

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
    subset_df <- subset(visits_df, Animal == animal & Light_Status == "Light")
    
    # Calculate the light number of visits, nosepokes, and licks by corner preference
    light_c1preference_v <- ifelse(nrow(subset_df) == 0, NA, nrow(subset_df[subset_df$Corner == 1,])/nrow(subset_df))
    light_c1preference_n <- ifelse(nrow(subset_df) == 0, NA, sum(subset_df[subset_df$Corner == 1,15],na.rm = T)/sum(subset_df$NosepokeNumber,na.rm =T))
    light_c1preference_l <- ifelse(nrow(subset_df) == 0, NA, sum(subset_df[subset_df$Corner == 1,17],na.rm = T)/sum(subset_df$LickNumber,na.rm =T))
    light_c2preference_v <- ifelse(nrow(subset_df) == 0, NA, nrow(subset_df[subset_df$Corner == 2,])/nrow(subset_df))
    light_c2preference_n <- ifelse(nrow(subset_df) == 0, NA, sum(subset_df[subset_df$Corner == 2,15],na.rm = T)/sum(subset_df$NosepokeNumber,na.rm =T))
    light_c2preference_l <- ifelse(nrow(subset_df) == 0, NA, sum(subset_df[subset_df$Corner == 2,17],na.rm = T)/sum(subset_df$LickNumber,na.rm =T))
    light_c3preference_v <- ifelse(nrow(subset_df) == 0, NA, nrow(subset_df[subset_df$Corner == 3,])/nrow(subset_df))
    light_c3preference_n <- ifelse(nrow(subset_df) == 0, NA, sum(subset_df[subset_df$Corner == 3,15],na.rm = T)/sum(subset_df$NosepokeNumber,na.rm =T))
    light_c3preference_l <- ifelse(nrow(subset_df) == 0, NA, sum(subset_df[subset_df$Corner == 3,17],na.rm = T)/sum(subset_df$LickNumber,na.rm =T))
    light_c4preference_v <- ifelse(nrow(subset_df) == 0, NA, nrow(subset_df[subset_df$Corner == 4,])/nrow(subset_df))
    light_c4preference_n <- ifelse(nrow(subset_df) == 0, NA, sum(subset_df[subset_df$Corner == 4,15],na.rm = T)/sum(subset_df$NosepokeNumber,na.rm =T))
    light_c4preference_l <- ifelse(nrow(subset_df) == 0, NA, sum(subset_df[subset_df$Corner == 4,17],na.rm = T)/sum(subset_df$LickNumber,na.rm =T))
    
    # Add the counts to the counts list
    counts[[length(counts) + 1]] <- light_c1preference_v
    counts[[length(counts) + 1]] <- light_c2preference_v
    counts[[length(counts) + 1]] <- light_c3preference_v
    counts[[length(counts) + 1]] <- light_c4preference_v
    counts[[length(counts) + 1]] <- light_c1preference_n
    counts[[length(counts) + 1]] <- light_c2preference_n
    counts[[length(counts) + 1]] <- light_c3preference_n
    counts[[length(counts) + 1]] <- light_c4preference_n
    counts[[length(counts) + 1]] <- light_c1preference_l
    counts[[length(counts) + 1]] <- light_c2preference_l
    counts[[length(counts) + 1]] <- light_c3preference_l
    counts[[length(counts) + 1]] <- light_c4preference_l
  }
  
  # Add a row with the counts to the results data frame
  results[nrow(results) + 1,] <- counts
}

# Clean up intermediate files
rm(animal,counts,visits_df,subset_df,visits_object,visits_objects,light_c1preference_v,light_c2preference_v,light_c3preference_v,light_c4preference_v,light_c1preference_n,light_c2preference_n,light_c3preference_n,light_c4preference_n,light_c1preference_l,light_c2preference_l,light_c3preference_l,light_c4preference_l)

# Merge with the actual results dataframe
results_corner <- merge(results_corner,results,by.x = 1,by.y = 1,all.x = T)

#### Corner preference calculation - Dark ####
# Get the names of all objects that will be used for these variables
visits_objects <- c("Habituation_visits","ExtinctionCh2_visits","ExtinctionCh3_visits")

column_names <- c("Animal", paste0(rep(gsub("_visits","",visits_objects), each = 12), c("_Dark_Corner1Preference_visits", "_Dark_Corner2Preference_visits", "_Dark_Corner3Preference_visits","_Dark_Corner4Preference_visits","_Dark_Corner1Preference_nosepokes", "_Dark_Corner2Preference_nosepokes", "_Dark_Corner3Preference_nosepokes","_Dark_Corner4Preference_nosepokes","_Dark_Corner1Preference_licks", "_Dark_Corner2Preference_licks", "_Dark_Corner3Preference_licks","_Dark_Corner4Preference_licks")))

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
    subset_df <- subset(visits_df, Animal == animal & Light_Status == "Dark")
    
    # Calculate the dark number of visits, nosepokes, and licks by corner preference
    dark_c1preference_v <- ifelse(nrow(subset_df) == 0, NA, nrow(subset_df[subset_df$Corner == 1,])/nrow(subset_df))
    dark_c1preference_n <- ifelse(nrow(subset_df) == 0, NA, sum(subset_df[subset_df$Corner == 1,15],na.rm = T)/sum(subset_df$NosepokeNumber,na.rm =T))
    dark_c1preference_l <- ifelse(nrow(subset_df) == 0, NA, sum(subset_df[subset_df$Corner == 1,17],na.rm = T)/sum(subset_df$LickNumber,na.rm =T))
    dark_c2preference_v <- ifelse(nrow(subset_df) == 0, NA, nrow(subset_df[subset_df$Corner == 2,])/nrow(subset_df))
    dark_c2preference_n <- ifelse(nrow(subset_df) == 0, NA, sum(subset_df[subset_df$Corner == 2,15],na.rm = T)/sum(subset_df$NosepokeNumber,na.rm =T))
    dark_c2preference_l <- ifelse(nrow(subset_df) == 0, NA, sum(subset_df[subset_df$Corner == 2,17],na.rm = T)/sum(subset_df$LickNumber,na.rm =T))
    dark_c3preference_v <- ifelse(nrow(subset_df) == 0, NA, nrow(subset_df[subset_df$Corner == 3,])/nrow(subset_df))
    dark_c3preference_n <- ifelse(nrow(subset_df) == 0, NA, sum(subset_df[subset_df$Corner == 3,15],na.rm = T)/sum(subset_df$NosepokeNumber,na.rm =T))
    dark_c3preference_l <- ifelse(nrow(subset_df) == 0, NA, sum(subset_df[subset_df$Corner == 3,17],na.rm = T)/sum(subset_df$LickNumber,na.rm =T))
    dark_c4preference_v <- ifelse(nrow(subset_df) == 0, NA, nrow(subset_df[subset_df$Corner == 4,])/nrow(subset_df))
    dark_c4preference_n <- ifelse(nrow(subset_df) == 0, NA, sum(subset_df[subset_df$Corner == 4,15],na.rm = T)/sum(subset_df$NosepokeNumber,na.rm =T))
    dark_c4preference_l <- ifelse(nrow(subset_df) == 0, NA, sum(subset_df[subset_df$Corner == 4,17],na.rm = T)/sum(subset_df$LickNumber,na.rm =T))
    
    # Add the counts to the counts list
    counts[[length(counts) + 1]] <- dark_c1preference_v
    counts[[length(counts) + 1]] <- dark_c2preference_v
    counts[[length(counts) + 1]] <- dark_c3preference_v
    counts[[length(counts) + 1]] <- dark_c4preference_v
    counts[[length(counts) + 1]] <- dark_c1preference_n
    counts[[length(counts) + 1]] <- dark_c2preference_n
    counts[[length(counts) + 1]] <- dark_c3preference_n
    counts[[length(counts) + 1]] <- dark_c4preference_n
    counts[[length(counts) + 1]] <- dark_c1preference_l
    counts[[length(counts) + 1]] <- dark_c2preference_l
    counts[[length(counts) + 1]] <- dark_c3preference_l
    counts[[length(counts) + 1]] <- dark_c4preference_l
  }
  
  # Add a row with the counts to the results data frame
  results[nrow(results) + 1,] <- counts
}

# Clean up intermediate files
rm(animal,counts,visits_df,subset_df,visits_object,visits_objects,dark_c1preference_v,dark_c2preference_v,dark_c3preference_v,dark_c4preference_v,dark_c1preference_n,dark_c2preference_n,dark_c3preference_n,dark_c4preference_n,dark_c1preference_l,dark_c2preference_l,dark_c3preference_l,dark_c4preference_l)

# Merge with the actual results dataframe
results_corner <- merge(results_corner,results,by.x = 1,by.y = 1,all.x = T)

#### Corner preference calculation - Total ####
# Get the names of all objects that will be used for these variables
visits_objects <- c("Habituation_visits","ExtinctionCh2_visits","ExtinctionCh3_visits")

# Create a vector of column names for the results data frame
column_names <- c("Animal", paste0(rep(gsub("_visits","",visits_objects), each = 12), c("_Total_CornerRaw1Preference_visits", "_Total_CornerRaw2Preference_visits", "_Total_CornerRaw3Preference_visits","_Total_CornerRaw4Preference_visits","_Total_CornerRaw1Preference_nosepokes", "_Total_CornerRaw2Preference_nosepokes", "_Total_CornerRaw3Preference_nosepokes","_Total_CornerRaw4Preference_nosepokes","_Total_CornerRaw1Preference_licks", "_Total_CornerRaw2Preference_licks", "_Total_CornerRaw3Preference_licks","_Total_CornerRaw4Preference_licks")))

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
    
    # Calculate the total number of visits, nosepokes, and licks by corner preference
    total_c1preference_v <- ifelse(nrow(subset_df) == 0, NA, nrow(subset_df[subset_df$Corner == 1,]))
    total_c1preference_n <- ifelse(nrow(subset_df) == 0, NA, sum(subset_df[subset_df$Corner == 1,15],na.rm = T))
    total_c1preference_l <- ifelse(nrow(subset_df) == 0, NA, sum(subset_df[subset_df$Corner == 1,17],na.rm = T))
    total_c2preference_v <- ifelse(nrow(subset_df) == 0, NA, nrow(subset_df[subset_df$Corner == 2,]))
    total_c2preference_n <- ifelse(nrow(subset_df) == 0, NA, sum(subset_df[subset_df$Corner == 2,15],na.rm = T))
    total_c2preference_l <- ifelse(nrow(subset_df) == 0, NA, sum(subset_df[subset_df$Corner == 2,17],na.rm = T))
    total_c3preference_v <- ifelse(nrow(subset_df) == 0, NA, nrow(subset_df[subset_df$Corner == 3,]))
    total_c3preference_n <- ifelse(nrow(subset_df) == 0, NA, sum(subset_df[subset_df$Corner == 3,15],na.rm = T))
    total_c3preference_l <- ifelse(nrow(subset_df) == 0, NA, sum(subset_df[subset_df$Corner == 3,17],na.rm = T))
    total_c4preference_v <- ifelse(nrow(subset_df) == 0, NA, nrow(subset_df[subset_df$Corner == 4,]))
    total_c4preference_n <- ifelse(nrow(subset_df) == 0, NA, sum(subset_df[subset_df$Corner == 4,15],na.rm = T))
    total_c4preference_l <- ifelse(nrow(subset_df) == 0, NA, sum(subset_df[subset_df$Corner == 4,17],na.rm = T))
    
    # Add the counts to the counts list
    counts[[length(counts) + 1]] <- total_c1preference_v
    counts[[length(counts) + 1]] <- total_c2preference_v
    counts[[length(counts) + 1]] <- total_c3preference_v
    counts[[length(counts) + 1]] <- total_c4preference_v
    counts[[length(counts) + 1]] <- total_c1preference_n
    counts[[length(counts) + 1]] <- total_c2preference_n
    counts[[length(counts) + 1]] <- total_c3preference_n
    counts[[length(counts) + 1]] <- total_c4preference_n
    counts[[length(counts) + 1]] <- total_c1preference_l
    counts[[length(counts) + 1]] <- total_c2preference_l
    counts[[length(counts) + 1]] <- total_c3preference_l
    counts[[length(counts) + 1]] <- total_c4preference_l
  }
  
  # Add a row with the counts to the results data frame
  results[nrow(results) + 1,] <- counts
}

# Clean up intermediate files
rm(animal,counts,visits_df,subset_df,visits_object,visits_objects,total_c1preference_v,total_c2preference_v,total_c3preference_v,total_c4preference_v,total_c1preference_n,total_c2preference_n,total_c3preference_n,total_c4preference_n,total_c1preference_l,total_c2preference_l,total_c3preference_l,total_c4preference_l)

# Merge with the actual results dataframe
results_corner <- merge(results_corner,results,by.x = 1,by.y = 1,all.x = T)

#### Corner preference calculation - Light ####
# Get the names of all objects that will be used for these variables
visits_objects <- c("Habituation_visits","ExtinctionCh2_visits","ExtinctionCh3_visits")

# Create a vector of column names for the results data frame
column_names <- c("Animal", paste0(rep(gsub("_visits","",visits_objects), each = 12), c("_Light_CornerRaw1Preference_visits", "_Light_CornerRaw2Preference_visits", "_Light_CornerRaw3Preference_visits","_Light_CornerRaw4Preference_visits","_Light_CornerRaw1Preference_nosepokes", "_Light_CornerRaw2Preference_nosepokes", "_Light_CornerRaw3Preference_nosepokes","_Light_CornerRaw4Preference_nosepokes","_Light_CornerRaw1Preference_licks", "_Light_CornerRaw2Preference_licks", "_Light_CornerRaw3Preference_licks","_Light_CornerRaw4Preference_licks")))

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
    subset_df <- subset(visits_df, Animal == animal & Light_Status == "Light")
    
    # Calculate the light number of visits, nosepokes, and licks by corner preference
    light_c1preference_v <- ifelse(nrow(subset_df) == 0, NA, nrow(subset_df[subset_df$Corner == 1,]))
    light_c1preference_n <- ifelse(nrow(subset_df) == 0, NA, sum(subset_df[subset_df$Corner == 1,15],na.rm = T))
    light_c1preference_l <- ifelse(nrow(subset_df) == 0, NA, sum(subset_df[subset_df$Corner == 1,17],na.rm = T))
    light_c2preference_v <- ifelse(nrow(subset_df) == 0, NA, nrow(subset_df[subset_df$Corner == 2,]))
    light_c2preference_n <- ifelse(nrow(subset_df) == 0, NA, sum(subset_df[subset_df$Corner == 2,15],na.rm = T))
    light_c2preference_l <- ifelse(nrow(subset_df) == 0, NA, sum(subset_df[subset_df$Corner == 2,17],na.rm = T))
    light_c3preference_v <- ifelse(nrow(subset_df) == 0, NA, nrow(subset_df[subset_df$Corner == 3,]))
    light_c3preference_n <- ifelse(nrow(subset_df) == 0, NA, sum(subset_df[subset_df$Corner == 3,15],na.rm = T))
    light_c3preference_l <- ifelse(nrow(subset_df) == 0, NA, sum(subset_df[subset_df$Corner == 3,17],na.rm = T))
    light_c4preference_v <- ifelse(nrow(subset_df) == 0, NA, nrow(subset_df[subset_df$Corner == 4,]))
    light_c4preference_n <- ifelse(nrow(subset_df) == 0, NA, sum(subset_df[subset_df$Corner == 4,15],na.rm = T))
    light_c4preference_l <- ifelse(nrow(subset_df) == 0, NA, sum(subset_df[subset_df$Corner == 4,17],na.rm = T))
    
    # Add the counts to the counts list
    counts[[length(counts) + 1]] <- light_c1preference_v
    counts[[length(counts) + 1]] <- light_c2preference_v
    counts[[length(counts) + 1]] <- light_c3preference_v
    counts[[length(counts) + 1]] <- light_c4preference_v
    counts[[length(counts) + 1]] <- light_c1preference_n
    counts[[length(counts) + 1]] <- light_c2preference_n
    counts[[length(counts) + 1]] <- light_c3preference_n
    counts[[length(counts) + 1]] <- light_c4preference_n
    counts[[length(counts) + 1]] <- light_c1preference_l
    counts[[length(counts) + 1]] <- light_c2preference_l
    counts[[length(counts) + 1]] <- light_c3preference_l
    counts[[length(counts) + 1]] <- light_c4preference_l
  }
  
  # Add a row with the counts to the results data frame
  results[nrow(results) + 1,] <- counts
}

# Clean up intermediate files
rm(animal,counts,visits_df,subset_df,visits_object,visits_objects,light_c1preference_v,light_c2preference_v,light_c3preference_v,light_c4preference_v,light_c1preference_n,light_c2preference_n,light_c3preference_n,light_c4preference_n,light_c1preference_l,light_c2preference_l,light_c3preference_l,light_c4preference_l)

# Merge with the actual results dataframe
results_corner <- merge(results_corner,results,by.x = 1,by.y = 1,all.x = T)

#### Corner preference calculation - Dark ####
# Get the names of all objects that will be used for these variables
visits_objects <- c("Habituation_visits","ExtinctionCh2_visits","ExtinctionCh3_visits")

column_names <- c("Animal", paste0(rep(gsub("_visits","",visits_objects), each = 12), c("_Dark_CornerRaw1Preference_visits", "_Dark_CornerRaw2Preference_visits", "_Dark_CornerRaw3Preference_visits","_Dark_CornerRaw4Preference_visits","_Dark_CornerRaw1Preference_nosepokes", "_Dark_CornerRaw2Preference_nosepokes", "_Dark_CornerRaw3Preference_nosepokes","_Dark_CornerRaw4Preference_nosepokes","_Dark_CornerRaw1Preference_licks", "_Dark_CornerRaw2Preference_licks", "_Dark_CornerRaw3Preference_licks","_Dark_CornerRaw4Preference_licks")))

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
    subset_df <- subset(visits_df, Animal == animal & Light_Status == "Dark")
    
    # Calculate the dark number of visits, nosepokes, and licks by corner preference
    dark_c1preference_v <- ifelse(nrow(subset_df) == 0, NA, nrow(subset_df[subset_df$Corner == 1,]))
    dark_c1preference_n <- ifelse(nrow(subset_df) == 0, NA, sum(subset_df[subset_df$Corner == 1,15],na.rm = T))
    dark_c1preference_l <- ifelse(nrow(subset_df) == 0, NA, sum(subset_df[subset_df$Corner == 1,17],na.rm = T))
    dark_c2preference_v <- ifelse(nrow(subset_df) == 0, NA, nrow(subset_df[subset_df$Corner == 2,]))
    dark_c2preference_n <- ifelse(nrow(subset_df) == 0, NA, sum(subset_df[subset_df$Corner == 2,15],na.rm = T))
    dark_c2preference_l <- ifelse(nrow(subset_df) == 0, NA, sum(subset_df[subset_df$Corner == 2,17],na.rm = T))
    dark_c3preference_v <- ifelse(nrow(subset_df) == 0, NA, nrow(subset_df[subset_df$Corner == 3,]))
    dark_c3preference_n <- ifelse(nrow(subset_df) == 0, NA, sum(subset_df[subset_df$Corner == 3,15],na.rm = T))
    dark_c3preference_l <- ifelse(nrow(subset_df) == 0, NA, sum(subset_df[subset_df$Corner == 3,17],na.rm = T))
    dark_c4preference_v <- ifelse(nrow(subset_df) == 0, NA, nrow(subset_df[subset_df$Corner == 4,]))
    dark_c4preference_n <- ifelse(nrow(subset_df) == 0, NA, sum(subset_df[subset_df$Corner == 4,15],na.rm = T))
    dark_c4preference_l <- ifelse(nrow(subset_df) == 0, NA, sum(subset_df[subset_df$Corner == 4,17],na.rm = T))
    
    # Add the counts to the counts list
    counts[[length(counts) + 1]] <- dark_c1preference_v
    counts[[length(counts) + 1]] <- dark_c2preference_v
    counts[[length(counts) + 1]] <- dark_c3preference_v
    counts[[length(counts) + 1]] <- dark_c4preference_v
    counts[[length(counts) + 1]] <- dark_c1preference_n
    counts[[length(counts) + 1]] <- dark_c2preference_n
    counts[[length(counts) + 1]] <- dark_c3preference_n
    counts[[length(counts) + 1]] <- dark_c4preference_n
    counts[[length(counts) + 1]] <- dark_c1preference_l
    counts[[length(counts) + 1]] <- dark_c2preference_l
    counts[[length(counts) + 1]] <- dark_c3preference_l
    counts[[length(counts) + 1]] <- dark_c4preference_l
  }
  
  # Add a row with the counts to the results data frame
  results[nrow(results) + 1,] <- counts
}

# Clean up intermediate files
rm(animal,counts,visits_df,subset_df,visits_object,visits_objects,dark_c1preference_v,dark_c2preference_v,dark_c3preference_v,dark_c4preference_v,dark_c1preference_n,dark_c2preference_n,dark_c3preference_n,dark_c4preference_n,dark_c1preference_l,dark_c2preference_l,dark_c3preference_l,dark_c4preference_l)

# Merge with the actual results dataframe
results_corner <- merge(results_corner,results,by.x = 1,by.y = 1,all.x = T)

write.xlsx(results_corner,paste("corner_preference_ICs_",project,".xlsx",sep=""),colNames = T,rowNames = F,keepNA = F)

#### Removing variables not used ####
## We are currently not interested in some specific variables due to their limitations. This is now true for "Light" measurements.
results_df <- results_df[, !grepl("Light", colnames(results_df))]

#### Statistical Analysis ####
if(length(grep("^group[0-9]+_name$", ls(envir = globalenv()))) == 2){
  # Create a vector of column names for the results data frame
  column_names <- c("Variable",paste("Number_Animals_",group1_name,sep = ""),paste("Number_Animals_",group2_name,sep = ""),paste("Mean_",group1_name,sep = ""),paste("SD_",group1_name,sep = ""),paste("Mean_",group2_name,sep = ""),paste("SD_",group2_name,sep = ""),paste("Shapiro_",group1_name,sep = ""),paste("Shapiro_",group2_name,sep = ""),"Levene","Statistical test","p-value","Effect size test","Effect size","CI High","CI Low")
  factor(results_df[,"Group"],levels = c(group1_name,group2_name)) -> group
  factor(results_df[,"Group"],levels = c(group1_name,group2_name)) -> results_df[,"Group"]
  # Create the results data frame
  results_stats <- setNames(data.frame(matrix(ncol = length(column_names), nrow = 0)), column_names)
  
  for (variable in 3:ncol(results_df)){
    counts <- list(colnames(results_df)[variable])
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
    } else {
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
      if (counts[[8]] > 0.05 & counts [[9]] > 0.05){
        if  (counts[[10]] > 0.05){
          counts[[length(counts) + 1]] <- "Two Sample t-test"
          counts[[length(counts) + 1]] <- t.test(results_df[,variable] ~ results_df[,"Group"],var.equal = T)$p.value
          counts[[length(counts) + 1]] <- "Cohen's d"
          counts[[length(counts) + 1]] <- effectsize::cohens_d(results_df[,variable] ~ results_df[,"Group"], data = results_df)$Cohens_d
          counts[[length(counts) + 1]] <- effectsize::cohens_d(results_df[,variable] ~ results_df[,"Group"], data = results_df)$CI_high
          counts[[length(counts) + 1]] <- effectsize::cohens_d(results_df[,variable] ~ results_df[,"Group"], data = results_df)$CI_low
        } else {
          counts[[length(counts) + 1]] <- "Welch Two Sample t-test"
          counts[[length(counts) + 1]] <- t.test(results_df[,variable] ~ results_df[,"Group"],var.equal = F)$p.value
          counts[[length(counts) + 1]] <- "Cohen's d"
          counts[[length(counts) + 1]] <- effectsize::cohens_d(results_df[,variable] ~ results_df[,"Group"], data = results_df,pooled_sd = F)$Cohens_d
          counts[[length(counts) + 1]] <- effectsize::cohens_d(results_df[,variable] ~ results_df[,"Group"], data = results_df,pooled_sd = F)$CI_high
          counts[[length(counts) + 1]] <- effectsize::cohens_d(results_df[,variable] ~ results_df[,"Group"], data = results_df,pooled_sd = F)$CI_low
        }
      } else {
        if (counts[[10]] > 0.05){
          counts[[length(counts) + 1]] <- "Wilcoxon rank sum test with continuity correction"
          counts[[length(counts) + 1]] <- wilcox.test(results_df[,variable] ~ results_df[,"Group"])$p.value
          counts[[length(counts) + 1]] <- "Cliff's delta"
          effectsize::cliffs_delta(results_df[,variable] ~ results_df[,"Group"]) -> hold_wc
          counts[[length(counts) + 1]] <- hold_wc$r_rank_biserial       
          counts[[length(counts) + 1]] <- hold_wc$CI_high
          counts[[length(counts) + 1]] <- hold_wc$CI_low
        } else {
          counts[[length(counts) + 1]] <- "Wilcoxon rank sum test with continuity correction - Heterogeneous variance"
          counts[[length(counts) + 1]] <- wilcox.test(results_df[,variable] ~ results_df[,"Group"])$p.value
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
} else {
  # 4 groups
  # Create a vector of column names for the results data frame
  ## Unfortunately the function for Dunn test ignores the names of groups, so a workaround is required
  column_names <- c("Variable",paste("Number_Animals_",group1_name,sep = ""),paste("Number_Animals_",group2_name,sep = ""),paste("Number_Animals_",group3_name,sep = ""),paste("Number_Animals_",group4_name,sep = ""),paste("Mean_",group1_name,sep = ""),paste("SD_",group1_name,sep = ""),paste("Mean_",group2_name,sep = ""),paste("SD_",group2_name,sep = ""),paste("Mean_",group3_name,sep = ""),paste("SD_",group3_name,sep = ""),paste("Mean_",group4_name,sep = ""),paste("SD_",group4_name,sep = ""),paste("Shapiro_",group1_name,sep = ""),paste("Shapiro_",group2_name,sep = ""),paste("Shapiro_",group3_name,sep = ""),paste("Shapiro_",group4_name,sep = ""),"Levene","Statistical test","p-value","Effect size test","Effect size","CI High","CI Low","Post-hoc test",paste(group1_name," vs ",group2_name,sep = ""),paste(group1_name," vs ",group3_name,sep = ""),paste(group1_name," vs ",group4_name,sep = ""),paste(group2_name," vs ",group3_name,sep = ""),paste(group2_name," vs ",group4_name,sep = ""),paste(group3_name," vs ",group4_name,sep = ""),"Effect size test post-hoc",paste("Effect size ",group1_name," vs ",group2_name,sep = ""),paste("CI High ",group1_name," vs ",group2_name,sep = ""),paste("CI Low ",group1_name," vs ",group2_name,sep = ""),paste("Effect size ",group1_name," vs ",group3_name,sep = ""),paste("CI High ",group1_name," vs ",group3_name,sep=""),paste("CI Low ",group1_name," vs ",group3_name,sep = ""),paste("Effect size ",group1_name," vs ",group4_name,sep = ""),paste("CI High ",group1_name," vs ",group4_name,sep = ""),paste("CI Low ",group1_name," vs ",group4_name,sep = ""),paste("Effect size ",group2_name," vs ",group3_name,sep = ""),paste("CI High ",group2_name," vs ",group3_name,sep = ""),paste("CI Low ",group2_name," vs ",group3_name,sep = ""),paste("Effect size ",group2_name," vs ",group4_name,sep = ""),paste("CI High ",group2_name," vs ",group4_name,sep = ""),paste("CI Low ",group2_name," vs ",group4_name,sep = ""),paste("Effect size ",group3_name," vs ",group4_name,sep = ""),paste("CI High ",group3_name," vs ",group4_name,sep = ""),paste("CI Low ",group3_name," vs ",group4_name,sep = ""))
  # Create the results data frame
  results_stats <- setNames(data.frame(matrix(ncol = length(column_names), nrow = 0)), column_names)
  for (variable in 3:ncol(results_df)){
    results_df -> bckp
    results_df[which(results_df$Group == group1_name),"Group"] <- "A"
    results_df[which(results_df$Group == group2_name),"Group"] <- "B"
    results_df[which(results_df$Group == group3_name),"Group"] <- "C"
    results_df[which(results_df$Group == group4_name),"Group"] <- "D"
    factor(results_df[,"Group"]) -> group
    factor(results_df[,"Group"]) -> results_df[,"Group"]
    counts <- list(colnames(results_df)[variable])
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
    } else {
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
      counts[[length(counts) + 1]] <- leveneTest(results_df[,variable] ~ results_df[,"Group"])$`Pr(>F)`[1]
      # Choosing the appropriate statistical test
      if (counts[[14]] > 0.05 & counts [[15]] > 0.05 & counts [[16]] > 0.05 & counts [[17]] > 0.05){
        if  (counts[[18]] > 0.05){
          counts[[length(counts) + 1]] <- "One-way ANOVA with equal variances"
          counts[[length(counts) + 1]] <- oneway.test(results_df[,variable]~results_df[,"Group"], data=results_df, na.action=na.omit, var.equal=TRUE)$p.value
          counts[[length(counts) + 1]] <- "Omega Squared"
          counts[[length(counts) + 1]] <-  omega_squared(oneway.test(results_df[,variable]~results_df[,"Group"], data=results_df, na.action=na.omit, var.equal=TRUE))$Omega2
          counts[[length(counts) + 1]] <- omega_squared(oneway.test(results_df[,variable]~results_df[,"Group"], data=results_df, na.action=na.omit, var.equal=TRUE))$CI_high
          counts[[length(counts) + 1]] <- omega_squared(oneway.test(results_df[,variable]~results_df[,"Group"], data=results_df, na.action=na.omit, var.equal=TRUE))$CI_low
          counts[[length(counts) + 1]] <- "Pairwise T-test - Not paired"
          pairwise.t.test(results_df[,variable],results_df[,"Group"], data=results_df,paired = F,p.adjust.method = "bonferroni") -> comparison
          counts[[length(counts) + 1]] <- comparison$p.value[1]
          counts[[length(counts) + 1]] <- comparison$p.value[2]
          counts[[length(counts) + 1]] <- comparison$p.value[3]
          counts[[length(counts) + 1]] <- comparison$p.value[5]
          counts[[length(counts) + 1]] <- comparison$p.value[6]
          counts[[length(counts) + 1]] <- comparison$p.value[9]
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
          counts[[length(counts) + 1]] <- "One-way ANOVA with unequal variances"
          counts[[length(counts) + 1]] <- oneway.test(results_df[,variable]~results_df[,"Group"], data=results_df, na.action=na.omit, var.equal=FALSE)$p.value
          counts[[length(counts) + 1]] <- "Omega Squared"
          counts[[length(counts) + 1]] <-  omega_squared(oneway.test(results_df[,variable]~results_df[,"Group"], data=results_df, na.action=na.omit, var.equal=TRUE))$Omega2
          counts[[length(counts) + 1]] <- omega_squared(oneway.test(results_df[,variable]~results_df[,"Group"], data=results_df, na.action=na.omit, var.equal=TRUE))$CI_high
          counts[[length(counts) + 1]] <- omega_squared(oneway.test(results_df[,variable]~results_df[,"Group"], data=results_df, na.action=na.omit, var.equal=TRUE))$CI_low
          counts[[length(counts) + 1]] <- "Games Howell Test"
          formula <- as.formula(paste(colnames(results_df)[variable], "~ Group"))
          results_df %>% games_howell_test(formula) -> comparison
          counts[[length(counts) + 1]] <- comparison$p.adj[1]
          counts[[length(counts) + 1]] <- comparison$p.adj[2]
          counts[[length(counts) + 1]] <- comparison$p.adj[3]
          counts[[length(counts) + 1]] <- comparison$p.adj[4]
          counts[[length(counts) + 1]] <- comparison$p.adj[5]
          counts[[length(counts) + 1]] <- comparison$p.adj[6]
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
        if (counts[[18]] > 0.05){
          counts[[length(counts) + 1]] <- "Kruskal-Wallis rank sum test"
          counts[[length(counts) + 1]] <- kruskal.test(results_df[,variable]~results_df[,"Group"], data=results_df, na.action=na.omit)$p.value
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
          
          dunn.test(results_df[,variable],results_df[,"Group"],method="bonferroni") -> comparison
          counts[[length(counts) + 1]] <- "Dunn Test"
          counts[[length(counts) + 1]] <- comparison$P.adjusted[1]
          counts[[length(counts) + 1]] <- comparison$P.adjusted[2]
          counts[[length(counts) + 1]] <- comparison$P.adjusted[4]
          counts[[length(counts) + 1]] <- comparison$P.adjusted[3]
          counts[[length(counts) + 1]] <- comparison$P.adjusted[5]
          counts[[length(counts) + 1]] <- comparison$P.adjusted[6]
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
          counts[[length(counts) + 1]] <- "Asymptotic K-Sample Fisher-Pitman Permutation Test"
          counts[[length(counts) + 1]] <- pvalue(oneway_test(results_df[,variable]~as.factor(results_df[,"Group"])))
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
          
          # perform the contrast test
          summary(glht(fit, linfct = mcp(group = contrasts), alternative = "two.sided"), test = adjusted(type = "bonferroni")) -> comparison
          counts[[length(counts) + 1]] <- "Simultaneous Tests for General Linear Hypotheses - Multiple Comparisons of Means"
          counts[[length(counts) + 1]] <- comparison$test$pvalues[1]
          counts[[length(counts) + 1]] <- comparison$test$pvalues[2]
          counts[[length(counts) + 1]] <- comparison$test$pvalues[3]
          counts[[length(counts) + 1]] <- comparison$test$pvalues[4]
          counts[[length(counts) + 1]] <- comparison$test$pvalues[5]
          counts[[length(counts) + 1]] <- comparison$test$pvalues[6]
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

#### Statistics for corner preference ####
corner_stats <- NULL

for (variable in seq(3,110,4)){
  results_corner[,c(1,2,variable,variable+1,variable+2,variable+3)] -> subset_corner
  colnames(subset_corner)[3:6] <- c("Corner1","Corner2","Corner3","Corner4")
  if(sum(is.na(subset_corner[,3])) == nrow(subset_corner)){
    next()
  } else {
    df_long <- pivot_longer(subset_corner, cols = c("Corner1", "Corner2", "Corner3", "Corner4"), names_to = "Corner", values_to = "Proportion")
    df_long %>%
      group_by(Group) %>%
      summarise(p.value = friedman.test(Proportion ~ Corner | ID)$p.value) -> hold_corner_stats
    hold_corner_stats$Variable <- gsub("Corner[0-9]","Corner_",colnames(results_corner)[variable])
    rbind(corner_stats,hold_corner_stats) -> corner_stats 
  }
}
write.xlsx(corner_stats,paste("corner_statistics_",project,".xlsx",sep=""),colNames = T,rowNames = F,keepNA = F)

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

#### Learning curves ####
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
    geom_smooth(aes(group = Group),method = "lm", formula = y ~ x - 1, se = TRUE, level = 0.95, size = 1.5, alpha = 0.8) + # aes(group = Project, fill = Project)
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

#### General statistics ####
if(length(grep("^group[0-9]+_name$", ls(envir = globalenv()))) == 4){
  for (variable in 3:ncol(results_df)){
    results_df[,c(1,2,variable)] -> df
    colnames(df)[3] <- "Variable1"
    measurement_plotted <- colnames(results_df)[variable]
    # Assuming your data frame is called df
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
      as.numeric(results_stats[which(results_stats$Variable == measurement_plotted),26:31]) -> p_values
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

#### Plots for corner preference ####
for (variable in seq(3,110,4)){
  melt(results_corner[,c(1,variable,variable+1,variable+2,variable+3)]) -> melt_corner
  p <- ggplot(melt_corner, aes(fill = variable, y = value, x = ID)) +   geom_bar(position = "stack", stat = "identity") +
    scale_y_continuous(labels = scales::percent) +
    labs(fill = "Corner", y = "Corner Preference") +
    theme_minimal() +
    theme(panel.grid.major = element_line(linewidth = 1)) +
    theme(axis.text.x = element_text(angle = 90)) +
    xlab(NULL) +
    scale_fill_discrete(name = "Corner Preference", labels = c("Corner 1", "Corner 2", "Corner 3", "Corner 4"))
  ggsave(paste(gsub("[0-9]Preference", "_Preference", colnames(results_corner)[variable]), "_",project,".png",sep=""),p,dpi = 600,width = 40,height = 15,units = "cm")
}