project_folder <- "/path/figure2"
project <- "longDTA"
results_folder <- "/path/figure2/results"

library(dplyr)
#### Place Learning ####
read.delim(paste(project_folder,"/",project,"/","Challenge_1/PlaceLearning/Visit_WT.txt",sep="")) -> PL_visits
#### Reversal Learning ####
read.delim(paste(project_folder,"/",project,"/","Challenge_1/ReversalLearning/Visit_WT.txt",sep="")) -> RL_visits
#### Multiple Reversal Learning ####
read.delim(paste(project_folder,"/",project,"/","Challenge_1/MultipleReversalLearning/Visit_WT.txt",sep="")) -> MRL_visits
#### ELM Acquisition 1 ####
read.delim(paste(project_folder,"/",project,"/","Challenge_2/ELM1_Acquisition/Visit_WT.txt",sep="")) -> ELM1A_visits
#### ELM Retrieval 1 ####
read.delim(paste(project_folder,"/",project,"/","Challenge_2/ELM1_Retrieval/Visit_WT.txt",sep="")) -> ELM1R_visits
#### ELM Acquisition 2 ####
read.delim(paste(project_folder,"/",project,"/","Challenge_2/ELM2_Acquisition/Visit_WT.txt",sep="")) -> ELM2A_visits
#### ELM Retrieval 2 ####
read.delim(paste(project_folder,"/",project,"/","Challenge_2/ELM2_Retrieval/Visit_WT.txt",sep="")) -> ELM2R_visits
#### Patrolling 1 - CW1 ####
read.delim(paste(project_folder,"/",project,"/","Challenge_3/Patrolling1/Visit_WT.txt",sep="")) -> CW1_visits
#### Patrolling 2 - CW2 ####
read.delim(paste(project_folder,"/",project,"/","Challenge_3/Patrolling2/Visit_WT.txt",sep="")) -> CW2_visits
#### Patrolling 3 - CCW1 ####
read.delim(paste(project_folder,"/",project,"/","Challenge_3/Patrolling3/Visit_WT.txt",sep="")) -> CCW1_visits
#### Patrolling 4 - CCW2 ####
read.delim(paste(project_folder,"/",project,"/","Challenge_3/Patrolling4/Visit_WT.txt",sep="")) -> CCW2_visits

# Get the names of all objects that will be looked at
visits_objects <- ls(pattern = "_visits$")

# Dataframe to hold results
individual_DA_progression <- NULL
individual_DA_total <- NULL
individual_DA_forgrouping <- NULL

all_animals <- unique(PL_visits$Animal)

for (animal in all_animals) {
  # Iterate through all the challenges
  for (visits_object in visits_objects) {
    # Get the current challenge
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
      rbind(individual_DA_progression,df) -> individual_DA_progression
    } else {
      df <- data.frame(ID = animal, Number_DAs = 0, Success_DAs = 0, Challenge = gsub("_visits","",visits_object))
      rbind(individual_DA_progression,df) -> individual_DA_progression
    }
    
    # Total
    if(nrow(subset_df) > 0){
      row.names(subset_df) <- NULL
      df <- data.frame(ID = animal, Number_DAs = nrow(subset_df), Success_DAs = nrow(subset_df[which(subset_df$CornerCondition == "Correct"),]), Challenge = gsub("_visits","",visits_object))
      if(subset_df[which(row.names(subset_df) == 1),"CornerCondition"] == "Correct"){
        df$Success_DAs <- 1
      }
      rbind(individual_DA_total,df) -> individual_DA_total
    } else {
      df <- data.frame(ID = animal, Number_DAs = 0, Success_DAs = 0, Challenge = gsub("_visits","",visits_object))
      rbind(individual_DA_total,df) -> individual_DA_total
    }
    
    # Grouping
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
          df[nrow(df),"Success_DAs"] <- 1
        } else {
          df[nrow(df),"Success_DAs"] <- 0
        }
      }
      rbind(individual_DA_forgrouping,df) -> individual_DA_forgrouping
    } else {
      df <- data.frame(ID = animal, Number_DAs = 0, Success_DAs = 0, Challenge = gsub("_visits","",visits_object))
      rbind(individual_DA_forgrouping,df) -> individual_DA_forgrouping
    }
  }
}

"Corn_Oil" -> individual_DA_progression$Group
"Corn_Oil" -> individual_DA_total$Group
"Corn_Oil" -> individual_DA_forgrouping$Group

summary_DA_total <- aggregate(cbind(Number_DAs, Success_DAs) ~ Challenge + Group, data = individual_DA_total, FUN = sum)
summary_DA_total$Percentage_successful_DAs <- round((summary_DA_total$Success_DAs/summary_DA_total$Number_DAs)*100,digits = 2)

individual_DA_forgrouping %>%
  group_by(Group, Challenge) %>%
  mutate(Starting_N = sum(Number_DAs == 1)) %>%
  group_by(Group, Challenge, Number_DAs) %>%
  summarize(Success_DAs = sum(Success_DAs),
            Number_Individuals = n_distinct(ID),
            Percentage_Success = (Success_DAs / Number_Individuals) * 100,
            Remaining = Number_Individuals / first(Starting_N)) -> individual_DA_forgrouping

setwd(results_folder)

write.table(individual_DA_progression,paste("individual_DA_progression_",project,".txt",sep=""),sep = "\t",col.names = T,row.names = F,quote = F)
write.table(individual_DA_total,paste("individual_DA_total_",project,".txt",sep=""),sep = "\t",col.names = T,row.names = F,quote = F)
write.table(summary_DA_total,paste("summary_DA_total_",project,".txt",sep=""),sep = "\t",col.names = T,row.names = F,quote = F)
write.table(individual_DA_forgrouping,paste("individual_DA_forgrouping_",project,".txt",sep=""),sep = "\t",col.names = T,row.names = F,quote = F)

project <- "fEPOEmx_ICs"

#### Place Learning ####
read.delim(paste(project_folder,"/",project,"/","Challenge_1/PlaceLearning/Visit_WT.txt",sep="")) -> PL_visits
#### Reversal Learning ####
read.delim(paste(project_folder,"/",project,"/","Challenge_1/ReversalLearning/Visit_WT.txt",sep="")) -> RL_visits
#### Multiple Reversal Learning ####
read.delim(paste(project_folder,"/",project,"/","Challenge_1/MultipleReversalLearning/Visit_WT.txt",sep="")) -> MRL_visits
#### ELM Acquisition 1 ####
read.delim(paste(project_folder,"/",project,"/","Challenge_2/ELM1_Acquisition/Visit_WT.txt",sep="")) -> ELM1A_visits
#### ELM Retrieval 1 ####
read.delim(paste(project_folder,"/",project,"/","Challenge_2/ELM1_Retrieval/Visit_WT.txt",sep="")) -> ELM1R_visits
#### ELM Acquisition 2 ####
read.delim(paste(project_folder,"/",project,"/","Challenge_2/ELM2_Acquisition/Visit_WT.txt",sep="")) -> ELM2A_visits
#### ELM Retrieval 2 ####
read.delim(paste(project_folder,"/",project,"/","Challenge_2/ELM2_Retrieval/Visit_WT.txt",sep="")) -> ELM2R_visits
#### Patrolling 1 - CW1 ####
read.delim(paste(project_folder,"/",project,"/","Challenge_3/Patrolling1/Visit_WT.txt",sep="")) -> CW1_visits
#### Patrolling 2 - CW2 ####
read.delim(paste(project_folder,"/",project,"/","Challenge_3/Patrolling2/Visit_WT.txt",sep="")) -> CW2_visits
#### Patrolling 3 - CCW1 ####
read.delim(paste(project_folder,"/",project,"/","Challenge_3/Patrolling3/Visit_WT.txt",sep="")) -> CCW1_visits
#### Patrolling 4 - CCW2 ####
read.delim(paste(project_folder,"/",project,"/","Challenge_3/Patrolling4/Visit_WT.txt",sep="")) -> CCW2_visits

# Get the names of all objects that will be looked at
visits_objects <- ls(pattern = "_visits$")

# Dataframe to hold results
individual_DA_progression <- NULL
individual_DA_total <- NULL
individual_DA_forgrouping <- NULL

all_animals <- unique(PL_visits$Animal)

for (animal in all_animals) {
  # Iterate through all the challenges
  for (visits_object in visits_objects) {
    # Get the current challenge
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
      rbind(individual_DA_progression,df) -> individual_DA_progression
    } else {
      df <- data.frame(ID = animal, Number_DAs = 0, Success_DAs = 0, Challenge = gsub("_visits","",visits_object))
      rbind(individual_DA_progression,df) -> individual_DA_progression
    }
    
    # Total
    if(nrow(subset_df) > 0){
      row.names(subset_df) <- NULL
      df <- data.frame(ID = animal, Number_DAs = nrow(subset_df), Success_DAs = nrow(subset_df[which(subset_df$CornerCondition == "Correct"),]), Challenge = gsub("_visits","",visits_object))
      if(subset_df[which(row.names(subset_df) == 1),"CornerCondition"] == "Correct"){
        df$Success_DAs <- 1
      }
      rbind(individual_DA_total,df) -> individual_DA_total
    } else {
      df <- data.frame(ID = animal, Number_DAs = 0, Success_DAs = 0, Challenge = gsub("_visits","",visits_object))
      rbind(individual_DA_total,df) -> individual_DA_total
    }
    
    # Grouping
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
          df[nrow(df),"Success_DAs"] <- 1
        } else {
          df[nrow(df),"Success_DAs"] <- 0
        }
      }
      rbind(individual_DA_forgrouping,df) -> individual_DA_forgrouping
    } else {
      df <- data.frame(ID = animal, Number_DAs = 0, Success_DAs = 0, Challenge = gsub("_visits","",visits_object))
      rbind(individual_DA_forgrouping,df) -> individual_DA_forgrouping
    }
  }
}

"WT" -> individual_DA_progression$Group
"WT" -> individual_DA_total$Group
"WT" -> individual_DA_forgrouping$Group

summary_DA_total <- aggregate(cbind(Number_DAs, Success_DAs) ~ Challenge + Group, data = individual_DA_total, FUN = sum)
summary_DA_total$Percentage_successful_DAs <- round((summary_DA_total$Success_DAs/summary_DA_total$Number_DAs)*100,digits = 2)

individual_DA_forgrouping %>%
  group_by(Group, Challenge) %>%
  mutate(Starting_N = sum(Number_DAs == 1)) %>%
  group_by(Group, Challenge, Number_DAs) %>%
  summarize(Success_DAs = sum(Success_DAs),
            Number_Individuals = n_distinct(ID),
            Percentage_Success = (Success_DAs / Number_Individuals) * 100,
            Remaining = Number_Individuals / first(Starting_N)) -> individual_DA_forgrouping

write.table(individual_DA_progression,paste("individual_DA_progression_",project,".txt",sep=""),sep = "\t",col.names = T,row.names = F,quote = F)
write.table(individual_DA_total,paste("individual_DA_total_",project,".txt",sep=""),sep = "\t",col.names = T,row.names = F,quote = F)
write.table(summary_DA_total,paste("summary_DA_total_",project,".txt",sep=""),sep = "\t",col.names = T,row.names = F,quote = F)
write.table(individual_DA_forgrouping,paste("individual_DA_forgrouping_",project,".txt",sep=""),sep = "\t",col.names = T,row.names = F,quote = F)

project <- "CNPxEPORfl_fl_females"

#### Place Learning ####
read.delim(paste(project_folder,"/",project,"/","Challenge_1/PlaceLearning/Visit_WT.txt",sep="")) -> PL_visits
#### Reversal Learning ####
read.delim(paste(project_folder,"/",project,"/","Challenge_1/ReversalLearning/Visit_WT.txt",sep="")) -> RL_visits
#### Multiple Reversal Learning ####
read.delim(paste(project_folder,"/",project,"/","Challenge_1/MultipleReversalLearning/Visit_WT.txt",sep="")) -> MRL_visits
#### ELM Acquisition 1 ####
read.delim(paste(project_folder,"/",project,"/","Challenge_2/ELM1_Acquisition/Visit_WT.txt",sep="")) -> ELM1A_visits
#### ELM Retrieval 1 ####
read.delim(paste(project_folder,"/",project,"/","Challenge_2/ELM1_Retrieval/Visit_WT.txt",sep="")) -> ELM1R_visits
#### ELM Acquisition 2 ####
read.delim(paste(project_folder,"/",project,"/","Challenge_2/ELM2_Acquisition/Visit_WT.txt",sep="")) -> ELM2A_visits
#### ELM Retrieval 2 ####
read.delim(paste(project_folder,"/",project,"/","Challenge_2/ELM2_Retrieval/Visit_WT.txt",sep="")) -> ELM2R_visits
#### Patrolling 1 - CW1 ####
read.delim(paste(project_folder,"/",project,"/","Challenge_3/Patrolling1/Visit_WT.txt",sep="")) -> CW1_visits
#### Patrolling 2 - CW2 ####
read.delim(paste(project_folder,"/",project,"/","Challenge_3/Patrolling2/Visit_WT.txt",sep="")) -> CW2_visits
#### Patrolling 3 - CCW1 ####
read.delim(paste(project_folder,"/",project,"/","Challenge_3/Patrolling3/Visit_WT.txt",sep="")) -> CCW1_visits
#### Patrolling 4 - CCW2 ####
read.delim(paste(project_folder,"/",project,"/","Challenge_3/Patrolling4/Visit_WT.txt",sep="")) -> CCW2_visits

# Get the names of all objects that will be looked at
visits_objects <- ls(pattern = "_visits$")

# Dataframe to hold results
individual_DA_progression <- NULL
individual_DA_total <- NULL
individual_DA_forgrouping <- NULL

all_animals <- unique(PL_visits$Animal)

for (animal in all_animals) {
  # Iterate through all the challenges
  for (visits_object in visits_objects) {
    # Get the current challenge
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
      rbind(individual_DA_progression,df) -> individual_DA_progression
    } else {
      df <- data.frame(ID = animal, Number_DAs = 0, Success_DAs = 0, Challenge = gsub("_visits","",visits_object))
      rbind(individual_DA_progression,df) -> individual_DA_progression
    }
    
    # Total
    if(nrow(subset_df) > 0){
      row.names(subset_df) <- NULL
      df <- data.frame(ID = animal, Number_DAs = nrow(subset_df), Success_DAs = nrow(subset_df[which(subset_df$CornerCondition == "Correct"),]), Challenge = gsub("_visits","",visits_object))
      if(subset_df[which(row.names(subset_df) == 1),"CornerCondition"] == "Correct"){
        df$Success_DAs <- 1
      }
      rbind(individual_DA_total,df) -> individual_DA_total
    } else {
      df <- data.frame(ID = animal, Number_DAs = 0, Success_DAs = 0, Challenge = gsub("_visits","",visits_object))
      rbind(individual_DA_total,df) -> individual_DA_total
    }
    
    # Grouping
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
          df[nrow(df),"Success_DAs"] <- 1
        } else {
          df[nrow(df),"Success_DAs"] <- 0
        }
      }
      rbind(individual_DA_forgrouping,df) -> individual_DA_forgrouping
    } else {
      df <- data.frame(ID = animal, Number_DAs = 0, Success_DAs = 0, Challenge = gsub("_visits","",visits_object))
      rbind(individual_DA_forgrouping,df) -> individual_DA_forgrouping
    }
  }
}

"WT" -> individual_DA_progression$Group
"WT" -> individual_DA_total$Group
"WT" -> individual_DA_forgrouping$Group

summary_DA_total <- aggregate(cbind(Number_DAs, Success_DAs) ~ Challenge + Group, data = individual_DA_total, FUN = sum)
summary_DA_total$Percentage_successful_DAs <- round((summary_DA_total$Success_DAs/summary_DA_total$Number_DAs)*100,digits = 2)

individual_DA_forgrouping %>%
  group_by(Group, Challenge) %>%
  mutate(Starting_N = sum(Number_DAs == 1)) %>%
  group_by(Group, Challenge, Number_DAs) %>%
  summarize(Success_DAs = sum(Success_DAs),
            Number_Individuals = n_distinct(ID),
            Percentage_Success = (Success_DAs / Number_Individuals) * 100,
            Remaining = Number_Individuals / first(Starting_N)) -> individual_DA_forgrouping

write.table(individual_DA_progression,paste("individual_DA_progression_",project,".txt",sep=""),sep = "\t",col.names = T,row.names = F,quote = F)
write.table(individual_DA_total,paste("individual_DA_total_",project,".txt",sep=""),sep = "\t",col.names = T,row.names = F,quote = F)
write.table(summary_DA_total,paste("summary_DA_total_",project,".txt",sep=""),sep = "\t",col.names = T,row.names = F,quote = F)
write.table(individual_DA_forgrouping,paste("individual_DA_forgrouping_",project,".txt",sep=""),sep = "\t",col.names = T,row.names = F,quote = F)