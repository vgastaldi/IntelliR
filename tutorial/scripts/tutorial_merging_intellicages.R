#### IntelliCage script ####
## Uniting datasets
## This script should be used in case you have animals split in more than one file per challenge.
## Example: Each one of our computers handles two cages. When running we end up with two different files and these should be merged.
## The files have to be organized in the way explained in the general readme. They are saved back to the correct format/organization and ready for processing.

#### Loading first set of cages ####
## Select your Visit.txt file for Challenge 1
file.choose() -> project1

#### Loading second set of cages ####
## Select your Visit.txt file for Challenge 1
file.choose() -> project2p

#### What should be the new name of the folder with the merged cages? ####
project_merged <- "Sucrose"

#### Run everything below at once ####
project_folder <- gsub("(.*/)([^/]*)/Challenge_1.*", "\\1", project1)
project_folder <- gsub("/$", "", project_folder)
project <- gsub(".*/([^/]*)/Challenge_1.*", "\\1", project1)

#### Challenge 1 ####
read.delim(paste(project_folder,"/",project,"/","Challenge_1/Visit.txt",sep="")) -> C1_visits
paste(C1_visits$VisitID,"_set1",sep = "") -> C1_visits$VisitID
read.delim(paste(project_folder,"/",project,"/","Challenge_1/Nosepoke.txt",sep="")) -> C1_nosepokes
paste(C1_nosepokes$VisitID,"_set1",sep = "") -> C1_nosepokes$VisitID

#### Second set ####
project_folder2 <- gsub("(.*/)([^/]*)/Challenge_1.*", "\\1", project2p)
project_folder2 <- gsub("/$", "", project_folder2)
project2 <- gsub(".*/([^/]*)/Challenge_1.*", "\\1", project2p)

#### Challenge 1 ####
read.delim(paste(project_folder2,"/",project2,"/","Challenge_1/Visit.txt",sep="")) -> C1_visits2
paste(C1_visits2$VisitID,"_set2",sep = "") -> C1_visits2$VisitID
read.delim(paste(project_folder2,"/",project2,"/","Challenge_1/Nosepoke.txt",sep="")) -> C1_nosepokes2
paste(C1_nosepokes2$VisitID,"_set2",sep = "") -> C1_nosepokes2$VisitID

#### In the case the experiments ran separately, it is necessary to adjust the dates to ensure the analysis runs properly ####
unique(C1_visits2$StartDate)

# How many dates appear after the command? These are the number of times you need to run the lines below. Here you will find what is needed if your task ran for one day. You can comment out the additional days as needed and more if required.

C1_visits2[which(C1_visits2$StartDate == unique(C1_visits2$StartDate)[1]),"StartDate"] <- unique(C1_visits$StartDate)[1]
C1_visits2[which(C1_visits2$StartDate == unique(C1_visits2$StartDate)[2]),"StartDate"] <- unique(C1_visits$StartDate)[2]
#C1_visits2[which(C1_visits2$StartDate == unique(C1_visits2$StartDate)[3]),"StartDate"] <- unique(C1_visits$StartDate)[3]
#C1_visits2[which(C1_visits2$StartDate == unique(C1_visits2$StartDate)[4]),"StartDate"] <- unique(C1_visits$StartDate)[4]
#C1_visits2[which(C1_visits2$StartDate == unique(C1_visits2$StartDate)[5]),"StartDate"] <- unique(C1_visits$StartDate)[5]

C1_nosepokes2[which(C1_nosepokes2$StartDate == unique(C1_nosepokes2$StartDate)[1]),"StartDate"] <- unique(C1_nosepokes$StartDate)[1]
C1_nosepokes2[which(C1_nosepokes2$StartDate == unique(C1_nosepokes2$StartDate)[2]),"StartDate"] <- unique(C1_nosepokes$StartDate)[2]
#C1_nosepokes2[which(C1_nosepokes2$StartDate == unique(C1_nosepokes2$StartDate)[3]),"StartDate"] <- unique(C1_nosepokes$StartDate)[3]
#C1_nosepokes2[which(C1_nosepokes2$StartDate == unique(C1_nosepokes2$StartDate)[4]),"StartDate"] <- unique(C1_nosepokes$StartDate)[4]
#C1_nosepokes2[which(C1_nosepokes2$StartDate == unique(C1_nosepokes2$StartDate)[5]),"StartDate"] <- unique(C1_nosepokes$StartDate)[5]

#### Putting all files together ####
rbind(C1_visits,C1_visits2) -> C1_visits
rbind(C1_nosepokes,C1_nosepokes2) -> C1_nosepokes

#### Writing files ####
setwd(project_folder)
dir.create(project_merged)
dir.create(paste(project_merged,"/","Challenge_1/",sep=""))

#### Challenge 1 ####
write.table(C1_visits,paste(project_folder,"/",project_merged,"/","Challenge_1/Visit.txt",sep=""),col.names = T,row.names = F,quote = F,sep = "\t")
write.table(C1_nosepokes,paste(project_folder,"/",project_merged,"/","Challenge_1/Nosepoke.txt",sep=""),col.names = T,row.names = F,quote = F,sep = "\t")