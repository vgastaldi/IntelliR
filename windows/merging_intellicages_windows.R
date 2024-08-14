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
project_merged <- "testing_combined"

#### Run everything below at once ####
project_folder <- gsub("(.*\\\\)([^\\\\]*)\\\\Challenge_1.*", "\\1", project1)
project_folder <- gsub("\\\\$", "", project_folder)
project <- gsub(".*\\\\([^\\\\]*)\\\\Challenge_1.*", "\\1", project1)

#### Challenge 1 ####
read.delim(paste(project_folder,"\\",project,"\\","Challenge_1\\Visit.txt",sep="")) -> C1_visits
paste(C1_visits$VisitID,"_set1",sep = "") -> C1_visits$VisitID
read.delim(paste(project_folder,"\\",project,"\\","Challenge_1\\Nosepoke.txt",sep="")) -> C1_nosepokes
paste(C1_nosepokes$VisitID,"_set1",sep = "") -> C1_nosepokes$VisitID
#### Challenge 2 ####
read.delim(paste(project_folder,"\\",project,"\\","Challenge_2\\Visit.txt",sep="")) -> C2_visits
paste(C2_visits$VisitID,"_set1",sep = "") -> C2_visits$VisitID
read.delim(paste(project_folder,"\\",project,"\\","Challenge_2\\Nosepoke.txt",sep="")) -> C2_nosepokes
paste(C2_nosepokes$VisitID,"_set1",sep = "") -> C2_nosepokes$VisitID
#### Challenge 3 ####
read.delim(paste(project_folder,"\\",project,"\\","Challenge_3\\Visit.txt",sep="")) -> C3_visits
paste(C3_visits$VisitID,"_set1",sep = "") -> C3_visits$VisitID
read.delim(paste(project_folder,"\\",project,"\\","Challenge_3\\Nosepoke.txt",sep="")) -> C3_nosepokes
paste(C3_nosepokes$VisitID,"_set1",sep = "") -> C3_nosepokes$VisitID

#### Second set ####
project_folder2 <- gsub("(.*\\\\)([^\\\\]*)\\\\Challenge_1.*", "\\1", project2p)
project_folder2 <- gsub("\\\\$", "", project_folder2)
project2 <- gsub(".*\\\\([^\\\\]*)\\\\Challenge_1.*", "\\1", project2p)

#### Challenge 1 ####
read.delim(paste(project_folder2,"\\",project2,"\\","Challenge_1\\Visit.txt",sep="")) -> C1_visits2
paste(C1_visits2$VisitID,"_set2",sep = "") -> C1_visits2$VisitID
read.delim(paste(project_folder2,"\\",project2,"\\","Challenge_1\\Nosepoke.txt",sep="")) -> C1_nosepokes2
paste(C1_nosepokes2$VisitID,"_set2",sep = "") -> C1_nosepokes2$VisitID
#### Challenge 2 ####
read.delim(paste(project_folder2,"\\",project2,"\\","Challenge_2\\Visit.txt",sep="")) -> C2_visits2
paste(C2_visits2$VisitID,"_set2",sep = "") -> C2_visits2$VisitID
read.delim(paste(project_folder2,"\\",project2,"\\","Challenge_2\\Nosepoke.txt",sep="")) -> C2_nosepokes2
paste(C2_nosepokes2$VisitID,"_set2",sep = "") -> C2_nosepokes2$VisitID
#### Challenge 3 ####
read.delim(paste(project_folder2,"\\",project2,"\\","Challenge_3\\Visit.txt",sep="")) -> C3_visits2
paste(C3_visits2$VisitID,"_set2",sep = "") -> C3_visits2$VisitID
read.delim(paste(project_folder2,"\\",project2,"\\","Challenge_3\\Nosepoke.txt",sep="")) -> C3_nosepokes2
paste(C3_nosepokes2$VisitID,"_set2",sep = "") -> C3_nosepokes2$VisitID

#### In the case the experiments ran separately, it is necessary to adjust the dates to ensure the analysis runs properly ####
C1_visits2[which(C1_visits2$StartDate == unique(C1_visits2$StartDate)[1]
),"StartDate"] <- unique(C1_visits$StartDate)[1]
C1_visits2[which(C1_visits2$StartDate == unique(C1_visits2$StartDate)[2]
),"StartDate"] <- unique(C1_visits$StartDate)[2]
C1_visits2[which(C1_visits2$StartDate == unique(C1_visits2$StartDate)[3]
),"StartDate"] <- unique(C1_visits$StartDate)[3]
C1_visits2[which(C1_visits2$StartDate == unique(C1_visits2$StartDate)[4]
),"StartDate"] <- unique(C1_visits$StartDate)[4]
C1_visits2[which(C1_visits2$StartDate == unique(C1_visits2$StartDate)[5]
),"StartDate"] <- unique(C1_visits$StartDate)[5]

C2_visits2[which(C2_visits2$StartDate == unique(C2_visits2$StartDate)[1]
),"StartDate"] <- unique(C2_visits$StartDate)[1]
C2_visits2[which(C2_visits2$StartDate == unique(C2_visits2$StartDate)[2]
),"StartDate"] <- unique(C2_visits$StartDate)[2]
C2_visits2[which(C2_visits2$StartDate == unique(C2_visits2$StartDate)[3]
),"StartDate"] <- unique(C2_visits$StartDate)[3]
C2_visits2[which(C2_visits2$StartDate == unique(C2_visits2$StartDate)[4]
),"StartDate"] <- unique(C2_visits$StartDate)[4]
C2_visits2[which(C2_visits2$StartDate == unique(C2_visits2$StartDate)[5]
),"StartDate"] <- unique(C2_visits$StartDate)[5]
C2_visits2[which(C2_visits2$StartDate == unique(C2_visits2$StartDate)[6]
),"StartDate"] <- unique(C2_visits$StartDate)[6]

C3_visits2[which(C3_visits2$StartDate == unique(C3_visits2$StartDate)[1]
),"StartDate"] <- unique(C3_visits$StartDate)[1]
C3_visits2[which(C3_visits2$StartDate == unique(C3_visits2$StartDate)[2]
),"StartDate"] <- unique(C3_visits$StartDate)[2]
C3_visits2[which(C3_visits2$StartDate == unique(C3_visits2$StartDate)[3]
),"StartDate"] <- unique(C3_visits$StartDate)[3]
C3_visits2[which(C3_visits2$StartDate == unique(C3_visits2$StartDate)[4]
),"StartDate"] <- unique(C3_visits$StartDate)[4]
C3_visits2[which(C3_visits2$StartDate == unique(C3_visits2$StartDate)[5]
),"StartDate"] <- unique(C3_visits$StartDate)[5]
C3_visits2[which(C3_visits2$StartDate == unique(C3_visits2$StartDate)[6]
),"StartDate"] <- unique(C3_visits$StartDate)[6]

C1_nosepokes2[which(C1_nosepokes2$StartDate == unique(C1_nosepokes2$StartDate)[1]
),"StartDate"] <- unique(C1_nosepokes$StartDate)[1]
C1_nosepokes2[which(C1_nosepokes2$StartDate == unique(C1_nosepokes2$StartDate)[2]
),"StartDate"] <- unique(C1_nosepokes$StartDate)[2]
C1_nosepokes2[which(C1_nosepokes2$StartDate == unique(C1_nosepokes2$StartDate)[3]
),"StartDate"] <- unique(C1_nosepokes$StartDate)[3]
C1_nosepokes2[which(C1_nosepokes2$StartDate == unique(C1_nosepokes2$StartDate)[4]
),"StartDate"] <- unique(C1_nosepokes$StartDate)[4]
C1_nosepokes2[which(C1_nosepokes2$StartDate == unique(C1_nosepokes2$StartDate)[5]
),"StartDate"] <- unique(C1_nosepokes$StartDate)[5]

C2_nosepokes2[which(C2_nosepokes2$StartDate == unique(C2_nosepokes2$StartDate)[1]
),"StartDate"] <- unique(C2_nosepokes$StartDate)[1]
C2_nosepokes2[which(C2_nosepokes2$StartDate == unique(C2_nosepokes2$StartDate)[2]
),"StartDate"] <- unique(C2_nosepokes$StartDate)[2]
C2_nosepokes2[which(C2_nosepokes2$StartDate == unique(C2_nosepokes2$StartDate)[3]
),"StartDate"] <- unique(C2_nosepokes$StartDate)[3]
C2_nosepokes2[which(C2_nosepokes2$StartDate == unique(C2_nosepokes2$StartDate)[4]
),"StartDate"] <- unique(C2_nosepokes$StartDate)[4]
C2_nosepokes2[which(C2_nosepokes2$StartDate == unique(C2_nosepokes2$StartDate)[5]
),"StartDate"] <- unique(C2_nosepokes$StartDate)[5]
C2_nosepokes2[which(C2_nosepokes2$StartDate == unique(C2_nosepokes2$StartDate)[6]
),"StartDate"] <- unique(C2_nosepokes$StartDate)[6]

C3_nosepokes2[which(C3_nosepokes2$StartDate == unique(C3_nosepokes2$StartDate)[1]
),"StartDate"] <- unique(C3_nosepokes$StartDate)[1]
C3_nosepokes2[which(C3_nosepokes2$StartDate == unique(C3_nosepokes2$StartDate)[2]
),"StartDate"] <- unique(C3_nosepokes$StartDate)[2]
C3_nosepokes2[which(C3_nosepokes2$StartDate == unique(C3_nosepokes2$StartDate)[3]
),"StartDate"] <- unique(C3_nosepokes$StartDate)[3]
C3_nosepokes2[which(C3_nosepokes2$StartDate == unique(C3_nosepokes2$StartDate)[4]
),"StartDate"] <- unique(C3_nosepokes$StartDate)[4]
C3_nosepokes2[which(C3_nosepokes2$StartDate == unique(C3_nosepokes2$StartDate)[5]
),"StartDate"] <- unique(C3_nosepokes$StartDate)[5]
C3_nosepokes2[which(C3_nosepokes2$StartDate == unique(C3_nosepokes2$StartDate)[6]
),"StartDate"] <- unique(C3_nosepokes$StartDate)[6]

#### Putting all files together ####
rbind(C1_visits,C1_visits2) -> C1_visits
rbind(C2_visits,C2_visits2) -> C2_visits
rbind(C3_visits,C3_visits2) -> C3_visits
rbind(C1_nosepokes,C1_nosepokes2) -> C1_nosepokes
rbind(C2_nosepokes,C2_nosepokes2) -> C2_nosepokes
rbind(C3_nosepokes,C3_nosepokes2) -> C3_nosepokes

#### Writing files ####
setwd(project_folder)
dir.create(project_merged)
dir.create(paste(project_merged,"\\","Challenge_1\\",sep=""))
dir.create(paste(project_merged,"\\","Challenge_2\\",sep=""))
dir.create(paste(project_merged,"\\","Challenge_3\\",sep=""))

#### Challenge 1 ####
write.table(C1_visits,paste(project_folder2,"\\",project_merged,"\\","Challenge_1\\Visit.txt",sep=""),col.names = T,row.names = F,quote = F,sep = "\t")
write.table(C1_nosepokes,paste(project_folder2,"\\",project_merged,"\\","Challenge_1\\Nosepoke.txt",sep=""),col.names = T,row.names = F,quote = F,sep = "\t")
#### Challenge 2 ####
write.table(C2_visits,paste(project_folder2,"\\",project_merged,"\\","Challenge_2\\Visit.txt",sep=""),col.names = T,row.names = F,quote = F,sep = "\t")
write.table(C2_nosepokes,paste(project_folder2,"\\",project_merged,"\\","Challenge_2\\Nosepoke.txt",sep=""),col.names = T,row.names = F,quote = F,sep = "\t")
#### Challenge 3 ####
write.table(C3_visits,paste(project_folder2,"\\",project_merged,"\\","Challenge_3\\Visit.txt",sep=""),col.names = T,row.names = F,quote = F,sep = "\t")
write.table(C3_nosepokes,paste(project_folder2,"\\",project_merged,"\\","Challenge_3\\Nosepoke.txt",sep=""),col.names = T,row.names = F,quote = F,sep = "\t")