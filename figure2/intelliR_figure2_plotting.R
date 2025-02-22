#### Figure 2 - IntelliR paper ####
## Controls

#### Load required packages ####
library(dplyr)
library(ggplot2)
library(emmeans)
set.seed(0)
#### Load all files that will be used ####
setwd("/path/figure2/results/")

list_DA_progression <- list.files(path = ".", pattern = "individual_DA_progression", recursive = T,full.names = T)
list_DA_total <- list.files(path = ".", pattern = "individual_DA_total", recursive = T,full.names = T)
list_DA_summary <- list.files(path = ".", pattern = "summary_DA", recursive = T,full.names = T)
list_DA_grouping <- list.files(path = ".", pattern = "individual_DA_forgrouping_", recursive = T,full.names = T)
  
DA_progression <- NULL
for (i in list_DA_progression){
  read.delim(i) -> hold
  gsub("^.*/|individual_DA_progression_|.txt","",i) -> hold$Project
  rbind(DA_progression,hold) -> DA_progression
}
rm(hold,i,list_DA_progression)

DA_total <- NULL
for (i in list_DA_total){
  read.delim(i) -> hold
  gsub("^.*/|individual_DA_total_|.txt","",i) -> hold$Project
  rbind(DA_total,hold) -> DA_total
}
rm(hold,i,list_DA_total)

DA_summary <- NULL
for (i in list_DA_summary){
  read.delim(i) -> hold
  gsub("^.*/|summary_DA_total_|.txt","",i) -> hold$Project
  rbind(DA_summary,hold) -> DA_summary
}
rm(hold,i,list_DA_summary)

DA_grouping <- NULL
for (i in list_DA_grouping){
  read.delim(i) -> hold
  gsub("^.*/|individual_DA_forgrouping_|.txt","",i) -> hold$Project
  rbind(DA_grouping,hold) -> DA_grouping
}
rm(hold,i,list_DA_grouping)

## Summary and Total will not be used for now

#### Keywords to filter control groups ####
kw_controls <- c("WT","Corn_Oil")

#### Filter files to keep controls ####
DA_progression[DA_progression$Group %in% kw_controls,] -> DA_progression
DA_total[DA_total$Group %in% kw_controls,] -> DA_total
DA_summary[DA_summary$Group %in% kw_controls,] -> DA_summary
DA_grouping[DA_grouping$Group %in% kw_controls,] -> DA_grouping

rm(kw_controls)

#### Plotting ####
## DA Progression
setwd("~/Documents/ownCloud/Collaborations/Behavior Unit/IC data/figure2/results")


#### Figure 2 ####
DA_progression$Success_DAs/DA_progression$Number_DAs -> DA_progression$Ongoing_Success
DA_progression[!is.nan(DA_progression$Ongoing_Success),] -> DA_progression
DA_progression[!grepl("Extinction",DA_progression$Challenge),] -> DA_progression
DA_progression[!grepl("Habituation",DA_progression$Challenge),] -> DA_progression


custom_labels <- c("longDTA" = "Cohort 1", "CNPxEPORfl_fl_females" = "Cohort 2", "fEPOEmx_ICs" = "Cohort 3")
custom_order <- c("longDTA","CNPxEPORfl_fl_females","fEPOEmx_ICs")

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
  p <- ggplot(df, aes(x = Number_DAs, y = Success_DAs, color = Project, group = ID)) +
    geom_line(alpha = 0.65) +
    geom_smooth(aes(group = Project),method = "lm", formula = y ~ x - 1, se = TRUE, level = 0.95, size = 1.5, alpha = 0.8) + # aes(group = Project, fill = Project)
    scale_color_manual(values = c("#fd7127", "#84c6d6", "#88ccb1"),breaks = custom_order, labels = custom_labels) +
    geom_line(data = artificial_data, aes(x = Number_DAs, y = Success_DAs, group = NULL), color = "black", linetype = "dashed", linewidth = 1.5) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 102.5),breaks = seq(50, max(df$Number_DAs), by = 50)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA),breaks = seq(10, max(df$Success_DAs), by = 10)) +
    theme_minimal() + 
    theme(
      axis.title = element_text(size = 30, face = "bold"),
      axis.text = element_text(size = 26),
      plot.title = element_text(size = 1),
      axis.line = element_line(),
      axis.ticks = element_line(color = "black", size = 0.5),
      axis.ticks.length = unit(0.1, "cm")
    ) +
    xlab("Drinking Attempts") +
    ylab("Successful Drinking Attempts") +
    labs(title = i)# + theme(legend.position = "none")
  ggsave(paste("for_legend",i,".png",sep = ""), p, width = 30, height = 25, units = "cm")
}

#### Statistics for the control cohort ####
#### Create a control regression ####
slope <- 0.25
intercept <- 0

all_comparisons_control_set <- NULL
all_comparisons_control_set_EM <- NULL

for (i in unique(DA_progression$Challenge)){
  DA_progression[which(DA_progression$Challenge == i),] -> df
  df[which(df$Number_DAs <= 100),] -> df
  # Generate the y-values for the artificial line directly using the desired slope and intercept
  artificial_data <- data.frame(ID = "dummy", Number_DAs = seq(1, max(df$Number_DAs), length.out = max(df$Number_DAs)))
  artificial_data$Success_DAs <- intercept + slope * artificial_data$Number_DAs
  artificial_data$Challenge <- i
  artificial_data$Group <- "dummy"
  artificial_data$Project <- "dummy"
  artificial_data$Ongoing_Success <- artificial_data$Success_DAs/artificial_data$Number_DAs
  rbind(df,artificial_data) -> df
  as.factor(df$Group) -> df$Group
  # Perform an ANCOVA
  ancova <- aov(Success_DAs ~ Number_DAs * Project, data = df)
  summary(emmeans(ancova, pairwise ~ Number_DAs:Project, at = list(Number_DAs = c(100)))) -> posthoc
  posthoc_df <- posthoc$contrasts[, c("contrast","estimate", "p.value")]
  posthoc_df$Challenge <- i
  posthoc$emmeans -> emmeans_df
  emmeans_df$Challenge <- i
  rbind(all_comparisons_control_set,posthoc_df) -> all_comparisons_control_set
  rbind(all_comparisons_control_set_EM,emmeans_df) -> all_comparisons_control_set_EM
}

as.character(all_comparisons_control_set$p.value) -> all_comparisons_control_set$p.value
all_comparisons_control_set$p.value[all_comparisons_control_set$p.value == "0"] <- "<2e-16"
openxlsx::write.xlsx(all_comparisons_control_set,"all_comparisons_control_set.xlsx")
openxlsx::write.xlsx(all_comparisons_control_set_EM,"all_comparisons_control_set_EM.xlsx")