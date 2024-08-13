#### Manual setup for IntelliR ####
# The lines below give the necessary variables for IntelliR to work
# The example uses 2 groups, but you can increase it up to four groups
read.delim(file.choose()) -> unblinding
group1_name <- "Name1"
group2_name <- "Name2"
group1_color <- "red"
group2_color <- "blue"
project <- "Project" # name of the folder where your files are
project_folder <- "~/Documents/IntelliCage_data" # put your path here
results_folder <- "~/Documents/IntelliCage_data/results" # put your path here