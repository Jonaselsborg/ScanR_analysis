# ScanR 2 Analysis of BiFC / Split Venus

#'''
#' Methodology
#' 
#' Q: What is: Parent Object ID (Well)	Parent Trace ID ?
#' 


rm(list=ls()) #clear workspace
# Load packages
library(ggplot2)
library(EBImage)
library(reshape2)
library(plyr)
library(dplyr)
library(ggpubr)
setwd("~/Dropbox/Master Thesis_UBI/UBI Data/Split Venus analysis (IR_2)") #wd

# Here follows a block of code to read the .txt files and annotate them.
combinations <- c("V1-H1.2", "V2-K63-S-UIM", "V1-ev+ V2-ev", "V1-H1.2+ V2-ev", "V1-ev+ V2-K63-S-UIM", "V1-H1.2+ V2-K63-S-UIM")

Slide1 <- read.table(file = "ParameterData_1_Main.txt", header = T, sep = "\t") #The file is tab seperated
Slide1$plasmid <- NA #create column
Slide1[which(Slide1$Well %in% c(1,2,3)), "plasmid"] <- combinations[1] #the %in% matches the vector to the Well value
Slide1[which(Slide1$Well %in% c(4,5,6)), "plasmid"] <- combinations[2]
#Slide1[which(Slide1$Well == 1 | Slide1$Well == 2 | Slide1$Well == 3), "plasmid"] <- combinations[1] #This is an alternative method
Slide1$replicate <- NA # Create a new column with empty values
Slide1[which(Slide1$Well %in% c(1,4)), "replicate"] <- "Replicate 1" # Annotate the replicates
Slide1[which(Slide1$Well %in% c(2,5)), "replicate"] <- "Replicate 2"
Slide1[which(Slide1$Well %in% c(3,6)), "replicate"] <- "Replicate 3"

arte1 <- read.table(file = "artefact_1.txt", header = T, sep = "\t") # Load a .txt file with all the artefacts for each microscopy slide

# This loop removes all the pictures with artefacts from slide 1 
for (i in 1:nrow(arte1)){
  well_no<-arte1[i,"well"]
  position_no<-arte1[i,"picture"]
  Slide1<-filter(Slide1, !(Well == well_no & Position ==position_no))
}


#----
Slide2 <- read.table(file = "ParameterData_2_Main.txt", header = T, sep = "\t")
Slide2$plasmid <- NA
Slide2[which(Slide2$Well %in% c(1,2,3)), "plasmid"] <- combinations[3]
Slide2[which(Slide2$Well %in% c(4,5,6)), "plasmid"] <- combinations[4]
Slide2$replicate <- NA
Slide2[which(Slide2$Well %in% c(1,4)), "replicate"] <- "Replicate 1"
Slide2[which(Slide2$Well %in% c(2,5)), "replicate"] <- "Replicate 2"
Slide2[which(Slide2$Well %in% c(3,6)), "replicate"] <- "Replicate 3"


arte2 <- read.table(file = "artefact_2.txt", header = T, sep = "\t")
for (i in 1:nrow(arte2)){
  well_no<-arte2[i,"well"]
  position_no<-arte2[i,"picture"]
  Slide2<-filter(Slide2, !(Well == well_no & Position ==position_no))
}

#----
Slide3 <- read.table(file = "ParameterData_3_Main.txt", header = T, sep = "\t")
Slide3$plasmid <- NA
Slide3[which(Slide3$Well %in% c(1,2,3)), "plasmid"] <- combinations[5]
Slide3[which(Slide3$Well %in% c(4,5,6)), "plasmid"] <- combinations[6]
Slide3$replicate <- NA
Slide3[which(Slide3$Well %in% c(1,4)), "replicate"] <- "Replicate 1"
Slide3[which(Slide3$Well %in% c(2,5)), "replicate"] <- "Replicate 2"
Slide3[which(Slide3$Well %in% c(3,6)), "replicate"] <- "Replicate 3"


arte3 <- read.table(file = "artefact_3.txt", header = T, sep = "\t")
for (i in 1:nrow(arte3)){
  well_no<-arte3[i,"well"]
  position_no<-arte3[i,"picture"]
  Slide3<-filter(Slide3, !(Well == well_no & Position ==position_no))
}



# import + IR treated slides

Slide4 <- read.table(file = "ParameterData_4_Main.txt", header = T, sep = "\t") 
Slide4$plasmid <- NA 
Slide4[which(Slide4$Well %in% c(1,2,3)), "plasmid"] <- combinations[1]
Slide4[which(Slide4$Well %in% c(4,5,6)), "plasmid"] <- combinations[2]
Slide4$replicate <- NA
Slide4[which(Slide4$Well %in% c(1,4)), "replicate"] <- "Replicate 1"
Slide4[which(Slide4$Well %in% c(2,5)), "replicate"] <- "Replicate 2"
Slide4[which(Slide4$Well %in% c(3,6)), "replicate"] <- "Replicate 3"

arte4 <- read.table(file = "artefact_4.txt", header = T, sep = "\t")
for (i in 1:nrow(arte4)){
  well_no<-arte4[i,"well"]
  position_no<-arte4[i,"picture"]
  Slide4<-filter(Slide4, !(Well == well_no & Position ==position_no))
}

Slide5 <- read.table(file = "ParameterData_5_Main.txt", header = T, sep = "\t")
Slide5$plasmid <- NA
Slide5[which(Slide5$Well %in% c(1,2,3)), "plasmid"] <- combinations[3]
Slide5[which(Slide5$Well %in% c(4,5,6)), "plasmid"] <- combinations[4]
Slide5$replicate <- NA
Slide5[which(Slide5$Well %in% c(1,4)), "replicate"] <- "Replicate 1"
Slide5[which(Slide5$Well %in% c(2,5)), "replicate"] <- "Replicate 2"
Slide5[which(Slide5$Well %in% c(3,6)), "replicate"] <- "Replicate 3"

arte5 <- read.table(file = "artefact_5.txt", header = T, sep = "\t") # no artefacts = no filter
#for (i in 1:nrow(arte5)){
 # well_no<-arte5[i,"well"]
  #position_no<-arte5[i,"picture"]
  #Slide5<-filter(Slide5, !(Well == well_no & Position ==position_no))
#}

Slide6 <- read.table(file = "ParameterData_6_Main.txt", header = T, sep = "\t")
Slide6$plasmid <- NA
Slide6[which(Slide6$Well %in% c(1,2,3)), "plasmid"] <- combinations[5]
Slide6[which(Slide6$Well %in% c(4,5,6)), "plasmid"] <- combinations[6]
Slide6$replicate <- NA
Slide6[which(Slide6$Well %in% c(1,4)), "replicate"] <- "Replicate 1"
Slide6[which(Slide6$Well %in% c(2,5)), "replicate"] <- "Replicate 2"
Slide6[which(Slide6$Well %in% c(3,6)), "replicate"] <- "Replicate 3"

arte6 <- read.table(file = "artefact_6.txt", header = T, sep = "\t") # no artefacts = no filter
#for (i in 1:nrow(arte6)){
 # well_no<-arte6[i,"well"]
  #position_no<-arte6[i,"picture"]
  #Slide6<-filter(Slide6, !(Well == well_no & Position ==position_no))
#}

#----

# samplesize normalization:
#   Make vector with the lowerst cell counts from each slide
the_lowest_slide<-c(
  min((Slide1 %>% count(Well))[2]),
  min((Slide2 %>% count(Well))[2]),
  min((Slide3 %>% count(Well))[2]),
  min((Slide4 %>% count(Well))[2]),
  min((Slide5 %>% count(Well))[2]),
  min((Slide6 %>% count(Well))[2])
)

norm_by_count <- the_lowest_slide[which.min(the_lowest_slide)] #which is the lowest?

# Randomly sample this amount from all different samples to normalize sample size

sample.Slide1 <- Slide1 %>%
  group_by(Well) %>%
  sample_n(norm_by_count)

sample.Slide2 <- Slide2 %>%
  group_by(Well) %>%
  sample_n(norm_by_count)

sample.Slide3 <- Slide3 %>%
  group_by(Well) %>%
  sample_n(norm_by_count)

sample.Slide4 <- Slide4 %>%
  group_by(Well) %>%
  sample_n(norm_by_count)

sample.Slide5 <- Slide5 %>%
  group_by(Well) %>%
  sample_n(norm_by_count)

sample.Slide6 <- Slide6 %>%
  group_by(Well) %>%
  sample_n(norm_by_count)


# B = no damage, C = +IR
B_slides <- rbind(sample.Slide1, sample.Slide2, sample.Slide3) #append rows 
B_slides$IR <- "0Gy" #damage = false
C_slides <- rbind(sample.Slide4, sample.Slide5, sample.Slide6)
C_slides$IR <- "4Gy"



scanR <- rbind(B_slides, C_slides) # this is your dataframe. 
#class(scanR)
scanR <- as.data.frame(scanR)
#class(scanR)

#for visualization
scanR$plasmid <- factor(scanR$plasmid, levels = combinations) #orders the data according to the correct order 1:6
levels(scanR$plasmid) <- gsub(" ", "\n", levels(scanR$plasmid)) #regex to replace space with newline for pretty plots

# 2x6 grid of histograms

ggplot(data = scanR, aes(x = Mean.Intensity.488.GFP_lowexp)) +
  geom_histogram(bins = 100) + 
  scale_y_log10() +
  facet_grid(plasmid ~ IR)

# For plotting just the no IR data
B_slides$plasmid <- factor(B_slides$plasmid, levels = combinations) #orders the data according to the correct order 1:6
levels(B_slides$plasmid) <- gsub(" ", "\n", levels(B_slides$plasmid)) #regex to replace space with newline for pretty plots

# jitter plot with medians
ggplot(data = B_slides, aes(B_slides$plasmid, B_slides$Mean.Intensity.488.GFP_lowexp)) +
  #theme(axis.text = element_text(size = 5)) +
  #scale_y_log10() +
  geom_jitter(width = 0.25, size = 0.5, ) + #colour = "darkgreen"
  stat_summary(fun.y = median, fun.ymin = median, fun.ymax = median,
               geom = "crossbar", width = 0.5, colour = "red")

# For plotting just the IR data
C_slides$plasmid <- factor(C_slides$plasmid, levels = combinations) #orders the data according to the correct order 1:6
levels(C_slides$plasmid) <- gsub(" ", "\n", levels(C_slides$plasmid)) #regex to replace space with newline for pretty plots


ggplot(data = C_slides, aes(C_slides$plasmid, C_slides$Mean.Intensity.488.GFP_lowexp)) +
  #theme(axis.text = element_text(size = 5)) +
  #scale_y_log10() +
  geom_jitter(width = 0.25, size = 0.5, ) + #colour = "darkgreen"
  stat_summary(fun.y = median, fun.ymin = median, fun.ymax = median,
               geom = "crossbar", width = 0.5, colour = "red")


# for gH2AX histograms, distributions of mean gH2AX per nuclei damage/no damage
ggplot(data = scanR, aes(x = Mean_H2AX_PerNuc)) +
  geom_histogram(bins = 50) + 
  #scale_x_log10() +
  facet_wrap(~IR, ncol = 1)


# Number of identified gH2AX foci per cell
ggplot(data = scanR, aes(x = IR, y = scanR$gH2AX..Counts)) +
  geom_boxplot() 



# GATE: GFP-positive cells
# determine GFP-positive gate (distribution of non-flourscent data)
nogfp <- filter(scanR, scanR$plasmid == combinations[2])
ggplot(data = nogfp, aes(x = nogfp$Mean.Intensity.488.GFP_lowexp)) +
  geom_histogram(bins = 50)

GFP_pos <- subset.data.frame(scanR, scanR$Mean.Intensity.488.GFP_lowexp>10) # 10 <- nrow 3786

# To investigate how gH2AX foci correlate with GFP intensity
H1.2_K63_damage <- filter(GFP_pos, plasmid == "V1-H1.2+\nV2-K63-S-UIM") # Subset only the H1.2 + ubi sensor
H1.2_K63_damage <- filter(H1.2_K63_damage, IR == "4Gy") # Subset only the cells that recieved IR
#H1.2_K63_damage <- filter(H1.2_K63_damage, Mean_H2AX_PerNuc > 0) # Subset only cells with gH2AX foci (some cells recived damage but did not show any foci)
#H1.2_K63_damage <- filter(H1.2_K63_damage, replicate == "Replicate 1") # Subset a particular replicate
#var(H1.2_K63_damage$Mean.Intensity.488.GFP_lowexp) # If you want to know the variance

# How does the number of gH2AX counts correlate with Venus intensity? 
ggscatter(H1.2_K63_damage, y = "Mean.Intensity.488.GFP_lowexp", x = "Mean_H2AX_PerNuc", add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          alpha = 0.2
          ) # x = "gH2AX..Counts"

# The distribution of this data
ggplot(data = H1.2_K63_damage, aes(x = Mean_H2AX_PerNuc)) +
  geom_histogram(bins = 50) + 
  #scale_x_log10() +
  facet_wrap(~IR, ncol = 1)

#----

# Plot the replicates and their GFP intensity for every concition. (boxpots)
ggplot(data = GFP_pos, aes(plasmid, Mean.Intensity.488.GFP_lowexp)) +
  geom_boxplot(outlier.alpha = 0.25, aes(fill = IR)) +
  facet_grid(replicate ~ .) +
  xlab("BiFC constructs") +
  ylab("Mean GFP intensity (AU)") +
  ggtitle("Boxplots of H1.2 + K63-Super-UIM BiFC assay with IR treatment")

###########
#############
############
###########

no_damage <- filter(GFP_pos, GFP_pos$IR == "0Gy" & GFP_pos$plasmid == "V1-H1.2+\nV2-K63-S-UIM")
damage <-   filter(GFP_pos, GFP_pos$IR == "4Gy" & GFP_pos$plasmid == "V1-H1.2+\nV2-K63-S-UIM")
summary(GFP_pos)


# kommentar, din n er så høj, at hvad som helst kan blive signifikant..
# This is a non-parametric test to see if the +/- IR samples' means (not seperates in replicates) are significantly different. They are very much so.
wilcox.test(no_damage$Mean.Intensity.488.GFP_lowexp, damage$Mean.Intensity.488.GFP_lowexp, alternative = "two.sided")

# --------------------------------------------- 
# For clairs favorite plot with means and SEM 
# Basically, get the means for the different conditions for the different replicates

#GFP_pos %>%
rep1_0_means <-  filter(GFP_pos, replicate == "Replicate 1" & GFP_pos$IR == "0Gy") %>%
  group_by(plasmid) %>%
  summarize(mean = mean(Mean.Intensity.488.GFP_lowexp))

rep2_0_means <-  filter(GFP_pos, replicate == "Replicate 2" & GFP_pos$IR == "0Gy") %>%
  group_by(plasmid) %>%
  summarize(mean = mean(Mean.Intensity.488.GFP_lowexp))

rep3_0_means <-  filter(GFP_pos, replicate == "Replicate 3" & GFP_pos$IR == "0Gy") %>%
  group_by(plasmid) %>%
  summarize(mean = mean(Mean.Intensity.488.GFP_lowexp))

rep1_4_means <-  filter(GFP_pos, replicate == "Replicate 1" & GFP_pos$IR == "4Gy") %>%
  group_by(plasmid) %>%
  summarize(mean = mean(Mean.Intensity.488.GFP_lowexp))

rep2_4_means <-  filter(GFP_pos, replicate == "Replicate 2" & GFP_pos$IR == "4Gy") %>%
  group_by(plasmid) %>%
  summarize(mean = mean(Mean.Intensity.488.GFP_lowexp))

rep3_4_means <-  filter(GFP_pos, replicate == "Replicate 3" & GFP_pos$IR == "4Gy") %>%
  group_by(plasmid) %>%
  summarize(mean = mean(Mean.Intensity.488.GFP_lowexp))


#fix this... you need to unmount dplyr, then load plyr. They cant crosstalk...
if("dplyr" %in% (.packages())){
  detach("package:dplyr", unload=TRUE) 
  detach("package:plyr", unload=TRUE) 
} 
library(plyr)


# Data exported to Excel for reformatting, but imported again here:
terrified_data <- read.table(file = "plot for terrified biologists.txt", sep = "\t", header = T)
colnames(terrified_data) <- c("damage", combinations[3:6])
m.terrified <- melt(terrified_data, id.vars = "damage") # melts dataframe into "long" format

# Is the (means of the) means of the damaged sample significantly different than the means of the undamaged?
t.test(terrified_data$`V1-H1.2+ V2-K63-S-UIM`[4:6],terrified_data$`V1-H1.2+ V2-K63-S-UIM`[1:3])

head(m.terrified)
##########
#http://www.sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization
#+++++++++++++++++++++++++
# Function to calculate the mean and the standard deviation
# for each group
#+++++++++++++++++++++++++
# data : a data frame
# varname : the name of a column containing the variable
#to be summariezed
# groupnames : vector of column names to be used as
# grouping variables
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}
########

# to make SEM? Alternate method.. Right now we use the data_summary above that use SD
# Standard deviation is a measure of dispersion of the data from the mean. 
# SEM is a measure of how precise is our estimate of the mean.
std <- function(x) sd(x)/sqrt(length(x))

# Here is the SEM
sd(terrified_data$`V1-H1.2+ V2-K63-S-UIM`[4:6])/sqrt(length(terrified_data$`V1-H1.2+ V2-K63-S-UIM`[4:6]))

# Or using the function:
std(terrified_data$`V1-H1.2+ V2-K63-S-UIM`[4:6])


#----

# Get the plotting params for the plot (means of means;value + SD of the 3 means;sd)
m.terrified.summ <- data_summary(m.terrified, varname="value", 
                    groupnames=c("damage", "variable"))

# Make the plot
ggplot(m.terrified.summ, aes(x=variable, y=value, fill=damage)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=.2,
                position=position_dodge(.9)) +
  xlab("BiFC constructs") +
  ylab("Mean GFP intensity (AU)") +
  ggtitle("Barplot of H1.2 + K63-Super-UIM BiFC assay with IR treatment")

library(dplyr) #load dplyr again..

#----

