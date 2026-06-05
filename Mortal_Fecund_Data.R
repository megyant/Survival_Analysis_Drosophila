
## load necesary libraries n
library(readxl)
library(ggplot2)
library(Hmisc) 
library(tidyverse)
library(emmeans)
library(lme4)
library(lmerTest)
library(ggrepel)
library(plyr)
library(dplyr)
library(survminer)
library(data.table)

### clean working environment
rm(list=ls()) 

select <- dplyr::select
filter <- dplyr::filter
summarise <- dplyr::summarise

# set WD
setwd("~/.") # Replace by the directory where your files are 

#### SURVIVAL TABLE ####
# read mortality data
# the starting file should be the one Mort_Gx created in script 1 

all_data_full = read.csv('datasets/Mort_5.csv')
head(all_data_full)
str(all_data_full)

#### preparing mortality fecundity datasets ####

## create new column with time in days (no decimals) to merge later with pupal counts
all_data_full =all_data_full %>%
  mutate(TimeRounded = round(timeINdays))

range(all_data_full$timeINdays-all_data_full$TimeRounded) ### range should be small not bigger 0.25 or so?

## only neeeded in G0 
## get only until day 10 - like in all other datasets 
all_data = all_data_full %>% 
  filter(TimeRounded < 11 )

#### subset to get only female data - to later use for fecundity 

all_data$Sex <- as.factor(all_data$Sex)
all_data_F <- subset(all_data, Sex == "F")

all_data_F$Unit = paste(all_data_F$VialNumber, all_data_F$Sex)
unique(all_data_F$Unit)

### change mortality coding so that it has only 0 and 1 ** Status - Censor
all_data_F = all_data_F %>%
  mutate(Status_01 = Censor-1)

# GET OVERVIEW
head(all_data_F)
#overview.alive = all_data_F %>% group_by(Unit) %>%
 # summarise(TotalID = n(), TotalDead = sum(Status_01))
# GET OVERVIEW
overview.alive = all_data_F %>% group_by(Unit) %>%
  summarise(TotalID = n(), TotalDead = sum(Status_01))

## problem  - overview.dead desn't work
overview.dead = all_data_F %>% group_by(Unit,TimeRounded) %>%
  summarise(TotalDeadF = sum(Status_01))

### change censoring coding so that it has only 0 and 1 ** Status - Censor

all_data_F = all_data_F %>%
  mutate(Status_censoredF = abs(Censor-2))

overview.censored = all_data_F %>% group_by(Unit,TimeRounded) %>%
  summarise(Censored = sum(Status_censoredF))

# overview.censored = all_data_F %>%
 # group_by( Unit,TimeRounded )  %>%
 # filter(Censor==0) %>% 
 #  summarise(CensoredF = sum(Status_censoredF))
#merge_12_red1 = all_data_F %>%
 # group_by( Unit )  %>%
  #summarise(deadF = sum(Status_01==1),
   #         TotalKnownF = length(Status_01))

# read mortality data 
Mort <- read_excel("B5_Mortality_Data_backbone.xlsx", sheet = "Mortality", na = "NA")  ## Replace here by correct file from corresponding G 
#### get summary table of dead and censored IDs ## Error
Mort_ds <- ddply(Mort, .(VialNumber), summarise, 
                 deadM= sum(DeadMales,na.rm = T),
                 deadF= sum(DeadFemales,na.rm = T),
                 censoredM= sum(CensoredMales,na.rm = T),
                 censoredF= sum(CensoredFemales,na.rm = T),
)
Mort_ds               


#### calculate alive at end  ####
## merge to backbone
backbone <- read_excel("B5_Mortality_Data_backbone.xlsx", 
                       sheet = "backbone", na = "NA") ## Replace here by excel file with correspoding G

merged_mort <- merge(backbone, Mort_ds, by="VialNumber", all.x = TRUE)



head(merged_mort)

# To fix the NA Bug
merged_mort$deadM[is.na(merged_mort$deadM)] =0
merged_mort$deadF[is.na(merged_mort$deadF)] =0
merged_mort$censoredF[is.na(merged_mort$censoredF)] =0
merged_mort$censoredM[is.na(merged_mort$censoredM)] =0


AliveEnd <- ddply(merged_mort, .(VialNumber), summarise, 
                AliveMales = M_Start-(deadM+censoredM),
               AliveFemales = F_Start-(deadF+censoredF))

AliveEnd$VialNumber = as.factor(AliveEnd$VialNumber)

levels(AliveEnd$VialNumber)

### check if all vialnumber are present and otherwise add missing ones
# in the case nobody had ided in those vials 

# Extract levels from the "VialNumber" factor in the backbone dataset
backbone_levels <- levels(as.factor(backbone$VialNumber))

# Create a data frame with all VialNumber values from the backbone levels
all_vials <- data.frame(VialNumber = as.numeric(backbone_levels))

# Merge data frames to identify missing values
merged_Alive <- merge(all_vials, AliveEnd, all.x = TRUE)

# Replace missing values with 10
merged_Alive$AliveMales[is.na(merged_Alive$AliveMales)] <- 10
merged_Alive$AliveFemales[is.na(merged_Alive$AliveFemales)] <- 10

# save file and load file
write.csv(merged_Alive, file = "AliveEnd_B5.csv") ## table with known flies in the end - use this table to see if sex information of censored 

## load dataset with alive IDs in the end
#AliveEnd <- read_excel("BactSysOral_G9.xlsx", sheet = "AliveEnd", na = "NA") ## Replace here by correct file from corresponding G 
#AliveEnd$VialNumber = as.factor(AliveEnd$VialNumber)

### merge dead and alive datasets ### ## ERROR
merge<- merge(merged_Alive,Mort_ds, by.x = "VialNumber", by.y = "VialNumber", all.x=T, all.y=T)

## NA error

merge[is.na(merge)]=0

### calculate new variables
merge_1 <-  merge %>%
  mutate(DeadTotal = rowSums(dplyr::select(., deadM, deadF), na.rm = TRUE),
         AliveTotal = rowSums(dplyr::select(., AliveMales, AliveFemales), na.rm = TRUE),
         CensoredTotal = rowSums(dplyr::select(., censoredM, censoredF), na.rm = TRUE))

merge_12 <-  merge_1 %>%
  mutate(TotalKnown = rowSums(dplyr::select(., DeadTotal, AliveTotal, CensoredTotal), na.rm = TRUE),
         TotalKnownF = rowSums(dplyr::select(., deadF, AliveFemales, censoredF), na.rm = TRUE),
         TotalKnownM = rowSums(dplyr::select(., deadM, AliveMales, censoredM), na.rm = TRUE))

# remove unnecessary columns      
#head(merge_12)
#merge_12_red<- subset(merge_12, select= -c( Date, Hour))

merge_12_red<-merge_12

#### get alive IDs info 
merge_12_red1<- subset(merge_12_red, select= c(VialNumber, deadF ,TotalKnownF))
merge_12_red1$Sex = "F"
merge_12_red1$Unit = paste(merge_12_red1$VialNumber, merge_12_red1$Sex)

#### get summary table of dead and known IDs
overview.alive = merge_12_red1

## loop to calculate alive IDs per day 
line.list = unique(all_data_F$Unit)

# Get alive animals per day ##ERROR - switched CensorF to Censor
new.data = data.frame()
for (i in c(line.list)) {
   #i = "1 F"  ## if you want to see what teh loop does for one of the rows 
  #print(i)
  alive.list  = subset(overview.alive,Unit==i)
  dead.list = subset(overview.dead,Unit==i)
  censor.list = subset(overview.censored,Unit==i)
  BottleBB = data.frame(Unit = c(i), TimeRounded = seq(1,6,1))
  temp = merge(BottleBB, dead.list, by=c("Unit","TimeRounded"),all.x = T)
  temp = merge(temp, censor.list, by=c("Unit","TimeRounded"),all.x = T)
  temp = temp %>% mutate(TotalDeadF = if_else(is.na(TotalDeadF), 0, TotalDeadF))   # if TotalDeadF is na, make it 0 If not, get the value
  temp = temp %>% mutate(Censored = if_else(is.na(Censored), 0, Censored))  # if CensoredF is na, make it 0 If not, get the value
  temp$TotalKnownF = alive.list$TotalKnownF[1]
  temp = within(temp, accumulated_Gone <- cumsum(TotalDeadF + Censored))  # calculate how may flies left the vials - cumulaive time (censored or dead)
  temp$minAliveF.at.Time = temp$TotalKnownF - temp$accumulated_Gone
  new.data = rbind(new.data,temp)
}
length(unique(new.data$Unit))

# see data
head(new.data)  ## this the mortality data summarized ready to be used for fecundity 

## Note: double check that these numbers in 'new.data' are correct 

mergeMort  <- new.data

## prepare merge with fecundity
ncols <- max(stringr::str_count(mergeMort$Unit, " ")) + 1
colmn <- paste("col", 1:ncols)

mergeMort <- mergeMort %>% 
  separate(Unit, sep = " ", into = colmn, remove = FALSE) 

# Rename multiple columns for old to new 
head(mergeMort)
mergeMort_1 <- setnames(mergeMort, old = c('col 1','col 2'), 
                        new = c('VialNumber','Sex'))

mergeMort <- mergeMort %>% 
  separate(Unit, sep = " ", into = colmn, remove = FALSE) 

library(data.table) ## 250 1F
# Rename multiple columns for old to new --- for some reason the dplyr 'rename' fucntion was not working
## so I used datatable instead
mergeMort_1 <- setnames(mergeMort, old = c('col 1','col 2'), 
                        new = c('VialNumber','Sex'))

head(mergeMort_1)

# subset survival data to females only 
mergeMort_1$Sex <- as.factor(mergeMort_1$Sex)
levels(droplevels(mergeMort_1$Sex) )

## create new columns for merge
head(mergeMort_1)
mergeMort_1$VialNumberTime <- paste(mergeMort_1$VialNumber,mergeMort_1$TimeRounded,sep="_")

head(mergeMort_1) ## mortality data ready for merge 


## Remove duplicated columns

mergeMort_1<-subset(mergeMort_1, select = -c(VialNumber, Sex))
names(mergeMort_1)

## add time zero
# from every Unit, add TimeRounded=0, 

# Get alive animals per day 
chuli = data.frame()

for (i in c(unique(mergeMort_1$Unit))) {
  print(i)
  temp = subset(mergeMort_1,Unit == i & TimeRounded == 1)
  temp$TimeRounded = 0
  temp$TotalDeadF = NA
  temp$Censored = NA
  temp$TotalKnownF = 10
  temp$accumulated_Gone = NA
  temp$minAliveF.at.Time = 10
  temp$VialNumberTime = paste0(temp$VialNumber,"_",temp$TimeRounded)
 # temp$ID = paste(temp$VialNumber,temp$TimeRounded)
  chuli = rbind(chuli,temp)
}

## merge data from day 0 to remaining  mortality data
names(mergeMort_1)
names(chuli)

mergeMort_1 <- rbind(mergeMort_1,chuli)

# Quick confirmation plot
head(mergeMort_1)
ggplot(mergeMort_1, aes(as.numeric(TimeRounded), minAliveF.at.Time)) + #geom_violin(adjust=5) +
  stat_summary(aes(), fun = mean, geom="line", size = 0.8, alpha=0.5) +
  stat_summary(aes(), fun.data = "mean_cl_boot", size = 0.8, alpha=1) +
  scale_x_continuous(n.breaks = 8) + theme_bw() + theme(aspect.ratio=0.66)

head(mergeMort_1) ## mortality data ready for merge 

mergeMort_1$ID = paste(mergeMort_1$VialNumber, mergeMort_1$TimeRounded)

#### FECUNDITY ANALYSES ####
fecund <- read_excel("B5_Mortality_Data_backbone.xlsx", 
                            sheet = "Fecundity", na = "NA") ## Replace here by excel file with correspoding G

# FOR GENERATION g12 handle day_1 and day_6
## make data from day 8 zero 
#fecund$Day_1 = NA
#fecund$Day_6 = NA

# restructure dataset to put all days in one column
#names(fecundmerge_r1)
fecundmerge_r1 <- fecund
names(fecundmerge_r1)
head(fecundmerge_r1)
## in G15 there is a problem wIth days names and with teh fact that teh first day only contains zeros,
# so for this g WE CAN NOT LOOK AT DYNAMICS NOR COMPARE ABSOLUTE NUMBERS 

#fecund_gat <- gather(fecundmerge_r1, key="Day", value="pupa", 2:7)  ## Replace here by selection first and last column with pupal counts

##for G15 use this instead
fecund_gat <- gather(fecundmerge_r1, key="Day", value="pupa", 2:6)  ## Replace here by selection first and last column with pupal counts


# Mean number of pupae per tube in experiment - see scheme slide 2
fecund_gat %>% group_by(factor(fecund_gat$Day)) %>% 
  summarise(N.larvae=mean(pupa,na.rm=TRUE))

# duplicate column Day 
fecund_gat$time <- fecund_gat$Day
### remove 'Day_' from column
fecund_gat$time<-gsub("Day_","",as.character(fecund_gat$time))

# duplicate column Day 
fecund_gat$TimeRounded <- fecund_gat$time

# create factor
fecund_gat $time <- as.factor(fecund_gat $time)
levels(fecund_gat $time)

mergePupa <-fecund_gat
levels(mergePupa $time)

### CORRECT THE PUPAE DATE!!!! IF TUBE = day4 then they are laid by FemalesAlive at day3
mergePupa$Time4PupalData <- mergePupa$TimeRounded

mergePupa$Time4PupalData <- as.numeric(mergePupa$Time4PupalData) - 1

head(mergePupa)

# Quick confirmation plot
p <-   ggplot(mergePupa, aes(as.numeric(Time4PupalData), pupa)) + #geom_violin(adjust=5) +
  stat_summary(aes(), fun = mean, geom="line", size = 0.8, alpha=0.5) +
  stat_summary(aes(), fun.data = "mean_cl_boot", size = 0.8, alpha=1) +
  scale_x_continuous(n.breaks = 8) + theme_bw() + theme(aspect.ratio=0.66) +
  xlab("Time (days)")+
  ylab("Number of pupa") +
  ggtitle("Daily fecundity")

p
ggsave("Pupa per day_B5.pdf", width = 12, height = 10, units = "cm")

## prepare merge 
mergePupa$ID = paste(mergePupa$VialNumber, mergePupa$Time4PupalData)

mergeMort_1$ID <- as.factor(mergeMort_1$ID)
mergePupa$ID <- as.factor(mergePupa$ID)



length(unique(levels(mergeMort_1$ID))) 
length(unique(levels(mergePupa$ID))) 


head(mergeMort_1)
head(mergePupa)

FECUNDITY_per_female = merge(mergeMort_1,mergePupa,by="ID", all.x=T, all.y=T)

FECUNDITY_per_female<-FECUNDITY_per_female[FECUNDITY_per_female$Sex!="NA",]

#### merge to backbone

backbone <- read_excel("B5_Mortality_Data_backbone.xlsx", 
                       sheet = "backbone", na = "NA") ## Replace here by excel file with correspoding G

head(FECUNDITY_per_female)
head(backbone)

# remove duplicated columns
names(FECUNDITY_per_female)
#FECUNDITY_per_female<-subset(FECUNDITY_per_female, select=-c("VialNumber.y"))

head(backbone)

FECUNDITY_per_female <- merge(FECUNDITY_per_female, backbone, by.x="VialNumber.x", by.y="VialNumber",
                     all = TRUE)

## remove NAs ## ERROR some rows don't have any estimations
FECUNDITY_per_female <- subset(FECUNDITY_per_female, ID != "NA")
FECUNDITY_per_female <- subset(FECUNDITY_per_female, VialNumber.y != "NA")
FECUNDITY_per_female <- subset(FECUNDITY_per_female, OR != "NA")



# Quick confirmation plot of PUPA per FEMALE ALIVE! ## ERROR Plot doesn't show
head(FECUNDITY_per_female)

p2 <-ggplot(FECUNDITY_per_female, aes(as.numeric(Time4PupalData), 
                                      pupa/minAliveF.at.Time, group=OR, color=OR)) +
  stat_summary(aes(), fun = mean, geom="line", size = 0.7, alpha=0.5) +
  stat_summary(aes(), fun.data = "mean_cl_boot", size = 0.7, alpha=1) +
  scale_x_continuous(n.breaks = 6) + theme_bw() + theme(aspect.ratio=0.66) +
   xlab("Time (days)")+
  ylab("Number of pupa") +
  scale_color_manual(values=c("tomato",  "blue" )) +
  facet_wrap(~ OR) + 
  ggtitle("Mortality-corrected daily fecundity") 
   #+geom_text_repel(data = FECUNDITY_per_female %>% 
   # group_by(OR) %>% 
  #filter(Time4PupalData == max(Time4PupalData)),aes(label = OR),nudge_x = 0.5,size = 3)

or_colors <- c(
  "Or59a" = "#b13a40", "Or56a" = "#b13a40", 
  "Or33c" = "#b13a40", "Or33b" = "#b13a40", 
  "Or65c" = "#b13a40", "Or98a" = "#54828a", 
  "Or22b" = "#54828a", "Or83c" = "#54828a", "Or82a" = "#54828a", 
  "Or85b" = "#54828a", "Or30a" = "#b8713c", "Or94b" = "#b8713c", 
  "Or83b" = "#858e35"
)

p2 + scale_color_manual(values = or_colors)
ggsave("Pupa per day_mort_.pdf", width = 20, height = 15, units = "cm")



######################################################3

p2 <- ggplot(FECUNDITY_per_female, aes(as.numeric(Time4PupalData), 
                                       pupa/minAliveF.at.Time, group = OR, color = OR)) +
  stat_summary(fun = mean, geom = "line", size = 0.7, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", size = 0.7, alpha = 1) +
  scale_x_continuous(labels = c(2, 4, 6, 8, 10)) +  # Adjust x-axis to show specific days
  theme_bw() + 
  theme(
    aspect.ratio = 0.66,  # Reduce gridlines
    panel.grid.minor = element_blank(),
    legend.position = "right",  # Legend appears only once on the side
    legend.title = element_blank(),  # No legend title
    legend.key = element_rect(fill = "white"),  # White background for legend
    axis.text.x = element_text(size = 10),
    strip.background = element_rect(fill = "white", color = "black"),  # Make facet background white (blank) 
    strip.text = element_text(color = "black")# Adjust x-axis text size for clarity
  ) +
  xlab("Time (days)") +
  ylab("Number of pupae") +
  facet_wrap(~ OR, scales = "free_x", strip.position = "top") +  # Move x-axis to bottom
  scale_color_manual(
    values = or_colors,  # Use the predefined colors
    labels = c(
      "Low mortality",  # Single legend entry for blue
      "High mortality",  # Single legend entry for red
      "Larval controls",  # Single legend entry for orange
      "ORCO"  # Single legend entry for green
    ),
    breaks = c("Or22b", "Or59a", "Or30a", "Or83b")  # Map one representative OR to each group
  ) +
  guides(color = guide_legend(override.aes = list(size = 0.5))) # Adjust legend dot size

p2

ggsave("Pupa per day_mort_.pdf", width = 20, height = 15, units = "cm")

library(cowplot)

p2_label<- cowplot::ggdraw() +
  draw_plot(p2) +
  draw_label(
    "B", 
    x = 0.01, y = 0.95,    # Adjust position (x, y in normalized coordinates: 0 to 1)
    hjust = 0, vjust = 1,  # Align left and top
    size = 11, fontface = "bold"
  )
p2_label
ggsave("~/Tese/Escrita/Plots/Fec_curves_label.pdf", p2_label, width = 25, height = 20, units = "cm")
ggsave("~/Tese/Escrita/Plots/Fec_curves_label.png", p2_label, width = 25, height = 20, units = "cm")


#####################################33

## Calculate pupa per female alive 
FECUNDITY_per_female <- mutate(FECUNDITY_per_female, 
                               N.larvae = pupa /minAliveF.at.Time)
head(FECUNDITY_per_female)

### there is a value Inf that needs to be removed 
FECUNDITY_per_female <- subset(FECUNDITY_per_female, N.larvae != "Inf")

## save file and in excel 
FECUNDITY_per_female$Generation <- 5

write.csv(FECUNDITY_per_female, file = "Fecundity_per_female_B5.csv") ## Replace here the correspoding G in the filename  


## save file and in excel 
#write.csv2(FECUNDITY_per_female, file = "Fecundity_per_female_G0.csv") ## Replace here the correspoding G in the filename  
## table with known flies in the end - use this table to see if sex information of censored 

#HERE

# fecundity per line
head(FECUNDITY_per_female)
FECUNDITY_per_line_rep <- ddply(FECUNDITY_per_female, .(OR, VialNumber.x), summarise, 
                                ReproOut=sum(N.larvae, na.rm=TRUE))
FECUNDITY_per_line_rep

## save file and in excel 
write.csv(FECUNDITY_per_line_rep, file = "Fecundity_per_treat_B5.csv")  ## Replace here the correspoding G in the filename  


#### FECUNDITY PLOTS ####

# Change directory to save plots
setwd("~/Tese/ORs/Datasets/Blocks/B5_Fecundity")

FECUNDITY_per_female_ds <- ddply(FECUNDITY_per_female, .( OR, VialNumber.x), summarise, 
                                 N.larvae_sum= sum(N.larvae,na.rm = T))



###Altered from here onwards###

legend_colors <- c(
  "High mortality" = "#b13a40", 
  "Low mortality" = "#54828a", 
  "Larval Control" = "#b8713c", 
  "ORCO" = "#858e35", 
  "Average mortality" = "#4D4D4D"
)

FECUNDITY_per_female_ds$category <- ifelse(
  FECUNDITY_per_female_ds$OR %in% c("Or59a", "Or56a", "Or33c", "Or33b", "Or65c"), "High mortality",
  ifelse(
    FECUNDITY_per_female_ds$OR %in% c("Or98a", "Or22b", "Or83c", "Or82a", "Or85b"), "Low mortality",
    ifelse(
      FECUNDITY_per_female_ds$OR %in% c("Or30a", "Or94b"), "Larval Control",
      ifelse(
        FECUNDITY_per_female_ds$OR == "Or83b", "ORCO", NA
      )
    )
  )
)

# Create the plot with the new legend
f10 <- ggplot(FECUNDITY_per_female_ds, aes(x = OR, y = N.larvae_sum, color = category)) +
  geom_boxplot() +
  geom_jitter() +
  xlab("OR Knockdown Lines") +
  ylab("Total pupal count") +
  scale_color_manual(values = legend_colors) +
  theme_bw() +
  theme(legend.title = element_blank()) + # Remove legend title
  guides(color = guide_legend(override.aes = list(size = 3))) # Adjust legend key size

# Add the average mortality point to the plot
f10 <- f10 + 
  stat_summary(fun.y = mean, geom = "point", size = 1.5, color = "#4D4D4D", aes(shape = "Average pupal count"))

# Customize the legend for the average mortality point
f10 <- f10 + scale_shape_manual(values = 16, name = "") + 
  guides(shape = guide_legend(override.aes = list(size = 3)))

# Display the plot
print(f10)
setwd("~/Tese/Escrita/Plots")

f10_label<- cowplot::ggdraw() +
  draw_plot(f10) +
  draw_label(
    "A", 
    x = 0.01, y = 0.95,    # Adjust position (x, y in normalized coordinates: 0 to 1)
    hjust = 0, vjust = 1,  # Align left and top
    size = 11, fontface = "bold"
  )
f10_label

ggsave("fecundity_Boxplot_label.pdf", f10_label, width = 25, height =15, units = "cm")
ggsave("fecundity_Boxplot_label.png", f10_label, width = 25, height =15, units = "cm")

##########End of Altering#########3


## plot per time point 
f <- ggplot(FECUNDITY_per_female, aes(x=as.factor(Time4PupalData), y=N.larvae, group=OR, color=OR)) + 
  geom_smooth(se = FALSE) +
  geom_point(shape=21) +
  xlab("Time (days after infection)") +
  ylab("Pupal count per alive female (per replicated vial)") +
  scale_color_manual(values=c(or_colors )) +
  #scale_color_manual(values=c( "springgreen2","grey20" )) +
  theme_bw()    

f
ggsave("fecundity_overTime_B5.pdf", width = 30, height =15, units = "cm") # Replace by correspoding G 

## plot per treatment - sum fecundity 
FECUNDITY_per_female_ds <- ddply(FECUNDITY_per_female, .( OR, VialNumber.x), summarise, 
                                      N.larvae_sum= sum(N.larvae,na.rm = T))


f4 <- ggplot(FECUNDITY_per_female_ds, aes(x=OR, y=N.larvae_sum, color=OR)) + 
  geom_boxplot() +
  geom_jitter() +
  xlab("OR Knockdown extreme lines") +
  ylab("Total pupal count") +
  # scale_color_manual(values=c("tomato",  "blue" )) +
  scale_color_manual(values=c( or_colors )) +
  theme_bw() 
f4 +  stat_summary(fun.y=mean, geom="point", size=1.5, color="#4D4D4D") 


ggsave("fecundity_perTreatment_B5.pdf", width = 30, height =15, units = "cm") # Replace by correspoding G 
#ggsave("fecundity_perTreatment_G15.pdf", width = 10, height =8, units = "cm") # Replace by correspoding G 

### stats for fecundity #### 

library(glmmTMB)
# Zero-inflated negative binomial model
FECUNDITY_per_female_ds$N.larvae_sum <- as.integer(FECUNDITY_per_female_ds$N.larvae_sum)

str(FECUNDITY_per_female_ds)

m4<- glmmTMB(N.larvae_sum~OR,
             zi=~OR,
             family=nbinom2(link='log'), FECUNDITY_per_female_ds)
fixef(m4)
summary(m4)
#  the conditional output represents the zero portion (or a logistic regression)
# the zero inflated output represents a "mixture" model of the two distributions 
glmmTMB:::Anova.glmmTMB(m4, type=3,component = "cond")
glmmTMB:::Anova.glmmTMB(m4, type=3,component = "zi")

library(effects)
library(multcomp)
#plot(allEffects(m4))

### stats  
# get pairwise potshoc using emmeans - averaged between sexes 
cld_dat_cond = as.data.frame (cld(emmeans(m4, ~ OR ), 
                                  type="response",component = "cond", Letters = letters))
cld_dat_cond

## emmeans pooled sexes
emmeans_cond <-as.data.frame( emmeans(m4, ~ OR , 
                                      type="response", component = "cond"))
emmeans_cond

## plot 
d <- 0.8

# f2 <- ggplot(FECUNDITY_per_female_info_ds, aes(x=interaction (Treatm_Pop,Treatment), y=N.larvae_sum, color=Treatm_Pop)) + 
library(ggbeeswarm)
str(FECUNDITY_per_female_ds)
r21 <-  ggplot(emmeans_cond, aes(OR, response, col=OR, fill=OR)) +
  geom_errorbar(aes(col=OR, ymin=asymp.LCL, ymax=asymp.UCL), width=0, size=0.75, position = position_dodge(d)) +
  geom_quasirandom(data=FECUNDITY_per_female_ds, aes(y=N.larvae_sum), alpha=0.2, dodge.width = d) +
  #  geom_quasirandom(data = FECUNDITY_per_female_info_ds, aes(y=N.larvae), width=0.1, size=1, alpha=0.2, position = position_dodge(d) ) +
  geom_point(aes(col=OR), size=2, stroke=1, position = position_dodge(d)) +
  #  geom_jitter(data=FECUNDITY_per_female_info_ds, aes(y=N.larvae), alpha=0.2, position = position_dodge(d)) +
#  scale_y_continuous(limits = c(0,60), breaks = seq(0,60,20)) +
  scale_color_manual(values=c( or_colors )) +
  xlab("OR") +
  ylab("emmeans")  +
  theme_bw() + 
  #theme(aspect.ratio = 2) +
  theme(legend.position = "right") 
r21  

ggsave("fecund_B5_emmeans.pdf", width = 12, height = 8, units = "cm")

### over time
FECUNDITY_per_female$N.larvae <- as.integer(FECUNDITY_per_female$N.larvae)
FECUNDITY_per_female$Time4PupalData <- as.integer(FECUNDITY_per_female$Time4PupalData)

m5<- glmmTMB(N.larvae~OR*Time4PupalData ,
             zi=~OR ,
             family=nbinom2(link='log'), FECUNDITY_per_female)
fixef(m5)
summary(m5)
#  the conditional output represents the zero portion (or a logistic regression)
# the zero inflated output represents a "mixture" model of the two distributions 
glmmTMB:::Anova.glmmTMB(m5, type=3,component = "cond")
glmmTMB:::Anova.glmmTMB(m5, type=3,component = "zi")

emmeans_cond <-as.data.frame( emmeans(m5, ~ OR*Time4PupalData , 
                                      type="response",component = "cond"))
emmeans_cond

