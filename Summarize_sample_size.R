library(neonUtilities)
library(readxl)
library(dplyr)

#### Setting up environment and pulling down the data
user<-Sys.getenv("USERNAME")

neon_token<-read.delim(paste0("/home/",user,"/NEON_TOKEN"),header = FALSE)[1,1]
#Set dpID for products of interest
Beetle_dpID<-"DP1.10022.001"

#Pull neon beetle data
neon_df<-neonUtilities::loadByProduct(
  dpID = Beetle_dpID,
  startdate = "2018-01",
  enddate = "2024-12",
  token = neon_token,
  include.provisional = FALSE, 
  check.size = FALSE #Check size flag breaks markdown file, set to FALSE
)

neon_para<-neon_df$bet_parataxonomistID
neon_expert<-neon_df$bet_expertTaxonomistIDProcessed

neon_para_clean <- neon_para %>%
  filter(!(individualID %in% neon_expert$individualID))
# Find common columns between the two data frames
common_cols <- intersect(names(neon_para_clean), names(neon_expert))

# Subset dataframes to the common columns
neon_para_common <- neon_para_clean[, common_cols]
neon_expert_common <- neon_expert[, common_cols]

neon_para_common$ID_status<-"para"
neon_expert_common$ID_status<-"expert"

# Row bind the two dataframes
combined_data <- bind_rows(neon_para_common, neon_expert_common)
getwd()
write.csv("./NEON_Pinned_2018-2024.csv")

#### Summarize all years
combined_data<-subset(combined_data, siteID!="PUUM")
combined_data$year<-substr(combined_data$collectDate, 1,4)
combined_data$tray<-paste0("Y",combined_data$year,"_",combined_data$domainID,"_",combined_data$scientificName,"_",combined_data$ID_status)
#Number of species
dim(table(combined_data$scientificName))
hist(table(combined_data$scientificName), main = NULL, xlab = "Count of Individuals per Species")
sort(table(combined_data$scientificName))

table(table(combined_data$scientificName))
dim(table(table(combined_data$scientificName)))
table(table(combined_data$scientificName))[11:dim(table(table(combined_data$scientificName)))]
sum(table(table(combined_data$scientificName))[11:dim(table(table(combined_data$scientificName)))]) #378 species with greater than 10 individuals
sum(table(table(combined_data$scientificName))) #680 total species
sum(table(table(combined_data$scientificName))[11:dim(table(table(combined_data$scientificName)))])/
  sum(table(table(combined_data$scientificName))) #680 total species


# Count the occurrences of each unique value in the column
value_counts <- table(combined_data[["scientificName"]])
value_counts10<-sort(value_counts)[(dim(value_counts)-sum(table(table(combined_data$scientificName))[11:dim(table(table(combined_data$scientificName)))])):dim(value_counts)]
sp_list<-names(value_counts10)

combined_data_filtered<-filter(combined_data,
                               scientificName %in% sp_list)

#Estimating Trays
dim(table(combined_data$tray)) # 4293 total trays (ignoring the fact that some will have multiple trays)
dim(table(combined_data_filtered$tray)) #3681 total trays from the list of species with greater than 10 individuals
sort(table(combined_data_filtered$tray), decreasing = TRUE)
table(table(combined_data_filtered$tray))
sum(table(table(combined_data_filtered$tray))) #3681 total trays from the list of species with greater than 10 individuals
sum(table(table(combined_data_filtered$tray))[20:dim(table(table(combined_data_filtered$tray)))]) # 1081 trays with 20 or more
total_A_tray<-sum(table(table(combined_data_filtered$tray))[20:dim(table(table(combined_data_filtered$tray)))]) # 1081 trays with 20 or more
total_C_tray<-sum(table(table(combined_data_filtered$tray))[1:5]) # 
total_B_tray<-sum(table(table(combined_data_filtered$tray))[6:19]) # 


table(combined_data_filtered$year)


#### ML Challenge ####
ML_Challenge<-subset(combined_data, domainID=="D01" |
                       domainID=="D02" | domainID=="D03" | 
                       domainID=="D04"  | domainID=="D05" | 
                       domainID=="D06" | domainID=="D07" | 
                       domainID=="D08" | domainID=="D09" | 
                       domainID=="D10" | domainID=="D17")
dim(ML_Challenge)
dim(combined_data)
dim(ML_Challenge)/dim(combined_data)

dim(table(ML_Challenge$tray))
sort(table(ML_Challenge$tray), decreasing = TRUE)

challeneg_species<-names(table(ML_Challenge$scientificName))
table(table(ML_Challenge$scientificName))
sum(table(table(ML_Challenge$scientificName))[10:dim(table(table(ML_Challenge$scientificName)))])

value_counts <- table(ML_Challenge[["scientificName"]])
value_counts10<-sort(value_counts)[(dim(value_counts)-sum(table(table(ML_Challenge$scientificName))[11:dim(table(table(ML_Challenge$scientificName)))])):dim(value_counts)]
ML_sp_list<-names(value_counts10)

write.csv(ML_sp_list, "./ML_sp_list.csv")





dim(table(combined_data$scientificName))
table(combined_data$scientificName)
png("/home/aly/Beetles/figures/beetle_hist.png",width = 5, height = 5, units = "in", res = 100)
hist(table(combined_data$scientificName), main = NULL, xlab = "Count of Individuals per Species")
dev.off()

table(table(combined_data$scientificName))
sum(82,39,35,29,24,21,11,11,11,11)
dim(table(combined_data$scientificName))
(sum(82,39,35,29,24,21,11,11,11,11)/dim(table(combined_data$scientificName)))
(sum(82,39,35)/dim(table(combined_data$scientificName)))
(82/dim(table(combined_data$scientificName)))

table(table(neon_expert$scientificName,neon_expert$siteID))

dim(table(neon_para$siteID))
table(neon_para$siteID)
png("/home/aly/Beetles/figures/beetle_site_hist.png",width = 5, height = 5, units = "in", res = 100)
hist(table(neon_para$siteID), main = NULL, xlab = "Count of Beetles per Site")
dev.off()

#Pull neon beetle data
neon_df<-neonUtilities::loadByProduct(
  dpID = Beetle_dpID,
  startdate = "2018-01",
  enddate = "2020-12",
  token = neon_token,
  include.provisional = FALSE, 
  check.size = FALSE #Check size flag breaks markdown file, set to FALSE
)

neon_para<-neon_df$bet_parataxonomistID
neon_expert<-neon_df$bet_expertTaxonomistIDProcessed

neon_para_clean <- neon_para %>%
  filter(!(individualID %in% neon_expert$individualID))
# Find common columns between the two dataframes
common_cols <- intersect(names(neon_para_clean), names(neon_expert))

# Subset dataframes to the common columns
neon_para_common <- neon_para_clean[, common_cols]
neon_expert_common <- neon_expert[, common_cols]

neon_para_common$ID_status<-"para"
neon_expert_common$ID_status<-"expert"

# Row bind the two dataframes
combined_data <- bind_rows(neon_para_common, neon_expert_common)

head(combined_data)
table(combined_data$taxonRank)
combined_data_clean<-subset(combined_data, taxonRank=="species"| taxonRank=="subspecies")

dim(table(combined_data_clean$scientificName))
table(combined_data_clean$scientificName)
png("/home/aly/Beetles/figures/beetle_hist.png",width = 5, height = 5, units = "in", res = 100)
hist(table(combined_data_clean$scientificName), main = NULL, xlab = "Count of Individuals per Species")
dev.off()

table(table(combined_data_clean$scientificName))
sum(table(table(combined_data_clean$scientificName))[1:9])
dim(table(combined_data$scientificName))
(sum(table(table(combined_data_clean$scientificName))[1:9])/dim(table(combined_data$scientificName)))
(sum(table(table(combined_data_clean$scientificName))[1:3])/dim(table(combined_data$scientificName)))
(sum(table(table(combined_data_clean$scientificName))[1])/dim(table(combined_data$scientificName)))

library(ggplot2)
ggplot(data = combined_data_clean, mapping = aes(x=))

# Create the histogram wrapped by siteID
# Count occurrences of scientificName by siteID
count_data <- combined_data_clean %>%
  group_by(siteID, scientificName) %>%
  summarise(count = n(), .groups = "drop")

# Plot histogram of counts wrapped by siteID
ggplot(count_data, aes(x = count)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  facet_wrap(~ siteID, scales = "free") +
  labs(x = "Count of Scientific Name", y = "Frequency", 
       title = "Histogram of Scientific Name Counts by SiteID") +
  theme_minimal()


table(combined_data_clean$siteID)
table(combined_data_clean$plotID)
table(combined_data_clean$plotID,combined_data_clean$siteID)
