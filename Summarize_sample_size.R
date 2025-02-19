library(neonUtilities)
library(readxl)
library(dplyr)

neon_token<-read.delim("/home/aly/NEON_TOKEN",header = FALSE)[1,1]
#Set dpID for products of interest
Beetle_dpID<-"DP1.10022.001"

#Pull neon beetle data
neon_df<-neonUtilities::loadByProduct(
  dpID = Beetle_dpID,
  startdate = "2018-01",
  enddate = "2018-12",
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

png("/home/aly/Beetles/figures/beetle_spp_site_hist.png",width = 14, height = 6, units = "in", res = 100)
# Plot histogram of counts wrapped by siteID
ggplot(count_data, aes(x = count)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  facet_wrap(~ siteID, scales = "free") +
  labs(x = "Count of Scientific Name", y = "Frequency", 
       title = "Histogram of Scientific Name Counts by SiteID") +
  theme_minimal()
dev.off()


table(combined_data_clean$siteID)
table(combined_data_clean$plotID)
table(combined_data_clean$plotID,combined_data_clean$siteID)
