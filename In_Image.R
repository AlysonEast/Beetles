library(neonUtilities)
library(dplyr)

neon_token<-read.delim("/home/alysoneast/NEON_TOKEN",header = FALSE)[1,1]
#Set dpID for products of interest
Beetle_dpID<-"DP1.10022.001"

#Pull neon beetle data
neon_df<-neonUtilities::loadByProduct(
  dpID = Beetle_dpID,
  startdate = "2017-01",
  enddate = "2017-12",
  token = neon_token,
  include.provisional = FALSE, 
  check.size = FALSE #Check size flag breaks markdown file, set to FALSE
)

#Pull out Para and expert dataframes
neon_para<-neon_df$bet_parataxonomistID
neon_expert<-neon_df$bet_expertTaxonomistIDProcessed

#filter out parataxonomist IDs that were later re-IDed by experts
neon_para_clean <- neon_para %>%
  filter(!(individualID %in% neon_expert$individualID))
# Find common columns between the two dataframes
common_cols <- intersect(names(neon_para_clean), names(neon_expert))

# Subset dataframes to the common columns
neon_para_common <- neon_para_clean[, common_cols]
neon_expert_common <- neon_expert[, common_cols]

#Add a column to specify the identifier type
neon_para_common$ID_status<-"para"
neon_expert_common$ID_status<-"expert"

# Row bind the two dataframes to create on harmonized reference
combined_data <- bind_rows(neon_para_common, neon_expert_common)
head(combined_data)
str(combined_data)
#Find beetles from image####
#filter by domain and species
in_image_possible<-subset(combined_data, domainID=="D07" &
                 scientificName=="Pasimachus punctulatus")

head(in_image_possible)
dim(in_image_possible)

in_image_possible$numberic_id<-substr(in_image_possible$individualID, 
                             (nchar(in_image_possible$individualID)-5), nchar(in_image_possible$individualID))
in_image_possible$numberic_id
in_image_possible$numberic_id<-as.numeric(in_image_possible$numberic_id)

#Range in image is 001590 - 002088
in_image<-subset(in_image_possible, numberic_id>=1590 & numberic_id<=2088)
dim(in_image)
table(in_image$ID_status)
