getwd()
setwd("./Hawaii/Beetles")
df<-read.csv("./BeetleMeasurements.csv")
meta<-read.csv("./NEON-SiteMap-Table.csv")

df<-merge(df, meta, by.x="siteID", by.y="siteCode", all.x=TRUE)

head(df)

df_sub<-subset(df, user_name=="IsaFluck")

head(df_sub)
str(df_sub)

ElytraLength<-subset(df_sub, structure=="ElytraLength")

hist(ElytraLength$dist_cm)

library(ggplot2)

site_spp<-table(ElytraLength$species, ElytraLength$siteID)
site_spp<-as.data.frame(site_spp)

ElytraLength<-subset(ElytraLength, siteID!="DELA" & siteID!="DSNY")

# Get unique siteID levels ordered by latitude
site_order <- unique(ElytraLength[order(ElytraLength$latitude, decreasing = TRUE), "siteID"])

# Convert siteID into a factor with the correct order
ElytraLength$siteID <- factor(ElytraLength$siteID, levels = site_order)

ggplot(data = ElytraLength, aes(x=dist_cm, fill = scientificName)) +
  geom_density(alpha=0.5) +
  theme(legend.position="none") +
  facet_wrap(.~siteID, scales = "free_y")
