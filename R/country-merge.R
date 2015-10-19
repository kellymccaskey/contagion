# to generate the data file by kelly mccaskey

# clear working directory
rm(list = ls())

# set working directory
setwd("~/Dropbox/projects/contagion/")

# load packages 
library(countrycode)

# load PRIO v. 4
prio <- read.csv("data/clean-data/UCDPPrioArmedConflictDataset4a-2014.csv")

# rename GWNoLoc cow country code 
# ccode1 is country j (original war)
# ccode2 is country i (contagion war)
prio$ccode1 <- prio$GWNoLoc

# load COW trade v 3.0
cowtrade <- read.csv("data/clean-data/24385-0001-Data-DataProducer.csv")

# rename variables to merge with prio
cowtrade$ccode1 <- cowtrade$CCODE1
cowtrade$ccode2 <- cowtrade$CCODE2
cowtrade$Year <- cowtrade$YEAR

# merge COW trade and PRIO on the trade in country j (original war) and year
cowtrade_prio <- merge(prio, cowtrade, by = c("ccode1", "Year"), all.x = TRUE, all.y = TRUE)

# code TypeOfConflict = NA to TypeOfConflict = 0
cowtrade_prio$TypeOfConflict[is.na(cowtrade_prio$TypeOfConflict)] <- 0

# load COW contiguity v 4
contig <- read.csv("data/clean-data/contdird.csv")

# rename COW contiguity for merging
# ccode1 is country j (original war)
# ccode2 is country i (possible contagion war)
contig$ccode1 <- contig$state1no
contig$ccode2 <- contig$state2no
contig$Year <- contig$year

# merge COW contiguity on trading partners 
# (country j with original war, country i with possible contagion)
cowtrade_prio_contig <- merge(cowtrade_prio, contig, by = c("ccode1", "ccode2", "Year"), all.x = TRUE, all.y = TRUE)

# create lag for contagion in country i and prepare for merge
prio$ccode2 <- prio$GWNoLoc
prio$TypeOfConflict_i <- prio$TypeOfConflict
cowtrade_prio_contig$year_lag <- cowtrade_prio_contig$Year + 1
prio$year_lag <- prio$Year

# merge on lag
cowtrade_prio_contig_prio <- merge(cowtrade_prio_contig, prio, by = c("ccode2", "year_lag"), all.x = TRUE, all.y = TRUE)
cowtrade_prio_contig_prio$TypeOfConflict_i[is.na(cowtrade_prio_contig_prio$TypeOfConflict_i)] <- 0

# clean variables delete all but civil wars/no wars
data <- subset(cowtrade_prio_contig_prio, TypeOfConflict_i >= 3 & TypeOfConflict.x >= 3)
data$TypeOfConflict_j <- data$TypeOfConflict.x
d <- data[c(1:12, 14:52, 81:82)]
d$ccode1 <- d$ccode1.x
d$Year <- d$Year.x
d$ConflictId <- d$ConflictId.x
d$Location <- d$Location.x
d$SideA <- d$SideA.x
d$SideB <- d$SideB.x
d$SideA2nd <- d$SideA2nd.x
d$SideB2nd.x <- d$SideB2nd.x
d$SideBID <- d$SideBID.x
d$Incompatibility <- d$Incompatibility.x
d$IntensityLevel <- d$IntensityLevel.x
d$CumulativeIntensity <- d$CumulativeIntensity.x
d$StartDate <- d$StartDate.x
d$StartPrec <- d$StartPrec.x
d$StartDate2 <- d$StartDate2.x
d$StartPrec2 <- d$StartPrec2.x
d$EpEnd <- d$EpEnd.x
d$EpEndDate <- d$EpEndDate.x
d$EpEndPrec <- d$EpEndPrec.x
d$GWNoA <- d$GWNoA.x
d$GWNoA2nd <- d$GWNoA2nd.x
d$GWNoB <- d$GWNoB.x
d$GWNoB2nd <- d$GWNoB2nd.x
d$GWNoLoc <- d$GWNoLoc.x
d$Region <- d$Region.x
d$Version_prio <- d$Version.x
d$version_cowcontig <- d$version
d$version_cowtrade <- d$VERSION

contagion_totaltrade <- d[c(1:2, 30:80)]

# write to csv
write.csv(contagion_totaltrade, "data/contagion_totaltrade.csv")

# # load total trade data merge with rest
# # add year lag to accomodate time to war
# t <- read.csv("data/clean-data/national_trade_3.0.csv")
# t$ccode2 <- t$ccode
# trade <- merge(contig_merged, t, by = c("ccode2", "year"), all.x = TRUE)
# # trade$year_lag <- trade$year + 1
# # trade_clean <- subset(trade, trade$conttype == c(1, 2, 3))
# 
# # remerge PRIO with trade to include civil war contagion
# d$ccode2 <- countrycode(d$Location, "country.name", "cown", warn = TRUE)
# # d$year_lag <- d$Year
# contagion_merged <- merge(trade, d, by = c("ccode2"), all.x = TRUE)
# 
# # save as .csv
# # write.csv(contagion_merged, "data/civil-trade-contig-merged.csv")
# 
# # clean data to exclude distances with more than 24 miles of water and non-civil conflicts
# # clean <- subset(contagion_merged, contagion_merged$conttype == c(1, 2, 3) & contagion_merged$TypeOfConflict.y == 3)
# 
# # rename variables
# contagion_merged$ccode1_conflict <- ifelse(contagion_merged$TypeOfConflict.x == 3, 1, 0)
# contagion_merged$ccode2_conflict <- ifelse(contagion_merged$TypeOfConflict.y == 3, 1, 0)
# 
# # remove NAs to reduce file size
# contagion_merged_na <- na.omit(contagion_merged)
# 
# # merge Gleditsch expanded trade and GDP data (v 6.0 - 2014)
# s <- read.table("data/clean-data/gdpv6.txt", header = TRUE)
# s$year_lag <- s$year
# s$ccode2 <- s$statenum
# merged2 <- merge(s, contagion_merged_na, by = c("ccode2", "year_lag"))
# 
# # update 2005 USD to 2013 USD
# merged2$realgdp2013 <- merged2$realgdp*(195.3/232.96)
# merged2$rgdppc2013 <- merged2$rgdppc*(195.3/232.96)
# 
# # load data
# # d <- read.csv("data/data-nogdp.csv")
# # r <- read.csv("data/data-gdp.csv")
# # data <- read.csv("data/data-contagion-merged.csv")
# # nmc <- read.csv("data/clean-data/NMC_v4_0.csv")
# # nmc$ccode2 <- nmc$ccode
# # nmc$Year <- nmc$year
# # d <- merge(nmc, data, by = c("ccode2", "year"))
# # r <- read.csv("data/clean-data/AhlquistWibbelsRidingTheWaveAJPSdata.csv")
# # r$ccode2 <- r$CoW.Polity.number
# # s <- merge(r, d, by = c("ccode2", "year"))
# # write.csv(s, "data/merged-2.csv")
# 
# # save as csv
# # write.csv(merged2, "data/data-1.csv")
# write.csv(contagion_merged, "data/data-contagion-merged.csv")
# write.csv(contagion_merged_na, "data/data-contagion-merged-na.csv")
