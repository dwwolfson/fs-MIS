# Line Plots for Figure 1 in MIS manuscript
library(dplyr)
library(ggplot2)
library(lubridate)
library(data.table)
library(rgdal)
library(gridExtra)

# The line plot should have 3 parts:
#   1) mean/median proportion of county area in an agreement (for counties that are in the SCWDS coverage)
#   2) Proportion of counties with SCWDS coverage for each year
#   3) Total annual take by county (w/ confidence intervals)  [a point for each year, connected by lines]
 
# I'll do number 3 first because that is the easiest and doesn't require SCWDS data

#Import Data
setwd("H:/Projects/MIS_CPUE_model")

#Merge take numbers
take<-read.csv("Data/ptake3.csv")
take<-take[,-1]

take<-take%>%
  group_by(Year, ST_CO)%>%
  mutate(yr.sum=sum(WKR_QTY))

take<-take[order(take$ST_CO, take$Year),]

take1<-take[,c("Year", "ST_CO", "yr.sum")]
take1<-take1[!duplicated(take1),]

#take out 2017 because it's incomplete and will look too small, 
# and years 2004 and 2005 to be consistent with maps
take1<-take1[take1$Year!="2017",]
take1<-take1[take1$Year>"2007",]

plot3<-ggplot(take1, aes(x=Year, y=yr.sum))+
  stat_summary(geom="ribbon", fun.data = mean_cl_normal, 
               fun.args=list(conf.int=0.95), fill="lightblue")+
  stat_summary(geom="line", fun.y=mean, linetype="dashed")+
  stat_summary(geom="point", fun.y = mean, color="red")+
  labs(y="Average annual take per county")+theme_classic()
  
# Looks ok, but it's not possible to add another y-axis in ggplot unless it's a direct transformation 
# of the first y-axis, so I'll have to do it in base to keep in one plot.

##################################################################################################################

# 2) Proportion of counties with SCWDS coverage for each year
# Now for the proportion of counties (in the US, not counting Hawaii and Alaska) that are in SCWDS pig layer

#Import individual shapefiles for each yr
yr08<-readOGR(dsn=path.expand("~/Projects/MIS DB/GIS_layers/SCWDS_county_layers/2008"),
                   layer="2008_counties")
yr09<-readOGR(dsn=path.expand("~/Projects/MIS DB/GIS_layers/SCWDS_county_layers/2009"),
              layer="2009_counties")
yr10<-readOGR(dsn=path.expand("~/Projects/MIS DB/GIS_layers/SCWDS_county_layers/2010"),
              layer="2010_counties")
yr11<-readOGR(dsn=path.expand("~/Projects/MIS DB/GIS_layers/SCWDS_county_layers/2011"),
              layer="2011_counties")
yr12<-readOGR(dsn=path.expand("~/Projects/MIS DB/GIS_layers/SCWDS_county_layers/2012"),
              layer="2012_counties")
yr13<-readOGR(dsn=path.expand("~/Projects/MIS DB/GIS_layers/SCWDS_county_layers/2013"),
              layer="2013_counties")
yr14<-readOGR(dsn=path.expand("~/Projects/MIS DB/GIS_layers/SCWDS_county_layers/2014"),
              layer="2014_counties")
yr15<-readOGR(dsn=path.expand("~/Projects/MIS DB/GIS_layers/SCWDS_county_layers/2015"),
              layer="2015_counties")
yr16<-readOGR(dsn=path.expand("~/Projects/MIS DB/GIS_layers/SCWDS_county_layers/2016"),
              layer="2016_counties")
df08<-yr08@data
df09<-yr09@data
df10<-yr10@data
df11<-yr11@data
df12<-yr12@data
df13<-yr13@data
df14<-yr14@data
df15<-yr15@data
df16<-yr16@data

#Variables to keep
vars<-c("NAME","STATE_NAME", "STATE_FIPS", "CNTY_FIPS", "FIPS", "Feral_SW", "area_sq_km", "area_acres")
# I'll use the number of counties in the lower 48 as 3109 because that's how many there are 
# in the NASS layer when you remove Hawaii and Alaska

df08<-df08[,vars]
df08$Year<-2008
df08$n_counties<-length(unique(paste(df08$STATE_NAME, df08$NAME)))
df08$prop.county<-round(df08$n_counties/3109, 2)

df09<-df09[,vars]
df09$Year<-2009
df09$n_counties<-length(unique(paste(df09$STATE_NAME, df09$NAME)))
df09$prop.county<-round(df09$n_counties/3109, 2)

df10<-df10[,vars]
df10$Year<-2010
df10$n_counties<-length(unique(paste(df10$STATE_NAME, df10$NAME)))
df10$prop.county<-round(df10$n_counties/3109, 2)

df11<-df11[,vars]
df11$Year<-2011
df11$n_counties<-length(unique(paste(df11$STATE_NAME, df11$NAME)))
df11$prop.county<-round(df11$n_counties/3109, 2)

df12<-df12[,vars]
df12$Year<-2012
df12$n_counties<-length(unique(paste(df12$STATE_NAME, df12$NAME)))
df12$prop.county<-round(df12$n_counties/3109, 2)

df13<-df13[,vars]
df13$Year<-2013
df13$n_counties<-length(unique(paste(df13$STATE_NAME, df13$NAME)))
df13$prop.county<-round(df13$n_counties/3109, 2)

df14<-df14[,vars]
df14$Year<-2014
df14$n_counties<-length(unique(paste(df14$STATE_NAME, df14$NAME)))
df14$prop.county<-round(df14$n_counties/3109, 2)

df15<-df15[,vars]
df15$Year<-2015
df15$n_counties<-length(unique(paste(df15$STATE_NAME, df15$NAME)))
df15$prop.county<-round(df15$n_counties/3109, 2)

df16<-df16[,vars]
df16$Year<-2016
df16$n_counties<-length(unique(paste(df16$STATE_NAME, df16$NAME)))
df16$prop.county<-round(df16$n_counties/3109, 2)

df<-rbind(df08, df09, df10, df11, df12, df13, df14, df15, df16)

table(df$Year, df$prop.county)

plot2<-ggplot(df, aes(x=Year, y=prop.county, group=1))+
  geom_line(color="black", size=2)+
  labs(x="Year", y="Proportion of Counties with SCWDS coverage")+
  theme_classic()


#########################################################################################################
# 1) mean/median proportion of county area in an agreement (for counties that are in the SCWDS coverage)

p<-read.csv("Data/from AMY/updated pulls/DataMIS/PropAgreements.csv")
length(unique(paste(p$ST_NAME, p$CNTY_NAME)))
p<-p[!duplicated(p),]
apply(is.na(p),2,sum)

p$ST_CO<-paste(tolower(p$ST_NAME), tolower(p$CNTY_NAME), sep=",")
p<-p[,-c(3,6,7)] #remove some unnecessary columns
#fill in empty end dates
p$start.dt<-as.POSIXct(p$AGRS_START_DATE, format='%d-%b-%y')
p$year.start<-year(p$start.dt)
p$end.dt<-as.POSIXct(p$AGRS_END_DATE, format='%d-%b-%y')
p$year.end<-year(p$end.dt)
p$year.end<-ifelse(is.na(p$year.end), 2017, p$year.end)

#Remove Hawaii, Virgin Islands, Puerto Rico, Guam, and "Test State"
p<-p[p$ST_NAME!="HAWAII",]
p<-p[p$ST_NAME!="GUAM",]
p<-p[p$ST_NAME!="VIRGIN ISLANDS",]
p<-p[p$ST_NAME!="TEST STATE",]
p<-p[p$ST_NAME!="PUERTO RICO",]
p<-droplevels(p)

#Remove records where the agreement end date is earlier than the start date
p$error<-ifelse(p$year.end<p$year.start, 1,0)
table(p$error)  #so 550 records had end dates before the start dates (only about 1% of data), I'll remove them 
p<-p[p$error!=1,]

# Get rows for unique years

# test<-p%>%
#   rowwise()%>%
#   do(data.frame(ID=.$ID, ST_CO=.$ST_CO, year.start=.$year.start,
#                 year.end=.$year.end, years=seq(.$year.start, .$year.end, by=1)))# works but is SLOW

dt.full<-setDT(p)[,list(AGRP_PRP_ID=AGRP_PRP_ID, ID=ID,PRPS_QTY=PRPS_QTY,ST_CO=ST_CO,
                       year.end=year.end,year.start=year.start, 
                       years=seq(year.start, year.end)), by=1:nrow(p)]

# Sum area for each county by year
county.aggs<-dt.full%>%
  group_by(ST_CO, years)%>%
  summarize(sum.acres=sum(PRPS_QTY))

#Add column for county sizes by merging with 
# df$ST_CO<-paste(tolower(df$STATE_NAME), tolower(df$NAME), sep=",")
# county.aggs<-merge(county.aggs, df[,c("ST_CO", "area_acres")], all.x=T)
# apply(is.na(county.aggs),2,sum)
# nas<-county.aggs[is.na(county.aggs$area_acres),]  #some mismatches of county names
# 
# #Deal with mismatches
# vec1<-unique(county.aggs$ST_CO)
# vec2<-unique(df$ST_CO)
# matching<-Reduce(intersect,list(vec1, vec2)) #so only 1080 county names match out of 1413
# 
outersect <- function(x, y) {
  sort(c(setdiff(x, y),
         setdiff(y, x)))
}
# diff<-outersect(vec1, vec2)
# bad1<-intersect(diff, vec1)
# bad2<-intersect(diff, vec2)
# dat<-data.frame(matrix(NA, nrow = 1493, ncol=5))
# dat$MIS<-c(vec1, rep(NA, 80))
# dat$SCWDS<-vec2
# dat$diff<-c(diff, rep(NA, 1493-length(diff)))
# dat$bad1<-c(bad1, rep(NA, 1493-length(bad1)))
# dat$bad2<-c(bad2, rep(NA, 1493-length(bad2)))
# dat<-dat[,-c(1:5)]
# str(dat)
# # write.csv(dat, "county differences b_w MIS and SWCDS.csv")
# 
# length(unique(nas$ST_CO))
# sort(unique(nas$ST_CO))
#Because there are 333 discrepancies, and some of those are from counties that have take, but are
# on the SCWDS layer, I'll instead just use a standard county shapefile in which I calculate the area
# of each county.

counties<-readOGR(dsn="H:/Projects/MIS_CPUE_model/GIS/shapefile with county area info",
              layer="County_LUT")
county.df<-counties@data
names(county.df)
county.df$ST_CO<-paste(tolower(county.df$STATE_NAME), tolower(county.df$NAME), sep=",")


# vec1<-unique(county.aggs$ST_CO)
# vec2<-unique(county.df$ST_CO)
# matching<-Reduce(intersect,list(vec1, vec2)) #so only 1080 county names match out of 1413
# 
# diff<-outersect(vec1, vec2)
# bad1<-intersect(diff, vec1)  #much better, only 38 problems this time
# bad2<-intersect(diff, vec2)
# dat<-data.frame(matrix(NA, nrow = 3143, ncol=5))
# dat$MIS<-c(vec1, rep(NA, (3143-length(vec1))))
# dat$all.counties<-vec2
# dat$diff<-c(diff, rep(NA, 3143-length(diff)))
# dat$bad1<-c(bad1, rep(NA, 3143-length(bad1)))
# dat$bad2<-c(bad2, rep(NA, 3143-length(bad2)))
# dat<-dat[,-c(1:5)]
# str(dat)
# # write.csv(dat, "county differences b_w MIS and county layer.csv")
# merge2<-merge(county.aggs, county.df[,c("ST_CO", "area")], all.x=T)
# apply(is.na(merge2),2,sum)
# nas<-merge2[is.na(merge2$area),]  #some mismatches of county names

# sort(unique(nas$ST_CO))
county.aggs$ST_CO<-recode(county.aggs$ST_CO,
                    "alabama,de kalb"="alabama,dekalb",
                    "alabama,st clair"="alabama,st. clair",
                    "arkansas,st francis"="arkansas,st. francis",
                    "florida,dade"="florida,miami-dade",
                    "florida,de soto"="florida,desoto",
                    "florida,st johns"="florida,st. johns",
                    "illinois,du page"="illinois,dupage",
                    "louisiana,st bernard"="louisiana,st. bernard",
                    "louisiana,st helena"="louisiana,st. helena",
                    "louisiana,st james"="louisiana,st. james",
                    "louisiana,st john the baptist"="louisiana,st. john the baptist",
                    "louisiana,st landry"="louisiana,st. landry",
                    "louisiana,st martin"="louisiana,st. martin",
                    "louisiana,st mary"="louisiana,st. mary",
                    "louisiana,st tammany"="louisiana,st. tammany",
                    "mississippi,tallatchie county"="mississippi,tallahatchie",
                    "new york,st lawrence"="new york,st. lawrence",
                    "tennessee,de kalb"="tennessee,dekalb",
                    "texas,de witt"="texas,dewitt")  #the other mismatches aren't easily fixed(b/c MIS names are bad)

county.aggs<-merge(county.aggs, county.df[,c("ST_CO", "area")], all.x=T)
apply(is.na(merge3),2,sum)
nas<-county.aggs[is.na(county.aggs$area),]  #some mismatches of county names

sort(unique(nas$ST_CO)) #only these 20 weirdos left, that's pretty good

# Divide summed area by total county size
str(county.aggs)
county.aggs$area<-as.numeric(as.character(county.aggs$area))
county.aggs$prop.county<-round(county.aggs$sum.acres/county.aggs$area, 6)
county.aggs<-county.aggs[order(county.aggs$ST_CO, county.aggs$years, decreasing = F),]

#Take out years before 2006, and 2017
county.aggs<-county.aggs[county.aggs$years>2007,]
county.aggs<-county.aggs[county.aggs$years!="2017",]
county.aggs1<-county.aggs  #easier to get back to

# ggplot(county.aggs, aes(x=years, y=prop.county))+
#   stat_summary(geom="ribbon", fun.data = mean_cl_normal, 
#         fun.args=list(conf.int=0.95), fill="lightblue")+
#   stat_summary(geom="line", fun.y=mean, linetype="dashed")+
#   stat_summary(geom="point", fun.y = mean, color="red")  #weird labels for years on x axis

#Make year a factor
county.aggs$years<-as.factor(county.aggs$years)

#Ggplot
plot1<-ggplot(county.aggs, aes(x=years, y=prop.county, group=1))+
  stat_summary(geom="ribbon", fun.data = mean_cl_normal, 
               fun.args=list(conf.int=0.95), fill="lightblue")+
  stat_summary(geom="line", fun.y=mean, linetype="dashed")+
  stat_summary(geom="point", fun.y = mean, color="red")+
  labs(y="Proportion of County with Agreement", x="Year")+
  theme_classic()
  
#combine plots 1 and 2
plot1_and_2<-ggplot(county.aggs, aes(x=years, y=prop.county, group=1))+
  stat_summary(geom="ribbon", fun.data = mean_cl_normal, 
               fun.args=list(conf.int=0.95), fill="lightblue")+
  stat_summary(geom="line", fun.y=mean, linetype="dashed")+
  stat_summary(geom="point", fun.y = mean, color="red")+
  labs(x="Years", y="Proportion of County with Agreement(dashed), Proportion with SCWDS coverage (solid)")+
  geom_line(data = df, aes(x=Year, y=prop.county, group=1), color="black", size=2)

  plot2<-ggplot(df, aes(x=Year, y=prop.county, group=1))+
  geom_line(color="black", size=2)+
  labs( y="Proportion of County with SCWDS coverage", x="Year")
  
  
  
  
grid.arrange(plot1, plot2,plot3)




