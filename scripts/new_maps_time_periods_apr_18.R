#Import SCWDS shapefiles and merge into three time periods for MIS Figure 1

library(sp)
library(raster)
library(rgeos)
library(spatstat)
library(rgdal)     
library(maptools)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(tidyverse)

#Import Data
setwd("H:/Projects/MIS_CPUE_model")

#Import merged shapefiles from each time period
yrs.12.13<-readOGR(dsn=path.expand("~/Projects/MIS DB/GIS_layers/Merged_counties"),
                   layer="yrs_12_13")
yrs.14.15<-readOGR(dsn=path.expand("~/Projects/MIS DB/GIS_layers/Merged_counties"),
                   layer="yrs_13_14")
yrs.16.17<-readOGR(dsn=path.expand("~/Projects/MIS DB/GIS_layers/Merged_counties"),
                   layer="yrs_15_16")
s1.df<-yrs.12.13@data
s2.df<-yrs.14.15@data
s3.df<-yrs.16.17@data

#Import NASS Ag district shapefile that has the counties and also a unique ID for ag districts spatially joined to it
ag<-readOGR(dsn=path.expand("~/Projects/MIS DB/GIS_layers/Merged_counties"),
            layer="Ag_full")
ag.df<-ag@data

#Merge together SCWDS data and NASS Ag District Data

#2012-2013
s1.df$Feral_SW<-as.numeric(s1.df$Feral_SW)
s1.ag.df<-merge(ag.df, s1.df[,c("FIPS", "Feral_SW")], by="FIPS", all.x=TRUE)
s1.ag.df[is.na(s1.ag.df$Feral_SW),"Feral_SW"]<-0

#for some reason there are duplicates in this db
s1.ag.df<-s1.ag.df[!duplicated(s1.ag.df),]


#2014-2015
s2.df$Feral_SW<-as.numeric(s2.df$Feral_SW)
s2.ag.df<-merge(ag.df, s2.df[,c("FIPS", "Feral_SW")], by="FIPS", all.x=TRUE)
s2.ag.df[is.na(s2.ag.df$Feral_SW),"Feral_SW"]<-0

#2016-2017
s3.df$Feral_SW<-as.numeric(s3.df$Feral_SW)
s3.ag.df<-merge(ag.df, s3.df[,c("FIPS", "Feral_SW")], by="FIPS", all.x=TRUE)
s3.ag.df[is.na(s3.ag.df$Feral_SW),"Feral_SW"]<-0

################################################################################################
#combine with MIS data and calculate averages
ptake<-read_csv("Data/Data_as_of_Jan18/Jan2018Pull/PigKillPropJan2018.csv")
ptake<-ptake[!duplicated(ptake),]
ptake<-ptake[ptake$FATE_FATE=="KILLED",]
ptake$WT_WORK_DATE=as.POSIXct(ptake$WT_WORK_DATE,format="%d-%b-%y")
ptake$Year=format(ptake$WT_WORK_DATE,"%Y")
ptake$ST_CO=paste(tolower(ptake$ST_NAME),",",tolower(ptake$CNTY_NAME),sep = "")
ptake<-ptake[ptake$WKR_INTENTIONAL=="Y",]

#remove unneeded columns from take dataframe
take<-ptake[,c("AGRP_PRP_ID","CMP_NAME", "WKR_QTY","WT_WORK_DATE", "Year", "ST_CO")]
take$Year<-as.integer(take$Year)

#Subset by Year
#2012-2013
df1<-take[take$Year==2012|take$Year==2013,]
df1<-df1[!duplicated(df1),]

#2014-2015
df2<-take[take$Year==2014|take$Year==2015,]
df2<-df2[!duplicated(df2),]

#2016-2017
df3<-take[take$Year==2016|take$Year==2017,]
df3<-df3[!duplicated(df3),]

#Need to aggregate take for each year first because there are a lot of rows
df1<-df1%>%
  group_by(Year, ST_CO)%>%
  mutate(yr.sum=sum(WKR_QTY))

df2<-df2%>%
  group_by(Year, ST_CO)%>%
  mutate(yr.sum=sum(WKR_QTY))

df3<-df3%>%
  group_by(Year, ST_CO)%>%
  mutate(yr.sum=sum(WKR_QTY))

# Add "state,county" variable to NASS/SCWDS layer to merge on (b/c of FIPS issues)
s1.ag.df$ST_CO<-paste(tolower(s1.ag.df$State), tolower(s1.ag.df$County), sep=",")
s2.ag.df$ST_CO<-paste(tolower(s2.ag.df$State), tolower(s2.ag.df$County), sep=",")
s3.ag.df$ST_CO<-paste(tolower(s3.ag.df$State), tolower(s3.ag.df$County), sep=",")

#Now merge
df1<-merge(s1.ag.df, df1[,c("ST_CO", "Year", "yr.sum")], by="ST_CO", all.x=TRUE)
df1<-df1[!duplicated(df1),]
df1[is.na(df1$yr.sum), "yr.sum"]<-0

df2<-merge(s2.ag.df, df2[,c("ST_CO", "Year", "yr.sum")], by="ST_CO", all.x=TRUE)
df2<-df2[!duplicated(df2),]
df2[is.na(df2$yr.sum), "yr.sum"]<-0

df3<-merge(s3.ag.df, df3[,c("ST_CO", "Year", "yr.sum")], by="ST_CO", all.x=TRUE)
df3<-df3[!duplicated(df3),]
df3[is.na(df3$yr.sum), "yr.sum"]<-0

#Calculate an annual average take for each ag district
avgs1<-na.omit(df1)%>%
  group_by(Ag_UID)%>%
  mutate(ag.avg=(sum(yr.sum))/length(unique(Year)))
df1<-merge(df1, avgs1[,c("Ag_UID", "ag.avg")], by="Ag_UID",all.x=T)

avgs2<-na.omit(df2)%>%
  group_by(Ag_UID)%>%
  mutate(ag.avg=(sum(yr.sum))/length(unique(Year)))
df2<-merge(df2, avgs2[,c("Ag_UID", "ag.avg")], by="Ag_UID",all.x=T)

avgs3<-na.omit(df3)%>%
  group_by(Ag_UID)%>%
  mutate(ag.avg=(sum(yr.sum))/length(unique(Year)))
df3<-merge(df3, avgs3[,c("Ag_UID", "ag.avg")], by="Ag_UID",all.x=T)

#take out "year" and "yearly sum" because we already have averages and they clog df with extra rows
df1<-df1[,-c(11,12)]
df2<-df2[,-c(11,12)]
df3<-df3[,-c(11,12)]

#Remove duplicates
df1<-df1[!duplicated(df1),]
df2<-df2[!duplicated(df2),]
df3<-df3[!duplicated(df3),]

#Make NA values into 0's for the average take per ag district
df1[is.na(df1$ag.avg),"ag.avg"]<-0
df2[is.na(df2$ag.avg),"ag.avg"]<-0
df3[is.na(df3$ag.avg),"ag.avg"]<-0

#Switch column name to lower case for merging with ggplot county dataframe
colnames(df1)[2]<-"st_co"
colnames(df2)[2]<-"st_co"
colnames(df3)[2]<-"st_co"

#export
write.csv(df1, "Data/df_for_fig_new_timeframes/df1.csv")
write.csv(df2, "Data/df_for_fig_new_timeframes/df2.csv")
write.csv(df3, "Data/df_for_fig_new_timeframes/df3.csv")

##############################################################################################
##############################################################################################
#Make the maps

#Base map
counties<-map_data("county")
states<-map_data("state")
# county.base<-ggplot(data=counties, mapping=aes(x=long, y=lat, group=group))+coord_fixed(1.3)+
#   geom_polygon(color="black", fill="white")
# p<-county.base+geom_polygon(data=counties, fill=NA, color="white")+geom_polygon(color="black", fill=NA)+
#   theme_void()
# base<-ggplot(data=counties, mapping=aes(x=long, y=lat, group=group))+coord_fixed(1.3)+
#   geom_polygon(color="black", fill="white")+geom_polygon(data=states, color="black", fill=NA)

########################################################################
#Dealing with mismatch between names of counties in the two dataframes
#Delete Alaska and Hawaii
df1<-df1[df1$State!="Hawaii",]
df1<-df1[df1$State!="Alaska",]

#Fix minor mistakes in place names
df1$st_co<-recode(df1$st_co,
                  "alabama,dekalb"="alabama,de kalb",
                  "alabama,st. clair"="alabama,st clair",
                  "arkansas,st. francis"="arkansas,st francis",
                  "district of columbia,district of columbia"="district of columbia,washington",
                  "florida,desoto"="florida,de soto",
                  "florida,st. johns"="florida,st johns",
                  "florida,st. lucie"="florida,st lucie",
                  "georgia,dekalb"="georgia,de kalb",
                  "illinois,dekalb"="illinois,de kalb",
                  "illinois,dupage"="illinois,du page",
                  "illinois,lasalle"="illinois,la salle",
                  "illinois,st. clair"="illinois,st clair",
                  "indiana,dekalb"="indiana,de kalb",
                  "indiana,laporte"="indiana,la porte",
                  "indiana,st. joseph"="indiana,st joseph",
                  "iowa,o'brien"="iowa,obrien",
                  "louisiana,st. bernard"="louisiana,st bernard",
                  "louisiana,st. charles"="louisiana,st charles",
                  "louisiana,st. helena"="louisiana,st helena",
                  "louisiana,st. james"="louisiana,st james",
                  "louisiana,st. john the baptist"="louisiana,st john the baptist",
                  "louisiana,st. landry"="louisiana,st landry",
                  "louisiana,st. martin"="louisiana,st martin",
                  "louisiana,st. mary"="louisiana,st mary",
                  "louisiana,st. tammany"="louisiana,st tammany",
                  "maryland,baltimore county"="maryland,baltimore",
                  "maryland,prince george's"="maryland,prince georges",
                  "maryland,queen anne's"="maryland,queen annes",
                  "maryland,st. mary's"="maryland,st marys",
                  "michigan,st. clair"="michigan,st clair",
                  "michigan,st. joseph"="michigan,st joseph",
                  "minnesota,st. louis"="minnesota,st louis",
                  "mississippi,desoto"="mississippi,de soto",
                  "missouri,dekalb"="missouri,de kalb",
                  "missouri,st. charles"="missouri,st charles",
                  "missouri,st. clair"="missouri,st clair",
                  "missouri,st. francois"="missouri,st francois",
                  "missouri,st. louis city"="missouri,st louis city",
                  "missouri,st. louis county"="missouri,st louis",
                  "missouri,ste. genevieve"="missouri,ste genevieve",
                  "new mexico,doãƒâ±a ana"="new mexico,dona ana",
                  "new york,st. lawrence"="new york,st lawrence",
                  "north dakota,lamoure"="north dakota,la moure",
                  "tennessee,dekalb"="tennessee,de kalb",
                  "texas,dewitt"="texas,de witt",
                  "virginia,bedford county"="virginia,bedford",
                  "virginia,fairfax county"="virginia,fairfax",
                  "virginia,franklin county"="virginia,franklin",
                  "virginia,richmond county"="virginia,richmond",
                  "virginia,roanoke county"="virginia,roanoke",
                  "wisconsin,st. croix"="wisconsin,st croix")





#Merge our data with the ggplot dataframe
counties$st_co<-paste(counties$region, counties$subregion, sep=",")

# The Yellowstone National Park information is being lost for some reason
ynp<-counties[counties$st_co=="montana,yellowstone national",]
ynp$Ag_UID<-169
ynp$Feral_SW<-0
ynp$ag.avg<-0

df1.map<-merge(counties, df1[,c("st_co","Ag_UID","Feral_SW","ag.avg")], by="st_co", all.x=T)
df1.map<-rbind(df1.map, ynp)
df1.map<-na.omit(df1.map)

df1.map<-df1.map[order(df1.map$group, df1.map$order, decreasing=F),]

# I need a way to assign ag districts as having SCWDS pigs (one or more counties, or no pigs)
df1.map<-df1.map%>%
  group_by(Ag_UID)%>%
  mutate(ag.pig.present=ifelse(sum(Feral_SW)==0, 0,1))

#############################################################################################################
# Now actually make the maps

#Map 1 2006-2012
df1.pigs<-df1.map[df1.map$ag.pig.present==1,]
df1.pigtake<-df1.pigs[df1.pigs$ag.avg>0,]

#Calculate quartiles
pos.brks<-round(quantile(df1.pigtake$ag.avg,probs=seq(0,1,.25), na.rm=T),0)

#Flip the values for take without SCWDS presence so that they can be a different color scheme
df1.map$ag.avg<-ifelse(df1.map$ag.pig.present>0, df1.map$ag.avg, df1.map$ag.avg*(-1))
summary(df1.map$ag.avg)  #The lowest value is -1, so we only need 1 interval below zero
sort(unique(df1.map$ag.avg))

#color scheme
color.palette<-c(rev(brewer.pal(3, "Blues")),"#000000", brewer.pal(4, "Reds")) # I actually only need one blue color, 
# but 3 is the minimum input with brewer.pal
mycols<-c("#9ECAE1", "#000000", "#FEE5D9", "#FCAE91", "#FB6A4A", "#CB181D")

# negs<-df1.map[df1.map$ag.avg<0,]   Can check the negative values here

# Better to "hardwire" the color hex values into the dataframe since messing with cut() and 
# scale_fill_manual on the fly is a pain
df1.map$cols<-ifelse(df1.map$ag.avg<0, "#9ECAE1",
                            ifelse(df1.map$ag.avg==0, "#000000",
                                   ifelse(df1.map$ag.avg>=1&df1.map$ag.avg<17, "#FEE5D9",  
                                          ifelse(df1.map$ag.avg>=17&df1.map$ag.avg<43, "#FCAE91",
                                                 ifelse(df1.map$ag.avg>=43&df1.map$ag.avg<195, "#FB6A4A","#CB181D")))))


#Convert color variable to factor
df1.map$cols<-factor(df1.map$cols, levels=c( "#9ECAE1",
                                            "#000000", "#FEE5D9", "#FCAE91", "#FB6A4A", 
                                            "#CB181D"))

## remove background and axis from plot
theme_change <- theme(
  plot.background = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  panel.background = element_blank(),
  panel.border = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_blank()
)


yrs12.13.gg<-ggplot(df1.map, mapping=aes(x=long, y=lat, group=group))+coord_fixed(1.3)+
  geom_polygon(data=df1.map, aes(fill=cols))+ 
  geom_polygon(data=df1.map[df1.map$ag.pig.present==0&df1.map$ag.avg==0,], fill="white")+
  scale_fill_manual(values=mycols,breaks=mycols, 
                    labels=c("1 (No SCWDS coverage)", "0","1 to 16", "17 to 42", "43 to 194", "195 to 6364"),
                    name="Average annual take by ag district")+
  geom_polygon(data=states, color="grey", fill=NA)+
  ggtitle("2012-2013")+theme_change+theme(plot.title=element_text(hjust=0.5))

ggsave("yrs12.13.pdf", width=8, height=12,device="pdf", path="Figures/Fig1_new_timescales")
########################################################################################################################

#Dealing with mismatch between names of counties in the two dataframes
#Delete Alaska and Hawaii
df2<-df2[df1$State!="Hawaii",]
df2<-df2[df1$State!="Alaska",]

#Fix minor mistakes in place names
df2$st_co<-recode(df2$st_co,
                  "alabama,dekalb"="alabama,de kalb",
                  "alabama,st. clair"="alabama,st clair",
                  "arkansas,st. francis"="arkansas,st francis",
                  "district of columbia,district of columbia"="district of columbia,washington",
                  "florida,desoto"="florida,de soto",
                  "florida,st. johns"="florida,st johns",
                  "florida,st. lucie"="florida,st lucie",
                  "georgia,dekalb"="georgia,de kalb",
                  "illinois,dekalb"="illinois,de kalb",
                  "illinois,dupage"="illinois,du page",
                  "illinois,lasalle"="illinois,la salle",
                  "illinois,st. clair"="illinois,st clair",
                  "indiana,dekalb"="indiana,de kalb",
                  "indiana,laporte"="indiana,la porte",
                  "indiana,st. joseph"="indiana,st joseph",
                  "iowa,o'brien"="iowa,obrien",
                  "louisiana,st. bernard"="louisiana,st bernard",
                  "louisiana,st. charles"="louisiana,st charles",
                  "louisiana,st. helena"="louisiana,st helena",
                  "louisiana,st. james"="louisiana,st james",
                  "louisiana,st. john the baptist"="louisiana,st john the baptist",
                  "louisiana,st. landry"="louisiana,st landry",
                  "louisiana,st. martin"="louisiana,st martin",
                  "louisiana,st. mary"="louisiana,st mary",
                  "louisiana,st. tammany"="louisiana,st tammany",
                  "maryland,baltimore county"="maryland,baltimore",
                  "maryland,prince george's"="maryland,prince georges",
                  "maryland,queen anne's"="maryland,queen annes",
                  "maryland,st. mary's"="maryland,st marys",
                  "michigan,st. clair"="michigan,st clair",
                  "michigan,st. joseph"="michigan,st joseph",
                  "minnesota,st. louis"="minnesota,st louis",
                  "mississippi,desoto"="mississippi,de soto",
                  "missouri,dekalb"="missouri,de kalb",
                  "missouri,st. charles"="missouri,st charles",
                  "missouri,st. clair"="missouri,st clair",
                  "missouri,st. francois"="missouri,st francois",
                  "missouri,st. louis city"="missouri,st louis city",
                  "missouri,st. louis county"="missouri,st louis",
                  "missouri,ste. genevieve"="missouri,ste genevieve",
                  "new mexico,doãƒâ±a ana"="new mexico,dona ana",
                  "new york,st. lawrence"="new york,st lawrence",
                  "north dakota,lamoure"="north dakota,la moure",
                  "tennessee,dekalb"="tennessee,de kalb",
                  "texas,dewitt"="texas,de witt",
                  "virginia,bedford county"="virginia,bedford",
                  "virginia,fairfax county"="virginia,fairfax",
                  "virginia,franklin county"="virginia,franklin",
                  "virginia,richmond county"="virginia,richmond",
                  "virginia,roanoke county"="virginia,roanoke",
                  "wisconsin,st. croix"="wisconsin,st croix")





#Merge our data with the ggplot dataframe
# counties$st_co<-paste(counties$region, counties$subregion, sep=",") #already merged from before

# The Yellowstone National Park information is being lost for some reason
# ynp<-counties[counties$st_co=="montana,yellowstone national",]
# ynp$Ag_UID<-169
# ynp$Feral_SW<-0
# ynp$ag.avg<-0      #already done above

df2.map<-merge(counties, df2[,c("st_co","Ag_UID","Feral_SW","ag.avg")], by="st_co", all.x=T)
df2.map<-rbind(df2.map, ynp)
df2.map<-na.omit(df2.map)

df2.map<-df2.map[order(df2.map$group, df2.map$order, decreasing=F),]

# I need a way to assign ag districts as having SCWDS pigs (one or more counties, or no pigs)
df2.map<-df2.map%>%
  group_by(Ag_UID)%>%
  mutate(ag.pig.present=ifelse(sum(Feral_SW)==0, 0,1))


#############################################################################################################
# Now actually make the maps

#Map 2 2014-2015
df2.pigs<-df2.map[df2.map$ag.pig.present==1,]
df2.pigtake<-df2.pigs[df2.pigs$ag.avg>0,]

#Calculate quartiles
pos.brks<-round(quantile(df2.pigtake$ag.avg,probs=seq(0,1,.25), na.rm=T),0)

#Flip the values for take without SCWDS presence so that they can be a different color scheme
df2.map$ag.avg<-ifelse(df2.map$ag.pig.present>0, df2.map$ag.avg, df2.map$ag.avg*(-1))
summary(df2.map$ag.avg)  #The lowest value is -6, so we only need 1 interval below zero
sort(unique(df2.map$ag.avg))

#color scheme
# color.palette<-c(rev(brewer.pal(3, "Blues")),"#000000", brewer.pal(4, "Reds")) # I actually only need one blue color, 
# but 3 is the minimum input with brewer.pal
mycols<-c("#9ECAE1", "#000000", "#FEE5D9", "#FCAE91", "#FB6A4A", "#CB181D")  #this will stay the same as in map1

# negs<-df1.map[df1.map$ag.avg<0,]   Can check the negative values here

# Better to "hardwire" the color hex values into the dataframe since messing with cut() and 
# scale_fill_manual on the fly is a pain
df2.map$cols<-ifelse(df2.map$ag.avg<0, "#9ECAE1",
                     ifelse(df2.map$ag.avg==0, "#000000",
                            ifelse(df2.map$ag.avg>=1&df2.map$ag.avg<20, "#FEE5D9",  
                                   ifelse(df2.map$ag.avg>=20&df2.map$ag.avg<74, "#FCAE91",
                                          ifelse(df2.map$ag.avg>=74&df2.map$ag.avg<178, "#FB6A4A","#CB181D")))))


#Convert color variable to factor
df2.map$cols<-factor(df2.map$cols, levels=c( "#9ECAE1",
                                             "#000000", "#FEE5D9", "#FCAE91", "#FB6A4A", 
                                             "#CB181D"))

## remove background and axis from plot
theme_change <- theme(
  plot.background = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  panel.background = element_blank(),
  panel.border = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_blank()
)


yrs14.15.gg<-ggplot(df2.map, mapping=aes(x=long, y=lat, group=group))+coord_fixed(1.3)+
  geom_polygon(data=df2.map, aes(fill=cols))+ 
  geom_polygon(data=df2.map[df2.map$ag.pig.present==0&df2.map$ag.avg==0,], fill="white")+
  scale_fill_manual(values=mycols,breaks=mycols, 
                    labels=c("1-6 (No SCWDS coverage)", "0","1 to 19", "20 to 73", "74 to 177", "178 to 5910"),
                    name="Average annual take by ag district")+
  geom_polygon(data=states, color="grey", fill=NA)+
  ggtitle("2014-2015")+theme_change+theme(plot.title=element_text(hjust=0.5))

ggsave("yrs14.15.pdf", width=8, height=12,device="pdf", path="Figures/Fig1_new_timescales")


#######################################################################################################
######################################################################################################

#Dealing with mismatch between names of counties in the two dataframes
#Delete Alaska and Hawaii
df3<-df3[df3$State!="Hawaii",]
df3<-df3[df3$State!="Alaska",]

#Fix minor mistakes in place names
df3$st_co<-recode(df3$st_co,
                  "alabama,dekalb"="alabama,de kalb",
                  "alabama,st. clair"="alabama,st clair",
                  "arkansas,st. francis"="arkansas,st francis",
                  "district of columbia,district of columbia"="district of columbia,washington",
                  "florida,desoto"="florida,de soto",
                  "florida,st. johns"="florida,st johns",
                  "florida,st. lucie"="florida,st lucie",
                  "georgia,dekalb"="georgia,de kalb",
                  "illinois,dekalb"="illinois,de kalb",
                  "illinois,dupage"="illinois,du page",
                  "illinois,lasalle"="illinois,la salle",
                  "illinois,st. clair"="illinois,st clair",
                  "indiana,dekalb"="indiana,de kalb",
                  "indiana,laporte"="indiana,la porte",
                  "indiana,st. joseph"="indiana,st joseph",
                  "iowa,o'brien"="iowa,obrien",
                  "louisiana,st. bernard"="louisiana,st bernard",
                  "louisiana,st. charles"="louisiana,st charles",
                  "louisiana,st. helena"="louisiana,st helena",
                  "louisiana,st. james"="louisiana,st james",
                  "louisiana,st. john the baptist"="louisiana,st john the baptist",
                  "louisiana,st. landry"="louisiana,st landry",
                  "louisiana,st. martin"="louisiana,st martin",
                  "louisiana,st. mary"="louisiana,st mary",
                  "louisiana,st. tammany"="louisiana,st tammany",
                  "maryland,baltimore county"="maryland,baltimore",
                  "maryland,prince george's"="maryland,prince georges",
                  "maryland,queen anne's"="maryland,queen annes",
                  "maryland,st. mary's"="maryland,st marys",
                  "michigan,st. clair"="michigan,st clair",
                  "michigan,st. joseph"="michigan,st joseph",
                  "minnesota,st. louis"="minnesota,st louis",
                  "mississippi,desoto"="mississippi,de soto",
                  "missouri,dekalb"="missouri,de kalb",
                  "missouri,st. charles"="missouri,st charles",
                  "missouri,st. clair"="missouri,st clair",
                  "missouri,st. francois"="missouri,st francois",
                  "missouri,st. louis city"="missouri,st louis city",
                  "missouri,st. louis county"="missouri,st louis",
                  "missouri,ste. genevieve"="missouri,ste genevieve",
                  "new mexico,doãƒâ±a ana"="new mexico,dona ana",
                  "new york,st. lawrence"="new york,st lawrence",
                  "north dakota,lamoure"="north dakota,la moure",
                  "tennessee,dekalb"="tennessee,de kalb",
                  "texas,dewitt"="texas,de witt",
                  "virginia,bedford county"="virginia,bedford",
                  "virginia,fairfax county"="virginia,fairfax",
                  "virginia,franklin county"="virginia,franklin",
                  "virginia,richmond county"="virginia,richmond",
                  "virginia,roanoke county"="virginia,roanoke",
                  "wisconsin,st. croix"="wisconsin,st croix")





#Merge our data with the ggplot dataframe
# counties$st_co<-paste(counties$region, counties$subregion, sep=",") #already merged from before

# The Yellowstone National Park information is being lost for some reason
# ynp<-counties[counties$st_co=="montana,yellowstone national",]
# ynp$Ag_UID<-169
# ynp$Feral_SW<-0
# ynp$ag.avg<-0      #already done above

df3.map<-merge(counties, df3[,c("st_co","Ag_UID","Feral_SW","ag.avg")], by="st_co", all.x=T)
df3.map<-rbind(df3.map, ynp)
df3.map<-na.omit(df3.map)

df3.map<-df3.map[order(df3.map$group, df3.map$order, decreasing=F),]

# I need a way to assign ag districts as having SCWDS pigs (one or more counties, or no pigs)
df3.map<-df3.map%>%
  group_by(Ag_UID)%>%
  mutate(ag.pig.present=ifelse(sum(Feral_SW)==0, 0,1))


#############################################################################################################
# Now actually make the maps

#Map 3 2016-2017
df3.pigs<-df3.map[df3.map$ag.pig.present==1,]
df3.pigtake<-df3.pigs[df3.pigs$ag.avg>0,]

#Calculate quartiles
pos.brks<-round(quantile(df3.pigtake$ag.avg,probs=seq(0,1,.25), na.rm=T),0)

#Flip the values for take without SCWDS presence so that they can be a different color scheme
df3.map$ag.avg<-ifelse(df3.map$ag.pig.present>0, df3.map$ag.avg, df3.map$ag.avg*(-1))
summary(df3.map$ag.avg)  #The lowest value is -839 (from Heavy Horns Ranch in Wisconsin in 2017)
sort(unique(df3.map$ag.avg))

#color scheme
color.palette<-c(rev(brewer.pal(4, "Blues")),"#000000", brewer.pal(4, "Reds")) # I actually only need one blue color,
# but 3 is the minimum input with brewer.pal
mycols<-c("#2171B5",  "#9ECAE1","#000000", "#FEE5D9", "#FCAE91", "#FB6A4A", "#CB181D" )

# negs<-df1.map[df1.map$ag.avg<0,]   Can check the negative values here

# Better to "hardwire" the color hex values into the dataframe since messing with cut() and 
# scale_fill_manual on the fly is a pain
df3.map$cols<-ifelse(df3.map$ag.avg<(-177), "#2171B5",
                     ifelse(df3.map$ag.avg>(-177)&df3.map$ag.avg<0,"#9ECAE1",
                     ifelse(df3.map$ag.avg==0, "#000000",
                            ifelse(df3.map$ag.avg>=1&df3.map$ag.avg<23, "#FEE5D9",  
                                   ifelse(df3.map$ag.avg>=23&df3.map$ag.avg<85, "#FCAE91",
                                          ifelse(df3.map$ag.avg>=85&df3.map$ag.avg<208, "#FB6A4A","#CB181D"))))))


#Convert color variable to factor
df3.map$cols<-factor(df3.map$cols, levels=c("#2171B5",  "#9ECAE1",
                                             "#000000", "#FEE5D9", "#FCAE91", "#FB6A4A", 
                                             "#CB181D"))

## remove background and axis from plot
theme_change <- theme(
  plot.background = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  panel.background = element_blank(),
  panel.border = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_blank()
)


yrs16.17.gg<-ggplot(df3.map, mapping=aes(x=long, y=lat, group=group))+coord_fixed(1.3)+
  geom_polygon(data=df3.map, aes(fill=cols))+ 
  geom_polygon(data=df3.map[df3.map$ag.pig.present==0&df3.map$ag.avg==0,], fill="white")+
  scale_fill_manual(values=mycols,breaks=mycols, 
                    labels=c("178 to 5910 (No SCWDS coverage)",
                             "1 to 19 (No SCWDS coverage)",
                             "0","1 to 19", "20 to 79", "80 to 348", "349 to 5780"),
                    name="Average annual take by ag district")+
  geom_polygon(data=states, color="grey", fill=NA)+
  ggtitle("2016-2017")+theme_change+theme(plot.title=element_text(hjust=0.5))

ggsave("yrs16.17.pdf", width=8, height=12,device="pdf", path="Figures/Fig1_new_timescales")














