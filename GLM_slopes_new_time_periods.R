# GLMS on figure 1 map data
#restart R to get clean environment

#import libararies
library(tidyverse)

#import datasets
ptake<-read_csv("Data/Data_as_of_Jan18/Jan2018Pull/PigKillPropJan2018.csv")
ptake<-ptake[!duplicated(ptake),]
ptake<-ptake[ptake$FATE_FATE=="KILLED",]
ptake$WT_WORK_DATE=as.POSIXct(ptake$WT_WORK_DATE,format="%d-%b-%y")
ptake$Year=format(ptake$WT_WORK_DATE,"%Y")
ptake$ST_CO=paste(tolower(ptake$ST_NAME),",",tolower(ptake$CNTY_NAME),sep = "")
ptake<-ptake[ptake$WKR_INTENTIONAL=="Y",]

#remove unneeded columns from take dataframe
take<-ptake[,c("ST_GSA_STATE_CD","ST_NAME","AGRP_PRP_ID","CMP_NAME", "WKR_QTY","WT_WORK_DATE", "Year", "ST_CO")]
take$Year<-as.integer(take$Year)
df<-take
########################################################################################
#For county-level estimates
#Subset by Year
#2012-2013
df1<-df[df$Year==2012|df$Year==2013,]
df1<-df1[!duplicated(df1),]

#2014-2015
df2<-df[df$Year==2014|df$Year==2015,]
df2<-df2[!duplicated(df2),]

#2016-2017
df3<-df[df$Year==2016|df$Year==2017,]
df3<-df3[!duplicated(df3),]

#Calculate annual take by county
df1<-df1%>%
  group_by(Year, ST_CO)%>%
  mutate(yr.sum=sum(WKR_QTY))

df2<-df2%>%
  group_by(Year, ST_CO)%>%
  mutate(yr.sum=sum(WKR_QTY))

df3<-df3%>%
  group_by(Year, ST_CO)%>%
  mutate(yr.sum=sum(WKR_QTY))

#Calculate mean annual take by period
df1<-df1%>%
  group_by(ST_CO)%>%
  mutate(avg.take=sum(unique(yr.sum))/length(unique(Year)))

df2<-df2%>%
  group_by(ST_CO)%>%
  mutate(avg.take=sum(unique(yr.sum))/length(unique(Year)))

df3<-df3%>%
  group_by(ST_CO)%>%
  mutate(avg.take=sum(unique(yr.sum))/length(unique(Year)))

#Combine datasets
df1$period<-1
df2$period<-2
df3$period<-3
dat<-rbind(df1, df2, df3)

#Aggregate data
dat<-dat[!duplicated(dat[,c("ST_NAME","ST_CO","avg.take","period")]),]
dat<-dat[,c("ST_NAME", "ST_CO", "avg.take", "period")]
colnames(dat)[3]<-"take"

#Take out zero's, they must be mistakes
dat<-dat[dat$take!=0,]  #only a few records
dat<-droplevels(dat)

#Fix a weird county name 
dat[dat$ST_CO=="nevada,humboldt"&dat$period==1,"take"]<-12
dat[dat$ST_CO=="nevada,humboldt"&dat$period==2,"take"]<-15
nev<-data.frame(ST_NAME="NEVADA", ST_CO="nevada,humboldt", take=1, period=3)
dat<-as.data.frame(dat)
dat<-rbind(dat, nev)
dat<-dat[!dat$ST_CO=="nevada,humboldt (e)",]

#Remove states that shouldn't be there
dat<-dat[!dat$ST_NAME=="GUAM",]
dat<-dat[!dat$ST_NAME=="PUERTO RICO",]
dat<-dat[!dat$ST_NAME=="VIRGIN ISLANDS",]
dat<-dat[!dat$ST_NAME=="TEST STATE",]
dat<-dat[!dat$ST_NAME=="HAWAII",]
dat<-droplevels(dat)

#Also remove states that don't have observations over multiple periods 
# (since we want to see the change over time), this will also conserve d.f. and 
# remove state with scant data that may have spurious relationships
dat<-dat[!dat$ST_NAME=="MARYLAND",]   #only one observation
dat<-dat[!dat$ST_NAME=="NEBRASKA",]    #two obs from only period 1
dat<-dat[!dat$ST_NAME=="WASHINGTON",]  #only 3 obs from period 3
dat<-dat[!dat$ST_NAME=="WISCONSIN",]   #only one obs from period 3
dat<-droplevels(dat)

# #I'm going to drop states that have less than 10 unique records
dat1<-dat%>%
  group_by(ST_NAME)%>%
  filter(n()>9)
dat1<-droplevels(dat1)  #only 20 states left

#get the log of take
dat1$log.take<-log(dat1$take)

mod1<-lm(log.take~period*ST_NAME, data=dat1)
summary(mod1)
coefs<-mod1$coefficients
mat<-matrix(nrow=40, ncol=4)
mat[1:40,1]<-variable.names(mod1)
mat[1:40,2]<-mod1$coefficients
mat[1:40, 3:4]<-confint(mod1)
baseline<-as.numeric(mat[2,2])

cv.st<-summary(mod1)$cov.unscaled

betas=data.frame(State=sort(as.character(unique(dat1$ST_NAME))),
                 Slope=0,Variance=0,Lower=0,Upper=0)
betas[1,c(2,3)]=c(as.numeric(mat[2,2]),cv.st[2,2])

for(i in 2:20){
  betas[i, 2]<-as.numeric(mat[(i+20),2])+baseline
  betas[i,3]<-cv.st[2,2]+cv.st[i+20,i+20]+2*cv.st[2,i+20]
}


betas$Lower=betas$Slope-1.96*sqrt(betas$Variance)
betas$Upper=betas$Slope+1.96*sqrt(betas$Variance)

# Add state classification category
betas$class<-c(4,#AL
               4,#Arkansa
               5,#CA
               5,#FL
               4,#Georgia
               2,#Kansas
               3,#Kentucky
               4,#Louisiana
               4,#Mississippi
               3,#Missouri
               2,#New Mexixo
               4,#North Carolina
               2,#Ohio
               4,#Oklahoma
               2,#Oregon
               4,#South Carolina
               3,#Tennessee
               5,#Texas
               3,#Virginia
               2)#West Virginia)
betas$class<-as.factor(betas$class)

# Reorder states by classification category (and by slope withing each class)
betas$State<-factor(betas$State, levels=betas$State[order(betas$class,decreasing = T, betas$Slope)])

ggplot(betas, aes(x=State, y=Slope, color=class))+
  geom_point(size=4)+geom_errorbar(aes(ymin=Lower, ymax=Upper), size=1.2)+
  xlim(rev(levels(betas$State)))+
  geom_hline(yintercept=(0))+coord_flip()+
  theme(axis.text = element_text(size=12,face="bold"),
        axis.title=element_text(size=14, face="bold"))+
  labs(col="Management\nClassification")
        
#Add state Fips to betas df
betas$fips<-c(1,#AL
               5,#Arkansa
               6,#CA
               12,#FL
               13,#Georgia
               20,#Kansas
               21,#Kentucky
               22,#Louisiana
               28,#Mississippi
               29,#Missouri
               35,#New Mexixo
               37,#North Carolina
               39,#Ohio
               40,#Oklahoma
               41,#Oregon
               45,#South Carolina
               47,#Tennessee
               48,#Texas
               51,#Virginia
               54)#West Virginia)
write.csv(betas, "output/slopes_from_glm/betas.csv")
