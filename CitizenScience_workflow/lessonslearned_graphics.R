# TODO: Add comment
# 
# Author: lsalas
###############################################################################


library(airtabler)
library(ggplot2)
library(plyr)
library(dplyr)

Sys.setenv(AIRTABLE_API_KEY="keyrGM8esxsQNc5aP")

project_management <- airtable(base = "appqawY2glKmrbSzr", table = c("Recordings","CS Hours Tracking"))
rec_at <- project_management$Recordings$select_all(view="viwE3U8yqHsMnwWW6")
cshours_at <- project_management$"CS Hours Tracking"$select_all(view="viwnZhxkNVnBPzZ9s")
cshours_at$Type<-gsub("ABG","General sound labeling",cshours_at$Type)
cshours_at$Type<-gsub("Arbimon","Bird call labeling",cshours_at$Type)

################################################
## Leo code begins here...

csh<-cshours_at
csh<-csh[,which(!names(csh) %in% c("id","createdTime"))]
names(csh)<-c("Date","Hours","Inits","Tasks","Type","Name","UserId","VolClass","VolStatus")
csh$UserId<-as.character(csh$UserId)
csh$Name<-as.character(csh$Name)

# combine student-volunteer and volunteer into one category
# removing dupes (there are seven)
csh$key<-paste0(csh$Date,"::",csh$Hours,"::",csh$Tasks,"::",csh$Type,"::",csh$Name,"::",csh$UserId,"::",csh$VolStatus)
cshdf<-ldply(unique(csh$key),function(kk,csh){
			tdf<-subset(csh,key==kk)
			return(tdf[1,])
		},csh=csh)
cshdf<-cshdf[,which(names(cshdf) != "key")]
cshdf$Name<-as.character(cshdf$Name)

# we care about several aggregates from cshdf table...
# but first adding a "skillLevel" variable...
skilldf<-data.frame(Type=sort(unique(cshdf$Type)),SkillLevel=c("Cloud-based/Remote activity","Specialized activity",
				"Field-based activity","Cloud-based/Remote activity","Specialized activity","Field-based activity","Specialized activity","Specialized activity"),AlphOrder=c(2,3,1,2,3,1,3,3))

plotdf<-aggregate(Hours~Type+UserId+Name,cshdf,sum)
plotdf$ParticipantType<-ifelse(plotdf$UserId=="Student-credit","Student","Volunteer")
plotdf<-merge(plotdf,skilldf,by="Type",all.x=TRUE)
plotdf$Type<-gsub("General sound labeling","Soundscape Components",plotdf$Type)
plotdf$Type<-gsub("Bird call labeling","Bird Call Labeling",plotdf$Type)
numvols<-aggregate(ParticipantType~Type,plotdf,NROW); names(numvols)<-gsub("ParticipantType","NumVols",names(numvols))
numvols$NumVols<-paste0("(N=",numvols$NumVols,")");numvols$y<-6
tskl<-unique(plotdf[,c("SkillLevel","Type")])
numvols<-merge(numvols,tskl,by="Type",all.x=T)

# Volunteer hours by activity --
# there is only one value for social media - it will be dropped if using violin plots, so...
plotdf$Type<-as.factor(plotdf$Type); plotdf$Type<-reorder(plotdf$Type,plotdf$AlphOrder)
pp<-ggplot(plotdf,aes(x=Type,y=log(Hours))) + geom_boxplot(aes(fill=ParticipantType)) + coord_flip() + theme_bw() + 
		labs(x="Task Type",y="Log(Number of Hours)",fill="Citizen Scientist Type") + facet_wrap(~SkillLevel,ncol=3)
pp + geom_text(data=numvols, mapping=aes(x=Type, y=y, label=NumVols),vjust=0.6,hjust=0.7,size=3) + 
		scale_fill_manual(values=c("#FF5733","#11AF16"))



# Number of participants by activity --
numvols<-aggregate(ParticipantType~Type,plotdf,NROW); names(numvols)<-gsub("ParticipantType","NumVols",names(numvols))
numvols<-merge(numvols,tskl,by="Type",all.x=T)
numvols$Type<-as.factor(numvols$Type)
ggplot(numvols,aes(x=Type,y=NumVols)) + geom_bar(stat="identity",aes(fill=SkillLevel)) + coord_flip() + theme_bw() + 
		labs(x="Task Type",y="Number of Volunteers",fill="Participant type")

# Average and range of number of tasks by participant
ntasksd<-aggregate(Type~Name,data=plotdf,FUN=NROW);names(ntasksd)<-c("Name","NumTasks")
summary(ntasksd$NumTasks)

# Log(Number of hours engaged in) vs number of activities in which they participated
plotdf<-merge(plotdf,ntasksd,by="Name", all.x=T)
nActdf<-aggregate(Hours~Name+NumTasks+ParticipantType,data=plotdf,sum)
numvols<-aggregate(Name~NumTasks+ParticipantType,nActdf,NROW); names(numvols)<-gsub("Name","NumVols",names(numvols))
numvols$NumVols<-paste0("(N=",numvols$NumVols,")");numvols$y<-5.6
cat6df<-subset(nActdf,NumTasks==6)
dd<-ggplot(nActdf,aes(x=as.factor(NumTasks),y=log(Hours))) + geom_violin(fill="#80BAC2",draw_quantiles=0.5,size=0.6) +     #aes(fill=as.factor(NumTasks))
		facet_wrap(~ParticipantType) + theme_bw() + 
		labs(x="Number of Tasks",y="Log(Hours engaged)") + theme(legend.position="none")
dd + geom_text(data=numvols,mapping=aes(x=as.factor(NumTasks),y=y,label=NumVols),vjust=-0.7,hjust=0.4,size=3) +
		geom_point(data=cat6df,aes(x=as.factor(NumTasks),y=log(Hours)),size=3,color="gray60")
## ...and ends here
#####################################################

### NOT DO!!


#"Bird call labeling"   "Social Media"
#create bar chart of volunteer hours by type
vol_hours_graph <- ggplot(cshours_at) + 
		geom_col(aes(x = reorder(Type, -Hours, FUN = sum), y = Hours, fill=Type)) + 
		scale_fill_manual(values = c("General sound labeling" = "#8c510a", "Data Upload" = "#d8b365", 
						"Field Work" = "#01665e", "GIS/Map-making" = "#c7eae5", 
						"Mail Deploy" = "#5ab4ac", "Meetings & Trainings" = "#f6e8c3",
						"Social Media" = "#76b56d", "Bird call labeling" = "#81728f")) +
		theme_bw() +
		theme(legend.position = "none") +
		theme(axis.text.x=element_text(size=13),axis.title.x=element_text(size=15)) +
		theme(axis.text.y=element_text(size=13),axis.title.y=element_text(size=15)) +
		theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
		scale_y_continuous(expand = expansion(mult = c(0, .1)), breaks=seq(0,3000,500), 
				labels = c("0","", "1000", "", "2000", "", "3000")) +   
		xlab("") + ylab("Number of Hours") + 
		theme(plot.title = element_text(hjust = 0.5)) + coord_flip()
##geom_text(data = cshours_at %>% 
#group_by(Type) %>% 
#summarise(Hours = sum(Hours, na.rm = TRUE)),
#aes(label = Hours, x = Type, y = Hours), inherit.aes = FALSE,
# vjust = -1, size = 2.5)

#save volunteer hours bar chart
png(filename="G:/Shared drives/NASA  S2L/Paper Development/Lessons learned/figures/Vol_Hours_Graphic.png", 
		units="in", width=6.5, height=3, res=600)
print(vol_hours_graph)
dev.off()

#rename rec_at$Private Property?
rec_at <- rec_at %>% rename(PrivateProp = `Private property?`) 

#summarize private vs. public property info, create new dataframe with this summary
prop_type <- c("Private", "Public")
value <- c(sum(rec_at$PrivateProp, na.rm=TRUE), sum(is.na(rec_at$PrivateProp)))
prop_df <- data.frame(prop_type)
prop_df$value <- value

#create pie chart of recording sites by property type (private vs. public)
property_type_graph <- ggplot(prop_df) +
		geom_bar(aes(x = "", y = value, fill = prop_type), stat = "identity", position="stack") +
		coord_polar("y", start=0) + geom_text(aes(x = "", y = value, label = value), position = position_stack(vjust = 0.2, reverse=FALSE)) +
		labs(fill = "Property Type") +
		theme(axis.title.x=element_blank(),
				axis.text.x=element_blank(),
				axis.ticks.x=element_blank()) +
		theme(axis.title.y=element_blank(),
				axis.text.y=element_blank(),
				axis.ticks.y=element_blank()) +
		theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
				panel.background = element_rect(fill = "transparent",colour = NA),
				plot.background = element_rect(fill = "transparent",colour = NA)) +
		scale_fill_manual(values = c("Private" = "#d8b365", "Public" = "#c7eae5")) +
		ggtitle("Recording Sites by Property Type") +
		theme(plot.title = element_text(hjust = 0.5))

#save property pie chart
png(filename="G:/Shared drives/NASA  S2L/Paper Development/Lessons learned/figures/Rec_Sites_Graphic.png", 
		units="in", width=5, height=5, res=600)
print(property_type_graph)
dev.off()



