# TODO: Add comment
# 
# Authors: Rose Snyder (rsnyder@pointblue.org), Leo Salas (lsalas@pointblue.org), and Matthew Clark (matthew.clark@sonoma.edu)
###############################################################################


#library(airtabler)
library(ggplot2)
library(plyr)
library(dplyr)

#Sys.setenv(AIRTABLE_API_KEY="put key here")

#project_management <- airtable(base = "base key here", table = c("Recordings","CS Hours Tracking"))
#rec_at <- project_management$Recordings$select_all(view="view key here")
#cshours_at <- project_management$"CS Hours Tracking"$select_all(view="view key here")
# data queried and saved in the file loaded below

pathToLocalGit<-"~/Soundscapes2Landscapes/"
load(paste0(pathToLocalGit,"CitizenScience_workflow/volunteerHours.RData"))
cshours_at$Type<-gsub("ABG","General sound labeling",cshours_at$Type)
cshours_at$Type<-gsub("Arbimon","Bird call labeling",cshours_at$Type)

# calculate volunteer hours by activity --
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

# Ready to make plots...

########################################
### Figure 6: Volunteer hours by activity --
plotdf<-aggregate(Hours~Type+UserId+Name,cshdf,sum)
plotdf$ParticipantType<-ifelse(plotdf$UserId=="Student-credit","Student","Volunteer")
plotdf<-merge(plotdf,skilldf,by="Type",all.x=TRUE)
plotdf$Type<-gsub("General sound labeling","Soundscape components",plotdf$Type)
plotdf$Type<-gsub("Bird call labeling","Bird call labeling",plotdf$Type)
plotdf$Type<-gsub("Social Media","Social media",plotdf$Type)
plotdf$Type<-gsub("Meetings & Trainings","Meetings and trainings",plotdf$Type)
plotdf$Type<-gsub("Data Upload","Data upload",plotdf$Type)
plotdf$Type<-gsub("Mail Deploy","Mail deploy",plotdf$Type)
plotdf$Type<-gsub("Field Work","Field work",plotdf$Type)
numvols<-aggregate(ParticipantType~Type,plotdf,NROW); names(numvols)<-gsub("ParticipantType","NumVols",names(numvols))
numvols$NumVols<-paste0("(N = ",numvols$NumVols,")");numvols$y<-6
tskl<-unique(plotdf[,c("SkillLevel","Type")])
numvols<-merge(numvols,tskl,by="Type",all.x=T)


# there is only one value for social media - it will be dropped if using violin plots, so...
plotdf$Type<-as.factor(plotdf$Type); plotdf$Type<-reorder(plotdf$Type,plotdf$AlphOrder)
pp<-ggplot(plotdf,aes(x=Type,y=log(Hours))) + geom_boxplot(aes(fill=ParticipantType)) + coord_flip() + theme_bw() + 
		labs(x="Task type",y="Log(Number of hours)",fill="Citizen scientist type") + facet_wrap(~SkillLevel,ncol=3) +
		geom_text(data=numvols, mapping=aes(x=Type, y=y, label=NumVols),vjust=0.6,hjust=0.7,size=3) + 
		scale_fill_manual(values=c("#FF5733","#11AF16")) +
		theme(strip.background = element_rect(color="black", fill="#D4AF37", size=1, linetype="solid")) +
		theme(strip.text.x = element_text(size = 12, color = "black", face = "plain"))

png(filename="G:/Shared drives/NASA  S2L/Paper Development/Lessons learned/acceptance docs/Figure6_Revised_22-05-16.png", 
		units="in", width=10, height=3, res=600)
print(pp)
dev.off()


########################################
### Figure 5: Average and range of number of tasks by participant
ntasksd<-aggregate(Type~Name,data=plotdf,FUN=NROW);names(ntasksd)<-c("Name","NumTasks")
summary(ntasksd$NumTasks)

plotdf<-merge(plotdf,ntasksd,by="Name", all.x=T)
nActdf<-aggregate(Hours~Name+NumTasks+ParticipantType,data=plotdf,sum)
numvols<-aggregate(Name~NumTasks+ParticipantType,nActdf,NROW); names(numvols)<-gsub("Name","NumVols",names(numvols))
numvols$NumVols<-paste0("(N = ",numvols$NumVols,")");numvols$y<-5.6
cat6df<-subset(nActdf,NumTasks==6)
dd<-ggplot(nActdf,aes(x=as.factor(NumTasks),y=log(Hours))) + geom_violin(fill="#80BAC2",draw_quantiles=0.5,size=0.6) +     #aes(fill=as.factor(NumTasks))
		facet_wrap(~ParticipantType) + theme_bw() + 
		labs(x="Number of tasks",y="Log(Hours engaged)") + theme(legend.position="none")
dd<- dd + geom_text(data=numvols,mapping=aes(x=as.factor(NumTasks),y=y,label=NumVols),vjust=-0.7,hjust=0.4,size=3) +
		geom_point(data=cat6df,aes(x=as.factor(NumTasks),y=log(Hours)),size=3,color="gray60")

png(filename="G:/Shared drives/NASA  S2L/Paper Development/Lessons learned/acceptance docs/Figure5_Revised_22-05-16.png", 
		units="in", width=8, height=4, res=600)
print(dd)
dev.off()

#####################################################

