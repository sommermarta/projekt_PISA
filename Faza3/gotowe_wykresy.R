library("ggplot2")
library(stringi)
library(dplyr)
library("wesanderson")
library(scales)
con <- url("http://biecek.pl/MIMUW/PISAeurope.rda")
load(con)


pisa2 <- data.frame(pisa[,c("ST26Q01","ST26Q02","ST26Q03","ST26Q04","ST26Q05","ST26Q06",
                            "ST26Q07","ST26Q08","ST26Q09","ST26Q10","ST26Q11","ST26Q12",
                            "ST26Q13","ST26Q14",
                            stri_paste("ST29Q0",1:8,sep=""),
                            stri_paste("ST35Q0",1:6,sep=""),
                            stri_paste("ST43Q0",1:6,sep=""),
                            stri_paste("ST46Q0",1:9,sep=""),
                            stri_paste("ST93Q0",c(1,3,4),sep=""),
                            "PV1MATH","W_FSTUWT", "PV1READ", "PV1SCIE"
)])
colnames(pisa2) <- c("PossessionsDesk",
                     "PossessionsOwnRoom",
                     "PossessionsStudyPlace",
                     "PossessionsComputer",
                     "PossessionsSoftware",
                     "PossessionsInternet",
                     "PossessionsLiterature",
                     "PossessionsPoetry",
                     "PossessionsArt",
                     "PossessionsTextbooks",
                     "PossessionsTechnicalReferenceBooks",
                     "PossessionsDictionary",
                     "PossessionsDishwasher",
                     "PossessionsDVD",
                     
                     "MathsInterestEnjoyReading",
                     "InstrumentalMotivationWorthwhileForWork",
                     "MathsInterestLookForwardToLessons",
                     "MathsInterestEnjoyMaths",
                     "InstrumentalMotivationWorthwhileForCareerChances",
                     "MathsInterestInterested",
                     "InstrumentalMotivationImportantForFutureStudy",
                     "InstrumentalMotivationHelpsToGetaJob",
                     
                     "SubjectiveNormsFriendsDoWellInMathematics",
                     "SubjectiveNormsFriendsWorkHardOnMathematics",
                     "SubjectiveNormsFriendsEnjoyMathematicsTests",
                     "SubjectiveNormsParentsBelieveStudyingMathematicsIsImportant",
                     "SubjectiveNormsParentsBelieveMathematicsIsImportantForCareer",
                     "SubjectiveNormsParentsLikeMathematics",
                     
                     "PerceivedControlCanSucceedWithEnoughEffort",
                     "PerceivedControlDoingWellIsCompletelyUpToMe",
                     "PerceivedControlFamilyDemandsAndProblems",
                     "PerceivedControlDifferentTeachers",
                     "PerceivedControlIfIWantedICouldPerformWell",
                     "PerceivedControlPerformPoorlyRegardless",
                     
                     "MathsWorkEthicHomeworkCompletedInTime",
                     "MathsWorkEthicWorkHardOnHomework",
                     "MathsWorkEthicPreparedForExams",
                     "MathsWorkEthicStudyHardForQuizzes",
                     "MathsWorkEthicStudyUntilIUnderstandEverything",
                     "MathsWorkEthicPayAttentionInClasses",
                     "MathsWorkEthicListenInClasses",
                     "MathsWorkEthicAvoidDistractionsWhenStudying",
                     "MathsWorkEthicKeepWorkOrganized",
                     
                     "PerseveranceGiveUpEasily",
                     # nie ma tej zmiennej w danych: "PerseverancePutOffDifficultProblems",
                     "PerseveranceRemainInterested",
                     "PerseveranceContinueToPerfection",
                     # nie ma tej zmiennej w danych: "PerseveranceExceedExpectations",
                     
                     
                     "math",
                     "weights",
                     "read",
                     "science"
)



motyw <- theme_bw(base_family = "serif", base_size = 28) +
  theme(legend.background = element_blank(),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        strip.background = element_blank(),
        plot.background = element_blank(),
        axis.line = element_blank(),
        panel.grid = element_blank(),
        legend.position = "top",
        axis.title.x = element_text(vjust=-0.4),
        axis.title.y = element_text(vjust=+1)
        )

## wykres pierwszy - lewy górny

ggplot(pisa2[!is.na(pisa2$MathsInterestEnjoyMaths),],aes(x=MathsInterestEnjoyMaths,
                                                         y=math,
                                                         col=MathsInterestEnjoyMaths)) +
  geom_boxplot(size=1.5) +
   scale_color_manual(values = wes.palette(4,"Moonrise2"))+
  xlab("Do you like Maths?")+
  ylab("Maths score") +
#   ggtitle("Do emotions matter?") +
  motyw+
   guides(colour=FALSE)

## wykres drugi - prawy górny

pisa_rozklad_lubienia <- pisa2[!is.na(pisa2$MathsInterestEnjoyMaths) & !is.na(pisa2$SubjectiveNormsParentsLikeMathematics),] %>%
  group_by(MathsInterestEnjoyMaths,SubjectiveNormsParentsLikeMathematics) %>%
  summarise(ile=n()) %>%
  group_by(SubjectiveNormsParentsLikeMathematics) %>%
  mutate(ile = ile/sum(ile))

pisa_rozklad_lubienia$SubjectiveNormsParentsLikeMathematics <-
  reorder(pisa_rozklad_lubienia$SubjectiveNormsParentsLikeMathematics, 16:1)


ggplot(pisa_rozklad_lubienia,aes(x=SubjectiveNormsParentsLikeMathematics, y=ile, fill=MathsInterestEnjoyMaths)) +
  geom_bar(stat="identity") +
  coord_flip() +
  geom_hline(aes(yintercept=0.25),linetype=2,col='black',size=1,alpha=0.4)+
  geom_hline(aes(yintercept=0.75),linetype=2,col='black',size=1,alpha=0.4)+
  geom_hline(aes(yintercept=0.50),linetype=2,col='black',size=1,alpha=0.4)+
   scale_fill_manual(values= wes.palette(4,"Royal1")[c(1,3,4,2)])+
  scale_y_continuous(labels = percent) +
  xlab("Do your parents like Maths?")+
  ylab("Fraction") +
  #ggtitle("How does parents' attitiude affect their children?")+
  guides(fill=guide_legend(title="Do you like Maths?", title.hjust=6)) +
  theme_bw(base_family = "serif", base_size = 28) +
  theme(legend.background = element_blank(),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        strip.background = element_blank(),
        plot.background = element_blank(),
        axis.line = element_blank(),
        panel.grid = element_blank(),
        legend.position = "top",
        axis.title.x = element_text(vjust=-0.4),
        axis.title.y = element_text(vjust=+1),
        legend.position =c(-15,0)
  )


# wykres 3 - lewy dolny

pisa3 <- pisa2[, c("math","weights", "read","PerseveranceGiveUpEasily", "MathsInterestEnjoyMaths", "MathsInterestEnjoyReading")]
pisa3 <- na.omit(pisa3)


ggplot(pisa3, aes(y = math, x =PerseveranceGiveUpEasily,col=PerseveranceGiveUpEasily))+
  geom_boxplot(size=1.5)+
  scale_color_manual(values = wes.palette(5,"Darjeeling"),name="")+
  xlab("Do you give up easily?")+
  ylab("Maths score")+
  #ggtitle("Is determination important?")+
   theme_bw(base_family = "serif", base_size = 30) +
#   theme(
#         panel.background = element_blank(),
#         panel.border = element_blank(),
#         strip.background = element_blank(),
#         plot.background = element_blank(),
#         axis.line = element_blank(),
#         panel.grid = element_blank())+
  theme(legend.background = element_blank(),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        strip.background = element_blank(),
        plot.background = element_blank(),
        axis.line = element_blank(),
        panel.grid = element_blank(),
        legend.position = "top",
        axis.title.x = element_text(vjust=+0.4),
        axis.title.y = element_text(vjust=+1),
        axis.text.x=element_text(angle=25))+
  guides(colour=FALSE)



## wykres czwarty - prawy dolny

pisaE <- pisa2[!is.na(pisa2$MathsWorkEthicHomeworkCompletedInTime) & !is.na(pisa2$MathsWorkEthicPayAttentionInClasses),]
pisaE <- pisaE %>%
  group_by(MathsWorkEthicHomeworkCompletedInTime,MathsWorkEthicPayAttentionInClasses) %>%
  summarise(math=weighted.mean(math,weights))


ggplot(pisaE,aes(x=MathsWorkEthicHomeworkCompletedInTime,
                 y=MathsWorkEthicPayAttentionInClasses,
                 fill=math)) +
  geom_raster() +
   scale_fill_gradient(high=wes.palette(5,"Cavalcanti")[1], 
                       low=wes.palette(5,"Cavalcanti")[2], name="Maths score") +
  xlab("Do you complete homework on time?")+
  ylab("Do you pay attention in classes?") +
#   ggtitle("Is it worth to pay attention in classes?") +
  theme_bw(base_family = "serif", base_size = 28) +
  theme(legend.background = element_blank(),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        strip.background = element_blank(),
        plot.background = element_blank(),
        axis.line = element_blank(),
        panel.grid = element_blank(),
        legend.position = "right",
        axis.title.x = element_text(vjust=-0.4, size=30),
        axis.title.y = element_text(vjust=+1, size = 30),
        axis.text.x=element_text(size=22)
        )
# to jest ten sam motyw tylko z jegenda po prawej
