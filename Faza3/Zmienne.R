load("Dane/wczytajZmiennePisa2.rda")


# con <- url("http://biecek.pl/MIMUW/PISAeurope.rda")
# load(con)
# 
# library(stringi)
# library(dplyr)
# 
# pisa <- pisa %>% filter( CNT == "Poland")
# pisa2 <- data.frame(pisa[,c("ST26Q01","ST26Q02","ST26Q03","ST26Q04","ST26Q05","ST26Q06",
#                  "ST26Q07","ST26Q08","ST26Q09","ST26Q10","ST26Q11","ST26Q12",
#                  "ST26Q13","ST26Q14",
#                   stri_paste("ST29Q0",1:8,sep=""),
#                   stri_paste("ST35Q0",1:6,sep=""),
#                   stri_paste("ST43Q0",1:6,sep=""),
#                   stri_paste("ST46Q0",1:9,sep=""),
#                  stri_paste("ST93Q0",c(1,3,4),sep=""),
#                  "PV1MATH","W_FSTUWT", "PV1READ", "PV1SCIE"
#                  )])
# colnames(pisa2) <- c("PossessionsDesk",
#                      "PossessionsOwnRoom",
#                      "PossessionsStudyPlace",
#                      "PossessionsComputer",
#                      "PossessionsSoftware",
#                      "PossessionsInternet",
#                      "PossessionsLiterature",
#                      "PossessionsPoetry",
#                      "PossessionsArt",
#                      "PossessionsTextbooks",
#                      "PossessionsTechnicalReferenceBooks",
#                      "PossessionsDictionary",
#                      "PossessionsDishwasher",
#                      "PossessionsDVD",
#                      
#                      "MathsInterestEnjoyReading",
#                      "InstrumentalMotivationWorthwhileForWork",
#                      "MathsInterestLookForwardToLessons",
#                      "MathsInterestEnjoyMaths",
#                      "InstrumentalMotivationWorthwhileForCareerChances",
#                      "MathsInterestInterested",
#                      "InstrumentalMotivationImportantForFutureStudy",
#                      "InstrumentalMotivationHelpsToGetaJob",
#                      
#                      "SubjectiveNormsFriendsDoWellInMathematics",
#                      "SubjectiveNormsFriendsWorkHardOnMathematics",
#                      "SubjectiveNormsFriendsEnjoyMathematicsTests",
#                      "SubjectiveNormsParentsBelieveStudyingMathematicsIsImportant",
#                      "SubjectiveNormsParentsBelieveMathematicsIsImportantForCareer",
#                      "SubjectiveNormsParentsLikeMathematics",
#                      
#                      "PerceivedControlCanSucceedWithEnoughEffort",
#                      "PerceivedControlDoingWellIsCompletelyUpToMe",
#                      "PerceivedControlFamilyDemandsAndProblems",
#                      "PerceivedControlDifferentTeachers",
#                      "PerceivedControlIfIWantedICouldPerformWell",
#                      "PerceivedControlPerformPoorlyRegardless",
#                      
#                      "MathsWorkEthicHomeworkCompletedInTime",
#                      "MathsWorkEthicWorkHardOnHomework",
#                      "MathsWorkEthicPreparedForExams",
#                      "MathsWorkEthicStudyHardForQuizzes",
#                      "MathsWorkEthicStudyUntilIUnderstandEverything",
#                      "MathsWorkEthicPayAttentionInClasses",
#                      "MathsWorkEthicListenInClasses",
#                      "MathsWorkEthicAvoidDistractionsWhenStudying",
#                      "MathsWorkEthicKeepWorkOrganized",
#                      
#                      "PerseveranceGiveUpEasily",
#                      # nie ma tej zmiennej w danych: "PerseverancePutOffDifficultProblems",
#                      "PerseveranceRemainInterested",
#                      "PerseveranceContinueToPerfection",
#                      # nie ma tej zmiennej w danych: "PerseveranceExceedExpectations",
#                      
#                      
#                      "math",
#                      "weights",
#                      "read",
#                      "science"
#                      )
# 
# save(pisa2, file= "Dane/wczytajZmiennePisa2.rda")
