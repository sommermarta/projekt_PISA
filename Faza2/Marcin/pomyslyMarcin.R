



#radarowy plot

# http://shinyapps.stat.ubc.ca/r-graph-catalog/# barchart sredniej wynikow z roznych testow 
# w podziale na lata albo na poziomy pewnej zmiennej






Marcin <- read.csv("polska2012pisa.csv")
names(Marcin)


library(ggplot2)
library(plyr)
library(reshape2)
library(gridExtra)
library(xkcd)



############

## Wykres 11
#mozaik plot 2 zmienne kategoryczne w 2012 roku



my_data_x <- as.matrix(table(na.omit(Marcin[,11:12]))
                       
my_data_x <- data.frame(MakeFriendsEasily = c("Agree", "Disagree", "StronglyAgree","StronglyDisagree"), Agree =my_data_x[,1], Disagree = my_data_x[, 2], 
                       StronglyAgree = my_data_x[, 3], StronglyDisagree = my_data_x[, 4])



# helper variables
my_data_x$total <- with(my_data_x, Agree+Disagree+StronglyAgree+StronglyDisagree)
my_data_x$xmax <- with(my_data_x, cumsum(total))
my_data_x$xmin <- with(my_data_x, xmax - total)

my_data_long <- melt(my_data_x, c("MakeFriendsEasily", "xmin", "xmax", "total"))

my_data_long$MakeFriendsEasily <- factor(my_data_long$MakeFriendsEasily, c("Agree", "Disagree", "StronglyAgree", "StronglyDisagree"))

my_data_long1 <- ddply(my_data_long, .(MakeFriendsEasily), transform, ymax = cumsum(value))
my_data_long1 <- ddply(my_data_long1, .(MakeFriendsEasily), transform, ymin = ymax - value)

my_data_long1$ymin_std <- with(my_data_long1, (ymin / total) * 100)
my_data_long1$ymax_std <- with(my_data_long1, (ymax / total) * 100)


head(my_data_long1)


help(package="xkcd")

font.add("xkcd", regular = "xkcd.ttf")





p <- ggplot(my_data_long1, 
           aes(ymin = ymin_std, ymax = ymax_std, xmin = xmin, 
               xmax = xmax, fill = variable)) +
  geom_rect(colour = "black", show_guide = FALSE, width = 0.5) +
  scale_fill_manual(values = c("grey20", "grey50", "grey80", "grey30")) +
  theme_bw() +
  theme(panel.border = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(size = 16, family = "xkcd"))+
  scale_x_continuous(breaks = c(1000, 2000,2500, 2970), labels = c("Agree", "", "Strongly Agree", "")) +
  scale_y_continuous(breaks = c(10, 50, 82, 94), 
                     labels = c("Agree", "Disagree", "Strongly \n agree", "Strongly \n disagree")) +
  xlab("I Make Friends Easily")+
   ylab("I Think School \n Is A Waste Of Time")+
  annotate("text", x = c(2000,2970), y = c(109,109), label = c("Disagree", "Strongly \n Disagree"),  
           size=4,family = "xkcd")+ # color="#3366ff",
  ggtitle("How do students feel in school?")

#install.packages("showtext")
library(showtext)
font.add("xkcd", "xkcd.ttf")
pdf("Marcin11.pdf", height = 7, width =7)
showtext.begin()
print(p)
showtext.end()
dev.off()



############
## Wykres 22
# rozklad wynikow z matmy, czytania i fizy dla pewnych poziomow pewnej zmiennej
# kategorycznej



data22<- na.omit(Marcin[,c(5,14:16)])
data33 <- melt(data22, "ParentsBelieveStudyingMathematicsIsImportant")
names(data33)[2:3] <- c("Test", "Score")

p2 <- ggplot(data33, aes(x = ParentsBelieveStudyingMathematicsIsImportant, y = Score)) +
   geom_boxplot(outlier.shape = 1) + 
   scale_x_discrete(limits = rev(levels(data22$ParentsBelieveStudyingMathematicsIsImportant))) +
   coord_flip() +
   ggtitle("Do You Agree With: Your Parents Believe  
           Studying Mathematics Is Important \n And What Is Your Score?") +
   theme_bw() +
   theme(panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         plot.title = element_text(size = rel(1.2), face = "bold", vjust = 1.5),
         axis.ticks.y = element_blank(),
         axis.title.y = element_blank(),
         text = element_text(size = 16, family = "xkcd"))+
   facet_wrap(~ Test)+
   scale_x_discrete("ParentsBelieveStudyingMathematicsIsImportant", 
                    labels = c("Strongly Agree", "Agree",
                               "Disagree", "Strongly Disagree" ))
#    


library(showtext)
font.add("xkcd", "xkcd.ttf")
pdf("Marcin22.pdf", height = 7, width = 10)
showtext.begin()
print(p2)
showtext.end()
dev.off()


#########
## Wykres 33


con <- url("http://biecek.pl/MIMUW/PISAeurope.rda")
load(con)


names(pisa)


sasiedzi <- which(pisa$CNT %in% c("Poland", "Czech Republic", "Germany", "Finland"))

sasiedzi2 <- pisa[sasiedzi,]
names(sasiedzi2)[1:12]
head(sasiedzi2[6:10])

pisaSasiedzi <- sasiedzi2[,c(1,6:258)] 
colnames(pisaSasiedzi) <- c(
   "Country",
   "BirthMonth",
   "BirthYear",
   "Gender",
   "AttendISCED0",
   "AgeAtISCED1",
   "RepeatISCED1",
   "RepeatISCED2",
   "RepeatISCED3",
   "LateForSchool",
   "SkipWholeSchoolDay",
   "1SkipClassesWithinSchoolDay",
   "AtHomeMother",
   "AtHomeFather",
   "AtHomeBrothers",
   "AtHomeSisters",
   "AtHomeGrandparents",
   "AtHomeOthers",
   "MotherHighestSchooling",
   "MotherQualificationsISCEDLevel6",
   "MotherQualificationsISCEDLevel5A",
   "MotherQualificationsISCEDLevel5B",
   "MotherQualificationsISCEDLevel4",
   "MotherCurrentJobStatus",
   "FatherHighestSchooling",
   "FatherQualificationsISCEDLevel6",
   "FatherQualificationsISCEDLevel5A",
   "FatherQualificationsISCEDLevel5B",
   "FatherQualificationsISCEDLevel4",
   "FatherCurrentJobStatus",
   "CountryOfBirthInternationalSelf",
   "CountryOfBirthInternationalMother",
   "CountryOfBirthInternationalFather",
   "AgeOfArrivalInCountryOfTest",
   "InternationalLanguageAtHome",
   "PossessionsDesk",
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
   "PossessionsSatelliteOrCableTVWithAtLeast30Channels",
   "PossessionsDigitalCamera",
   "PossessionsPlasmaOrLCDTv",
   "HowManyCellularPhones",
   "HowManyTelevisions",
   "HowManyComputers",
   "HowManyCars",
   "HowManyRoomsBathOrShower",
   "HowManyBooksAtHome",
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
   "MathsSelfEfficacyUsingATrainTimetable",
   "MathsSelfEfficacyCalculatingTVDiscount",
   "MathsSelfEfficacyCalculatingSquareMetresOfTiles",
   "MathsSelfEfficacyUnderstandingGraphsInNewspapers",
   "MathsSelfEfficacySolvingEquation1",
   "MathsSelfEfficacyDistanceToScale",
   "MathsSelfEfficacySolvingEquation2",
   "MathsSelfEfficacyCalculatePetrolConsumptionRate",
   "MathsAnxietyWorryThatItWillBeDifficult",
   "MathsSelfConceptNotGoodAtMaths",
   "MathsAnxietyGetVeryTense",
   "MathsSelfConceptGetGoodGrades",
   "MathsAnxietyGetVeryNervous",
   "MathsSelfConceptLearnQuickly",
   "MathsSelfConceptOneOfBestSubjects",
   "MathsAnxietyFeelHelpless",
   "MathsSelfConceptUnderstandDifficultWork",
   "MathsAnxietyWorryAboutGettingPoorGrades",
   "PerceivedControlCanSucceedWithEnoughEffort",
   "PerceivedControlDoingWellIsCompletelyUpToMe",
   "PerceivedControlFamilyDemandsAndProblems",
   "PerceivedControlDifferentTeachers",
   "PerceivedControlIfIWantedICouldPerformWell",
   "PerceivedControlPerformPoorlyRegardless",
   "AttributionsToFailureNotGoodAtMathsProblems",
   "AttributionsToFailureTeacherDidNotExplainWell",
   "AttributionsToFailureBadGuesses",
   "AttributionsToFailureMaterialTooHard",
   "AttributionsToFailureTeacherDidntGetStudentsInterested",
   "AttributionsToFailureUnlucky",
   "MathsWorkEthicHomeworkCompletedInTime",
   "MathsWorkEthicWorkHardOnHomework",
   "MathsWorkEthicPreparedForExams",
   "MathsWorkEthicStudyHardForQuizzes",
   "MathsWorkEthicStudyUntilIUnderstandEverything",
   "MathsWorkEthicPayAttentionInClasses",
   "MathsWorkEthicListenInClasses",
   "MathsWorkEthicAvoidDistractionsWhenStudying",
   "MathsWorkEthicKeepWorkOrganized",
   "MathsIntentionsMathematicsVsLanguageCoursesAfterSchool",
   "MathsIntentionsMathematicsVsScienceRelatedMajorinCollege",
   "MathsIntentionsStudyHarderInMathematicsVsLanguageClasses",
   "MathsIntentionsTakeMaximumNumberOfMathematicsVsScienceClasses",
   "MathsIntentionsPursuingACareerThatInvolvesMathematicsVsScience",
   "MathsBehaviourTalkAboutMathsWithFriends",
   "MathsBehaviourHelpFriendsWithMaths",
   "MathsBehaviourExtracurricularActivity",
   "MathsBehaviourParticipateInCompetitions",
   "MathsBehaviourStudyMoreThan2ExtraHoursADay",
   "MathsBehaviourPlayChess",
   "MathsBehaviourComputerProgramming",
   "MathsBehaviourParticipateInMathsClub",
   "LearningStrategiesImportantPartsVsExistingKnowledgeVsLearnByHeart",
   "LearningStrategiesImproveUnderstandingVsNewWaysVsMemory",
   "LearningStrategiesOtherSubjectsVsLearningGoalsvsRehearseProblems",
   "LearningStrategiesRepeatExamplesVsEverydayApplicationsVsMoreInformation",
   "OutOfSchoolLessonsTestLang",
   "OutOfSchoolLessonsMaths",
   "OutOfSchoolLessonsScience",
   "OutOfSchoolLessonsOther",
   "OutOfSchoolStudyTimeHomework",
   "OutOfSchoolStudyTimeGuidedHomework",
   "OutOfSchoolStudyTimePersonalTutor",
   "OutOfSchoolStudyTimeCommercialCompany",
   "OutOfSchoolStudyTimeWithParent",
   "OutOfSchoolStudyTimeComputer",
   "ExperienceWithAppliedMathsTasksUseTrainTimetable",
   "ExperienceWithAppliedMathsTasksCalculatePriceincludingTax",
   "ExperienceWithAppliedMathsTasksCalculateSquareMetres",
   "ExperienceWithAppliedMathsTasksUnderstandScientificTables",
   "ExperienceWithPureMathsTasksSolveEquation1",
   "ExperienceWithAppliedMathsTasksUseAMapToCalculateDistance",
   "ExperienceWithPureMathsTasksSolveEquation2",
   "ExperienceWithAppliedMathsTasksCalculatePowerConsumptionRate",
   "ExperienceWithAppliedMathsTasksSolveEquation3",
   "FamiliarityWithMathsConceptsExponentialFunction",
   "FamiliarityWithMathsConceptsDivisor",
   "FamiliarityWithMathsConceptsQuadraticFunction",
   "OverclaimingProperNumber",
   "FamiliarityWithMathsConceptsLinearEquation",
   "FamiliarityWithMathsConceptsVectors",
   "FamiliarityWithMathsConceptsComplexNumber",
   "FamiliarityWithMathsConceptsRationalNumber",
   "FamiliarityWithMathsConceptsRadicals",
   "OverclaimingSubjunctiveScaling",
   "FamiliarityWithMathsConceptsPolygon",
   "OverclaimingDeclarativeFraction",
   "FamiliarityWithMathsConceptsCongruentFigure",
   "FamiliarityWithMathsConceptsCosine",
   "FamiliarityWithMathsConceptsArithmeticMean",
   "FamiliarityWithMathsConceptsProbability",
   "MinInClassPeriodTestLang",
   "MinInClassPeriodMaths",
   "MinInClassPeriodScience",
   "NoOfClassPeriodP/wkTestLang",
   "NoOfClassPeriodP/wkMaths",
   "NoOfClassPeriodP/wkScience",
   "NoOfALLClassPeriodAWeek",
   "ClassSizeNoOfStudentsInTestLanguageClass",
   "OTLAlgebraicWordProblemInMathsLesson",
   "OTLAlgebraicWordProblemInTests",
   "OTLProceduralTaskInMathsLesson",
   "OTLProceduralTaskInTests",
   "OTLPureMathsReasoningInMathsLesson",
   "OTLPureMathsReasoningInTests",
   "OTLAppliedMathsReasoningInMathsLesson",
   "OTLAppliedMathsReasoningInTests",
   "MathsTeachingTeacherShowsInterest",
   "MathsTeachingExtraHelp",
   "MathsTeachingTeacherHelps",
   "MathsTeachingTeacherContinues",
   "MathsTeachingExpressOpinions",
   "TeacherDirectedInstructionSetsClearGoals",
   "TeacherDirectedInstructionEncouragesThinkingAndReasoning",
   "StudentOrientationDifferentiatesBetweenStudentsWhenGivingTasks",
   "StudentOrientationAssignsComplexProjects",
   "FormativeAssessmentGivesFeedback",
   "TeacherDirectedInstructionChecksUnderstanding",
   "StudentOrientationHasStudentsWorkInSmallGroups",
   "TeacherDirectedInstructionSummarizesPreviousLessons",
   "StudentOrientationPlansClassroomActivities",
   "FormativeAssessmentGivesFeedbackOnStrengthsAndWeaknesses",
   "FormativeAssessmentInformsAboutExpectations",
   "TeacherDirectedInstructionInformsAboutLearningGoals",
   "FormativeAssessmentTellsHowToGetBetter",
   "CognitiveActivationTeacherEncouragesToReflectProblems",
   "CognitiveActivationGivesProblemsThatRequireToThink",
   "CognitiveActivationAsksToUseOwnProcedures",
   "CognitiveActivationPresentsProblemsWithNoObviousSolutions",
   "CognitiveActivationPresentsProblemsInDifferentContexts",
   "CognitiveActivationHelpsLearnFromMistakes",
   "CognitiveActivationAsksForExplanations",
   "CognitiveActivationApplyWhatWeLearned",
   "CognitiveActivationProblemsWithMultipleSolutions",
   "DisciplinaryClimateStudentsDonëtListen",
   "DisciplinaryClimateNoiseAndDisorder",
   "DisciplinaryClimateTeacherHasToWaitUntilItsQuiet",
   "DisciplinaryClimateStudentsDoNotWorkWell",
   "DisciplinaryClimateStudentsStartWorkingLate",
   "VignetteTeacherSupportHomeworkEveryOtherDay/BackInTime",
   "VignetteTeacherSupportHomeworkOnceAWeek/BackInTime",
   "VignetteTeacherSupportHomeworkOnceAWeek/NotBackInTime",
   "TeacherSupportLetsUsKnowWeHaveToWorkHard",
   "TeacherSupportProvidesExtraHelpWhenNeeded",
   "TeacherSupportHelpsStudentsWithLearning",
   "TeacherSupportGivesOpportunityToExpressOpinions",
   "VignetteClassroomManagementStudentsFrequentlyInterrupt/TeacherArrivesEarly",
   "VignetteClassroomManagementStudentsAreCalm/TeacherArrivesOnTime",
   "VignetteClassroomManagementStudentsFrequentlyInterrupt/TeacherArrivesLate",
   "ClassroomManagementStudentsListen",
   "ClassroomManagementTeacherKeepsClassOrderly",
   "ClassroomManagementTeacherStartsOnTime",
   "ClassroomManagementWaitLongToQuietDown",
   "StudentTeacherRelationsGetAlongWithTeachers",
   "StudentTeacherRelationsTeachersAreInterested",
   "StudentTeacherRelationsTeachersListenToStudents",
   "StudentTeacherRelationsTeachersHelpStudents",
   "StudentTeacherRelationsTeachersTreatStudentsFair",
   "SenseOfBelongingFeelLikeOutsider",
   "SenseOfBelongingMakeFriendsEasily",
   "SenseOfBelongingBelongAtSchool",
   "SenseOfBelongingFeelAwkwardAtSchool",
   "SenseOfBelongingLikedByOtherStudents",
   "SenseOfBelongingFeelLonelyAtSchool",
   "SenseOfBelongingFeelHappyAtSchool",
   "SenseOfBelongingThingsAreIdealAtSchool",
   "SenseOfBelongingSatisfiedAtSchool",
   "AttitudeTowardsSchoolDoesLittleToPrepareMeForLife",
   "AttitudeTowardsSchoolWasteOfTime",
   "AttitudeTowardsSchoolGaveMeConfidence",
   "AttitudeTowardsSchoolUsefulForJob",
   "AttitudeTowardSchoolHelpsToGetAJob",
   "AttitudeTowardSchoolPrepareForCollege",
   "AttitudeTowardSchoolEnjoyGoodGrades",
   "AttitudeTowardSchoolTryingHardIsImportant",
   "PerceivedControlCanSucceedWithEnoughEffort",
   "PerceivedControlMyChoiceWhetherIWillBeGood",
   "PerceivedControlProblemsPreventFromPuttingEffortIntoSchoo",
   "PerceivedControlDifferentTeachersWouldMakeMeTryHarder",
   "PerceivedControlCouldPerformWellIfIWanted",
   "PerceivedControlPerformPoorRegardless",
   "PerseveranceGiveUpEasily"
)



# procentowy udzial pewnej zmiennej w podziale na rok albo inne zmienne http://shinyapps.stat.ubc.ca/r-graph-catalog/#
# np stosunek naucyzcieli do uczniow oceniany w 2009, 2012 i 2006 roku

datax <- pisaSasiedzi[ c("Country", "MathsAnxietyWorryThatItWillBeDifficult"
              ,"MathsBehaviourStudyMoreThan2ExtraHoursADay")]

datax <- na.omit(datax)
datax <- table(datax[, c(1,3)])

datax2 <- data.frame(Country= c("Czech Republic", "Germany", "Finland", "Poland"), 
                     Always = datax[c(2:4,10),1], Often = datax[c(2:4,10),2], 
                     Sometimes = datax[c(2:4,10),3], NeverOrRarely = datax[c(2:4,10),4])

datax3 <- data.frame(t(apply(datax2[, 2:5], 1, function(x) x/sum(x))), Country= c("Czech Republic", "Germany", "Finland", "Poland"))
datax2 <- melt(datax3)


percent <- paste0(seq(0,1,0.25), "\%")

Marcin3 <- ggplot(datax2, aes(x=Country, y=value, fill=variable)) + geom_bar(stat="identity") +
   theme_bw() +
   labs(y="",x="Country") +
   coord_flip() +
   theme( legend.position = "top") +
   geom_hline(aes(yintercept=0.25),linetype=2,col='black',size=1,alpha=0.4)+
   geom_hline(aes(yintercept=0.75),linetype=2,col='black',size=1,alpha=0.4)+
   geom_hline(aes(yintercept=0.50),linetype=2,col='black',size=1,alpha=0.4)+
   guides(fill=guide_legend(title.position="top", title=""))+
   theme( text= element_text(family = "xkcd"),
          title =element_text(family = "xkcd")
   )+
   ggtitle( "Maths Anxiety: \n Do You Worry That It Will Be Difficult")
# +
#    scale_y_continuous(labels = percent)


library(showtext)
font.add("xkcd", "xkcd.ttf")
pdf("Marcin33.pdf", height = 7, width = 10)
showtext.begin()
print(Marcin3)
showtext.end()
dev.off()




#########
## Wykres 34

datax <- pisaSasiedzi[ c("Country", "MathsAnxietyWorryThatItWillBeDifficult"
                         ,"MathsBehaviourStudyMoreThan2ExtraHoursADay")]

datax <- na.omit(datax)
datax <- table(datax[, c(1,2)])

datax2 <- data.frame(Country= c("Czech Republic", "Germany", "Finland", "Poland"), 
                     StronglyAgree = datax[c(2:4,10),1], Agree = datax[c(2:4,10),2], 
                     Disagree = datax[c(2:4,10),3], StronglyDisagree = datax[c(2:4,10),4])

datax3 <- data.frame(t(apply(datax2[, 2:5], 1, function(x) x/sum(x))), Country= c("Czech Republic", "Germany", "Finland", "Poland"))
datax2 <- melt(datax3)

Marcin4 <- ggplot(datax2, aes(x=Country, y=value, fill=variable)) + geom_bar(stat="identity") +
   theme_bw() +
   labs(y="",x="Country") +
   coord_flip() +
   theme( legend.position = "top") +
   geom_hline(aes(yintercept=0.25),linetype=2,col='black',size=1,alpha=0.4)+
   geom_hline(aes(yintercept=0.75),linetype=2,col='black',size=1,alpha=0.4)+
   geom_hline(aes(yintercept=0.50),linetype=2,col='black',size=1,alpha=0.4)+
   guides(fill=guide_legend(title.position="top", title=""))+
   theme( text= element_text(family = "xkcd"),
          title =element_text(family = "xkcd")
   )+
   ggtitle( "Maths Behaviour: \n Study More Than 2 Extra Hours A Day")


library(showtext)
font.add("xkcd", "xkcd.ttf")
pdf("Marcin44.pdf", height = 7, width = 10)
showtext.begin()
print(Marcin4)
showtext.end()
dev.off()
