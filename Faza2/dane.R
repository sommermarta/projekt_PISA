# data download & preparations

con <- url("http://beta.icm.edu.pl/PISAcontest/data/student2012.rda")
load(con)
polska2012 <- student2012[student2012[, 1] == "Poland", ]

#Numbered Column Names
#http://pisa2012.acer.edu.au/downloads/M_stu_codebook.pdf
#till page number 150
polska2012 <- polska2012[,-c(1:9)] # nation identification data


scores.index <- which( names(polska2012) %in% c("PV1MATH", "PV1READ", "PV1SCIE"))

ciekawe <- polska2012[,c(3,9,36,69,80,120,151,181,229,231,240,251, scores.index)]
names(ciekawe) <- c("Gender",
   "LateForSchool",
   "PossessionsOwnRoom",
   "ParentsBelieveStudyingMathematicsIsImportant",
   "WorryThatMathsWillBeDifficult",
   "StudyMathsMoreThan2ExtraHoursADay",
   "FamiliarityWithMathsConceptsLinearEquation",
   "MathsTeachingTeacherHelps",
   "ThinksTeachersTreatStudentsFair",
   "MakeFriendsEasily",
   "ThinksSchoolIsWasteOfTime",
   "CouldPerformWellIfIWanted",
   "MathScore",
   "ReadScore",
   "ScienceScore")



write.csv(ciekawe, "polska2012pisa.csv")
