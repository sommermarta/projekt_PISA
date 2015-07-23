
library(shiny)
library(ggplot2)

shinyUI(fluidPage(
   
   # Application title
   titlePanel("PISA2012 polish children scores !"),
   
   
   sidebarLayout(
      sidebarPanel(
         selectInput("which.row", "Choose column grouping variable:", 
                     choices = c("Gender","LateForSchool",
                                 "PossessionsOwnRoom","ParentsBelieveStudyingMathematicsIsImportant",
                                 "WorryThatMathsWillBeDifficult","StudyMathsMoreThan2ExtraHoursADay",
                                 "FamiliarityWithMathsConceptsLinearEquation","MathsTeachingTeacherHelps",
                                 "ThinksTeachersTreatStudentsFair","MakeFriendsEasily",
                                 "ThinksSchoolIsWasteOfTime","CouldPerformWellIfIWanted")),
         # rows and columns have been misnamed
         
         
         selectInput("which.col", "Choose row grouping variable:", 
                     choices = c("Gender","LateForSchool",
                                 "PossessionsOwnRoom","ParentsBelieveStudyingMathematicsIsImportant",
                                 "WorryThatMathsWillBeDifficult","StudyMathsMoreThan2ExtraHoursADay",
                                 "FamiliarityWithMathsConceptsLinearEquation","MathsTeachingTeacherHelps",
                                 "ThinksTeachersTreatStudentsFair","MakeFriendsEasily",
                                 "ThinksSchoolIsWasteOfTime","CouldPerformWellIfIWanted")),
         
         selectInput("which.score", "Choose score variable:", 
                     choices = list("Math Score" = "MathScore",
                                 "Read Score" = "ReadScore",
                                 "Science Score" = "ScienceScore")),
         checkboxInput("margins", "Show margins", FALSE),
         sliderInput("bin",
                     "Width of binwidth:",
                     min = 10,
                     max = 100,
                     step = 10,
                     value = 20)
      ),
      
      
      mainPanel(
         tabsetPanel(type = "tabs", 
                     tabPanel("Histogram", plotOutput("distPlot")), 
                     tabPanel("Score summaries", verbatimTextOutput("summary"))
                     
         )
      )
   )))