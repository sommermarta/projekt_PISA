library(ggplot2)
load("Dane/wczytajZmiennePisa2.rda")

# install.packages("wesanderson")

# library(stringi)
# library(dplyr)
library("wesanderson")
library(ggthemes)
library(dplyr)

pisa3 <- pisa2[, c("math","weights", "read","PerseveranceGiveUpEasily", "MathsInterestEnjoyMaths", "MathsInterestEnjoyReading")]

pisa3 <- na.omit(pisa3)
# pisa3 <- pisa3 %>% filter( math > 500)


ggplot(pisa3, aes(y = math, x =PerseveranceGiveUpEasily,
                  col=PerseveranceGiveUpEasily))+
   geom_point(size=2)+
   geom_boxplot()+
   # geom_smooth(method="lm",size=2)+ 
   scale_color_manual(values = wes.palette(5,"Darjeeling"), 
                      name="")+
   xlab(" ")+
   ylab("Math test score")+
   ggtitle("Is it like you to Give Up Easily?")+
   theme_bw(base_family = "serif", base_size = 11*2) + 
   theme(legend.background = element_blank(), 
         legend.key = element_blank(), 
         panel.background = element_blank(),
         panel.border = element_blank(), 
         strip.background = element_blank(),
         plot.background = element_blank(), 
         axis.line = element_blank(),
         panel.grid = element_blank(),
         legend.position = "top")+
   geom_jitter(alpha=0.5)


    # facet_grid( .~MathsInterestEnjoyReading)


