
library("showtext")
library("dplyr")
library("ggplot2")

con <- url("http://biecek.pl/MIMUW/PISAeurope.rda")
load(con)

spoznienie <- pisa$ST08Q01
entuzjazmnauczycieli <- pisa$SC26Q02
matkapracuje <- pisa$ST15Q01
ojciecpracuje <- pisa$ST19Q01
czymaszinternet <- pisa$ST26Q06
kraj <- pisa$CNT
math <- pisa$PV1MATH
wagi <- pisa$W_FSTUWT

dane <- as.data.frame(list(spoznienie, entuzjazmnauczycieli, matkapracuje,
                           ojciecpracuje, czymaszinternet, kraj, math, wagi))
names(dane) <- c("spoznienie", "entuzjazmnauczycieli", "matkapracuje",
                 "ojciecpracuje", "czymaszinternet", "kraj", "math",
                 "wagi")


levels(dane$matkapracuje) <- c("Working full-time", "Working part-time",
                               "Not working,\nbut looking for a job",
                               "Other\n e.g. home duties, retired")

############################# matka pracuje #########################

to <- dane %>%
   filter(kraj=="Poland") %>%
   group_by(spoznienie,matkapracuje) %>%
   summarise(sr=weighted.mean(math,wagi,na.rm=TRUE),
             liczba=n())

to <- na.omit(to)

Marta1 <- ggplot(to,aes(x=matkapracuje, y=spoznienie, fill=sr, 
              label=paste(round(sr),"pt")))+
   geom_raster()+
   geom_text(family="mono",size=7)+
   scale_fill_gradient("Math\ntest\nresult",low = "white", high = "red1")+
   theme(axis.text.y = element_text(colour="grey20",
                                    size=18, face="plain", family='xkcd'),
         axis.text.x = element_text(colour="grey20",
                                    size=18,face="plain",family='xkcd'),
         axis.title.x = element_text(size=23,face="plain",family='xkcd',
                                     colour="red1"),
         axis.title.y = element_text(size=23, family='xkcd',colour="red1"),
         panel.grid=element_blank(), 
         panel.background=element_blank(),
         legend.title=element_text(size=15, face="plain",family="xkcd"))+
   labs(x="\nWhat is your mother currently doing?", 
        y="\nIn the last two full weeks of school,\n how many times did you arrive late for school?\n")+
   coord_flip()

####################### entuzjazm nauczycieli #######################

dane2 <- na.omit(dane)

Marta2 <- ggplot(dane2, aes(x=entuzjazmnauczycieli,y=math))+
   theme(panel.border=element_rect(fill=NA,colour="gray"),
         axis.text.y = element_text(colour="grey20",
                                    size=20, face="plain", family='xkcd'),
         axis.text.x = element_text(colour="grey20",
                                    size=20,face="plain",family='xkcd'),
         axis.title.x = element_text(size=23,face="plain",family='xkcd',
                                     colour="red1",vjust=0),
         axis.title.y = element_text(size=23, family='xkcd',
                                     colour="red1", vjust=1.5),
         panel.grid.major.x=element_blank(),
         panel.grid.minor.x=element_blank(),
         panel.background=element_blank())+
   geom_boxplot(position="dodge")+
   labs(x="\nThink about the teachers in your school.\nDecide how much do you agree with the following statement:\n\'Teachers work with enthusiasm.\'",
        y="\nMath test result")

# pdf-y:

getwd()

font.add("xkcd", "xkcd.ttf")
pdf("Marta22.pdf", height = 12, width =12)
showtext.begin()
print(Marta2 )
showtext.end()
dev.off()

font.add("xkcd", "xkcd.ttf")
pdf("Marta11.pdf", height = 12, width =16)
showtext.begin()
print(Marta1)
showtext.end()
dev.off()

