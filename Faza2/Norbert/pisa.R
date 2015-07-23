con <- url("http://biecek.pl/MIMUW/PISAeurope.rda")
load(con)
pisa <- pisa[pisa[,1]=="Poland",]
library(ggplot2)
library(dplyr)
library(gridExtra)
#liczba ksiazek ST28Q01
# wynik z matematyki "PV1MATH" 
# i do mathematics because i enjoy it ST29Q04
# teacher shows interest ST77Q01

#"ST28Q01" %in% names(pisa)

pisa1 <- pisa[!is.na(pisa$ST28Q01),]

pisa2 <- pisa1[!is.na(pisa1$ST29Q04),]
pisa2 <- pisa2 %>% group_by(ST28Q01,ST77Q01) %>% summarise(n = sum(W_FSTUWT)) 
procenty <- pisa2 %>% group_by(ST28Q01) %>% summarise(k = sum(n)) 
pisa2[,3] <- pisa2[,3]/rep(procenty[,2],each=4)
pisa2 <- pisa2 %>% group_by(ST28Q01) %>% arrange(ST77Q01)


       
pisa3 <- pisa1[!is.na(pisa1$ST29Q04) && !is.na(pisa1$PV1MATH),]
pisa3$PV1MATH <- cut(pisa3$PV1MATH,c(0,400,500,600,700,1000))
pisa3 <- pisa3 %>% group_by(PV1MATH,ST29Q04) %>% summarise(n = sum(W_FSTUWT)) 
procenty <- pisa3 %>% group_by(PV1MATH) %>% summarise(k = sum(n)) 
pisa3[,3] <- round(pisa3[,3]/rep(procenty[,2],each=5)*100)
pisa3[is.na(pisa3[,2]),2] <- "NA"
t <- simplify2array(apply(pisa3,1, function(x) matrix(rep(x,x[3]),ncol=3,byrow=TRUE)))
pisa3 <- data.frame(do.call(rbind, t))
colnames(pisa3) <- c("PV1MATH","ST29Q04","n")


pisa4 <- pisa1[!is.na(pisa1$ST77Q01) && !is.na(pisa1$ST29Q04),]
pisa4 <- pisa4 %>% group_by(ST29Q04,ST77Q01) %>% summarise(n = sum(W_FSTUWT)) 
procenty <- pisa4 %>% group_by(ST29Q04) %>% summarise(k = sum(n)) 
pisa4[,3] <- round(pisa4[,3]/rep(procenty[,2],each=5)*100)


p1 <- ggplot(pisa1,aes(x=ST28Q01,y=PV1MATH)) + 
   theme( text = element_text(size = 16, family = "xkcd"))+
  geom_boxplot() + 
  xlab("Number of books") + ylab("Math score")


p2 <- ggplot(pisa2,aes(x=ST28Q01,y=ST77Q01,fill=n)) + 
   theme( text = element_text(size = 16, family = "xkcd"))+
  geom_raster() + 
  xlab(NULL) + ylab("Does teacher show interest?") +
  theme(axis.text.x = element_text(angle = 45,vjust=0.75))

p3 <- ggplot(pisa3,aes(x=PV1MATH,fill=ST29Q04)) + 
   theme( text = element_text(size = 16, family = "xkcd"))+
  geom_bar() + coord_flip() + 
  theme(legend.position="left") + labs(y = "Do you enjoy mathematics", x = "")

p4 <- ggplot(pisa4,aes(x=ST29Q04,y=ST77Q01)) + 
   theme( text = element_text(size = 16, family = "xkcd"))+
  geom_point(aes(size = n)) +
  theme(legend.position="left") + theme(axis.title.y=element_blank(),axis.title.x=element_blank())

Norbert <- grid.arrange(p3,p1,p4,p2,ncol=2,nrow=2)



library(showtext)
font.add("xkcd", "xkcd.ttf")
pdf("Norbert11.pdf", height = 12, width =16)
showtext.begin()
print(grid.arrange(p3,p1,p4,p2,ncol=2,nrow=2))
showtext.end()
dev.off()




