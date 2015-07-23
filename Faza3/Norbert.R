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


motyw <- theme_bw(base_family = "serif", base_size = 11*2) +
  theme(legend.background = element_blank(),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        strip.background = element_blank(),
        plot.background = element_blank(),
        axis.line = element_blank(),
        panel.grid = element_blank(),
        legend.position = "top")


ggplot(pisa2[!is.na(pisa2$MathsInterestEnjoyMaths),],aes(x=MathsInterestEnjoyMaths,y=math)) +
  geom_boxplot() +
  xlab("Do students like Math?")+
  ylab("Math score") +
  ggtitle("JAKIS TYTUL")+
  motyw

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
  scale_color_manual(values = wes.palette(5,"Darjeeling"),name="")+
  xlab("Do parents like Math?")+
  ylab(" ") +
  ggtitle("Hows parents attitude affect their children?")+
  guides(fill=guide_legend(title="Does student like Math?")) +
  motyw 


pisaE <- pisaE  %>% 
  group_by(MathsWorkEthicHomeworkCompletedInTime,MathsWorkEthicPayAttentionInClasses) %>%
  summarise(math=weighted.mean(math,weights))

ggplot(pisaE,aes(x=MathsWorkEthicHomeworkCompletedInTime,
                 y=MathsWorkEthicPayAttentionInClasses,
                 fill=math)) + 
  geom_raster() +
  scale_fill_gradient(low="red", high="green3") +
  xlab("Do students complete homeworks on time?")+
  ylab("Do students pay attention in classes?") +
  guides(fill=guide_legend(title="Math score")) +
  ggtitle("JAKIS TYTUL")

