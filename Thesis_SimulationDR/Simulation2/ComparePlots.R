no.sheet = 1:5
sheet.name = c("Industry","Education","NGO","Consultant","Government")


library("xlsx")
Means= do.call(rbind,lapply(no.sheet,function(x)
{
raw.dat=read.xlsx("Data.xlsx",sheetIndex=x)
colMeans(raw.dat,na.rm=TRUE)

}
))

Means.data=data.frame(Group=sheet.name,Means)
require(plyr)
require(reshape2)
plot.data= melt(Means.data)
library(ggplot2)
AllGroup=ggplot(plot.data,aes(y=value,x=variable,group=Group,color=Group))+
  geom_line(lwd=1.5) +
  xlab(" ")+ylab("Average Score") + theme(axis.text.x = element_text(angle = 15, hjust = 1))
ggsave(filename="All.pdf")

subset1 = subset(plot.data, Group%in%c("Industry","Education","NGO"))

sb1plot=ggplot(subset1,aes(y=value,x=variable,group=Group,color=Group))+
  geom_line(lwd=1.5) +
  xlab(" ")+ylab("Average Score") + theme(axis.text.x = element_text(angle = 15, hjust = 1))
ggsave(filename=paste0(paste(unique(subset1$Group),collapse="",sep="_"),".pdf"))

subset2 = subset(plot.data, Group%in%c("Industry","Consultant","Government"))

sb2plot=ggplot(subset2,aes(y=value,x=variable,group=Group,color=Group))+
  geom_line(lwd=1.5) +
  xlab(" ")+ylab("Average Score") + theme(axis.text.x = element_text(angle = 15, hjust = 1))
ggsave(filename=paste0(paste(unique(subset2$Group),collapse="",sep="_"),".pdf"))