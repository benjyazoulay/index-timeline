library(plotly)
library(stringr)
library(ggplot2)
library(plotly)
library(ggrepel)
library(ggrepel)
index<-readLines("C:/Users/Benjamin/Desktop/index.txt")
index=str_replace_all(index,"([[:digit:]]) ([[:alpha:]])", "\\1\n\\2")

index
writeLines(index,"C:/Users/Benjamin/Desktop/index1.txt")

index<-read.csv("C:/Users/Benjamin/Desktop/index.csv",encoding = "UTF-8",sep = ",",header = F)

for (i in 3:length(index)) {
  index[,i]=str_replace_all(index[,i]," ","")
  index[,i]=str_replace_all(index[,i],"-",":")
  index[,i]=paste0(substr(index[,i],1,3),":",substr(index[,i],4,7))
}
for (i in 3:length(index)) {
  index[,i]=str_replace_all(index[,i],"::",":")
  index[,i]=str_replace_all(index[,i],":$","")
  index[,i]=str_replace_all(index[,i],"NA:NA","")
}
for (i in 3:length(index)) {
  index[,i]=str_replace_all(index[,i],":$","")
}
index$V16[1]="212"
# index$V3[22]="199,200"
index$V3[315]="155"
# index$V4[92]="230,231,232"
# index$V4[117]="140,141"
for (i in 3:length(index)) {
  for (j in 1:length(index$V1)) {
    if(str_detect(index[j,i],":")==T){
      print(str_c(j," ",i))
      a=as.integer(str_extract(index[j,i],"[:digit:]+"))
      b=str_remove(str_extract(index[j,i],":.+"),":")
      b=iconv(b,to="ASCII//TRANSLIT")
      b=abs(as.integer(b))
      print(a)
      print(b)
      index[j,i]=paste(seq(a,b),collapse=",")
      if(i>3){index[j,3]=str_c(index[j,3],",",index[j,i])}
    }
  }}
index$V3[7]="264,273"
index$V2[7]=""
index=cbind(index$V1,index$V2,index$V3)
index=as.data.frame(index)
colnames(index)=c("nom","prenom","pages")
index$nb=str_count(index$pages,",")+1
index$moyenne=str_split(index$pages,",")
for(i in 1:length(index$pages)){
  index$moyenne[i]=round(mean(as.integer(unlist(as.vector(index$moyenne[i])))))
}
index$nom=str_c(index$prenom," ",index$nom)
index$moyenne=as.integer(index$moyenne)
index=index[-175,]
# plot=plot_ly(index, x = ~moyenne, y = ~nb, text = ~nom, type = 'scatter', mode = 'markers',size = ~nb,sizes = c(10, 25),
#              marker = list(sizemode="diameter", opacity = 0))
# plot=plot%>%add_text()

plot=ggplot(index,aes(x=moyenne,y=nb))+geom_text_repel(label=index$nom,size=index$nb/2,angle=30)+theme_tufte()+theme(plot.background = element_rect(fill = 'white', colour = 'white'))
plot=ggplot(index,aes(x=moyenne,y=nb))+geom_text_repel(label=index$nom,size=2+log(index$nb),angle=20)+scale_y_continuous(trans='log')+theme_tufte()+theme(plot.background = element_rect(fill = 'white', colour = 'white'))
plot=ggplot(index,aes(x=moyenne,y=nb))+geom_text_repel(label=index$nom,size=2,angle=20)+scale_y_continuous(trans='log')+theme_tufte()+theme(plot.background = element_rect(fill = 'white', colour = 'white'))
plot

plot=ggplot(index,aes(x=moyenne,y=nb))+geom_text_repel(label=index$nom,size=2+log(index$nb),angle=20)+
  scale_y_continuous(trans='log')+
  theme_tufte()+theme(strip.text.y = element_text(angle = 0),
                      panel.spacing.x=unit(0, "lines"),
                      panel.spacing.y=unit(0, "lines"),
                      panel.margin.x=unit(0, "lines") , panel.margin.y=unit(0,"lines"),
                      axis.title = element_blank(),
                      axis.ticks = element_blank(),
                      panel.grid = element_blank(),
                      axis.text.y= element_blank(),
                      axis.ticks.y = element_blank(),
                      axis.text.x= element_blank(),
                      axis.ticks.x= element_blank(),
                      axis.line.x= element_blank(),
                      axis.line.y= element_blank(),plot.background = element_rect(fill = 'white', colour = 'white'))
plot+ggsave("C:/Users/Benjamin/Desktop/index.png",scale=4)
ggplotly(plot)
