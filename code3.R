#lm code for Tables 1
library(nlme)
data1<-read.table("Data1-FDis.csv",header = TRUE, sep = ",")
data1$year<-factor(data1$year)
data1$block<-factor(data1$block)
fit<-lme(FDis~NAR:duration, random=~1|year/block,data1)
summary(fit)

#code for Fig.1
library(patchwork)
library(plotrix)
library(ggplot2)
data<-read.csv("Data2-effect.csv",header = T)
data$Duration<-factor(data$Duration)
data$Effect<-factor(data$Effect,levels = c("Effect of ME addition on Soil ME content",
                                           "Effect of Fe addition on Soil Fe content",
                                           "Effect of Mg addition on Soil Mg content",
                                           "Effect of Ca addition on Soil Ca content",
                                           "Effect of K addition on Soil K content",
                                           "Effect of S addition on Soil S content",
                                           "Effect of P addition on Soil P content",
                                           "Effect of N addition on Soil N content"))
ggplot(data, aes(x=Ratio, y=Effect,fill=Duration)) +
  scale_fill_manual(values = c("#fc8708", "#13a6db"))+
  geom_errorbar(width=.15,cex=0.6,aes(xmin = Ratio-Se,xmax = Ratio+Se))+
  geom_point(color="black",shape=21,size=4)+
  theme_bw()+
  geom_vline(aes(xintercept=0),colour="Black",size=1,linetype="dashed")+xlab("Relative effect ratio (%)")+
  theme(axis.text.x=element_text(size=11,color="black",face="bold",angle=90), 
        axis.text.y=element_text(size=11,color="black",face="bold"), 
        axis.title=element_text(size=11,face="bold"),text=element_text(size=11))+
  theme(text=element_text(size=11,  family="sans", face="bold"))+
  theme(legend.text = element_text(colour="black", size = 11, face = "bold"),
        legend.title =element_blank(),
        legend.position = ("top"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x=element_text(angle=0),
        panel.border = element_rect(colour = "black")) +
  theme(strip.text = element_text(size=11,face="bold"))
ggsave("Figure 1.tiff",width=15,height=10,units = "cm",dpi=300)

#code for Fig.2
library(patchwork)
library(plotrix)
library(ggplot2)
data<-read.csv("Data2-effect.csv",header = T)
data$duration<-factor(data$duration)
data$effect<-factor(data$effect,levels = c("Effect of ME addition on CWM-ME content",
                                           "Effect of Fe addition on CWM-Fe content",
                                           "Effect of Mg addition on CWM-Mg content",
                                           "Effect of Ca addition on CWM-Ca content",
                                           "Effect of K addition on CWM-K content",
                                           "Effect of S addition on CWM-S content",
                                           "Effect of P addition on CWM-P content",
                                           "Effect of N addition on CWM-N content"))
ggplot(data, aes(x=ratio, y=effect,fill=duration)) +
  scale_fill_manual(values = c("#fc8708", "#13a6db"))+
  scale_y_discrete()+
  geom_errorbar(width=.15,cex=0.6,aes(xmin = ratio-se,xmax = ratio+se))+
  geom_point(color="black",shape=21,size=4)+
  theme_bw()+
  geom_vline(aes(xintercept=0),colour="Black",size=1,linetype="dashed")+xlab("Relative effect ratio (%)")+
  theme(axis.text.x=element_text(size=11,color="black",face="bold",angle=90), 
        axis.text.y=element_text(size=11,color="black",face="bold"), 
        axis.title=element_text(size=11,face="bold"),text=element_text(size=11))+
  coord_cartesian(xlim = c(-15, 30))+
  theme(text=element_text(size=11,  family="sans", face="bold"))+
  theme(legend.text = element_text(colour="black", size = 11, face = "bold"),
        legend.title =element_blank(),
        legend.position = ("top"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x=element_text(angle=0),
        panel.border = element_rect(colour = "black")) +
  theme(strip.text = element_text(size=11,face="bold"))
ggsave("Figure 2.tiff",width=15,height=10,units = "cm",dpi=300)

#code for Fig.3
library(ggplot2)
library(nlme)
library(colorRamps)
library(reshape2)
library(scales)
library(plotrix)
library(patchwork)
data<-read.table("Data3-correlation.csv",header = TRUE, sep = ",")
data$Nutrients<-factor(data$Nutrients,levels = c('N','P','S','K','Ca','Mg','Fe','ME'))
ggplot(data, aes(x=Ratio, y=ratio)) +
  geom_errorbar(width=.15,cex=0.6,aes(xmin = Ratio-Se,xmax = Ratio+Se))+
  geom_errorbar(width=.15,cex=0.6,aes(ymin = ratio-se,ymax = ratio+se))+
  geom_smooth(method = 'lm',se = TRUE, level=0.95,color="black",size=1)+
  geom_point(color="black",aes(x=Ratio, y=ratio,fill=Nutrients),shape=21,size=3)+
  scale_fill_manual( values = c("#e72319", "#ff7425","#c2bf32","#009e73", 
                                "#c1dff2","#0072b2","#ea18cb", "#880b85"))+
  xlab(bquote('Effects on soil nutrient contents (%)'))+
  ylab(bquote('Effects on plant nutrient concentrations (%)'))+
  annotate("text",x=25,y=60,parse=TRUE,label='atop(italic(R^2)==0.64)',size=5)+
  coord_cartesian(xlim=c(-25,200),ylim = c(-10, 80))+
  theme_bw()+
  theme(text=element_text(size=12,  family="sans", face="bold"))+
  theme(axis.text=element_text(size=12,face="bold"),
        axis.title=element_text(size=12,face="bold"))
ggsave("Figure 3.tiff",width=13,height=10,units = "cm",dpi=300)

#code for Fig.4
library(ggplot2)
library(nlme)
library(colorRamps)
library(reshape2)
library(scales)
library(plotrix)
library(patchwork)
data<-read.table("Data1-FDis.csv",header = TRUE, sep = ",")
data$year<-factor(data$year)
top_bar <- function(FDis){  
  return(mean(FDis)+std.error(FDis)) 
}
bottom_bar <- function(FDis){
  return(mean(FDis)-std.error(FDis))
}
plot1<-ggplot(data=data,aes(x=NAR,y=FDis))+
  stat_summary(geom = 'point',size=4)+
  stat_summary(geom = 'errorbar',width=.15,cex=0.6,fun.min = bottom_bar,fun.max = top_bar)+
  xlab(bquote('Number of added nutrients'))+
  ylab(bquote('Plant multi-dimensional stoichiometric divergence'))+
  annotate("text",x=3,y=2,parse=TRUE,label=bquote('a~(3~year~means)'),size=7)+
  scale_x_continuous(breaks=0:8*1)+coord_cartesian(ylim = c(0.5, 2))+
  theme_bw()+
  theme(text=element_text(size=20,  family="sans", face="bold"))+
  theme(axis.text=element_text(size=16,face="bold"),
        axis.title=element_text(size=16,face="bold"))
plot1
plot2<-ggplot(data=data,aes(x=NAR,y=FDis,fill=year))+
  scale_fill_manual( values = c("#0072b2", "#009e73","#e69f00"))+
  stat_summary(geom = 'errorbar',width=.15,cex=0.6,fun.min = bottom_bar,fun.max = top_bar)+
  stat_summary(geom = 'point',size=4,color="black",shape=21)+
  xlab(bquote('Number of added nutrients'))+
  annotate("text",x=0,y=2,parse=TRUE,label=bquote('b'),size=7)+
  scale_x_continuous(breaks=0:8*1)+coord_cartesian(ylim = c(0.5, 2))+
  theme_bw()+
  theme(text=element_text(size=20,  family="sans", face="bold"))+
  theme(axis.text.x=element_text(size=16,face="bold"),axis.text.y=element_blank(), 
        axis.title.x=element_text(size=16,face="bold"),axis.title.y=element_blank())
plot2
plot1+plot2+ plot_layout(ncol = 2, byrow = TRUE)
ggsave("Figure 4.tiff",width=21,height=18,units = "cm",dpi=300)

