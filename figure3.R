## Code to make Figure 3 in the paper showing decrease of amplitudes
## of seasonal modluation
## 
## A. Ledberg 2020 10 10


## reformat the confidence intervals so that they can be included in the plot
upper <- list()
lower <- list()
for (i in 1:length(cilist)){
    upper[[i]] <- cilist[[i]][2]
    lower[[i]] <- cilist[[i]][1]
}
upper <- unlist(upper)
lower <- unlist(lower)
pdat <- data.frame(byear=byears,pow=unlist(hratio),upper=upper,lower=lower,sex="men")
upper <- list()
lower <- list()
for (i in 1:length(cilistk)){
    upper[[i]] <- cilistk[[i]][2]
    lower[[i]] <- cilistk[[i]][1]
}
upper <- unlist(upper)
lower <- unlist(lower)
pdat <- rbind(pdat,data.frame(byear=byears,pow=unlist(hratiok),upper=upper,lower=lower,sex="women"))

## set sizes for text
axistext <- 18
axistitle <- 18
striptext <- 14
legendtext <- 16
legendtitle <- 16
legendkey <- 14
fface <- "bold"
tface <- "plain"
tsize <- 6
linesize=1
linesize2=0.8
psize <- 1
require(scales)

p1 <- ggplot(pdat,aes(x=byear,y=pow,col=sex))+geom_ribbon(aes(ymin=lower,ymax=upper,fill=sex),alpha=0.3)+geom_line(linetype=2)+geom_point(size=psize)+theme_bw()+theme(axis.text=element_text(size=axistext),axis.title=element_text(size=axistitle),panel.margin=unit(x=1,units="cm"),strip.text.x=element_text(size=striptext),legend.text=element_text(size=legendtext),legend.key.size=unit(legendkey, "points"),legend.title=element_blank(),legend.position=c(0.8,0.8))+xlab("cohort birth year")+ylab("mortality rate ratio")+scale_y_continuous(breaks=seq(1, 2.2, 0.2),limits=c(1,2.2))

plot(p1)

