## Code to reproduce Figure 1 in the paper
##
## A. Ledberg 2020 10 10

require(scales)

## select birth cohorts from 1800 to 1901
bylist <- seq(1800,1900,by=10)

## generate a data frame for plotting using ggplot2
pdat <- hazards[byear %in% bylist]

## need to filter the hazard data with a running line smoother (local mean)
ppdat <- data.frame()
## filter kernel
ker <- rep(1,60)/60
for (s in c("M","K")){
    for (y in bylist){
        tmp <- pdat[sex==s & byear==y]
        tmp$floghaz <- filter((tmp$h),ker)
        tmp$floghaz <- log(tmp$floghaz)
        ppdat <- rbind(ppdat,tmp)
    }
}

## set sizes for figure text
axistext <- 14
axistitle <- 16
striptext <- 16
legendtext <- 14
legendtitle <- 16
legendkey <- 16
fface <- "bold"
tface <- "plain"
tsize <- 5
xbreaks <- list()
blist <- seq(1860,1980,by=20)
rkn <- 1
for (y in blist){
    ds <- paste(y,"01-01",sep="-")
    xbreaks[[rkn]] <- as.Date(ds)
    rkn <- rkn+1
}
xbreaks <- do.call("c",xbreaks)
## label the cohorts
alabs <- bylist
abreaks <- list()
alist <- seq(1865,1965,by=10)
rkn <- 1
for (y in alist){
    ds <- paste(y,"01-01",sep="-")
    abreaks[[rkn]] <- as.Date(ds)
    rkn <- rkn+1
}
abreaks <- do.call("c",abreaks)
levels(ppdat$sex) <- c("men","women")

## do not need to plot data for each day, suffices to plot for each week
ppdat$week <- week(ppdat$date)
ppdat$year <- year(ppdat$date)
ppdat <- ppdat[order(byear,year,month,week)]

indx <- (ppdat$time %% 14)==0
subppdat <- ppdat[indx,]


## split into two plots to make oscillations more visible
p1 <- ggplot(data=subppdat[byear > 1801 & byear < 1860],aes(x=date,y=floghaz,col=sex,shape=factor(byear)))+geom_line()+theme_bw()+theme(axis.text=element_text(size=axistext),axis.title=element_text(size=axistitle),panel.margin=unit(x=1,units="cm"),strip.text.x=element_text(size=striptext),legend.text=element_text(size=legendtext),legend.key.size=unit(legendkey, "points"),legend.title=element_blank(),legend.position=c(0.1,0.85)) + scale_x_date(breaks=xbreaks, labels=blist)+xlab("time (calendar year)")+ylab("log mortality rate")+annotate("text",x=abreaks[2:6],y=-10.2,label=alabs[2:6],check_overlap=FALSE,fontface=tface,size=tsize)

p2 <- ggplot(data=subppdat[byear >= 1860],aes(x=date,y=floghaz,col=sex,shape=factor(byear)))+geom_line()+theme_bw()+theme(axis.text=element_text(size=axistext),axis.title=element_text(size=axistitle),panel.margin=unit(x=1,units="cm"),strip.text.x=element_text(size=striptext),legend.text=element_text(size=legendtext),legend.key.size=unit(legendkey, "points"),legend.title=element_blank(),legend.position=c(0.1,0.85)) + scale_x_date(breaks=xbreaks, labels=blist)+xlab("time (calendar year)")+ylab("log mortality rate")+annotate("text",x=abreaks[7:11],y=-10.6,label=alabs[7:11],check_overlap=FALSE,fontface=tface,size=tsize)

lay=rbind(c(1,1),c(2,2))
pg <- grid.arrange(p1,p2,layout_matrix=lay)
plot(pg)

