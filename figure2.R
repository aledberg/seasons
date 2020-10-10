## Code to make Figure 2 in the paper, showing how well the sinusoidal
## model fits the data
## 
## A. Ledberg 2020 10 10 

## plot the data for three selected birth cohorts 
bylist <- c(1800,1850,1900)

## generate a data frame for plotting using ggplot2
pdat <- hazards[byear %in% bylist]

## need to filter the hazard data with a running line smoother (local mean)
ppdat <- data.frame()
## filter
ker <- rep(1,90)/90
for (s in c("M","K")){
    for (y in bylist){
        tmp <- pdat[sex==s & byear==y]
        tmp$floghaz <- filter((tmp$h),ker)
        ## test to subsample
        tmp <- tmp[seq(1,dim(tmp)[1],10)]
        tmp$floghaz <- log(tmp$floghaz)
        indx <- which(y == byears)
        if (s=="M")
            co <- coeflist[[indx]]
        else
            co <- coeflistk[[indx]]
        ##ta <- 1:dim(tmp)[1]
        ta <- tmp$time
        amp <- sqrt(co[3]**2+co[4]**2)
        theta <- atan(-co[3]/co[4])
        tmp$est <- co[1]-amp+co[2]*ta+amp*(1+cos(ta/365.25*2*pi+theta))
        ppdat <- rbind(ppdat,tmp)

    }
}
## set sizes for text
axistext <- 16
axistitle <- 20
striptext <- 16
legendtext <- 14
legendtitle <- 16
legendkey <- 16
fface <- "bold"
tface <- "plain"
tsize <- 6
linesize=0.5
linesize2=0.5
psize <- 0.1
require(scales)

## label the cohorts
alabs <- list()
abreaks <- list()
rkn <- 1
for (y in bylist){
    ds <- paste(y+66,"01-01",sep="-")
    abreaks[[rkn]] <- as.Date(ds)
    alabs[[rkn]] <- y
    rkn <- rkn+1
}
abreaks <- do.call("c",abreaks)


plotlist <- list()
rkn <- 1
rkn2 <- 1
for (y in bylist){
    for (s in c("M","K")){
        tdat <- ppdat[sex==s & byear==y]
        plotlist[[rkn]] <- ggplot(tdat,aes(x=date,y=floghaz))+theme_bw()+geom_line(linetype=2,color="black")+geom_point(size=psize,color="black")+theme(axis.text=element_text(size=axistext),axis.title=element_text(size=axistitle),panel.margin=unit(x=1,units="cm"),strip.text.x=element_text(size=striptext),legend.text=element_text(size=legendtext),legend.key.size=unit(legendkey, "points"),legend.title=element_text(size=legendtitle))+geom_line(aes(x=date,y=est),color="magenta",linetype=1,size=linesize2,alpha=1)+xlab("time (calendar year)")+ylab("log mortality rate")+annotate("text",x=abreaks[rkn2],y=max(tdat$floghaz,na.rm=1)-.4,label=paste("women born",y),check_overlap=FALSE,fontface=tface,size=tsize)
        rkn <- rkn+1
    }
    rkn2 <- rkn2+1
}
lay=rbind(c(1,1),c(2,2),c(3,3))
grid.arrange(plotlist[[2]],plotlist[[4]],plotlist[[6]],layout_matrix=lay)


