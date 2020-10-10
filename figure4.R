## Make Figure 4 which A: illustrate the decomposition, B: life duration for women, and C: life duration for men
## 
## A. Ledberg 2020 10 10

## First define functions used to evaluate the integrals in Eq 5 and 6 in the paper

gompsin <- function(x,amp=a1,phase=ph1,rate=r1,shape=s1){
    ##return(shape*exp(x/365.25*rate)*exp(amp*(1+cos(x/365.25*2*pi+phase))))
    return(shape*exp(x*rate)*exp(amp*(1+cos(x/365.25*2*pi+phase))))
}


## to estimate the mean we evaluate the integral numerically
estimateMean <- function(amp,phase,rate,shape){
    ta <- seq(0,365.25*50,by=1) 
    su <- 0
    plist <- list()
    hlist <- list()
    rkn <- 1
    for (t in ta){
        hgompsin <- integrate(gompsin,0,t,amp=amp,phase=phase,rate=rate,shape=shape)
        ##su <- su+t*exp(-hgompsin$value/365)
        gsin <- gompsin(t,amp=amp,phase=phase,rate=rate,shape=shape)
        su <- su+t*gsin*exp(-hgompsin$value)
        ##plist[[rkn]] <- exp(-hgompsin$value/365)
        plist[[rkn]] <- gsin*exp(-hgompsin$value)
        hlist[[rkn]] <- gsin
        rkn <- rkn+1
    }
    plist <- unlist(plist)
    hlist <- unlist(hlist)
    ## returns a list of results
    dum <- list()
    dum[[1]] <- su
    dum[[2]] <- plist
    dum[[3]] <- hlist
    return(dum)
}

## calculate the mean of the pure gompertz 
gompertzmean <- function(ta,rate,shape){
    dum <- unlist(lapply(ta,function(x) x*dgompertz(x,rate,shape)))
    return(sum(dum)/365.25)
}



## First an illustration of the lower bound for the mortality
## this is for women born 1800

bylist <- c(1800)

## generate a data frame for plotting using ggplot2
pdat <- hazards[byear %in% bylist]

##pdat <- pdat[order(sex,byear,cyear,month)]
ppdat <- data.frame()
## filter
ker <- rep(1,90)/90
for (s in c("K")){
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
        tmp$zest <- co[1]-amp+co[2]*ta
        tmp$uzest <- co[1]+amp+co[2]*ta
        ppdat <- rbind(ppdat,tmp)

    }
}

## then we produce the other plots showing the expected life duration given the models
## first estimate what the life durations would have been had the oscillations been absent
## estimate what the survival would have been if the oscillations were absent
gemsurv <- list()
ta <- 0:(60*365.25)
for (i in 1:length(coeflist)){
    co <- coeflist[[i]]
    phase <- atan(-co[4]/co[3])
    amp <- sqrt(co[3]**2+co[4]**2)
    rate <- co[2]
    shape <- exp(co[1]-amp)
    gemsurv[[i]] <- gompertzmean(ta,rate,shape)
}
gemsurv <- unlist(gemsurv)
## same for women
gemsurvk <- list()
for (i in 1:length(coeflist)){
    co <- coeflistk[[i]]
    phase <- atan(-co[4]/co[3])
    amp <- sqrt(co[3]**2+co[4]**2)
    rate <- co[2]
    shape <- exp(co[1]-amp)
    gemsurvk[[i]] <- gompertzmean(ta,rate,shape)
}
gemsurvk <- unlist(gemsurvk)

## also estimate the life expectancy give the correct model
## calculate the estimated survival based on the estimated
## models, mainly to check that the models are correct. 
emsurv <- list()
for (i in 1:length(coeflist)){
    co <- coeflist[[i]]
    phase <- atan(-co[4]/co[3])
    amp <- sqrt(co[3]**2+co[4]**2)
    rate <- co[2]
    shape <- exp(co[1]-amp)
    est <- estimateMean(amp,phase,rate,shape)
    emsurv[[i]] <- as.numeric(est[[1]])/365.25
}
emsurv <- unlist(emsurv)
## same for women
emsurvk <- list()
for (i in 1:length(coeflist)){
    co <- coeflistk[[i]]
    phase <- atan(-co[4]/co[3])
    amp <- sqrt(co[3]**2+co[4]**2)
    rate <- co[2]
    shape <- exp(co[1]-amp)
    est <- estimateMean(amp,phase,rate,shape)
    emsurvk[[i]] <- as.numeric(est[[1]])/365.25
}
emsurvk <- unlist(emsurvk)

axistext <- 14
axistitle <- 14
striptext <- 14
legendtext <- 12
legendtitle <- 14
legendkey <- 12
fface <- "bold"
tface <- "plain"
tsize <- 6
tsize2 <- 5
linesize=0.5
linesize2=0.5
linetype=1
psize <- 0.1

plotlist <- list()
rkn <- 1
rkn2 <- 1
for (y in bylist){
    for (s in c("K")){
        tdat <- ppdat[sex==s & byear==y]
        plotlist[[rkn]] <- ggplot(tdat)+theme_bw()+geom_line(aes(x=date,y=floghaz,color="data"),linetype=1)+theme(axis.text=element_text(size=axistext),axis.title=element_text(size=axistitle),panel.margin=unit(x=1,units="cm"),strip.text.x=element_text(size=striptext),legend.text=element_text(size=legendtext),legend.key.size=unit(legendkey, "points"),legend.title=element_blank(),legend.position=c(0.82,0.25),legend.background=element_blank())+geom_line(aes(x=date,y=est,color="full model"),linetype=1,size=linesize2,alpha=1)+geom_line(aes(x=date,y=zest,color="lower-bound mortality"),linetype=1,size=linesize2,alpha=1)+xlab("time (calendar year)")+ylab("log mortality rate")+annotate("text",x=as.Date("1860-01-01"),y=max(tdat$floghaz,na.rm=1)-.4,label="A",check_overlap=FALSE,fontface=fface,size=tsize)+scale_color_manual(values = c('data' = 'black','full model' = 'magenta','lower-bound mortality' = 'green'))
        rkn <- rkn+1
    }
    rkn2 <- rkn2+1
}

plot(plotlist[[1]])

sdat <- data.frame(byear=byears,surv=unlist(msurv),esurv=emsurv+59.5,maxsurv=gemsurv+59.5,sex="men")
sdat <- rbind(sdat,data.frame(byear=byears,surv=unlist(msurvk),esurv=emsurvk+59.5,maxsurv=gemsurvk+59.5,sex="women"))
sdat <- data.table(sdat)

 
psize <- 0.6
rkn <- 2
labs <- c("B","C")
for (s in (c("men","women"))){
    dum <- sdat[sex==s]
    plotlist[[rkn]] <- ggplot(data=dum)+geom_point(aes(x=byear,y=surv,color="data"),size=psize)+geom_line(aes(x=byear,y=esurv,color="full model"),linetype=1)+ylim(72,83)+geom_line(aes(x=byear,y=maxsurv,color="lower-bound mortality"),linetype=1)+geom_point(aes(x=byear,y=surv),color="black",size=psize)+theme_bw()+theme(axis.text=element_text(size=axistext),axis.title=element_text(size=axistitle),panel.margin=unit(x=1,units="cm"),strip.text.x=element_text(size=striptext),legend.text=element_text(size=legendtext),legend.key.size=unit(legendkey, "points"),legend.position=c(0.82,0.25),legend.title=element_blank(),legend.key = element_rect(colour = NA, fill = NA),legend.background=element_blank())+xlab("")+ylab(TeX("mean age (years)"))+annotate("text",x=1800,y=82,label=labs[rkn-1],check_overlap=FALSE,fontface=fface,size=tsize)+scale_color_manual(values = c('data' = 'black','full model' = 'magenta','lower-bound mortality' = 'green')) +annotate("text",x=1850,y=82,label=s,check_overlap=FALSE,fontface=tface,size=tsize2)
    rkn <- rkn+1
}


plot(plotlist[[3]])

## make also a panel showing the difference between the lower-bound and seasonal mortliaty
psize <- 0.6
rkn <- 4
labs <- c("B","C")
sdat$diff <- sdat$surv-sdat$maxsurv
plotlist[[rkn]] <- ggplot(sdat,aes(x=byear,y=diff,color=sex))+geom_line(linetype=1)+geom_point(size=psize)+theme_bw()+theme(axis.text=element_text(size=axistext),axis.title=element_text(size=axistitle),panel.margin=unit(x=1,units="cm"),strip.text.x=element_text(size=striptext),legend.text=element_text(size=legendtext),legend.key.size=unit(legendkey, "points"),legend.title=element_blank(),legend.position=c(0.85,0.4),legend.key = element_rect(colour = NA, fill = NA),legend.background=element_blank())+xlab("cohort birth year")+ylab(expression("seasonal contrib. (yrs)"))+annotate("text",x=1800,y=-0.3,label="D",check_overlap=FALSE,fontface=fface,size=tsize)+ylim(-4,0)

plot(plot_grid(plotlist[[1]],plotlist[[2]],plotlist[[3]],plotlist[[4]],nrow=4,align='v'))

