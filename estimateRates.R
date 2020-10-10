## script that estimate hazards rates on a daily basis given a list of birth years and
## a list of data frames, this routine estimate hazards for individual birth cohorts

## assumes that this script is called from main.R and that there is a data.table
## that is called dead

#######################################################################
## split the data for faster access to the birth years in the
## estimateHazard routine below
setkey(dead,byear)
dsplit <- split(dead,by=c("byear"),flatten=FALSE)
byearList <- list()
for (i in 1:length(dsplit)){
    byearList[[i]] <- dsplit[[i]]$byear[1]
}
byearList <- unlist(byearList)


## define a funciton to estimate confidence intervals
## a more correct version where the CIs are based on samples from a multiviarate normal
hconfintMult <- function(fit){
    N <- 100000
    p <- summary(fit)$coefficients
    co <- vcov(fit)
    ra <- mvrnorm(N,p[2:4,1],co[2:4,2:4])
    c <- ra[,2]
    s <- ra[,3]
    amp <- sqrt(c**2+s**2)
    h <- exp(2*amp-ra[,1]*182.625)
    ci <- quantile(h,c(0.025,0.975))
    return(ci)
}


## allocate some output lists
opow <- list()
msurv <- list()
opowk <- list()
msurvk <- list()
hazards <- data.frame()
coeflist <- list()
coeflistk <- list()
cilist <- list()
cilistk <- list()
anovalist <- list()
anovalistk <- list()
hratio <- list()
hratiok <- list()
hazardsList <- list()
rkn <- 1
rkn2 <- 1
## loop over birth years
for (y in byears){
    ##hazards <- data.frame() ## reomve this later
    print(y)
    
    indx <- which(byearList==y)
    cohort <- dsplit[[indx]]
    
    ## need to count the total number of persons in the cohort
    scohort <- cohort[dyear>=(y+startAge)]
    Nmen <- sum(scohort$imputedSex=="M")
    Nwomen <- sum(scohort$imputedSex=="K")
    ## loop over sex
    for (kon in c("M","K")){ ## note that sex is also an internal varaible, so we use kon instead
        if (kon=="M"){
            Ntot <- Nmen
        } else{
            Ntot <- Nwomen
        }
        print(paste("sex: ", kon, "; N: ", Ntot))
        tmpk <- scohort[imputedSex==kon]
        
        ## count number of cases per day
        cdat <- tmpk[,.N,by=ddate]
        ## trick to handle the situation where there were 0 cases
        stdate <- as.Date(paste((y+startAge),"01-01",sep="-"))
        ##enddate <- as.Date(paste(y+ilength-1,"12-31",sep="-"))
        enddate <- as.Date(paste(y+startAge+nyears,"12-31",sep="-"))
        datedf <- data.frame(ddate=seq.Date(stdate,enddate,by=1),N=0)
        cdat <- merge(cdat,datedf,by="ddate",all.y=1)
        cdat$N.x[is.na(cdat$N.x)] <- 0
        cdat[,"N.y":=NULL]
        names(cdat) <- c("date","freq")
        cdat$month <- month(cdat$date)
        cdat$day <- yday(cdat$date)
        cdat <- cdat[order(date)]
        
        ## to find how many were alive at start of each day we just subtract the cumsum
        ## from the total number
        sdvec <- cumsum(cdat$freq)
        dvec <- cdat$freq
        survvec <- rep(Ntot,length(sdvec))
        survvec[2:length(survvec)] <- survvec[2:length(survvec)]-sdvec[1:(length(sdvec)-1)]
        ## estimate daily hazard rates as number of cases divided by number of exposed
        hest <- dvec/survvec
        
        ## hazards <- rbind(hazards,data.frame(h=hest,sex=kon,byear=y,cyear=caldata$year,month=caldata$month))
        tdf <- data.frame(h=hest,sex=kon,styear=y+startAge,byear=y,date=cdat$date,month=cdat$month,time=seq(1,length(hest)),freq=cdat$freq,nsurv=survvec)
        ##tdf$winter <- tdf$month<3| tdf$month==12
        
        ## much faster to do this at once later on
        ##hazards <- rbind(hazards,tdf)
        hazardsList[[rkn2]] <- tdf
        rkn2 <- rkn2+1
        ## fit a Posisson regression model to the log hazards
        fit0 <- glm(freq~1+time,offset=log(nsurv),family="poisson",data=tdf)
        fit <- glm(freq~1+time+cos(time/365.25*2*pi)+sin(time/365.25*2*pi),offset=log(nsurv),family="poisson",data=tdf)
        an <- anova(fit0,fit,test="LRT")
        ## estimate the confidence interval of the rate ratio
        ci <- hconfintMult(fit)
        ##pre <- exp(predict(fit,newdata=data.frame(res=ptemp,time=500)))
        ##pre <- (predict(fit,type="res"))
        ##           print(length(res))
        ##phazards <- rbind(phazards,data.frame(h=pre,sex=kon,byear=y))
        ## significance of fit
        if (kon=="M"){
            opow[[rkn]] <- sqrt(fit$coefficients[3]**2+fit$coefficients[4]**2)
            msurv[[rkn]] <- mean(tmpk$T)/365.25
            ## no R2 for GLMs
            ##r2[[rkn]] <- summary(fit)$r.squared
            ##nullr2[[rkn]] <- summary(fit0)$r.squared
            coeflist[[rkn]] <- fit$coefficients
            cilist[[rkn]] <- ci
            ##anovalist[[rkn]] <- an$"Pr(>F)"
            hratio[[rkn]] <- exp(2*opow[[rkn]]-coeflist[[rkn]][2]*182.625)
            anovalist[[rkn]] <- an$"Pr(>Chi)"[2]
        }else{
            opowk[[rkn]] <- sqrt(fit$coefficients[3]**2+fit$coefficients[4]**2)
            msurvk[[rkn]] <- mean(tmpk$T)/365.25
            ## no R2 for GLMs
            ##r2[[rkn]] <- summary(fit)$r.squared
            ##nullr2[[rkn]] <- summary(fit0)$r.squared
            coeflistk[[rkn]] <- fit$coefficients
            cilistk[[rkn]] <- ci
            hratiok[[rkn]] <- exp(2*opowk[[rkn]]-coeflist[[rkn]][2]*182.625)
            ##anovalist[[rkn]] <- an$"Pr(>F)"
            
            anovalistk[[rkn]] <- an$"Pr(>Chi)"[2]
        }
        
    }
    rkn <- rkn+1
}
hazards <- rbindlist(hazardsList)
hazards <- data.table(hazards)
##phazards <- data.table(phazards)
estimateRatesFlag <- 1

anovalist <- unlist(anovalist)
anovalistk <- unlist(anovalistk)
