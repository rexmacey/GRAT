setwd("C:/Users/Rex/Documents/Quant Trading/GRAT")
library(quantmod)
library(xts)
library(FinCal)
spx.xts<-readRDS(file="spx_xts.rds")
spx.ret<-dailyReturn(spx.xts,leading = FALSE)
spx.ret<-spx.ret[complete.cases(spx.ret)]

trial<-function(duration,investment,leverage,fee.long,fee.short,threshold,
                grat.pmt,fee.idx=0,ret.method="randhist",taxrate=0.4,aswas=1){
    rgf<-function(i=1,method="randhist"){ #return generating function
        if (method=="randhist") {
            out<-as.numeric(sample(spx.ret,days,replace=TRUE))
        } else if (method=="aswashist"){
            out<-as.numeric(spx.ret[i:(i+days-1)])
        } 
        return(out)
    }
    out<-list()
    
    out$long.val<-investment  #3x/-3x
    out$short.val<-investment
    out$heir.ttl<-0
    out$grantor.val<-0
    out$bh.heir.val<-investment*2  # buy and hold
    out$bh.grantor.val<-0
    out$not.long.val<-investment #no threshold
    out$not.short.val<-investment
    out$not.grantor.val<-0
    out$pretax.ttl <- 0
    out$bh.pretax.ttl <- 0
    out$not.pretax.ttl <- 0
    out$afttax.ttl <- 0
    out$bh.afttax.ttl <- 0
    out$not.afttax.ttl <- 0
    out$maxlong<-0 # used to determine best threshold
    out$maxshort<-0
    out$threshold.day<-NA
    
    use.leverage<-TRUE
    
    threshold.val<-investment*(1+threshold)
    days<-252*duration
    grat.pmt.ttl<-0
    market.ret<-rgf(i=aswas,method=ret.method)
    for (i in 1:days){
        mkt.adj1<-market.ret[i]-fee.idx/100/252
        out$grantor.val     <- out$grantor.val*(1+mkt.adj1)
        out$not.grantor.val <- out$not.grantor.val*(1+mkt.adj1)
        out$bh.heir.val     <- out$bh.heir.val*(1+mkt.adj1)
        out$bh.grantor.val  <- out$bh.grantor.val*(1+mkt.adj1)
        out$not.long.val    <- out$not.long.val*(1+leverage*market.ret[i]-fee.xlong/100/252)
        out$not.short.val   <- out$not.short.val*(1-leverage*market.ret[i]-fee.xshort/100/252)
        if (out$not.long.val+grat.pmt.ttl>out$maxlong) out$maxlong<-out$not.long.val+grat.pmt.ttl
        if (out$not.short.val+grat.pmt.ttl>out$maxshort) out$maxshort<-out$not.short.val+grat.pmt.ttl
        if (use.leverage) {
            out$long.val      <- out$long.val*(1+leverage*market.ret[i]-fee.xlong/100/252)
            out$short.val     <- out$short.val*(1-leverage*market.ret[i]-fee.xshort/100/252)
        } else {
            out$long.val  <- out$long.val*(1+mkt.adj1)
            out$short.val <- out$short.val*(1+mkt.adj1)
        }
        if ((out$long.val >= threshold.val) | (out$short.val>=threshold.val)){
            use.leverage<-FALSE
            out$threshold.day<-i
        }
        if (i %% 252 == 0) {
            out$grantor.val    <- out$grantor.val+min(grat.pmt,out$long.val)+min(grat.pmt,out$short.val)
            out$long.val       <- out$long.val-min(grat.pmt,out$long.val)
            out$short.val      <- out$short.val-min(grat.pmt,out$short.val)
            out$not.grantor.val<- out$not.grantor.val+min(grat.pmt,out$not.long.val)+min(grat.pmt,out$not.short.val)
            out$not.long.val   <- out$not.long.val-min(grat.pmt,out$not.long.val)
            out$not.short.val  <- out$not.short.val-min(grat.pmt,out$not.short.val)
            out$bh.grantor.val <- out$bh.grantor.val+min(2*grat.pmt,out$bh.heir.val)
            out$bh.heir.val    <- out$bh.heir.val-min(2*grat.pmt,out$bh.heir.val) # use 2x because all money in 1 acct
            threshold.val<-threshold.val-grat.pmt
            grat.pmt.ttl<-grat.pmt.ttl+grat.pmt
        }
    }
    out$pretax.ttl <- out$long.val+out$short.val+out$grantor.val
    out$heir.ttl<-out$long.val+out$short.val
    out$bh.pretax.ttl <- out$bh.heir.val+out$bh.grantor.val
    out$not.pretax.ttl <- out$not.long.val+out$not.short.val+out$not.grantor.val
    out$afttax.ttl <- out$long.val+out$short.val+out$grantor.val*(1-taxrate)
    out$bh.afttax.ttl <- out$bh.heir.val+out$bh.grantor.val*(1-taxrate)
    out$not.afttax.ttl <- out$not.long.val+out$not.short.val+out$not.grantor.val*(1-taxrate)
    return(out)
}

duration<-2
investment<-1000000 #amount into EACH of 2 grats
leverage<-3
fee.xlong<-0.95
fee.xshort<-0.95
fee.idx<-0.10
threshold<- .25
required.rate<-0.02
grat.pmt<-pmt(required.rate,n=duration,-investment,0,0)
set.seed(101)
n.trials=3000

result<-t(replicate(n.trials,trial(duration,investment,leverage,fee.long,fee.short,threshold,grat.pmt,fee.idx,"randhist",taxrate=0.4,aswas=1)))
cnames<-colnames(result)
result<-matrix(unlist(result),nrow=n.trials,ncol=length(cnames),dimnames = list(NULL,cnames))

n.trials=1000
thresholds=c(.15,.2,.21,.22,.23,.24,.25,.26,.27,.28,.29,.3)
threshold.analysis=matrix(0,ncol=2,nrow=length(thresholds),dimnames = list(thresholds,c("MeanDiff","MedianDiff")))
# let's find best threshold
for (x in thresholds){
    result<-t(replicate(n.trials,trial(duration,investment,leverage,fee.long,fee.short,x,grat.pmt,fee.idx,"randhist",taxrate=0.4,aswas=1)))
    cnames<-colnames(result)
    result<-matrix(unlist(result),nrow=n.trials,ncol=length(cnames),dimnames = list(NULL,cnames))
    threshold.analysis[as.character(x),"MeanDiff"] <- mean(result[,"afttax.ttl"])-mean(result[,"bh.afttax.ttl"])
    threshold.analysis[as.character(x),"MedianDiff"] <- median(result[,"afttax.ttl"])-median(result[,"bh.afttax.ttl"])    
}
threshold.analysis

n.trials=length(spx.ret)-duration*252+1
result2<-t(sapply(1:n.trials,function (x) trial(duration,investment,leverage,fee.long,fee.short,threshold,grat.pmt,fee.idx,"aswashist",taxrate=0.4,aswas=x)))
result2<-matrix(unlist(result2),nrow=n.trials,ncol=length(cnames),dimnames = list(NULL,cnames))

print("Results of 3000 Trials")
summary(result)
print("Threshold Analysis")
threshold.analysis
print("Historical Analysis")
summary(result2)
