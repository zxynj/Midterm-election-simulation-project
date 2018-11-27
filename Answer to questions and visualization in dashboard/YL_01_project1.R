
rm(list=ls())
# common parameters

alpha = 0.05

r = c(0.34,0.33,0.33)
sampleSizeVector=c(500,1000,1500)
nrep=2000

####### method 2 paramenters

m = 3 
x = seq(1,4,length=1000)
y = 1 - 2*(1 - pnorm(x)) - 4*(m-2)*(1-pnorm(x*sqrt(2)))
a = min(x[y >= 1 - alpha])
#plot to show this
# plot(x,y,type="l")
# abline(h = 1-alpha,col="red")
# abline(v = a,col="red")


M = m*(m-1)/2
cv = qchisq(1-alpha/M,M-1)

## confidence Interval method 1&2

confiFun <- function(singleSample,type,i,j){
  N = sum(singleSample[1:3])
  # print(N)
  phat_i =singleSample[i] / N
  phat_j = singleSample[j] / N
  
  delta = phat_i- phat_j
  d_ij = phat_i + phat_j - delta^2
  
  # method 1 and method 2
  confiLower = c(delta - sqrt( cv * d_ij /N), delta -a/sqrt(N))
  confiUpper = c(delta + sqrt( cv * d_ij /N), delta + a/sqrt(N))
  return(data.frame(confiLower = confiLower,confiUpper = confiUpper,
                    type=type,method=c(1L,2L),trueValue = p[i]-p[j],
                    trialID=singleSample[[4]]))
}


confiDataFun <- function(sampleSize){
  set.seed(321)
  df = rbind(rmultinom(nrep, size = sampleSize, prob = p),
             trialID = 1:nrep)
  
  delta_12 = apply(df, 2,confiFun,type="p1-p2",i=1,j=2)
  delta_13 = apply(df, 2,confiFun,type="p1-p3",i=1,j=3)
  delta_23 = apply(df, 2,confiFun,type="p2-p3",i=2,j=3)
  
  res = Reduce(rbind,c(delta_12,delta_13,delta_23))
  res$sampleSize = sampleSize
  return(res)
}


###  Q 1a
p = c(0.5,0.3,0.2)

confiList = lapply(sampleSizeVector, confiDataFun)
confiData = Reduce(rbind,confiList)

head(confiData)
saveRDS(confiData,"YL_Q1a.rds")

### Q 1b

p = c(0.5,0.48,0.02)

confiList = lapply(sampleSizeVector, confiDataFun)
confiData = Reduce(rbind,confiList)

head(confiData)
saveRDS(confiData,"YL_Q1b.rds")


### Q 2


p = c(0.46,0.44,0.1)

confiMixDataFun <- function(sampleSize,q,r = c(0.34,0.33,0.33),nrep=2){
  set.seed(321)
  
  sampleSize_NV = rbinom(nrep, sampleSize, q)
  sampleSize_V = sampleSize - sampleSize_NV
  
  VList = lapply(sampleSize_V,function(x){
    rmultinom(1, size = x, prob = p)
  })
  Vmat = Reduce(cbind,VList) # transfer to matrix format
  
  NVList = lapply(sampleSize_NV,function(x){
    rmultinom(1, size = x, prob = r)
  })
  NVmat = Reduce(cbind,NVList) 
  dfmix = Vmat+ NVmat
  dfmix = rbind(dfmix,trailID=1:nrep)
  delta_12 = apply(dfmix, 2,confiFun,type="p1-p2",i=1,j=2)
  delta_13 = apply(dfmix, 2,confiFun,type="p1-p3",i=1,j=3)
  delta_23 = apply(dfmix, 2,confiFun,type="p2-p3",i=2,j=3)
  
  res = Reduce(rbind,c(delta_12,delta_13,delta_23))
  res$sampleSize = sampleSize
  return(res)
}

confiMixDataFun(sampleSize = 500,q=0.1,nrep=2)


resMix = lapply(seq(0.1,0.5,by=0.1), function(x){
  resList = lapply(sampleSizeVector, confiMixDataFun,q=x,nrep=2000)
  resDF = Reduce(rbind,resList)
  resDF$mixRatio = as.character(x) # Record q
  return(resDF)
})

resMix

#### interesting: 
#### 0.3 can only access by character
#### solution: convert all mix as character
#### the filter function can auto convert number to string at RHS


tmp = resMix[[3]]
tmp[tmp$mixRatio==0.3,]


restb = Reduce(rbind,resMix)
restb[restb$mixRatio==0.3,]

restb[restb$mixRatio=='0.3',]


unique(restb$mixRatio)


saveRDS(restb,'YL_Q2.rds')
