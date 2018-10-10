
rm(list=ls())
# common parameters

alpha = 0.05

r = c(0.34,0.33,0.33)
sampleSizeVector=c(500,1000,1500)
nrep=10

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
  N = sum(singleSample)
  # print(N)
  phat_i =singleSample[i] / N
  phat_j = singleSample[j] / N
  
  delta = phat_i- phat_j
  d_ij = phat_i + phat_j - delta^2
  
  # method 1 and method 2
  confiLower = c(delta - sqrt( cv * d_ij /N), delta -a/sqrt(N))
  confiUpper = c(delta + sqrt( cv * d_ij /sampleSize), delta + a/sqrt(N))
  return(data.frame(confiLower = confiLower,confiUpper = confiUpper,
                    type=type,method=c(1L,2L),trueValue = p[i]-p[j]))
}


confiDataFun <- function(sampleSize){
  set.seed(321)
  df = rmultinom(nrep, size = sampleSize, prob = p)
  
  delta_12 = apply(df, 2,confiFun,type="p1-p2",i=1,j=2)
  delta_13 = apply(df, 2,confiFun,type="p1-p3",i=1,j=3)
  delta_23 = apply(df, 2,confiFun,type="p2-p3",i=2,j=3)
  
  res = Reduce(rbind,c(delta_12,delta_13,delta_23))
  res$sampleSize = sampleSize
  return(res)
}



p = c(0.5,0.3,0.2)

sampleSize=20

confiList = lapply(sampleSizeVector[1], confiDataFun)

confiData = Reduce(rbind,confiList)
nrow(confiData[confiData$confiLower< (-0.1),])


head(confiData)




### part 2
q = 0.1
nrep
sampleSize

sampleSize_NV = rbinom(nrep, sampleSize, q)

sampleSize_V = sampleSize - sampleSize_NV

df = rmultinom(nrep, size = sampleSize, prob = p)

VList = lapply(sampleSize_V,function(x){
  rmultinom(1, size = x, prob = p)
})

Vmat = Reduce(cbind,VList)

NVList = lapply(sampleSize_NV,function(x){
  rmultinom(1, size = x, prob = r)
})
NVmat = Reduce(cbind,NVList)

dfmix = Vmat+ NVmat
dfmix

tmp2 = apply(dfmix, 2,confiFun,type="p1-p2",i=1,j=2)
tmp2
