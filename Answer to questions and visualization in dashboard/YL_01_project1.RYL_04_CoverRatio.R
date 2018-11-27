rm(list=ls())
library(tidyverse)

### question 1a ####

dfraw = read_rds("YL_Q1a.rds") %>% as.tibble()
df = dfraw

df_cover = df %>% mutate(covered = ifelse((trueValue>=confiLower) & (trueValue<=confiUpper),TRUE,FALSE))
df_cover %>% filter(!covered)

sum(df_cover$covered)

ind_1a = df_cover %>% group_by(sampleSize,method,type,trueValue) %>% summarise(coverRatio = sum(covered)/n()) %>% ungroup()
ind_1a

#### jointly ####

df_joint = df_cover %>% group_by(sampleSize,method,trialID) %>% summarise(jointCover = all(covered)) %>% ungroup()
joint_1a = df_joint %>% group_by(sampleSize,method) %>% summarise(jointRatio = sum(jointCover)/n()) %>% ungroup()
joint_1a



### question 1b ####

dfraw = read_rds("YL_Q1b.rds") %>% as.tibble()
df = dfraw
df_cover = df %>% mutate(covered = ifelse((trueValue>=confiLower) & (trueValue<=confiUpper),TRUE,FALSE))

df_cover %>% filter(!covered)
sum(df_cover$covered)

ind_1b = df_cover %>% group_by(sampleSize,method,type,trueValue) %>% summarise(coverRatio = sum(covered)/n()) %>% ungroup()

#### jointly ####

df_joint = df_cover %>% group_by(sampleSize,method,trialID) %>% summarise(jointCover = all(covered)) %>% ungroup()
joint_1b = df_joint %>% group_by(sampleSize,method) %>% summarise(jointRatio = sum(jointCover)/n()) %>% ungroup()
joint_1b


### question 2 ####

dfraw = read_rds("YL_Q2.rds") %>% as.tibble()
df = dfraw
df_cover = df %>% mutate(covered = ifelse((trueValue>=confiLower) & (trueValue<=confiUpper),TRUE,FALSE))

df_cover %>% filter(!covered)
sum(df_cover$covered)

ind_2 = df_cover %>% group_by(mixRatio,sampleSize,method,type,trueValue) %>% summarise(coverRatio = sum(covered)/n()) %>% ungroup()


#### jointly ####

df_joint = df_cover %>% group_by(mixRatio,sampleSize,method,trialID) %>% summarise(jointCover = all(covered)) %>% ungroup()
joint_2 = df_joint %>% group_by(mixRatio,sampleSize,method) %>% summarise(jointRatio = sum(jointCover)/n()) %>% ungroup()
joint_2



#### combine

ind_1a
joint_1a

ind_1a$question = "1a"
ind_1b$question = "1b"
ind_2$question = "2"

res = bind_rows(ind_1a,ind_1b,ind_2)

res

write_rds(res,"YL_04_coverRatio_ind.rds")


joint_1a$question = "1a"
joint_1b$question = "1b"
joint_2$question = "2"



res = bind_rows(joint_1a,joint_1b,joint_2)

res

write_rds(res,"YL_04_coverRatio_joint.rds")


#### visualization 

rm(list=ls())
library(tidyverse)
library(plotly)

dfraw = read_rds("YL_04_coverRatio_ind.rds")

df = dfraw %>% mutate(trueValue = as.factor(trueValue))

df$trueValue[1]

### question 1 two columns
df1 = df %>% filter(question %>% str_detect("1"))
df1

i = 1
plot_ly(data = df1 %>% filter(method==i),x=~sampleSize,y=~coverRatio,color=~trueValue,type = 'bar') %>% 
  layout(title=paste("Coverage Ratio for","Method",i),yaxis=list(title = "Coverage Ratio",range=c(0.9,1)))


i = 2
plot_ly(data = df1 %>% filter(method==i),x=~sampleSize,y=~coverRatio,color=~trueValue,type = 'bar') %>% 
  layout(title=paste("Coverage Ratio for","Method",i),yaxis=list(title = "Coverage Ratio",range=c(0.9,1)))


### question 2 ###

df2 = df %>% filter(question %>% str_detect("2"))
df2

### three columns, for three trueValue or type

tvals = df2 %>% pull(trueValue) %>% unique() %>% sort()
tvals

i=1
tval = tvals[2]

plot_ly(df2 %>% filter(trueValue==tval,method==1),x=~sampleSize,y=~coverRatio,color=~mixRatio,type="bar") %>% 
  layout(title=paste("Original true Value:",tval,"\t Method",i),yaxis=list(title = "Coverage Ratio"))


p1 = plot_ly(df2 %>% filter(trueValue==tval,method==1),x=~sampleSize,y=~coverRatio,color=~mixRatio,type="bar")
p2 = plot_ly(df2 %>% filter(trueValue==tval,method==2),x=~sampleSize,y=~coverRatio,color=~mixRatio,type="bar")



#### joint plot #### 


dfjointraw = read_rds("YL_04_coverRatio_joint.rds")
dfjoint = dfjointraw %>% mutate(method = as.factor(method))


### question 1 two columns
dfjoint1 = dfjoint %>% filter(question %>% str_detect("1"))
dfjoint1

i = "1a"
plot_ly(data = dfjoint1 %>% filter(question==i),
        x=~sampleSize,y=~jointRatio,color=~method,type = 'bar',name=~paste("method",method)) %>% 
  layout(title=paste("Joint Coverage Ratio for","Question",i),yaxis=list(title = "Joint Coverage Ratio",range=c(0.9,1)))

i = "1b"
plot_ly(data = dfjoint1 %>% filter(question==i),
        x=~sampleSize,y=~jointRatio,color=~method,type = 'bar',name=~paste("method",method)) %>% 
  layout(title=paste("Joint Coverage Ratio for","Question",i),yaxis=list(title = "Joint Coverage Ratio",range=c(0.9,1)))


##### question 2 #####

dfjoint2 = dfjoint %>% filter(question %>% str_detect("2"))
dfjoint2

i = 1
plot_ly(data = dfjoint2 %>% filter(method==i),
        x=~sampleSize,y=~jointRatio,color=~mixRatio,type = 'bar',name=~paste("MixRatio",mixRatio)) %>% 
  layout(title=paste("Joint Coverage Ratio for","Method",i),yaxis=list(title = "Joint Coverage Ratio"))

i = 2
plot_ly(data = dfjoint2 %>% filter(method==i),
        x=~sampleSize,y=~jointRatio,color=~mixRatio,type = 'bar',name=~paste("MixRatio",mixRatio)) %>% 
  layout(title=paste("Joint Coverage Ratio for","Method",i),yaxis=list(title = "Joint Coverage Ratio"))







