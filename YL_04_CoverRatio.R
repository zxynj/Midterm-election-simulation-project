rm(list=ls())
library(tidyverse)

### question 1a ####

dfraw = read_rds("YL_Q1a.rds") %>% as.tibble()
df = dfraw

df_cover = df %>% mutate(covered = ifelse((trueValue>=confiLower) & (trueValue<=confiUpper),TRUE,FALSE))
df_cover %>% filter(!covered)

sum(df_cover$covered)

res_1a = df_cover %>% group_by(sampleSize,method,type,trueValue) %>% summarise(coverRatio = sum(covered)/n()) %>% ungroup()
res_1a

### question 1b ####

dfraw = read_rds("YL_Q1b.rds") %>% as.tibble()
df = dfraw
df_cover = df %>% mutate(covered = ifelse((trueValue>=confiLower) & (trueValue<=confiUpper),TRUE,FALSE))

df_cover %>% filter(!covered)
sum(df_cover$covered)

res_1b = df_cover %>% group_by(sampleSize,method,type,trueValue) %>% summarise(coverRatio = sum(covered)/n()) %>% ungroup()
res_1b


### question 2 ####

dfraw = read_rds("YL_Q2.rds") %>% as.tibble()
df = dfraw
df_cover = df %>% mutate(covered = ifelse((trueValue>=confiLower) & (trueValue<=confiUpper),TRUE,FALSE))

df_cover %>% filter(!covered)
sum(df_cover$covered)

res_2 = df_cover %>% group_by(mixRatio,sampleSize,method,type,trueValue) %>% summarise(coverRatio = sum(covered)/n()) %>% ungroup()
res_2



#### combine

res_1a$question = "1a"
res_1b$question = "1b"
res_2$question = "2"


res = bind_rows(res_1a,res_1b,res_2)

res

write_rds(res,"YL_04_coverRatio.rds")


#### visualization 

rm(list=ls())
library(tidyverse)
library(plotly)

dfraw = read_rds("YL_04_coverRatio.rds")

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



