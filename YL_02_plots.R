rm(list=ls())
library(tidyverse)
# library(plotly)

ofolder = "YL_02_plots/"

# Question 1 a
dfraw = read_rds("YL_Q1a.rds") %>% as.tibble()
filePrefix = "Q1a"

# assign ID for plotting
df = dfraw %>% mutate(method=as.factor(method))

tv = df %>% pull(trueValue) %>% unique()
sv = df %>% pull(sampleSize) %>% unique()
sv

delta = df %>% pull(type) %>% unique()

ggplot(df %>% filter(sampleSize==500,trueValue==0.2),aes(x=trialID)) +
  geom_segment(aes(x=trialID,y=confiLower,xend=trialID,yend=confiUpper),color='orange1') +
  geom_hline(yintercept = 0.2)


df_sort = df %>% group_by(sampleSize,method,trueValue) %>% arrange(desc(confiUpper)) %>% mutate(sortID = 1:n()) %>% ungroup()

s=500;t=0.2
ggplot(df_sort %>% filter(sampleSize==s,trueValue==t),aes(x=sortID)) +
  geom_ribbon(aes(ymin= confiLower,ymax = confiUpper,fill=method),alpha=0.5)+
  geom_hline(yintercept = t,size=1,color="dodgerblue4")+
  scale_y_continuous(breaks=seq(-0.25,0.65,0.1),limits=c(-0.25, 0.65)) +
  theme_minimal() + theme(legend.position = 'bottom') +
  ggtitle(paste(filePrefix,'\t Sample size:',s,"\t True value:",t))


for (s in sv) {
  for (t in tv) {
    g = ggplot(df_sort %>% filter(sampleSize==s,trueValue==t),aes(x=sortID)) +
      geom_ribbon(aes(ymin= confiLower,ymax = confiUpper,fill=method),alpha=0.5)+
      geom_hline(yintercept = t,size=1,color="dodgerblue4")+
      scale_y_continuous(breaks=seq(-0.25,0.65,0.1),limits=c(-0.25, 0.65)) +
      theme_minimal() + theme(legend.position = 'bottom') +
      ggtitle(paste(filePrefix,'\t Sample size:',s,"\t True value:",t))
    ggsave(paste0(ofolder,filePrefix,'_',s,'_',t*100,'.png'),g)
        
  }
}


# Question 1 b
dfraw = read_rds("YL_Q1b.rds") %>% as.tibble()
filePrefix = "Q1b"

# assign ID for plotting
df = dfraw %>% group_by(sampleSize,method,trueValue) %>% mutate(ID = 1:n()) %>% ungroup() %>% mutate(method=as.factor(method))
df %>% filter(sampleSize==1000)
tv = df %>% pull(trueValue) %>% unique()
sv = df %>% pull(sampleSize) %>% unique()

# ggplot(df %>% filter(sampleSize==500,trueValue==0.2),aes(x=ID)) + 
#   geom_segment(aes(x=ID,y=confiLower,xend=ID,yend=confiUpper),color='orange1') +
#   geom_hline(yintercept = 0.2)


df_sort = df %>% group_by(sampleSize,method,trueValue) %>% arrange(desc(confiUpper)) %>% mutate(sortID = 1:n()) %>% ungroup()

for (s in sv) {
  for (t in tv) {
    g = ggplot(df_sort %>% filter(sampleSize==s,trueValue==t),aes(x=sortID)) +
      geom_ribbon(aes(ymin= confiLower,ymax = confiUpper,fill=method),alpha=0.5)+
      scale_y_continuous(breaks=seq(-0.25,0.65,0.1),limits=c(-0.25, 0.65)) +
      geom_hline(yintercept = t,size=1,color="dodgerblue4")+
      theme_minimal() + theme(legend.position = 'bottom') +
      ggtitle(paste(filePrefix,'\t Sample size:',s,"\t True value:",t))
    ggsave(paste0(ofolder,filePrefix,'_',s,'_',t*100,'.png'),g)
    
  }
}





# Question 2
dfraw = read_rds("YL_Q2.rds") %>% as.tibble()
filePrefix = "Q2"

# assign ID for plotting
df = dfraw %>% group_by(mixRatio,sampleSize,method,trueValue) %>% mutate(ID = 1:n()) %>% ungroup() %>% mutate(method=as.factor(method))
df %>% filter(sampleSize==1000)

qv = df %>% pull(mixRatio) %>% unique()
tv = df %>% pull(trueValue) %>% unique()
sv = df %>% pull(sampleSize) %>% unique()

# ggplot(df %>% filter(sampleSize==500,trueValue==0.2),aes(x=ID)) + 
#   geom_segment(aes(x=ID,y=confiLower,xend=ID,yend=confiUpper),color='orange1') +
#   geom_hline(yintercept = 0.2)


df_sort = df %>% group_by(mixRatio,sampleSize,method,trueValue) %>% arrange(desc(confiUpper)) %>% mutate(sortID = 1:n()) %>% ungroup()

for (q in qv) {
  for (s in sv) {
    for (t in tv) {
      g = ggplot(df_sort %>% filter(mixRatio==q,sampleSize==s,trueValue==t),aes(x=sortID)) +
        geom_ribbon(aes(ymin= confiLower,ymax = confiUpper,fill=method),alpha=0.5)+
        geom_hline(yintercept = t,size=1,color="dodgerblue4")+
        scale_y_continuous(breaks=seq(-0.25,0.65,0.1),limits=c(-0.25, 0.65)) +
        theme_minimal() + theme(legend.position = 'bottom') +
        ggtitle(paste(filePrefix,'\t MixRatio:',q,'\t Sample size:',s,"\t True value:",t))
      ggsave(paste0(ofolder,filePrefix,'_q',q,'_',s,'_',t*100,'.png'),g)
      
    }
  }
}













### archive

### zoom view
# ggplot(df_sort %>% filter(sampleSize==500,trueValue==tv[1]),aes(x=sortID)) +
#   geom_ribbon(aes(ymin= confiLower,ymax = confiUpper,fill=method),alpha=0.5)+
#   geom_hline(yintercept = tv[1],size=1,color="dodgerblue4")+
#   theme_minimal() + coord_cartesian(xlim = c(0, 100))
# 
# 
# ggplot(df_sort %>% filter(sampleSize==500,trueValue==tv[1]),aes(x=sortID)) +
#   geom_ribbon(aes(ymin= confiLower,ymax = confiUpper,fill=method),alpha=0.5)+
#   geom_hline(yintercept = tv[1],size=1,color="dodgerblue4")+
#   theme_minimal() + coord_cartesian(xlim = c(1900, 2000))


ggplot(df_sort %>% filter(sampleSize==500,trueValue==tv[2]),aes(x=sortID)) +
  geom_ribbon(aes(ymin= confiLower,ymax = confiUpper,fill=method),alpha=0.5)+
  geom_hline(yintercept = tv[2])

ggplot(df_sort %>% filter(sampleSize==500,trueValue==tv[3]),aes(x=sortID)) +
  geom_ribbon(aes(ymin= confiLower,ymax = confiUpper,fill=method),alpha=0.5)+
  geom_hline(yintercept = tv[3])
