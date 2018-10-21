library(shiny)
library(parallel)
library(ggplot2)

source("https://raw.githubusercontent.com/zxynj/Midterm-election-simulation-project/master/All%20in%20one%20shiny%20app/functions.R")
cl=makeCluster(1)
clusterExport(cl=cl, varlist=c("data_simulation_fun","estimation_process_fun","m1_ci_fun","m2_ci_fun","simulation_coverage_fun","execute_all_fun"),envir=environment())

ui <- fluidPage(
  titlePanel("Midterm election simulation project Stat 6341"),
  sidebarLayout(
    sidebarPanel(
      p(strong("Population parameters"),style = "color:blue"),
      numericInput("p1", "Candidate 1 preference (p\u2081)", 0.2, 0, 1, 0.01),
      numericInput("p2", "Candidate 2 preference (p\u2082)", 0.3, 0, 1, 0.01),
      numericInput("p3", "Candidate 3 preference (p\u2083)", 0.5, 0, 1, 0.01),
      checkboxInput('contam', tags$b("Contanimation"), FALSE),
      uiOutput('r1_ui'),
      uiOutput('r2_ui'),
      uiOutput('r3_ui'),
      p(strong("Dynamic parameters"),style = "color:blue"),
      uiOutput('x_axis_ui'),
      uiOutput('N_ui'),
      uiOutput('nrep_ui'),
      uiOutput('alpha_ui'),
      uiOutput('q_ui')
    ),
    mainPanel(
      plotOutput("simult_ci_plot"),
      br(),
      br(),
      plotOutput("mean_ci_width_plot"),
      br(),
      br(),
      plotOutput("max_ci_width_plot")
    )
  )
)

server <- function(input, output) {
  #ui
  output$r1_ui <- renderUI({
    if (input$contam) {return(numericInput("r1", "Candidate 1 preference for contamination population (r\u2081)", 0.1, 0, 1, 0.01))}
    NULL
  })
  output$r2_ui <- renderUI({
    if (input$contam) {return(numericInput("r2", "Candidate 2 preference for contamination population (r\u2082)", 0.4, 0, 1, 0.01))}
    NULL
  })
  output$r3_ui <- renderUI({
    if (input$contam) {return(numericInput("r3", "Candidate 3 preference for contamination population (r\u2083)", 0.5, 0, 1, 0.01))}
    NULL
  })
  output$x_axis_ui <- renderUI({
    if (input$contam) {return(selectInput("x_axis", "X Axis",c("Sample size per replication (N)","Number of replications (nrep)","Significance level (\u03B1)","Contamination percentage (q)"),selected = "Sample size per replication"))}
    selectInput("x_axis", "X Axis",c("Sample size per replication (N)","Number of replications (nrep)","Significance level (\u03B1)"),selected = "Sample size per replication")
  })
  output$N_ui <- renderUI({
    if (is.null(input$x_axis)) {return(NULL)}
    if (input$x_axis=="Sample size per replication (N)") {return(sliderInput("N_range", "Sample size per replication (N)", 10, 5000, c(50,2000),step=10))}
    sliderInput("N", "Sample size per replication (N)", 10, 5000, c(1500),step=10)
  })
  output$nrep_ui <- renderUI({
    if (is.null(input$x_axis)) {return(NULL)}
    if (input$x_axis=="Number of replications (nrep)") {return(sliderInput("nrep_range", "Number of replications (nrep)", 10, 10000, c(50,2500),step=10))}
    sliderInput("nrep", "Number of replications (nrep)", 10, 10000, c(2000),step=10)
  })
  output$alpha_ui <- renderUI({
    if (is.null(input$x_axis)) {return(NULL)}
    if (input$x_axis=="Significance level (\u03B1)") {return(sliderInput("alpha_range", "Significance level (\u03B1)", 0.01, 0.2, c(0.01,0.1),step=0.01))}
    sliderInput("alpha", "Significance level (\u03B1)", 0.01, 0.2, c(0.05),step=0.01)
  })
  output$q_ui <- renderUI({
    if (is.null(input$x_axis)) {return(NULL)}
    if (input$contam) {
      if (input$x_axis=="Contamination percentage (q)") {return(sliderInput("q_range", "Contamination percentage (q)", 0, 1, c(0.05,0.6),step=0.01))}
      return(sliderInput("q", "Contamination percentage (q)", 0, 1, c(0.1),step=0.01))
    }
    NULL
  })
  
  #simultaneous confidence interval coverage
  output$simult_ci_plot <- renderPlot({
    if (!input$contam) {
      if (is.null(input$x_axis)) {return(NULL)}
      validate(need(input$p1+input$p2+input$p3==1, "Please make sure p\u2081 + p\u2082 + p\u2083 = 1."))
      p=c(input$p1,input$p2,input$p3)
      contamination=FALSE
      if (input$x_axis=="Sample size per replication (N)") {
        if (is.null(input$N_range[1])) {return(NULL)}
        nrep=input$nrep
        alpha=input$alpha
        x=seq(input$N_range[1],input$N_range[2],by=10)
        clusterExport(cl=cl, varlist=c("p","contamination","nrep","alpha","x"),envir=environment())
        
        y1=parSapply(cl=cl,x,function(x) execute_all_fun(multinom_p=p,contamination=contamination,nsize=x,nrep=nrep,alpha=alpha)$simulation_coverage_m1$simult_coverage_prob)
        y2=parSapply(cl=cl,x,function(x) execute_all_fun(multinom_p=p,contamination=contamination,nsize=x,nrep=nrep,alpha=alpha)$simulation_coverage_m1$simult_coverage_prob)
        
        return(ggplot(melt(data.frame(x=x,y1=y1,y2=y2), id.var="x"), aes(x,value,colour=variable))+
                 geom_point()+
                 geom_smooth(method ="auto")+
                 scale_colour_discrete(name  ="",breaks=c("y1", "y2"),labels=c("Method 1", "Method 2"))+
                 ggtitle("Plot of Simultaneous Confidence Interval Coverage") +
                 theme(plot.title = element_text(face = "bold")) +
                 xlab("N") + ylab("Coverage probability") +
                 xlim(min(x),max(x)) +
                 ylim(min(y1,y2),max(y1,y2)))
      }
      if (input$x_axis=="Number of replications (nrep)") {
        if (is.null(input$nrep_range[1])) {return(NULL)}
        N=input$N
        alpha=input$alpha
        x=seq(input$nrep_range[1],input$nrep_range[2],by=10)
        clusterExport(cl=cl, varlist=c("p","contamination","N","alpha","x"),envir=environment())
        
        y1=parSapply(cl=cl,x,function(x) execute_all_fun(multinom_p=p,contamination=contamination,nsize=N,nrep=x,alpha=alpha)$simulation_coverage_m1$simult_coverage_prob)
        y2=parSapply(cl=cl,x,function(x) execute_all_fun(multinom_p=p,contamination=contamination,nsize=N,nrep=x,alpha=alpha)$simulation_coverage_m2$simult_coverage_prob)
        
        return(ggplot(melt(data.frame(x=x,y1=y1,y2=y2), id.var="x"), aes(x,value,colour=variable))+
                 geom_point()+
                 geom_smooth(method ="auto")+
                 scale_colour_discrete(name  ="",breaks=c("y1", "y2"),labels=c("Method 1", "Method 2"))+
                 ggtitle("Plot of Simultaneous Confidence Interval Coverage") +
                 theme(plot.title = element_text(face = "bold")) +
                 xlab("nrep") + ylab("Coverage probability") +
                 xlim(min(x),max(x)) +
                 ylim(min(y1,y2),max(y1,y2)))
      }
      if (input$x_axis=="Significance level (\u03B1)") {
        if (is.null(input$alpha_range[1])) {return(NULL)}
        N=input$N
        nrep=input$nrep
        x=seq(input$alpha_range[1],input$alpha_range[2],by=0.01)
        clusterExport(cl=cl, varlist=c("p","contamination","N","nrep","x"),envir=environment())
        
        y1=parSapply(cl=cl,x,function(x) execute_all_fun(multinom_p=p,contamination=contamination,nsize=N,nrep=nrep,alpha=x)$simulation_coverage_m1$simult_coverage_prob)
        y2=parSapply(cl=cl,x,function(x) execute_all_fun(multinom_p=p,contamination=contamination,nsize=N,nrep=nrep,alpha=x)$simulation_coverage_m2$simult_coverage_prob)
        
        return(ggplot(melt(data.frame(x=x,y1=y1,y2=y2), id.var="x"), aes(x,value,colour=variable))+
                 geom_point()+
                 geom_smooth(method ="auto")+
                 scale_colour_discrete(name  ="",breaks=c("y1", "y2"),labels=c("Method 1", "Method 2"))+
                 ggtitle("Plot of Simultaneous Confidence Interval Coverage") +
                 theme(plot.title = element_text(face = "bold")) +
                 xlab("\u03B1") + ylab("Coverage probability") +
                 xlim(min(x),max(x)) +
                 ylim(min(y1,y2),max(y1,y2)))
      }
    }
    if (input$contam) {
      if (is.null(input$x_axis)) {return(NULL)}
      validate(need(input$p1+input$p2+input$p3==1, "Please make sure p\u2081 + p\u2082 + p\u2083 = 1."))
      validate(need(input$r1+input$r2+input$r3==1, "Please make sure r\u2081 + r\u2082 + r\u2083 = 1."))
      p=c(input$p1,input$p2,input$p3)
      r=c(input$r1,input$r2,input$r3)
      contamination=TRUE
      if (input$x_axis=="Sample size per replication (N)") {
        if (is.null(input$N_range[1])) {return(NULL)}
        nrep=input$nrep
        alpha=input$alpha
        q=input$q
        x=seq(input$N_range[1],input$N_range[2],by=10)
        clusterExport(cl=cl, varlist=c("p","r","contamination","nrep","alpha","q","x"),envir=environment())
        
        y1=parSapply(cl=cl,x,function(x) execute_all_fun(multinom_p=p,contamination=contamination,contem_multinom_p=r,contem_q=q,nsize=x,nrep=nrep,alpha=alpha)$simulation_coverage_m1$simult_coverage_prob)
        y2=parSapply(cl=cl,x,function(x) execute_all_fun(multinom_p=p,contamination=contamination,contem_multinom_p=r,contem_q=q,nsize=x,nrep=nrep,alpha=alpha)$simulation_coverage_m2$simult_coverage_prob)
        
        return(ggplot(melt(data.frame(x=x,y1=y1,y2=y2), id.var="x"), aes(x,value,colour=variable))+
                 geom_point()+
                 geom_smooth(method ="auto")+
                 scale_colour_discrete(name  ="",breaks=c("y1", "y2"),labels=c("Method 1", "Method 2"))+
                 ggtitle("Plot of Simultaneous Confidence Interval Coverage") +
                 theme(plot.title = element_text(face = "bold")) +
                 xlab("N") + ylab("Coverage probability") +
                 xlim(min(x),max(x)) +
                 ylim(min(y1,y2),max(y1,y2)))
      }
      if (input$x_axis=="Number of replications (nrep)") {
        if (is.null(input$nrep_range[1])) {return(NULL)}
        N=input$N
        alpha=input$alpha
        q=input$q
        x=seq(input$nrep_range[1],input$nrep_range[2],by=10)
        clusterExport(cl=cl, varlist=c("p","r","contamination","N","alpha","q","x"),envir=environment())
        
        y1=parSapply(cl=cl,x,function(x) execute_all_fun(multinom_p=p,contamination=contamination,contem_multinom_p=r,contem_q=q,nsize=N,nrep=x,alpha=alpha)$simulation_coverage_m1$simult_coverage_prob)
        y2=parSapply(cl=cl,x,function(x) execute_all_fun(multinom_p=p,contamination=contamination,contem_multinom_p=r,contem_q=q,nsize=x,nrep=x,alpha=alpha)$simulation_coverage_m2$simult_coverage_prob)
        
        return(ggplot(melt(data.frame(x=x,y1=y1,y2=y2), id.var="x"), aes(x,value,colour=variable))+
                 geom_point()+
                 geom_smooth(method ="auto")+
                 scale_colour_discrete(name  ="",breaks=c("y1", "y2"),labels=c("Method 1", "Method 2"))+
                 ggtitle("Plot of Simultaneous Confidence Interval Coverage") +
                 theme(plot.title = element_text(face = "bold")) +
                 xlab("nrep") + ylab("Coverage probability") +
                 xlim(min(x),max(x)) +
                 ylim(min(y1,y2),max(y1,y2)))
      }
      if (input$x_axis=="Significance level (\u03B1)") {
        if (is.null(input$alpha_range[1])) {return(NULL)}
        N=input$N
        nrep=input$nrep
        q=input$q
        x=seq(input$alpha_range[1],input$alpha_range[2],by=0.01)
        clusterExport(cl=cl, varlist=c("p","r","contamination","N","nrep","q","x"),envir=environment())
        
        y1=parSapply(cl=cl,x,function(x) execute_all_fun(multinom_p=p,contamination=contamination,contem_multinom_p=r,contem_q=q,nsize=N,nrep=nrep,alpha=x)$simulation_coverage_m1$simult_coverage_prob)
        y2=parSapply(cl=cl,x,function(x) execute_all_fun(multinom_p=p,contamination=contamination,contem_multinom_p=r,contem_q=q,nsize=N,nrep=nrep,alpha=x)$simulation_coverage_m2$simult_coverage_prob)
        
        return(ggplot(melt(data.frame(x=x,y1=y1,y2=y2), id.var="x"), aes(x,value,colour=variable))+
                 geom_point()+
                 geom_smooth(method ="auto")+
                 scale_colour_discrete(name  ="",breaks=c("y1", "y2"),labels=c("Method 1", "Method 2"))+
                 ggtitle("Plot of Simultaneous Confidence Interval Coverage") +
                 theme(plot.title = element_text(face = "bold")) +
                 xlab("\u03B1") + ylab("Coverage probability") +
                 xlim(min(x),max(x)) +
                 ylim(min(y1,y2),max(y1,y2)))
      }
      if (input$x_axis=="Contamination percentage (q)") {
        if (is.null(input$q_range[1])) {return(NULL)}
        N=input$N
        nrep=input$nrep
        alpha=input$alpha
        x=seq(input$q_range[1],input$q_range[2],by=0.01)
        clusterExport(cl=cl, varlist=c("p","r","contamination","N","nrep","alpha","x"),envir=environment())
        
        y1=parSapply(cl=cl,x,function(x) execute_all_fun(multinom_p=p,contamination=contamination,contem_multinom_p=r,contem_q=x,nsize=N,nrep=nrep,alpha=alpha)$simulation_coverage_m1$simult_coverage_prob)
        y2=parSapply(cl=cl,x,function(x) execute_all_fun(multinom_p=p,contamination=contamination,contem_multinom_p=r,contem_q=x,nsize=N,nrep=nrep,alpha=alpha)$simulation_coverage_m2$simult_coverage_prob)
        
        return(ggplot(melt(data.frame(x=x,y1=y1,y2=y2), id.var="x"), aes(x,value,colour=variable))+
                 geom_point()+
                 geom_smooth(method ="auto")+
                 scale_colour_discrete(name  ="",breaks=c("y1", "y2"),labels=c("Method 1", "Method 2"))+
                 ggtitle("Plot of Simultaneous Confidence Interval Coverage") +
                 theme(plot.title = element_text(face = "bold")) +
                 xlab("q") + ylab("Coverage probability") +
                 xlim(min(x),max(x)) +
                 ylim(min(y1,y2),max(y1,y2)))
      }
    }
    NULL
  })
  #avg confidence interval width
  output$mean_ci_width_plot <- renderPlot({
    if (!input$contam) {
      if (is.null(input$x_axis)) {return(NULL)}
      validate(need(input$p1+input$p2+input$p3==1, "Please make sure p\u2081 + p\u2082 + p\u2083 = 1."))
      p=c(input$p1,input$p2,input$p3)
      contamination=FALSE
      if (input$x_axis=="Sample size per replication (N)") {
        if (is.null(input$N_range[1])) {return(NULL)}
        nrep=input$nrep
        alpha=input$alpha
        x=seq(input$N_range[1],input$N_range[2],by=10)
        clusterExport(cl=cl, varlist=c("p","contamination","nrep","alpha","x"),envir=environment())
        
        y1=parSapply(cl=cl,x,function(x) execute_all_fun(multinom_p=p,contamination=contamination,nsize=x,nrep=nrep,alpha=alpha)$simulation_coverage_m1$avg_ci_width)
        y2=parSapply(cl=cl,x,function(x) execute_all_fun(multinom_p=p,contamination=contamination,nsize=x,nrep=nrep,alpha=alpha)$simulation_coverage_m1$avg_ci_width)
        
        return(ggplot(melt(data.frame(x=x,y1=y1,y2=y2), id.var="x"), aes(x,value,colour=variable))+
                 geom_point()+
                 geom_smooth(method ="auto")+
                 scale_colour_discrete(name  ="",breaks=c("y1", "y2"),labels=c("Method 1", "Method 2"))+
                 ggtitle("Plot of Average Width of Simultaneous Confidence Interval") +
                 theme(plot.title = element_text(face = "bold")) +
                 xlab("N") + ylab("Average width") +
                 xlim(min(x),max(x)) +
                 ylim(min(y1,y2),max(y1,y2)))
      }
      if (input$x_axis=="Number of replications (nrep)") {
        if (is.null(input$nrep_range[1])) {return(NULL)}
        N=input$N
        alpha=input$alpha
        x=seq(input$nrep_range[1],input$nrep_range[2],by=10)
        clusterExport(cl=cl, varlist=c("p","contamination","N","alpha","x"),envir=environment())
        
        y1=parSapply(cl=cl,x,function(x) execute_all_fun(multinom_p=p,contamination=contamination,nsize=N,nrep=x,alpha=alpha)$simulation_coverage_m1$avg_ci_width)
        y2=parSapply(cl=cl,x,function(x) execute_all_fun(multinom_p=p,contamination=contamination,nsize=N,nrep=x,alpha=alpha)$simulation_coverage_m2$avg_ci_width)
        
        return(ggplot(melt(data.frame(x=x,y1=y1,y2=y2), id.var="x"), aes(x,value,colour=variable))+
                 geom_point()+
                 geom_smooth(method ="auto")+
                 scale_colour_discrete(name  ="",breaks=c("y1", "y2"),labels=c("Method 1", "Method 2"))+
                 ggtitle("Plot of Average Width of Simultaneous Confidence Interval") +
                 theme(plot.title = element_text(face = "bold")) +
                 xlab("nrep") + ylab("Average width") +
                 xlim(min(x),max(x)) +
                 ylim(min(y1,y2),max(y1,y2)))
      }
      if (input$x_axis=="Significance level (\u03B1)") {
        if (is.null(input$alpha_range[1])) {return(NULL)}
        N=input$N
        nrep=input$nrep
        x=seq(input$alpha_range[1],input$alpha_range[2],by=0.01)
        clusterExport(cl=cl, varlist=c("p","contamination","N","nrep","x"),envir=environment())
        
        y1=parSapply(cl=cl,x,function(x) execute_all_fun(multinom_p=p,contamination=contamination,nsize=N,nrep=nrep,alpha=x)$simulation_coverage_m1$avg_ci_width)
        y2=parSapply(cl=cl,x,function(x) execute_all_fun(multinom_p=p,contamination=contamination,nsize=N,nrep=nrep,alpha=x)$simulation_coverage_m2$avg_ci_width)
        
        return(ggplot(melt(data.frame(x=x,y1=y1,y2=y2), id.var="x"), aes(x,value,colour=variable))+
                 geom_point()+
                 geom_smooth(method ="auto")+
                 scale_colour_discrete(name  ="",breaks=c("y1", "y2"),labels=c("Method 1", "Method 2"))+
                 ggtitle("Plot of Average Width of Simultaneous Confidence Interval") +
                 theme(plot.title = element_text(face = "bold")) +
                 xlab("\u03B1") + ylab("Average width") +
                 xlim(min(x),max(x)) +
                 ylim(min(y1,y2),max(y1,y2)))
      }
    }
    if (input$contam) {
      if (is.null(input$x_axis)) {return(NULL)}
      validate(need(input$p1+input$p2+input$p3==1, "Please make sure p\u2081 + p\u2082 + p\u2083 = 1."))
      validate(need(input$r1+input$r2+input$r3==1, "Please make sure r\u2081 + r\u2082 + r\u2083 = 1."))
      p=c(input$p1,input$p2,input$p3)
      r=c(input$r1,input$r2,input$r3)
      contamination=TRUE
      if (input$x_axis=="Sample size per replication (N)") {
        if (is.null(input$N_range[1])) {return(NULL)}
        nrep=input$nrep
        alpha=input$alpha
        q=input$q
        x=seq(input$N_range[1],input$N_range[2],by=10)
        clusterExport(cl=cl, varlist=c("p","r","contamination","nrep","alpha","q","x"),envir=environment())
        
        y1=parSapply(cl=cl,x,function(x) execute_all_fun(multinom_p=p,contamination=contamination,contem_multinom_p=r,contem_q=q,nsize=x,nrep=nrep,alpha=alpha)$simulation_coverage_m1$avg_ci_width)
        y2=parSapply(cl=cl,x,function(x) execute_all_fun(multinom_p=p,contamination=contamination,contem_multinom_p=r,contem_q=q,nsize=x,nrep=nrep,alpha=alpha)$simulation_coverage_m2$avg_ci_width)
        
        return(ggplot(melt(data.frame(x=x,y1=y1,y2=y2), id.var="x"), aes(x,value,colour=variable))+
                 geom_point()+
                 geom_smooth(method ="auto")+
                 scale_colour_discrete(name  ="",breaks=c("y1", "y2"),labels=c("Method 1", "Method 2"))+
                 ggtitle("Plot of Average Width of Simultaneous Confidence Interval") +
                 theme(plot.title = element_text(face = "bold")) +
                 xlab("N") + ylab("Average width") +
                 xlim(min(x),max(x)) +
                 ylim(min(y1,y2),max(y1,y2)))
      }
      if (input$x_axis=="Number of replications (nrep)") {
        if (is.null(input$nrep_range[1])) {return(NULL)}
        N=input$N
        alpha=input$alpha
        q=input$q
        x=seq(input$nrep_range[1],input$nrep_range[2],by=10)
        clusterExport(cl=cl, varlist=c("p","r","contamination","N","alpha","q","x"),envir=environment())
        
        y1=parSapply(cl=cl,x,function(x) execute_all_fun(multinom_p=p,contamination=contamination,contem_multinom_p=r,contem_q=q,nsize=N,nrep=x,alpha=alpha)$simulation_coverage_m1$avg_ci_width)
        y2=parSapply(cl=cl,x,function(x) execute_all_fun(multinom_p=p,contamination=contamination,contem_multinom_p=r,contem_q=q,nsize=x,nrep=x,alpha=alpha)$simulation_coverage_m2$avg_ci_width)
        
        return(ggplot(melt(data.frame(x=x,y1=y1,y2=y2), id.var="x"), aes(x,value,colour=variable))+
                 geom_point()+
                 geom_smooth(method ="auto")+
                 scale_colour_discrete(name  ="",breaks=c("y1", "y2"),labels=c("Method 1", "Method 2"))+
                 ggtitle("Plot of Average Width of Simultaneous Confidence Interval") +
                 theme(plot.title = element_text(face = "bold")) +
                 xlab("nrep") + ylab("Average width") +
                 xlim(min(x),max(x)) +
                 ylim(min(y1,y2),max(y1,y2)))
      }
      if (input$x_axis=="Significance level (\u03B1)") {
        if (is.null(input$alpha_range[1])) {return(NULL)}
        N=input$N
        nrep=input$nrep
        q=input$q
        x=seq(input$alpha_range[1],input$alpha_range[2],by=0.01)
        clusterExport(cl=cl, varlist=c("p","r","contamination","N","nrep","q","x"),envir=environment())
        
        y1=parSapply(cl=cl,x,function(x) execute_all_fun(multinom_p=p,contamination=contamination,contem_multinom_p=r,contem_q=q,nsize=N,nrep=nrep,alpha=x)$simulation_coverage_m1$avg_ci_width)
        y2=parSapply(cl=cl,x,function(x) execute_all_fun(multinom_p=p,contamination=contamination,contem_multinom_p=r,contem_q=q,nsize=N,nrep=nrep,alpha=x)$simulation_coverage_m2$avg_ci_width)
        
        return(ggplot(melt(data.frame(x=x,y1=y1,y2=y2), id.var="x"), aes(x,value,colour=variable))+
                 geom_point()+
                 geom_smooth(method ="auto")+
                 scale_colour_discrete(name  ="",breaks=c("y1", "y2"),labels=c("Method 1", "Method 2"))+
                 ggtitle("Plot of Average Width of Simultaneous Confidence Interval") +
                 theme(plot.title = element_text(face = "bold")) +
                 xlab("\u03B1") + ylab("Average width") +
                 xlim(min(x),max(x)) +
                 ylim(min(y1,y2),max(y1,y2)))
      }
      if (input$x_axis=="Contamination percentage (q)") {
        if (is.null(input$q_range[1])) {return(NULL)}
        N=input$N
        nrep=input$nrep
        alpha=input$alpha
        x=seq(input$q_range[1],input$q_range[2],by=0.01)
        clusterExport(cl=cl, varlist=c("p","r","contamination","N","nrep","alpha","x"),envir=environment())
        
        y1=parSapply(cl=cl,x,function(x) execute_all_fun(multinom_p=p,contamination=contamination,contem_multinom_p=r,contem_q=x,nsize=N,nrep=nrep,alpha=alpha)$simulation_coverage_m1$avg_ci_width)
        y2=parSapply(cl=cl,x,function(x) execute_all_fun(multinom_p=p,contamination=contamination,contem_multinom_p=r,contem_q=x,nsize=N,nrep=nrep,alpha=alpha)$simulation_coverage_m2$avg_ci_width)
        
        return(ggplot(melt(data.frame(x=x,y1=y1,y2=y2), id.var="x"), aes(x,value,colour=variable))+
                 geom_point()+
                 geom_smooth(method ="auto")+
                 scale_colour_discrete(name  ="",breaks=c("y1", "y2"),labels=c("Method 1", "Method 2"))+
                 ggtitle("Plot of Average Width of Simultaneous Confidence Interval") +
                 theme(plot.title = element_text(face = "bold")) +
                 xlab("q") + ylab("Average width") +
                 xlim(min(x),max(x)) +
                 ylim(min(y1,y2),max(y1,y2)))
      }
    }
    NULL
  })
  #max confidence interval width
  output$max_ci_width_plot <- renderPlot({
    if (!input$contam) {
      if (is.null(input$x_axis)) {return(NULL)}
      validate(need(input$p1+input$p2+input$p3==1, "Please make sure p\u2081 + p\u2082 + p\u2083 = 1."))
      p=c(input$p1,input$p2,input$p3)
      contamination=FALSE
      if (input$x_axis=="Sample size per replication (N)") {
        if (is.null(input$N_range[1])) {return(NULL)}
        nrep=input$nrep
        alpha=input$alpha
        x=seq(input$N_range[1],input$N_range[2],by=10)
        clusterExport(cl=cl, varlist=c("p","contamination","nrep","alpha","x"),envir=environment())
        
        y1=parSapply(cl=cl,x,function(x) execute_all_fun(multinom_p=p,contamination=contamination,nsize=x,nrep=nrep,alpha=alpha)$simulation_coverage_m1$max_ci_width)
        y2=parSapply(cl=cl,x,function(x) execute_all_fun(multinom_p=p,contamination=contamination,nsize=x,nrep=nrep,alpha=alpha)$simulation_coverage_m1$max_ci_width)
        
        return(ggplot(melt(data.frame(x=x,y1=y1,y2=y2), id.var="x"), aes(x,value,colour=variable))+
                 geom_point()+
                 geom_smooth(method ="auto")+
                 scale_colour_discrete(name  ="",breaks=c("y1", "y2"),labels=c("Method 1", "Method 2"))+
                 ggtitle("Plot of Maximum Width of Simultaneous Confidence Interval") +
                 theme(plot.title = element_text(face = "bold")) +
                 xlab("N") + ylab("Maximum width") +
                 xlim(min(x),max(x)) +
                 ylim(min(y1,y2),max(y1,y2)))
      }
      if (input$x_axis=="Number of replications (nrep)") {
        if (is.null(input$nrep_range[1])) {return(NULL)}
        N=input$N
        alpha=input$alpha
        x=seq(input$nrep_range[1],input$nrep_range[2],by=10)
        clusterExport(cl=cl, varlist=c("p","contamination","N","alpha","x"),envir=environment())
        
        y1=parSapply(cl=cl,x,function(x) execute_all_fun(multinom_p=p,contamination=contamination,nsize=N,nrep=x,alpha=alpha)$simulation_coverage_m1$max_ci_width)
        y2=parSapply(cl=cl,x,function(x) execute_all_fun(multinom_p=p,contamination=contamination,nsize=N,nrep=x,alpha=alpha)$simulation_coverage_m2$max_ci_width)
        
        return(ggplot(melt(data.frame(x=x,y1=y1,y2=y2), id.var="x"), aes(x,value,colour=variable))+
                 geom_point()+
                 geom_smooth(method ="auto")+
                 scale_colour_discrete(name  ="",breaks=c("y1", "y2"),labels=c("Method 1", "Method 2"))+
                 ggtitle("Plot of Maximum Width of Simultaneous Confidence Interval") +
                 theme(plot.title = element_text(face = "bold")) +
                 xlab("nrep") + ylab("Maximum width") +
                 xlim(min(x),max(x)) +
                 ylim(min(y1,y2),max(y1,y2)))
      }
      if (input$x_axis=="Significance level (\u03B1)") {
        if (is.null(input$alpha_range[1])) {return(NULL)}
        N=input$N
        nrep=input$nrep
        x=seq(input$alpha_range[1],input$alpha_range[2],by=0.01)
        clusterExport(cl=cl, varlist=c("p","contamination","N","nrep","x"),envir=environment())
        
        y1=parSapply(cl=cl,x,function(x) execute_all_fun(multinom_p=p,contamination=contamination,nsize=N,nrep=nrep,alpha=x)$simulation_coverage_m1$max_ci_width)
        y2=parSapply(cl=cl,x,function(x) execute_all_fun(multinom_p=p,contamination=contamination,nsize=N,nrep=nrep,alpha=x)$simulation_coverage_m2$max_ci_width)
        
        return(ggplot(melt(data.frame(x=x,y1=y1,y2=y2), id.var="x"), aes(x,value,colour=variable))+
                 geom_point()+
                 geom_smooth(method ="auto")+
                 scale_colour_discrete(name  ="",breaks=c("y1", "y2"),labels=c("Method 1", "Method 2"))+
                 ggtitle("Plot of Maximum Width of Simultaneous Confidence Interval") +
                 theme(plot.title = element_text(face = "bold")) +
                 xlab("\u03B1") + ylab("Maximum width") +
                 xlim(min(x),max(x)) +
                 ylim(min(y1,y2),max(y1,y2)))
      }
    }
    if (input$contam) {
      if (is.null(input$x_axis)) {return(NULL)}
      validate(need(input$p1+input$p2+input$p3==1, "Please make sure p\u2081 + p\u2082 + p\u2083 = 1."))
      validate(need(input$r1+input$r2+input$r3==1, "Please make sure r\u2081 + r\u2082 + r\u2083 = 1."))
      p=c(input$p1,input$p2,input$p3)
      r=c(input$r1,input$r2,input$r3)
      contamination=TRUE
      if (input$x_axis=="Sample size per replication (N)") {
        if (is.null(input$N_range[1])) {return(NULL)}
        nrep=input$nrep
        alpha=input$alpha
        q=input$q
        x=seq(input$N_range[1],input$N_range[2],by=10)
        clusterExport(cl=cl, varlist=c("p","r","contamination","nrep","alpha","q","x"),envir=environment())
        
        y1=parSapply(cl=cl,x,function(x) execute_all_fun(multinom_p=p,contamination=contamination,contem_multinom_p=r,contem_q=q,nsize=x,nrep=nrep,alpha=alpha)$simulation_coverage_m1$max_ci_width)
        y2=parSapply(cl=cl,x,function(x) execute_all_fun(multinom_p=p,contamination=contamination,contem_multinom_p=r,contem_q=q,nsize=x,nrep=nrep,alpha=alpha)$simulation_coverage_m2$max_ci_width)
        
        return(ggplot(melt(data.frame(x=x,y1=y1,y2=y2), id.var="x"), aes(x,value,colour=variable))+
                 geom_point()+
                 geom_smooth(method ="auto")+
                 scale_colour_discrete(name  ="",breaks=c("y1", "y2"),labels=c("Method 1", "Method 2"))+
                 ggtitle("Plot of Maximum Width of Simultaneous Confidence Interval") +
                 theme(plot.title = element_text(face = "bold")) +
                 xlab("N") + ylab("Maximum width") +
                 xlim(min(x),max(x)) +
                 ylim(min(y1,y2),max(y1,y2)))
      }
      if (input$x_axis=="Number of replications (nrep)") {
        if (is.null(input$nrep_range[1])) {return(NULL)}
        N=input$N
        alpha=input$alpha
        q=input$q
        x=seq(input$nrep_range[1],input$nrep_range[2],by=10)
        clusterExport(cl=cl, varlist=c("p","r","contamination","N","alpha","q","x"),envir=environment())
        
        y1=parSapply(cl=cl,x,function(x) execute_all_fun(multinom_p=p,contamination=contamination,contem_multinom_p=r,contem_q=q,nsize=N,nrep=x,alpha=alpha)$simulation_coverage_m1$max_ci_width)
        y2=parSapply(cl=cl,x,function(x) execute_all_fun(multinom_p=p,contamination=contamination,contem_multinom_p=r,contem_q=q,nsize=x,nrep=x,alpha=alpha)$simulation_coverage_m2$max_ci_width)
        
        return(ggplot(melt(data.frame(x=x,y1=y1,y2=y2), id.var="x"), aes(x,value,colour=variable))+
                 geom_point()+
                 geom_smooth(method ="auto")+
                 scale_colour_discrete(name  ="",breaks=c("y1", "y2"),labels=c("Method 1", "Method 2"))+
                 ggtitle("Plot of Maximum Width of Simultaneous Confidence Interval") +
                 theme(plot.title = element_text(face = "bold")) +
                 xlab("nrep") + ylab("Maximum width") +
                 xlim(min(x),max(x)) +
                 ylim(min(y1,y2),max(y1,y2)))
      }
      if (input$x_axis=="Significance level (\u03B1)") {
        if (is.null(input$alpha_range[1])) {return(NULL)}
        N=input$N
        nrep=input$nrep
        q=input$q
        x=seq(input$alpha_range[1],input$alpha_range[2],by=0.01)
        clusterExport(cl=cl, varlist=c("p","r","contamination","N","nrep","q","x"),envir=environment())
        
        y1=parSapply(cl=cl,x,function(x) execute_all_fun(multinom_p=p,contamination=contamination,contem_multinom_p=r,contem_q=q,nsize=N,nrep=nrep,alpha=x)$simulation_coverage_m1$max_ci_width)
        y2=parSapply(cl=cl,x,function(x) execute_all_fun(multinom_p=p,contamination=contamination,contem_multinom_p=r,contem_q=q,nsize=N,nrep=nrep,alpha=x)$simulation_coverage_m2$max_ci_width)
        
        return(ggplot(melt(data.frame(x=x,y1=y1,y2=y2), id.var="x"), aes(x,value,colour=variable))+
                 geom_point()+
                 geom_smooth(method ="auto")+
                 scale_colour_discrete(name  ="",breaks=c("y1", "y2"),labels=c("Method 1", "Method 2"))+
                 ggtitle("Plot of Maximum Width of Simultaneous Confidence Interval") +
                 theme(plot.title = element_text(face = "bold")) +
                 xlab("\u03B1") + ylab("Maximum width") +
                 xlim(min(x),max(x)) +
                 ylim(min(y1,y2),max(y1,y2)))
      }
      if (input$x_axis=="Contamination percentage (q)") {
        if (is.null(input$q_range[1])) {return(NULL)}
        N=input$N
        nrep=input$nrep
        alpha=input$alpha
        x=seq(input$q_range[1],input$q_range[2],by=0.01)
        clusterExport(cl=cl, varlist=c("p","r","contamination","N","nrep","alpha","x"),envir=environment())
        
        y1=parSapply(cl=cl,x,function(x) execute_all_fun(multinom_p=p,contamination=contamination,contem_multinom_p=r,contem_q=x,nsize=N,nrep=nrep,alpha=alpha)$simulation_coverage_m1$max_ci_width)
        y2=parSapply(cl=cl,x,function(x) execute_all_fun(multinom_p=p,contamination=contamination,contem_multinom_p=r,contem_q=x,nsize=N,nrep=nrep,alpha=alpha)$simulation_coverage_m2$max_ci_width)
        
        return(ggplot(melt(data.frame(x=x,y1=y1,y2=y2), id.var="x"), aes(x,value,colour=variable))+
                 geom_point()+
                 geom_smooth(method ="auto")+
                 scale_colour_discrete(name  ="",breaks=c("y1", "y2"),labels=c("Method 1", "Method 2"))+
                 ggtitle("Plot of Maximum Width of Simultaneous Confidence Interval") +
                 theme(plot.title = element_text(face = "bold")) +
                 xlab("q") + ylab("Maximum width") +
                 xlim(min(x),max(x)) +
                 ylim(min(y1,y2),max(y1,y2)))
      }
    }
    NULL
  })
}

shinyApp(ui = ui, server = server)






