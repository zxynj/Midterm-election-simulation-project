library(shiny)
library(parallel)
library(rpgm)
library(ggplot2)
library(compiler)
library(gridExtra)
library(reshape2)

source("https://raw.githubusercontent.com/zxynj/Midterm-election-simulation-project/master/All%20in%20one%20shiny%20app/simplified%20functions2.R")
execute_all_fun_comp <- cmpfun(execute_all_fun3)

cl=makeCluster(8)
clusterExport(cl=cl, varlist=c("execute_all_fun_comp","colMaxs"),envir=environment())

ui <- fluidPage(
  titlePanel("Midterm election simulation project Stat 6341"),
  sidebarLayout(
    sidebarPanel(
      p(strong("Population parameters"),style = "color:blue"),
      numericInput("p1", "Candidate 1 preference (p\u2081)", 0.5, 0, 1, 0.01),
      numericInput("p2", "Candidate 2 preference (p\u2082)", 0.3, 0, 1, 0.01),
      numericInput("p3", "Candidate 3 preference (p\u2083)", 0.2, 0, 1, 0.01),
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
      plotOutput("plot",height = "900px")
    )
  )
)

server <- function(input, output) {
  #ui
  output$r1_ui <- renderUI({
    if (input$contam) {return(numericInput("r1", "Candidate 1 preference for contamination population (r\u2081)", 0.34, 0, 1, 0.01))}
    NULL
  })
  output$r2_ui <- renderUI({
    if (input$contam) {return(numericInput("r2", "Candidate 2 preference for contamination population (r\u2082)", 0.33, 0, 1, 0.01))}
    NULL
  })
  output$r3_ui <- renderUI({
    if (input$contam) {return(numericInput("r3", "Candidate 3 preference for contamination population (r\u2083)", 0.33, 0, 1, 0.01))}
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
    if (input$x_axis=="Significance level (\u03B1)") {return(sliderInput("alpha_range", "Significance level (\u03B1)", 0.01, 0.6, c(0.01,0.1),step=0.01))}
    sliderInput("alpha", "Significance level (\u03B1)", 0.01, 0.6, c(0.05),step=0.01)
  })
  output$q_ui <- renderUI({
    if (is.null(input$x_axis)) {return(NULL)}
    if (input$contam) {
      if (input$x_axis=="Contamination percentage (q)") {return(sliderInput("q_range", "Contamination percentage (q)", 0, 1, c(0.05,0.6),step=0.01))}
      return(sliderInput("q", "Contamination percentage (q)", 0, 1, c(0.1),step=0.01))
    }
    NULL
  })
  #plot
  output$plot <- renderPlot({
    if (is.null(input$x_axis)) {return(NULL)}
    p=c(input$p1,input$p2,input$p3)
    validate(need(sum(p)==1, "Please make sure p\u2081 + p\u2082 + p\u2083 = 1."))
      if (!input$contam) {
        contamination=FALSE
        if (input$x_axis=="Sample size per replication (N)") {
          if (is.null(input$N_range[1])) {return(NULL)}
          nrep=input$nrep
          alpha=input$alpha
          x=seq(input$N_range[1],input$N_range[2],by=10)
          clusterExport(cl=cl, varlist=c("p","contamination","nrep","alpha","x"),envir=environment())
          
          result=matrix(unlist(parSapply(cl=cl,x,function(x) execute_all_fun_comp(multinom_p=p,contamination=contamination,N=x,nrep=nrep,alpha=alpha),simplify=FALSE)),nrow=6)
          ci_coverage_prob_m1=result[1,]
          ci_coverage_prob_m2=result[2,]
          ci_avg_width_m1=result[3,]
          ci_avg_width_m2=result[4,]
          ci_max_width_m1=result[5,]
          ci_max_width_m2=result[6,]
          
          ci_coverage_prob_plot=ggplot(melt(data.frame(x=x,y1=ci_coverage_prob_m1,y2=ci_coverage_prob_m2), id.var="x"), aes(x,value,colour=variable))+
            geom_point()+
            geom_smooth(method ="auto")+
            scale_colour_discrete(name  ="",breaks=c("y1", "y2"),labels=c("Method 1", "Method 2")) +
            ggtitle("Simultaneous Confidence Interval Coverage Probability") +
            theme(plot.title = element_text(face = "bold"),
                  legend.key.size=unit(1,"cm"),
                  legend.text=element_text(size=12),
                  legend.position="bottom",
                  legend.direction="horizontal",
                  legend.key.width=unit(2,"cm")) +
            xlab("N") + ylab("Coverage probability") +
            xlim(min(x),max(x)) +
            ylim(min(ci_coverage_prob_m1,ci_coverage_prob_m2),max(ci_coverage_prob_m1,ci_coverage_prob_m2))
          ci_avg_width_plot=ggplot(melt(data.frame(x=x,y1=ci_avg_width_m1,y2=ci_avg_width_m2), id.var="x"), aes(x,value,colour=variable))+
            geom_point()+
            geom_smooth(method ="auto")+
            scale_colour_discrete(name  ="",breaks=c("y1", "y2")) +
            ggtitle("Average Width of Simultaneous Confidence Interval") +
            theme(plot.title = element_text(face = "bold"),legend.position="none") +
            xlab("N") + ylab("Average width") +
            xlim(min(x),max(x)) +
            ylim(min(ci_avg_width_m1,ci_avg_width_m2),max(ci_avg_width_m1,ci_avg_width_m2))
          ci_max_width_plot=ggplot(melt(data.frame(x=x,y1=ci_max_width_m1,y2=ci_max_width_m2), id.var="x"), aes(x,value,colour=variable))+
            geom_point()+
            geom_smooth(method ="auto")+
            scale_colour_discrete(name  ="",breaks=c("y1", "y2")) +
            ggtitle("Maximum Width of Simultaneous Confidence Interval") +
            theme(plot.title = element_text(face = "bold"),legend.position="none") +
            xlab("N") + ylab("Maximum width") +
            xlim(min(x),max(x)) +
            ylim(min(ci_max_width_m1,ci_max_width_m2),max(ci_max_width_m1,ci_max_width_m2))
  
          return(grid.arrange(
            grobs = list(ci_coverage_prob_plot,ci_avg_width_plot,ci_max_width_plot),
            widths = c(2, 1),
            layout_matrix = cbind(c(1,1),c(2,3))))
        }
        if (input$x_axis=="Number of replications (nrep)") {
          if (is.null(input$nrep_range[1])) {return(NULL)}
          N=input$N
          alpha=input$alpha
          x=seq(input$nrep_range[1],input$nrep_range[2],by=10)
          clusterExport(cl=cl, varlist=c("p","contamination","N","alpha","x"),envir=environment())
          
          result=matrix(unlist(parSapply(cl=cl,x,function(x) execute_all_fun_comp(multinom_p=p,contamination=contamination,N=N,nrep=x,alpha=alpha),simplify=FALSE)),nrow=6)
          ci_coverage_prob_m1=result[1,]
          ci_coverage_prob_m2=result[2,]
          ci_avg_width_m1=result[3,]
          ci_avg_width_m2=result[4,]
          ci_max_width_m1=result[5,]
          ci_max_width_m2=result[6,]
          
          ci_coverage_prob_plot=ggplot(melt(data.frame(x=x,y1=ci_coverage_prob_m1,y2=ci_coverage_prob_m2), id.var="x"), aes(x,value,colour=variable))+
            geom_point()+
            geom_smooth(method ="auto")+
            scale_colour_discrete(name  ="",breaks=c("y1", "y2"),labels=c("Method 1", "Method 2")) +
            ggtitle("Simultaneous Confidence Interval Coverage Probability") +
            theme(plot.title = element_text(face = "bold"),
                  legend.key.size=unit(1,"cm"),
                  legend.text=element_text(size=12),
                  legend.position="bottom",
                  legend.direction="horizontal",
                  legend.key.width=unit(2,"cm")) +
            xlab("nrep") + ylab("Coverage probability") +
            xlim(min(x),max(x)) +
            ylim(min(ci_coverage_prob_m1,ci_coverage_prob_m2),max(ci_coverage_prob_m1,ci_coverage_prob_m2))
          ci_avg_width_plot=ggplot(melt(data.frame(x=x,y1=ci_avg_width_m1,y2=ci_avg_width_m2), id.var="x"), aes(x,value,colour=variable))+
            geom_point()+
            geom_smooth(method ="auto")+
            scale_colour_discrete(name  ="",breaks=c("y1", "y2")) +
            ggtitle("Average Width of Simultaneous Confidence Interval") +
            theme(plot.title = element_text(face = "bold"),legend.position="none") +
            xlab("nrep") + ylab("Average width") +
            xlim(min(x),max(x)) +
            ylim(min(ci_avg_width_m1,ci_avg_width_m2),max(ci_avg_width_m1,ci_avg_width_m2))
          ci_max_width_plot=ggplot(melt(data.frame(x=x,y1=ci_max_width_m1,y2=ci_max_width_m2), id.var="x"), aes(x,value,colour=variable))+
            geom_point()+
            geom_smooth(method ="auto")+
            scale_colour_discrete(name  ="",breaks=c("y1", "y2")) +
            ggtitle("Maximum Width of Simultaneous Confidence Interval") +
            theme(plot.title = element_text(face = "bold"),legend.position="none") +
            xlab("nrep") + ylab("Maximum width") +
            xlim(min(x),max(x)) +
            ylim(min(ci_max_width_m1,ci_max_width_m2),max(ci_max_width_m1,ci_max_width_m2))
          
          return(grid.arrange(
            grobs = list(ci_coverage_prob_plot,ci_avg_width_plot,ci_max_width_plot),
            widths = c(2, 1),
            layout_matrix = cbind(c(1,1),c(2,3))))
        }
        if (input$x_axis=="Significance level (\u03B1)") {
          if (is.null(input$alpha_range[1])) {return(NULL)}
          N=input$N
          nrep=input$nrep
          x=seq(input$alpha_range[1],input$alpha_range[2],by=0.01)
          clusterExport(cl=cl, varlist=c("p","contamination","N","nrep","x"),envir=environment())
          
          result=matrix(unlist(parSapply(cl=cl,x,function(x) execute_all_fun_comp(multinom_p=p,contamination=contamination,N=N,nrep=nrep,alpha=x),simplify=FALSE)),nrow=6)
          ci_coverage_prob_m1=result[1,]
          ci_coverage_prob_m2=result[2,]
          ci_avg_width_m1=result[3,]
          ci_avg_width_m2=result[4,]
          ci_max_width_m1=result[5,]
          ci_max_width_m2=result[6,]
          
          ci_coverage_prob_plot=ggplot(melt(data.frame(x=x,y1=ci_coverage_prob_m1,y2=ci_coverage_prob_m2), id.var="x"), aes(x,value,colour=variable))+
            geom_point()+
            geom_smooth(method ="auto")+
            scale_colour_discrete(name  ="",breaks=c("y1", "y2"),labels=c("Method 1", "Method 2")) +
            ggtitle("Simultaneous Confidence Interval Coverage Probability") +
            theme(plot.title = element_text(face = "bold"),
                  legend.key.size=unit(1,"cm"),
                  legend.text=element_text(size=12),
                  legend.position="bottom",
                  legend.direction="horizontal",
                  legend.key.width=unit(2,"cm")) +
            xlab("\u03B1") + ylab("Coverage probability") +
            xlim(min(x),max(x)) +
            ylim(min(ci_coverage_prob_m1,ci_coverage_prob_m2),max(ci_coverage_prob_m1,ci_coverage_prob_m2))
          ci_avg_width_plot=ggplot(melt(data.frame(x=x,y1=ci_avg_width_m1,y2=ci_avg_width_m2), id.var="x"), aes(x,value,colour=variable))+
            geom_point()+
            geom_smooth(method ="auto")+
            scale_colour_discrete(name  ="",breaks=c("y1", "y2")) +
            ggtitle("Average Width of Simultaneous Confidence Interval") +
            theme(plot.title = element_text(face = "bold"),legend.position="none") +
            xlab("\u03B1") + ylab("Average width") +
            xlim(min(x),max(x)) +
            ylim(min(ci_avg_width_m1,ci_avg_width_m2),max(ci_avg_width_m1,ci_avg_width_m2))
          ci_max_width_plot=ggplot(melt(data.frame(x=x,y1=ci_max_width_m1,y2=ci_max_width_m2), id.var="x"), aes(x,value,colour=variable))+
            geom_point()+
            geom_smooth(method ="auto")+
            scale_colour_discrete(name  ="",breaks=c("y1", "y2")) +
            ggtitle("Maximum Width of Simultaneous Confidence Interval") +
            theme(plot.title = element_text(face = "bold"),legend.position="none") +
            xlab("\u03B1") + ylab("Maximum width") +
            xlim(min(x),max(x)) +
            ylim(min(ci_max_width_m1,ci_max_width_m2),max(ci_max_width_m1,ci_max_width_m2))
          
          return(grid.arrange(
            grobs = list(ci_coverage_prob_plot,ci_avg_width_plot,ci_max_width_plot),
            widths = c(2, 1),
            layout_matrix = cbind(c(1,1),c(2,3))))
        }
      }
      if (input$contam) {
        r=c(input$r1,input$r2,input$r3)
        validate(need(sum(r)==1, "Please make sure r\u2081 + r\u2082 + r\u2083 = 1."))
        contamination=TRUE
        if (input$x_axis=="Sample size per replication (N)") {
          if (is.null(input$N_range[1])) {return(NULL)}
          nrep=input$nrep
          alpha=input$alpha
          q=input$q
          x=seq(input$N_range[1],input$N_range[2],by=10)
          clusterExport(cl=cl, varlist=c("p","r","contamination","nrep","alpha","q","x"),envir=environment())
          
          result=matrix(unlist(parSapply(cl=cl,x,function(x) execute_all_fun_comp(multinom_p=p,contamination=contamination,contem_multinom_r=r,q=q,N=x,nrep=nrep,alpha=alpha),simplify=FALSE)),nrow=6)
          ci_coverage_prob_m1=result[1,]
          ci_coverage_prob_m2=result[2,]
          ci_avg_width_m1=result[3,]
          ci_avg_width_m2=result[4,]
          ci_max_width_m1=result[5,]
          ci_max_width_m2=result[6,]
          
          ci_coverage_prob_plot=ggplot(melt(data.frame(x=x,y1=ci_coverage_prob_m1,y2=ci_coverage_prob_m2), id.var="x"), aes(x,value,colour=variable))+
            geom_point()+
            geom_smooth(method ="auto")+
            scale_colour_discrete(name  ="",breaks=c("y1", "y2"),labels=c("Method 1", "Method 2")) +
            ggtitle("Simultaneous Confidence Interval Coverage Probability") +
            theme(plot.title = element_text(face = "bold"),
                  legend.key.size=unit(1,"cm"),
                  legend.text=element_text(size=12),
                  legend.position="bottom",
                  legend.direction="horizontal",
                  legend.key.width=unit(2,"cm")) +
            xlab("N") + ylab("Coverage probability") +
            xlim(min(x),max(x)) +
            ylim(min(ci_coverage_prob_m1,ci_coverage_prob_m2),max(ci_coverage_prob_m1,ci_coverage_prob_m2))
          ci_avg_width_plot=ggplot(melt(data.frame(x=x,y1=ci_avg_width_m1,y2=ci_avg_width_m2), id.var="x"), aes(x,value,colour=variable))+
            geom_point()+
            geom_smooth(method ="auto")+
            scale_colour_discrete(name  ="",breaks=c("y1", "y2")) +
            ggtitle("Average Width of Simultaneous Confidence Interval") +
            theme(plot.title = element_text(face = "bold"),legend.position="none") +
            xlab("N") + ylab("Average width") +
            xlim(min(x),max(x)) +
            ylim(min(ci_avg_width_m1,ci_avg_width_m2),max(ci_avg_width_m1,ci_avg_width_m2))
          ci_max_width_plot=ggplot(melt(data.frame(x=x,y1=ci_max_width_m1,y2=ci_max_width_m2), id.var="x"), aes(x,value,colour=variable))+
            geom_point()+
            geom_smooth(method ="auto")+
            scale_colour_discrete(name  ="",breaks=c("y1", "y2")) +
            ggtitle("Maximum Width of Simultaneous Confidence Interval") +
            theme(plot.title = element_text(face = "bold"),legend.position="none") +
            xlab("N") + ylab("Maximum width") +
            xlim(min(x),max(x)) +
            ylim(min(ci_max_width_m1,ci_max_width_m2),max(ci_max_width_m1,ci_max_width_m2))
          
          return(grid.arrange(
            grobs = list(ci_coverage_prob_plot,ci_avg_width_plot,ci_max_width_plot),
            widths = c(2, 1),
            layout_matrix = cbind(c(1,1),c(2,3))))
        }
        if (input$x_axis=="Number of replications (nrep)") {
          if (is.null(input$nrep_range[1])) {return(NULL)}
          N=input$N
          alpha=input$alpha
          q=input$q
          x=seq(input$nrep_range[1],input$nrep_range[2],by=10)
          clusterExport(cl=cl, varlist=c("p","r","contamination","N","alpha","q","x"),envir=environment())
          
          result=matrix(unlist(parSapply(cl=cl,x,function(x) execute_all_fun_comp(multinom_p=p,contamination=contamination,contem_multinom_r=r,q=q,N=N,nrep=x,alpha=alpha),simplify=FALSE)),nrow=6)
          ci_coverage_prob_m1=result[1,]
          ci_coverage_prob_m2=result[2,]
          ci_avg_width_m1=result[3,]
          ci_avg_width_m2=result[4,]
          ci_max_width_m1=result[5,]
          ci_max_width_m2=result[6,]
          
          ci_coverage_prob_plot=ggplot(melt(data.frame(x=x,y1=ci_coverage_prob_m1,y2=ci_coverage_prob_m2), id.var="x"), aes(x,value,colour=variable))+
            geom_point()+
            geom_smooth(method ="auto")+
            scale_colour_discrete(name  ="",breaks=c("y1", "y2"),labels=c("Method 1", "Method 2")) +
            ggtitle("Simultaneous Confidence Interval Coverage Probability") +
            theme(plot.title = element_text(face = "bold"),
                  legend.key.size=unit(1,"cm"),
                  legend.text=element_text(size=12),
                  legend.position="bottom",
                  legend.direction="horizontal",
                  legend.key.width=unit(2,"cm")) +
            xlab("nrep") + ylab("Coverage probability") +
            xlim(min(x),max(x)) +
            ylim(min(ci_coverage_prob_m1,ci_coverage_prob_m2),max(ci_coverage_prob_m1,ci_coverage_prob_m2))
          ci_avg_width_plot=ggplot(melt(data.frame(x=x,y1=ci_avg_width_m1,y2=ci_avg_width_m2), id.var="x"), aes(x,value,colour=variable))+
            geom_point()+
            geom_smooth(method ="auto")+
            scale_colour_discrete(name  ="",breaks=c("y1", "y2")) +
            ggtitle("Average Width of Simultaneous Confidence Interval") +
            theme(plot.title = element_text(face = "bold"),legend.position="none") +
            xlab("nrep") + ylab("Average width") +
            xlim(min(x),max(x)) +
            ylim(min(ci_avg_width_m1,ci_avg_width_m2),max(ci_avg_width_m1,ci_avg_width_m2))
          ci_max_width_plot=ggplot(melt(data.frame(x=x,y1=ci_max_width_m1,y2=ci_max_width_m2), id.var="x"), aes(x,value,colour=variable))+
            geom_point()+
            geom_smooth(method ="auto")+
            scale_colour_discrete(name  ="",breaks=c("y1", "y2")) +
            ggtitle("Maximum Width of Simultaneous Confidence Interval") +
            theme(plot.title = element_text(face = "bold"),legend.position="none") +
            xlab("nrep") + ylab("Maximum width") +
            xlim(min(x),max(x)) +
            ylim(min(ci_max_width_m1,ci_max_width_m2),max(ci_max_width_m1,ci_max_width_m2))
          
          return(grid.arrange(
            grobs = list(ci_coverage_prob_plot,ci_avg_width_plot,ci_max_width_plot),
            widths = c(2, 1),
            layout_matrix = cbind(c(1,1),c(2,3))))
        }
        if (input$x_axis=="Significance level (\u03B1)") {
          if (is.null(input$alpha_range[1])) {return(NULL)}
          N=input$N
          nrep=input$nrep
          q=input$q
          x=seq(input$alpha_range[1],input$alpha_range[2],by=0.01)
          clusterExport(cl=cl, varlist=c("p","r","contamination","N","nrep","q","x"),envir=environment())
          
          result=matrix(unlist(parSapply(cl=cl,x,function(x) execute_all_fun_comp(multinom_p=p,contamination=contamination,contem_multinom_r=r,q=q,N=N,nrep=nrep,alpha=x),simplify=FALSE)),nrow=6)
          ci_coverage_prob_m1=result[1,]
          ci_coverage_prob_m2=result[2,]
          ci_avg_width_m1=result[3,]
          ci_avg_width_m2=result[4,]
          ci_max_width_m1=result[5,]
          ci_max_width_m2=result[6,]
          
          ci_coverage_prob_plot=ggplot(melt(data.frame(x=x,y1=ci_coverage_prob_m1,y2=ci_coverage_prob_m2), id.var="x"), aes(x,value,colour=variable))+
            geom_point()+
            geom_smooth(method ="auto")+
            scale_colour_discrete(name  ="",breaks=c("y1", "y2"),labels=c("Method 1", "Method 2")) +
            ggtitle("Simultaneous Confidence Interval Coverage Probability") +
            theme(plot.title = element_text(face = "bold"),
                  legend.key.size=unit(1,"cm"),
                  legend.text=element_text(size=12),
                  legend.position="bottom",
                  legend.direction="horizontal",
                  legend.key.width=unit(2,"cm")) +
            xlab("\u03B1") + ylab("Coverage probability") +
            xlim(min(x),max(x)) +
            ylim(min(ci_coverage_prob_m1,ci_coverage_prob_m2),max(ci_coverage_prob_m1,ci_coverage_prob_m2))
          ci_avg_width_plot=ggplot(melt(data.frame(x=x,y1=ci_avg_width_m1,y2=ci_avg_width_m2), id.var="x"), aes(x,value,colour=variable))+
            geom_point()+
            geom_smooth(method ="auto")+
            scale_colour_discrete(name  ="",breaks=c("y1", "y2")) +
            ggtitle("Average Width of Simultaneous Confidence Interval") +
            theme(plot.title = element_text(face = "bold"),legend.position="none") +
            xlab("\u03B1") + ylab("Average width") +
            xlim(min(x),max(x)) +
            ylim(min(ci_avg_width_m1,ci_avg_width_m2),max(ci_avg_width_m1,ci_avg_width_m2))
          ci_max_width_plot=ggplot(melt(data.frame(x=x,y1=ci_max_width_m1,y2=ci_max_width_m2), id.var="x"), aes(x,value,colour=variable))+
            geom_point()+
            geom_smooth(method ="auto")+
            scale_colour_discrete(name  ="",breaks=c("y1", "y2")) +
            ggtitle("Maximum Width of Simultaneous Confidence Interval") +
            theme(plot.title = element_text(face = "bold"),legend.position="none") +
            xlab("\u03B1") + ylab("Maximum width") +
            xlim(min(x),max(x)) +
            ylim(min(ci_max_width_m1,ci_max_width_m2),max(ci_max_width_m1,ci_max_width_m2))
          
          return(grid.arrange(
            grobs = list(ci_coverage_prob_plot,ci_avg_width_plot,ci_max_width_plot),
            widths = c(2, 1),
            layout_matrix = cbind(c(1,1),c(2,3))))
        }
        if (input$x_axis=="Contamination percentage (q)") {
          if (is.null(input$q_range[1])) {return(NULL)}
          N=input$N
          nrep=input$nrep
          alpha=input$alpha
          x=seq(input$q_range[1],input$q_range[2],by=0.01)
          clusterExport(cl=cl, varlist=c("p","r","contamination","N","nrep","alpha","x"),envir=environment())
          
          result=matrix(unlist(parSapply(cl=cl,x,function(x) execute_all_fun_comp(multinom_p=p,contamination=contamination,contem_multinom_r=r,q=x,N=N,nrep=nrep,alpha=alpha),simplify=FALSE)),nrow=6)
          ci_coverage_prob_m1=result[1,]
          ci_coverage_prob_m2=result[2,]
          ci_avg_width_m1=result[3,]
          ci_avg_width_m2=result[4,]
          ci_max_width_m1=result[5,]
          ci_max_width_m2=result[6,]
          
          ci_coverage_prob_plot=ggplot(melt(data.frame(x=x,y1=ci_coverage_prob_m1,y2=ci_coverage_prob_m2), id.var="x"), aes(x,value,colour=variable))+
            geom_point()+
            geom_smooth(method ="auto")+
            scale_colour_discrete(name  ="",breaks=c("y1", "y2"),labels=c("Method 1", "Method 2")) +
            ggtitle("Simultaneous Confidence Interval Coverage Probability") +
            theme(plot.title = element_text(face = "bold"),
                  legend.key.size=unit(1,"cm"),
                  legend.text=element_text(size=12),
                  legend.position="bottom",
                  legend.direction="horizontal",
                  legend.key.width=unit(2,"cm")) +
            xlab("q") + ylab("Coverage probability") +
            xlim(min(x),max(x)) +
            ylim(min(ci_coverage_prob_m1,ci_coverage_prob_m2),max(ci_coverage_prob_m1,ci_coverage_prob_m2))
          ci_avg_width_plot=ggplot(melt(data.frame(x=x,y1=ci_avg_width_m1,y2=ci_avg_width_m2), id.var="x"), aes(x,value,colour=variable))+
            geom_point()+
            geom_smooth(method ="auto")+
            scale_colour_discrete(name  ="",breaks=c("y1", "y2")) +
            ggtitle("Average Width of Simultaneous Confidence Interval") +
            theme(plot.title = element_text(face = "bold"),legend.position="none") +
            xlab("q") + ylab("Average width") +
            xlim(min(x),max(x)) +
            ylim(min(ci_avg_width_m1,ci_avg_width_m2),max(ci_avg_width_m1,ci_avg_width_m2))
          ci_max_width_plot=ggplot(melt(data.frame(x=x,y1=ci_max_width_m1,y2=ci_max_width_m2), id.var="x"), aes(x,value,colour=variable))+
            geom_point()+
            geom_smooth(method ="auto")+
            scale_colour_discrete(name  ="",breaks=c("y1", "y2")) +
            ggtitle("Maximum Width of Simultaneous Confidence Interval") +
            theme(plot.title = element_text(face = "bold"),legend.position="none") +
            xlab("q") + ylab("Maximum width") +
            xlim(min(x),max(x)) +
            ylim(min(ci_max_width_m1,ci_max_width_m2),max(ci_max_width_m1,ci_max_width_m2))
          
          return(grid.arrange(
            grobs = list(ci_coverage_prob_plot,ci_avg_width_plot,ci_max_width_plot),
            widths = c(2, 1),
            layout_matrix = cbind(c(1,1),c(2,3))))
        }
      }
  })
}

shinyApp(ui = ui, server = server)






