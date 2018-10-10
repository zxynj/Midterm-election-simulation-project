source("C:\\Users\\xinyu.zhang\\Documents\\R\\stat6341\\project1\\functions.R")

set.seed(321)
#Q1
execute_all(multinom_p=c(0.50,0.30,0.20),contamination=FALSE,nsize=500,nrep=2000,alpha=0.05)
execute_all(multinom_p=c(0.50,0.30,0.20),contamination=FALSE,nsize=1000,nrep=2000,alpha=0.05)
execute_all(multinom_p=c(0.50,0.30,0.20),contamination=FALSE,nsize=1500,nrep=2000,alpha=0.05)

execute_all(multinom_p=c(0.50,0.48,0.02),contamination=FALSE,nsize=500,nrep=2000,alpha=0.05)
execute_all(multinom_p=c(0.50,0.48,0.02),contamination=FALSE,nsize=1000,nrep=2000,alpha=0.05)
execute_all(multinom_p=c(0.50,0.48,0.02),contamination=FALSE,nsize=1500,nrep=2000,alpha=0.05)

#Q2
execute_all(multinom_p=c(0.40,0.30,0.20),contamination=TRUE,contem_multinom_p=c(0.34,0.33,0.33),contem_p=0.10,nsize=500,nrep=2000,alpha=0.05)
execute_all(multinom_p=c(0.40,0.30,0.20),contamination=TRUE,contem_multinom_p=c(0.34,0.33,0.33),contem_p=0.20,nsize=500,nrep=2000,alpha=0.05)
execute_all(multinom_p=c(0.40,0.30,0.20),contamination=TRUE,contem_multinom_p=c(0.34,0.33,0.33),contem_p=0.30,nsize=500,nrep=2000,alpha=0.05)
execute_all(multinom_p=c(0.40,0.30,0.20),contamination=TRUE,contem_multinom_p=c(0.34,0.33,0.33),contem_p=0.40,nsize=500,nrep=2000,alpha=0.05)
execute_all(multinom_p=c(0.40,0.30,0.20),contamination=TRUE,contem_multinom_p=c(0.34,0.33,0.33),contem_p=0.50,nsize=500,nrep=2000,alpha=0.05)

execute_all(multinom_p=c(0.40,0.30,0.20),contamination=TRUE,contem_multinom_p=c(0.34,0.33,0.33),contem_p=0.10,nsize=1000,nrep=2000,alpha=0.05)
execute_all(multinom_p=c(0.40,0.30,0.20),contamination=TRUE,contem_multinom_p=c(0.34,0.33,0.33),contem_p=0.20,nsize=1000,nrep=2000,alpha=0.05)
execute_all(multinom_p=c(0.40,0.30,0.20),contamination=TRUE,contem_multinom_p=c(0.34,0.33,0.33),contem_p=0.30,nsize=1000,nrep=2000,alpha=0.05)
execute_all(multinom_p=c(0.40,0.30,0.20),contamination=TRUE,contem_multinom_p=c(0.34,0.33,0.33),contem_p=0.40,nsize=1000,nrep=2000,alpha=0.05)
execute_all(multinom_p=c(0.40,0.30,0.20),contamination=TRUE,contem_multinom_p=c(0.34,0.33,0.33),contem_p=0.50,nsize=1000,nrep=2000,alpha=0.05)

execute_all(multinom_p=c(0.40,0.30,0.20),contamination=TRUE,contem_multinom_p=c(0.34,0.33,0.33),contem_p=0.10,nsize=1500,nrep=2000,alpha=0.05)
execute_all(multinom_p=c(0.40,0.30,0.20),contamination=TRUE,contem_multinom_p=c(0.34,0.33,0.33),contem_p=0.20,nsize=1500,nrep=2000,alpha=0.05)
execute_all(multinom_p=c(0.40,0.30,0.20),contamination=TRUE,contem_multinom_p=c(0.34,0.33,0.33),contem_p=0.30,nsize=1500,nrep=2000,alpha=0.05)
execute_all(multinom_p=c(0.40,0.30,0.20),contamination=TRUE,contem_multinom_p=c(0.34,0.33,0.33),contem_p=0.40,nsize=1500,nrep=2000,alpha=0.05)
execute_all(multinom_p=c(0.40,0.30,0.20),contamination=TRUE,contem_multinom_p=c(0.34,0.33,0.33),contem_p=0.50,nsize=1500,nrep=2000,alpha=0.05)










set.seed(321)
data_simulation=data_simulation(multinom_p=c(0.4,0.3,0.3),
                                contamination=TRUE, contem_multinom_p=c(0.34,0.33,0.33), contem_p=0.1,
                                nsize=500, nrep=20)
estimation_process=estimation_process(data_simulation)
m1_ci = m1_ci(estimation_process,alpha=0.05)
m2_ci = m2_ci(estimation_process,alpha=0.05)
simulation_coverage(m1_ci)
simulation_coverage(m2_ci)
