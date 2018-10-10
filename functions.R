
data_simulation = function(multinom_p, contamination=FALSE, contem_multinom_p, contem_p, nsize, nrep) {
  if(contamination) {
    nsize_contam=rbinom(nrep,nsize,contem_p)
    return(list(data_sim=sapply(nsize_contam,function(x) rmultinom(1,x,contem_multinom_p)) + sapply(nsize-nsize_contam,function(x) rmultinom(1,x,multinom_p)),
                multinom_p=multinom_p,
                nsize=nsize))
    }
  else {
    return(list(data_sim=rmultinom(nrep,nsize,multinom_p),
                multinom_p=multinom_p,
                nsize=nsize))
    }
}
    
estimation_process = function(data_simulation) {
  p_hat=data_simulation$data_sim/data_simulation$nsize
  comb_row=combn(c(1:length(data_simulation$multinom_p)),2)#lookup matrix
  ncomb=ncol(comb_row)
  delta_hat_vector=c()
  paired_p_hat_sum_vector=c()
  delta_p=c()
  for (i in c(1:ncomb)){
    delta_hat_vector=c(delta_hat_vector,p_hat[comb_row[1,i],]-p_hat[comb_row[2,i],])
    paired_p_hat_sum_vector=c(paired_p_hat_sum_vector,p_hat[comb_row[1,i],]+p_hat[comb_row[2,i],])
    delta_p=c(delta_p,data_simulation$multinom_p[comb_row[1,i]]-data_simulation$multinom_p[comb_row[2,i]])
  }
  return(list(delta_hat=matrix(delta_hat_vector,nrow=ncomb,byrow = TRUE),
              paired_p_hat_sum=matrix(paired_p_hat_sum_vector,nrow=ncomb,byrow = TRUE),
              comb_row=comb_row,
              delta_p=delta_p,
              multinom_p=data_simulation$multinom_p,
              nsize=data_simulation$nsize,
              ncomb=ncomb))
}

m1_ci = function(estimation_process,alpha=0.05) {
  a=qchisq(1-alpha/estimation_process$ncomb,estimation_process$ncomb-1)
  d=estimation_process$paired_p_hat_sum-estimation_process$delta_hat^2
  return(list(lower=estimation_process$delta_hat-sqrt(a*d/estimation_process$nsize),
              upper=estimation_process$delta_hat+sqrt(a*d/estimation_process$nsize),
              delta_p=estimation_process$delta_p))
}

m2_ci = function(estimation_process,alpha=0.05) {
  a=uniroot(function(x) alpha-2*(1-pnorm(x))-4*(length(estimation_process$multinom_p)-2)*(1-pnorm(x*sqrt(2))),lower=-3,upper=3)$root
  return(list(lower=estimation_process$delta_hat-a/sqrt(estimation_process$nsize),
              upper=estimation_process$delta_hat+a/sqrt(estimation_process$nsize),
              delta_p=estimation_process$delta_p))
}

simulation_coverage = function(m_ci) {
  ci_out= m_ci$upper < m_ci$delta_p | m_ci$lower > m_ci$delta_p
  coverage_prob=apply(ci_out,1,mean)
  return(list(coverage_prob=coverage_prob,
              alpha_simu=1-prod(1-coverage_prob),
              delta_p=m_ci$delta_p))
}

execute_all = function(multinom_p, contamination=TRUE, contem_multinom_p, contem_p, nsize=500, nrep=20, alpha=0.05) {
  data_simulation=data_simulation(multinom_p=multinom_p, contamination=contamination, contem_multinom_p=contem_multinom_p, contem_p=contem_p, nsize=nsize, nrep=nrep)
  estimation_process=estimation_process(data_simulation)
  m1_ci = m1_ci(estimation_process,alpha=alpha)
  m2_ci = m2_ci(estimation_process,alpha=alpha)
  return(c(simulation_coverage(m1_ci)$alpha_simu,
  simulation_coverage(m2_ci)$alpha_simu))
}
