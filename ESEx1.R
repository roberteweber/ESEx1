ex1 <- c(6,38,134,82)

lnLmulti<-function(param,data){
  N<-param[1];
  p1<-param[2];
  p2<-param[3];
  
  t<-data[1];
  Mt1<-data[2];
  M<-data[3];
  m<-data[4];
  
  lfactorial(N)-lfactorial(N-Mt1)+Mt1*log(p1)+(t*N-M-Mt1)*log(1-p1)+m*log(p2)+(M-m)*log(1-p2)
}

ex1.multi<-optim(c(50,0.3,0.7), lnLmulti, method='L-BFGS-B', lower=c(NA,0.0001,0.0001), upper=c(NA,1-0.0001,1-0.0001), hessian=T, data=ex1, control=list(fnscale=-1))

warnings()

ex1.multi

