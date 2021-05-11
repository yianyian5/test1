library(psych)
library(openxlsx)



## k-因子模型数据构造函数

construct_factory<-function(n,k,p,ra){
  
  ##构造符合预期特定的因子模型
  F<-matrix(rnorm(n*k),nrow = n,ncol = k)
  gamma_1<-matrix(runif(k*p,min = -2,max = 2),nrow = k,ncol = p)
  var_f<-sum(diag(t(gamma_1)%*%gamma_1))
  U<-rnorm(n,sd = runif(1,min = 1,max = 1.5))
  for (ii in 2:p){
    U<-cbind(U,rnorm(n,sd = runif(1,min = 1,max = 1.5)))
  }
  var_u<-sum(diag(t(U)%*%U))  
  U<-U*sqrt(var_f/(var_u*(1/ra-1)))
  ##按照给定的独特变差比列构造U
  X<-F%*%gamma_1 + U
  res<-list(X=X,U=U,F=F,gamma_1=gamma_1,var_f=var_f,var_u=sum(diag(t(U)%*%U)))
}


## 极大似然解法
maxlikehood_factory<-function(x,k,p,n,loading_x){
  ans1<-factanal(x,factors = k,scores = "regression")
  U_ans1<-x - ans1$scores%*%t(ans1$loadings)
  ##做置换
  U_index<-U_ans1[sample(1:n,n),1]
  for (ii in 2:p){
    U_index<-cbind(U_index,U_ans1[sample(1:n,n),ii])
  }
  X2<-ans1$scores%*%t(ans1$loadings) + U_index
  ##再次求解
  ans2<-factanal(X2,factors = k,scores = "regression")
  ##对比结果
  diff1<-sum(abs(ans1$loadings%*%t(ans1$loadings) - ans2$loadings%*%t(ans2$loadings)))/sum(abs(ans1$loadings%*%t(ans1$loadings)))
  diff2<-sum(abs(t(loading_x)%*%loading_x - ans1$loadings%*%t(ans1$loadings)))/sum(abs(t(loading_x)%*%loading_x))
  c(diff2,diff1)
}


##第一组
nn<-120
kk<-4
pp<-10
ra<-0.2
result_k_p<-c(1,2,3,4,5,6)
set.seed(546)
for (ii in 1:200) {
  XX<-construct_factory(n = nn,k = kk,p = pp,ra = ra)
  result_k_p<-cbind(result_k_p,c(ra,kk,pp,nn,
                                   maxlikehood_factory(XX$X,k = kk, p = pp,n = nn,
                                                       loading_x = XX$gamma_1)))
  if(ncol(result_k_p)==201) break
}

result_k_p<-result_k_p[,-1] 

plot(x = result_k_p[5,],y = result_k_p[6,],xlim = c(min(result_k_p[5,]),max(result_k_p[5,])),
     ylim = c(min(result_k_p[6,]),max(result_k_p[6,])))

write.xlsx(t(result_k_p),"permutation1.xlsx")

##第二组
nn<-120
kk<-5
pp<-15
ra<-0.2
result_k_p1<-c(1,2,3,4,5,6)
set.seed(1234)
for (ii in 1:200) {
  XX<-construct_factory(n = nn,k = kk,p = pp,ra = ra)
  result_k_p1<-cbind(result_k_p1,c(ra,kk,pp,nn,
                                   maxlikehood_factory(XX$X,k = kk, p = pp,n = nn,
                                                       loading_x = XX$gamma_1)))
  if(ncol(result_k_p1)==201) break
  }

result_k_p1<-result_k_p1[,-1] 

plot(x = result_k_p1[5,],y = result_k_p1[6,],xlim = c(min(result_k_p1[5,]),max(result_k_p1[5,])),
     ylim = c(min(result_k_p1[6,]),max(result_k_p1[6,])))

write.xlsx(t(result_k_p1),"permutation2.xlsx")

##第三组
nn<-120
kk<-6
pp<-12
ra<-0.2
result_k_p2<-c(1,2,3,4,5,6)
set.seed(839)
for (ii in 1:200) {
  XX<-construct_factory(n = nn,k = kk,p = pp,ra = ra)
  result_k_p2<-cbind(result_k_p2,c(ra,kk,pp,nn,
                                   maxlikehood_factory(XX$X,k = kk, p = pp,n = nn,
                                                       loading_x = XX$gamma_1)))
  if(ncol(result_k_p2)==201) break
}

result_k_p2<-result_k_p2[,-1] 

plot(x = result_k_p2[5,],y = result_k_p2[6,],xlim = c(min(result_k_p2[5,]),max(result_k_p2[5,])),
     ylim = c(min(result_k_p2[6,]),max(result_k_p2[6,])))

write.xlsx(t(result_k_p2),"permutation3.xlsx")




##第四组
nn<-150
kk<-4
pp<-15
ra<-0.2
result_k_p3<-c(1,2,3,4,5,6)
set.seed(8899)
for (ii in 1:200) {
  XX<-construct_factory(n = nn,k = kk,p = pp,ra = ra)
  result_k_p3<-cbind(result_k_p3,c(ra,kk,pp,nn,
                                   maxlikehood_factory(XX$X,k = kk, p = pp,n = nn,
                                                       loading_x = XX$gamma_1)))
  if(ncol(result_k_p3)==201) break
}


result_k_p3<-result_k_p3[,-1] 

plot(x = result_k_p3[5,],y = result_k_p3[6,],xlim = c(min(result_k_p3[5,]),max(result_k_p3[5,])),
     ylim = c(min(result_k_p3[6,]),max(result_k_p3[6,])))

write.xlsx(t(result_k_p3),"permutation4.xlsx")





##第五组
nn<-150
kk<-5
pp<-12
ra<-0.2
result_k_p4<-c(1,2,3,4,5,6)
set.seed(28794)
for (ii in 1:200) {
  XX<-construct_factory(n = nn,k = kk,p = pp,ra = ra)
  result_k_p4<-cbind(result_k_p4,c(ra,kk,pp,nn,
                                   maxlikehood_factory(XX$X,k = kk, p = pp,n = nn,
                                                       loading_x = XX$gamma_1)))
  if(ncol(result_k_p4)==201) break
}


result_k_p4<-result_k_p4[,-1] 

plot(x = result_k_p4[5,],y = result_k_p4[6,],xlim = c(min(result_k_p4[5,]),max(result_k_p4[5,])),
     ylim = c(min(result_k_p4[6,]),max(result_k_p4[6,])))

write.xlsx(t(result_k_p4),"permutation5.xlsx")




##第六组
nn<-150
kk<-6
pp<-10
ra<-0.2

result_k_p5<-c(1,2,3,4,5,6)
set.seed(657)
for (ii in 1:200) {
  XX<-construct_factory(n = nn,k = kk,p = pp,ra = ra)
  result_k_p5<-cbind(result_k_p5,c(ra,kk,pp,nn,
                                   maxlikehood_factory(XX$X,k = kk, p = pp,n = nn,
                                                       loading_x = XX$gamma_1)))
  if(ncol(result_k_p5)==201) break
}


result_k_p5<-result_k_p5[,-1] 

plot(x = result_k_p5[5,],y = result_k_p5[6,],xlim = c(min(result_k_p5[5,]),max(result_k_p5[5,])),
     ylim = c(min(result_k_p5[6,]),max(result_k_p5[6,])))

write.xlsx(t(result_k_p5),"permutation6.xlsx")






##第七组
nn<-180
kk<-4
pp<-12
ra<-0.2

result_k_p6<-c(1,2,3,4,5,6)
set.seed(6927)
for (ii in 1:200) {
  XX<-construct_factory(n = nn,k = kk,p = pp,ra = ra)
  result_k_p6<-cbind(result_k_p6,c(ra,kk,pp,nn,
                                   maxlikehood_factory(XX$X,k = kk, p = pp,n = nn,
                                                       loading_x = XX$gamma_1)))
  if(ncol(result_k_p6)==201) break
}


result_k_p6<-result_k_p6[,-1] 

plot(x = result_k_p6[5,],y = result_k_p6[6,],xlim = c(min(result_k_p6[5,]),max(result_k_p6[5,])),
     ylim = c(min(result_k_p6[6,]),max(result_k_p6[6,])))

write.xlsx(t(result_k_p6),"permutation7.xlsx")


##第八组
nn<-180
kk<-5
pp<-10
ra<-0.2

result_k_p7<-c(1,2,3,4,5,6)
set.seed(8357)
for (ii in 1:200) {
  XX<-construct_factory(n = nn,k = kk,p = pp,ra = ra)
  result_k_p7<-cbind(result_k_p7,c(ra,kk,pp,nn,
                                   maxlikehood_factory(XX$X,k = kk, p = pp,n = nn,
                                                       loading_x = XX$gamma_1)))
  if(ncol(result_k_p7)==201) break
}


result_k_p7<-result_k_p7[,-1] 

plot(x = result_k_p7[5,],y = result_k_p7[6,],xlim = c(min(result_k_p7[5,]),max(result_k_p7[5,])),
     ylim = c(min(result_k_p7[6,]),max(result_k_p7[6,])))

write.xlsx(t(result_k_p7),"permutation8.xlsx")


##第九组
nn<-180
kk<-6
pp<-15
ra<-0.2

result_k_p8<-c(1,2,3,4,5,6)
set.seed(2021)
for (ii in 1:200) {
  XX<-construct_factory(n = nn,k = kk,p = pp,ra = ra)
  result_k_p8<-cbind(result_k_p8,c(ra,kk,pp,nn,
                                   maxlikehood_factory(XX$X,k = kk, p = pp,n = nn,
                                                       loading_x = XX$gamma_1)))
  if(ncol(result_k_p8)==201) break
}


result_k_p8<-result_k_p8[,-1] 

plot(x = result_k_p8[5,],y = result_k_p8[6,],xlim = c(min(result_k_p8[5,]),max(result_k_p8[5,])),
     ylim = c(min(result_k_p8[6,]),max(result_k_p8[6,])))

write.xlsx(t(result_k_p8),"permutation9.xlsx")
