library(psych)
library(openxlsx)
playerdata<-read.csv('playerdata.csv')
playerdata_x<-as.matrix(playerdata[1:200,1:6])
KMO(cor(playerdata_x))
cortest.bartlett(cor(playerdata_x),n = 200)
result2<-factanal(playerdata_x,factors = 3,scores='regression')
result2$loadings%*%t(result2$loadings)
eigen(result2$loadings%*%t(result2$loadings))
cor_playerdata<-cor(playerdata_x)

c1<-c(0.796,0.537,0.881,0.576,0.820)
c2<-c(0.485,0.820,0.336,0.746,0.535,0.121)
c3<-c(0.183,0.148,0.988)

finalscore<-result2[["scores"]][1:6,]
Finalscore<-(45*result2[["scores"]][1:6,1] + 31.3*result2[["scores"]][1:6,2] + 
               17.2*result2[["scores"]][1:6,3])/(45+31.3+17.2)


#数据中心化标准化
scale_data<-scale(playerdata_x)
#估计因子得分


score1<-result1$scores

D <- it   #特殊方差
A <- T  #载荷矩阵
D1 <- solve(D)  #D的逆矩阵
x <- scale_data

#回归方法
score2 <- t(t(T)%*% solve(cor_playerdata)%*%t(scale_data))

score3<-result2$scores


##得残差阵
U_ans1<-scale_data - score1%*%t(result1$loadings)
U_ans2<-scale_data - score2%*%t(T)
U_ans3<-scale_data - score3%*%t(result2$loadings)

set.seed(12345)
##置换检验
U_index1<-U_ans1[sample(1:200,200),1]
for (ii in 2:6){
  U_index1<-cbind(U_index1,U_ans1[sample(1:200,200),ii])
}
X2<-score1%*%t(result1$loadings) + U_index1
##再次求解
ans2<-factanal(X2,factors = 3,scores = "regression")
##对比结果
diff1<-sum(abs(result1$loadings%*%t(result1$loadings) - ans2$loadings%*%t(ans2$loadings)))/sum(abs(result1$loadings%*%t(result1$loadings)))


U_index2<-U_ans2[sample(1:200,200),1]
for (ii in 2:6){
  U_index2<-cbind(U_index2,U_ans2[sample(1:200,200),ii])
}
X3<-score2%*%t(T) + U_index2
##再次求解
ans3<-factanal(X3,factors = 3,scores = "regression")
##对比结果
diff2<-sum(abs(T%*%t(T) - ans3$loadings%*%t(ans3$loadings)))/sum(abs(T%*%t(T)))

U_index3<-U_ans3[sample(1:200,200),1]
for (ii in 2:6){
  U_index3<-cbind(U_index3,U_ans3[sample(1:200,200),ii])
}
X4<-score3%*%t(result2$loadings) + U_index3
##再次求解
ans4<-factanal(X4,factors = 3,scores = "regression")
##对比结果
diff3<-sum(abs(result2$loadings%*%t(result2$loadings) - ans4$loadings%*%t(ans4$loadings)))/sum(abs(result2$loadings%*%t(result2$loadings)))


result2$loadings

