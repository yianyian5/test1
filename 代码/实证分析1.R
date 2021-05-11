library(psych)
library(openxlsx)
playerdata<-read.csv('playerdata.csv')
playerdata1<-read.csv('data.csv')
describe(playerdata1)
playerdata_x<-as.matrix(playerdata[1:200,1:6])
KMO(cor(playerdata_x))
cortest.bartlett(cor(playerdata_x),n = 180)
cor_playerdata<-cor(playerdata_x)
fa.parallel(cor_playerdata,n.obs = 200,fa='fa')

result1<-fa(playerdata_x,nfactors = 3,rotate='none',scores='regression',fm = 'pa')
result1$loadings%*%t(result1$loadings) + result1$residual
eigen(result1$loadings%*%t(result1$loadings))

solution<-list()
     


ii<-6
XX<-cor_playerdata-result1$residual
XX<-XX[,-ii]
eigen(t(XX)%*%XX)
eigen(XX)

e<-numeric(ncol(cor_playerdata))
e[ii]<-1
XX<-cbind(XX,e)
eigen(t(XX)%*%XX)
write.xlsx(XX,'XX.xlsx')

YY<-cor(playerdata_x) - result1$residual + diag(c(0,0,0,0,0,0.3))
eigen(YY)


it<-result1$residual - diag(c(0,0,0,0,0,0.3))
sigma<-cor_playerdata-it
result0<-eigen(sigma)

T<-result0$vectors[,1:3]%*%diag(sqrt(result0$values[1:3]))
cor_playerdata-T%*%t(T)
T%*%t(T) + it

