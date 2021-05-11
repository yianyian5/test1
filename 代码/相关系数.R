library(openxlsx)
cor_permutation<-numeric(9)
X1<-read.xlsx('permutation1.xlsx')
cor_permutation[1]<-cor(X1$V5,X1$V6,method = 'pearson')

X2<-read.xlsx('permutation2.xlsx')
cor_permutation[2]<-cor(X2$V5,X2$V6,method = 'pearson')

X3<-read.xlsx('permutation3.xlsx')
cor_permutation[3]<-cor(X3$V5,X3$V6,method = 'pearson')

X4<-read.xlsx('permutation4.xlsx')
cor_permutation[4]<-cor(X4$V5,X4$V6,method = 'pearson')

X5<-read.xlsx('permutation5.xlsx')
cor_permutation[5]<-cor(X5$V5,X5$V6,method = 'pearson')

X6<-read.xlsx('permutation6.xlsx')
cor_permutation[6]<-cor(X6$V5,X6$V6,method = 'pearson')

X7<-read.xlsx('permutation7.xlsx')
cor_permutation[7]<-cor(X7$V5,X7$V6,method = 'pearson')

X8<-read.xlsx('permutation8.xlsx')
cor_permutation[8]<-cor(X8$V5,X8$V6,method = 'pearson')

X9<-read.xlsx('permutation9.xlsx')
cor_permutation[9]<-cor(X9$V5,X9$V6,method = 'pearson')

cor_permutation
