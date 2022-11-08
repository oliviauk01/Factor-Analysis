setwd("/Users/olivia/Desktop/IMC/463 ML/Assignment/hw6")
music <- read.csv("music.csv", header = T)
library(psych)
head(music)
str(music)
alpha(music)
## set column names as numeric
colnames(music) <- c(1:28)
colnames(music)
principal(music[,])
fit = principal(music[,])
round(fit$values, 4)

## a.
## for prcomp
plot(prcomp(music[,c(1:27)]))
screeplot(prcomp(music[,c(1:27)]))
abline(h=1)
## for principal
fit1<-principal(music[,c(1:27)])
plot(1:27, fit1$values, type="b",xlab="The number of factors",ylab="Eigenvalues")
abline(h=1) # Kaiser criterion
#nfactor =3
newdat <- music[,c(1:27)]
## b.


## waynao's code

##b.
fit2 <- principal(newdat, nfactor =3)
print(fit2$loadings, cutoff=0.5, digits=3,sort=TRUE)

##c. 
alpha(music[,c(-4,-26,-28)])$total
#d.
alpha(music[,c(1:27)])

#e.
## rotation
fit3 = principal(music[,c(-4,-26,-28)], nfactor=3)
fit3
print(fit3$loadings, cutoff =0.5, digits=3, sort=TRUE)
round(fit3$values, 4)
#Cumulative Var        0.28 0.54 0.68
fit3$rot.mat
#[,1]       [,2]       [,3]
#[1,]  0.6330490 0.63716047  0.4396311
#[2,] -0.5050575 0.77034645 -0.3892085
#[3,] -0.5866565 0.02434908  0.8094697

round(fit3$loadings %*% fit3$rot.mat, 2)

## Results are the same
#sum(fit1$values[1:2])
#sum(fit2$values[1:2])

print(fit3$loadings, cutoff =0.5, digits=3, sort=TRUE)

fit3 = principal(music[,c(-4,-26,-28)], nfactor=3)
fit3
print(fit3$loadings, cutoff =0.5, digits=3, sort=TRUE)


##f.
fit4<-principal(music[,c(-4,-26,-28)],nfactor=3,score = TRUE, rotate="promax")
cor(fit4$scores)

fit5<-principal(music[,c(-28,-1,-17,-7,-14,-4,-26,-8,-9,-10,-11)],nfactor=3,score = TRUE, rotate="promax")
cor(fit5$scores)

fit6 <- principal(music[,c(-4,-6,-7,(-16:-20),(-22:-24),-27,-28)])


round(apply(cbind()))

##g.

music$factor1<-apply(music[,c(8:15,21,24)], 1, mean)
music$factor2<-apply(music[,c(1:3,5:7,16,19:20,23,25)], 1, mean)
music$factor3<-apply(music[,c(13,17:18,22,24,27)], 1, mean)

##
music$factor1<-apply(music[,c(12:13,15,21,24)], 1, mean)
music$factor2<-apply(music[,c(2:3,5:6,16,19,20,23,25)], 1, mean)
music$factor3<-apply(music[,c(13,18,22,24,27)], 1, mean)
fittime<-lm(time1~factor1+factor2+factor3,data=music)
summary(fittime)

## NFACTOR =3
##-28
#alpha(newdat)


fit = principal(music[,-28])
round(fit$values, 4)

## -4

principal(music[,-4])
fit = principal(music[,c(-4)])
round(fit$values, 4)



principal(music[,c(-4,-6,-7,(-16:-18),-28)])
fit = principal(music[,c(-4,-28)])
round(fit$values, 4)

test <- music[,c(-4,-6,-7,(-16:-20),(-22:-24),-27,-28)]
fit = principal(test)
#principal((music[,c(-4,-6,-7,(-16:-20),(-22:-24),-27,-28)])
round(fit$values, 4)
alpha(test)
alpha(music[,c(-4,-6,-7,(-16:-20),(-22:-24),-27,-28)])


fit$loadings

##?
round(fit$communality, 4)
round(diag(fit$loadings %*% t(fit$loadings)), 4)
round(fit$uniquenesses, 4)



cor(music, fit$scores)




## NFACTOR =2
fit3 = principal(music, nfactor=3, rotate="none")
fit3
## scree plot
plot(prcomp(music))
fit1 = principal(music, nfactor=2, rotate="none")
fit1
#Cumulative Var         0.52 0.61
plot(1:28, fit1$values, type="b")
abline(h=1)
fit1
#Cumulative Var         0.52 0.61

## test
plot(prcomp(test))
fit4 = principal(test, nfactor=2, rotate="none")
fit4
#Cumulative Var        0.62 0.71






### MY CODE
## rotation
plot(prcomp(music))
fit2 = principal(music, nfactor=2)
fit2
#Cumulative Var        0.32 0.61
fit2$rot.mat
#           [,1]      [,2]
#[1,]  0.7364474 0.6764948
#[2,] -0.6764948 0.7364474
round(fit2$loadings %*% fit2$rot.mat, 2)

## Results are the same
sum(fit1$values[1:2])
sum(fit2$values[1:2])

print(fit2$loadings, cutoff =0.3, digits=3, sort=TRUE)
