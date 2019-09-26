#use package
library(geosphere)
library(devtools)
library(baidumap)
library(ggmap)
library(ggplot2)
library(ridge)
library(car)
install_github('badbye/baidumap')

#plot on map
house=read.csv("house.csv",header = TRUE,fileEncoding="GBK")
plot(house$LONG,house$LAT,xlim = c(116.15,116.6),ylim = c(39.8,40.08),pch=19,col="blue")
options(baidumap.key ='gjB1Z6GO4p8yPNh0G7VPsekPchRaqbbp')
ggmap(getBaiduMap('北京市', width=1000, height=1000, zoom=12, scale = 2, messaging=FALSE))
+geom_point(data=house,aes(x=house$LONG,y=house$LAT),colour="blue")

#compute the distance to subway and school
subway=read.csv("subway.csv",header = TRUE)
Mindistance1=c()
for (i in 1:dim(house)[1]) {
  MIN=1000000
  for (j in 1:dim(subway)[1]) {
    t1=c(house$LONG[i],house$LAT[i])
    t2=c(subway$Lon[j],subway$Lat[j])
    t3=c(subway$Lon[j],house$LAT[i])
    tmp1=rbind(t1=t1,t3=t3)
    tmp2=rbind(t2=t2,t3=t3)
    dis1=distm(tmp1)
    dis2=distm(tmp2)
    distance=dis1[1,2]+dis2[1,2]
    if (distance<MIN){MIN=distance}
  }
  Mindistance1[i]=MIN
  MIN=100000
}
primaryschool=read.csv("primaryschool.csv",header = TRUE)
Mindistance2=c()
for (i in 1:dim(house)[1]) {
  MIN=1000000
  for (j in 1:dim(primaryschool)[1]) {
    t1=c(house$LONG[i],house$LAT[i])
    t2=c(primaryschool$Lon[j],primaryschool$Lat[j])
    t3=c(primaryschool$Lon[j],house$LAT[i])
    tmp1=rbind(t1=t1,t3=t3)
    tmp2=rbind(t2=t2,t3=t3)
    dis1=distm(tmp1)
    dis2=distm(tmp2)
    distance=dis1[1,2]+dis2[1,2]
    if (distance<MIN){MIN=distance}
  }
  Mindistance2[i]=MIN
  MIN=100000
}
middleschool=read.csv("middleschool.csv",header = TRUE)
Mindistance3=c()
for (i in 1:dim(house)[1]) {
  MIN=1000000
  for (j in 1:dim(middleschool)[1]) {
    t1=c(house$LONG[i],house$LAT[i])
    t2=c(middleschool$Lon[j],middleschool$Lat[j])
    t3=c(middleschool$Lon[j],house$LAT[i])
    tmp1=rbind(t1=t1,t3=t3)
    tmp2=rbind(t2=t2,t3=t3)
    dis1=distm(tmp1)
    dis2=distm(tmp2)
    distance=dis1[1,2]+dis2[1,2]
    if (distance<MIN){MIN=distance}
  }
  Mindistance3[i]=MIN
  MIN=100000
}

#set var
data=house
data=data[-c(9,10,11,12)]
data$distanceTOsubway=Mindistance1
data$distanceTOprimary=Mindistance2
data$distanceTOmiddles=Mindistance3
data$CATE=factor(data$CATE)
data$floor=factor(data$floor)
save(data,file="house.Rdata")

#plot
#北京二手房
#画出房价的频数直方图
x=data$price
hist(x,xlab="单位面积房价（万元/平方米）",ylab="频数")
#画出单位面积房价关于卧室数量的箱线图
A<-subset(data$price,data$bedrooms=="1")
B<-subset(data$price,data$bedrooms=="2")
C<-subset(data$price,data$bedrooms=="3")
D<-subset(data$price,data$bedrooms=="4")
E<-subset(data$price,data$bedrooms=="5")
boxplot(A,B,C,D,E,xlab="卧室数量",ylab="单位面积房价（元/平方米）",col="lightblue")
title("单位面积房价关于卧室数量的箱线图")
#画出单位面积房价关于客厅数量的箱线图
a<-subset(data$price,data$halls=="0")
b<-subset(data$price,data$halls=="1")
c<-subset(data$price,data$halls=="2")
boxplot(a,b,c,names=c("0","1","2"),xlab="客厅数量",ylab="单位面积房价（元/平方米）",col="lightblue")
title("单位面积房价关于客厅数量的箱线图")
#画出单位面积房价关于楼层的箱线图
d<-subset(data$price,data$floor=="low")
e<-subset(data$price,data$floor=="middle")
f<-subset(data$price,data$floor=="high")
g<-subset(data$price,data$floor=="basement")
boxplot(d,e,f,g,names=c("low","middle","high","basement"),xlab="楼层",ylab="单位面积房价（元/平方米）",col="lightblue")
title("单位面积房价关于楼层的箱线图")
#房价关于所在城区的箱线图
y<-data$price;y
n<-length(y);n
x1<-subset(data,data$CATE=="chaoyang")
x1#将朝阳区所在行提取出来
n1<-length(x1$CATE);n1
x2<-subset(data,data$CATE=="haidian")
x2#将海淀区所在行提取出来
n2<-length(x2$CATE);n2
x3<-subset(data,data$CATE=="fengtai")
x3#将丰台区所在行提取出来
n3<-length(x3$CATE);n3
x4<-subset(data,data$CATE=="xicheng")
x4#将西城区所在行提取出来
n4<-length(x4$CATE);n4
x5<-subset(data,data$CATE=="dongcheng")
x5#将东城区所在行提取出来
n5<-length(x5$CATE);n5
x6<-subset(data,data$CATE=="shijingshan")
x6#将石景山区所在行提取出来
n6<-length(x6$CATE);n6
y1<-x1$price;y1
y2<-x2$price;y2
y3<-x3$price;y3
y4<-x4$price;y4
y5<-x5$price;y5
y6<-x6$price;y6
y<-c(y1,y2,y3,y4,y5,y6);y
boxplot(y1,y2,y3,y4,y5,y6,names=c("朝阳","海淀","丰台","西城","东城","石景山"),ylab="单位面积房价（元/平方米）",col="lightblue")
title("房价关于所在城区的箱线图")
#房价关于地铁的箱线图
subway1<-subset(data,data$subway==1)
subway1#所在行提取出来
subway0<-subset(data,data$subway==0)
subway0#所在行提取出来
ysubway1<-subway1$price;ysubway1
ysubway0<-subway0$price;ysubway0
boxplot(ysubway1,ysubway0,names=c("靠近地铁","不靠近地铁"),ylab="单位面积房价（元/平方米）",col=c("lightblue", "pink"))
title("房价关于地铁的箱线图")
#房价关于学区的箱线图
school1<-subset(data,data$school==1)
school1#所在行提取出来
school0<-subset(data,data$subway==0)
school0#所在行提取出来
yschool1<-school1$price;yschool1
yschool0<-school0$price;yschool0
boxplot(yschool1,yschool0,names=c("学区房","非学区房"),ylab="单位面积房价（元/平方米）",col=c("lightblue", "pink"))
title("房价关于学区的箱线图")

#simple liner regression
lm1=lm(data=data,price~CATE+bedrooms+halls+AREA+floor+subway+school
       +distanceTOsubway+distanceTOprimary+distanceTOmiddle)
summary(lm1)

#log transfrom simple liner regression
lm21=lm(data=data,log(price)~CATE+bedrooms+halls+AREA+floor+subway+school
       +distanceTOsubway+distanceTOprimary+distanceTOmiddle)
summary(lm21)#R^2=0.6081
lm22=lm(data=data,price~CATE+bedrooms+halls+log(AREA)+floor+subway+school
         +distanceTOsubway+distanceTOprimary+distanceTOmiddle)
summary(lm22)#R^2=0.5868
lm23=lm(data=data,log(price)~CATE+bedrooms+halls+log(AREA)+floor+subway+school
         +distanceTOsubway+distanceTOprimary+distanceTOmiddle)
summary(lm23)#R^2=0.6152
lm24=lm(data=data,log(price)~CATE+bedrooms+halls+log(AREA)+floor+subway+school
         +log(distanceTOsubway)+log(distanceTOprimary)+log(distanceTOmiddle))
summary(lm24)#R^2=0.6180

#log transform interaction liner regression
lm31=lm(data=data,log(price)~CATE+bedrooms+halls+AREA+floor+subway+school
                  +distanceTOsubway+distanceTOprimary+distanceTOmiddle
                  +subway*AREA+subway*halls+subway*bedrooms
                  +subway*distanceTOsubway+subway*distanceTOprimary+subway*distanceTOmiddle
                  +school*AREA+school*halls+school*bedrooms
                  +school*distanceTOsubway+school*distanceTOprimary+school*distanceTOmiddle
                  +floor*AREA+floor*halls+floor*bedrooms
                  +floor*distanceTOsubway+floor*distanceTOprimary+floor*distanceTOmiddle
                  +CATE*AREA+CATE*halls+CATE*bedrooms
                  +CATE*distanceTOsubway+CATE*distanceTOprimary+CATE*distanceTOmiddle)
summary(lm31)#R^2=0.6353
lm32=lm(data=data,log(price)~CATE+bedrooms+halls+AREA+floor+subway+school
       +distanceTOsubway+distanceTOprimary+distanceTOmiddle
       +subway*AREA+subway*halls+subway*bedrooms
       +subway*distanceTOsubway+subway*distanceTOprimary+subway*distanceTOmiddle
       +school*AREA+school*halls+school*bedrooms
       +school*distanceTOsubway+school*distanceTOprimary+school*distanceTOmiddle
       +CATE*AREA+CATE*halls+CATE*bedrooms
       +CATE*distanceTOsubway+CATE*distanceTOprimary+CATE*distanceTOmiddle)
summary(lm32)#R^2=0.6316
lm33=lm(data=data,log(price)~CATE+bedrooms+halls+log(AREA)+floor+subway+school
       +log(distanceTOsubway)+log(distanceTOprimary)+log(distanceTOmiddle)
       +subway*log(AREA)+subway*halls+subway*bedrooms
       +subway*distanceTOsubway+subway*distanceTOprimary+subway*distanceTOmiddle
       +school*AREA+school*halls+school*bedrooms
       +school*distanceTOsubway+school*distanceTOprimary+school*distanceTOmiddle
       +CATE*AREA+CATE*halls+CATE*bedrooms
       +CATE*distanceTOsubway+CATE*distanceTOprimary+CATE*distanceTOmiddle)
summary(lm33)#R^2=0.6460
lm34=lm(data=data,log(price)~CATE+bedrooms+halls+log(AREA)+floor+subway+school
       +log(distanceTOsubway)+log(distanceTOprimary)+log(distanceTOmiddle)
       +subway*log(AREA)+subway*halls+subway*bedrooms
       +subway*distanceTOsubway+subway*distanceTOprimary+subway*distanceTOmiddle
       +school*AREA+school*halls+school*bedrooms
       +school*distanceTOsubway+school*distanceTOprimary+school*distanceTOmiddle
       +floor*AREA+floor*halls+floor*bedrooms
       +floor*distanceTOsubway+floor*distanceTOprimary+floor*distanceTOmiddle
       +CATE*AREA+CATE*halls+CATE*bedrooms
       +CATE*distanceTOsubway+CATE*distanceTOprimary+CATE*distanceTOmiddle)
summary(lm34)#R^2=0.6497
lm34step=step(lm34)
summary(lm3step)
vif(lm3);vif(lm3step)
lm3lr=lm.ridge(data=data,log(price)~CATE+bedrooms+halls+AREA+floor+subway+school
                +distanceTOsubway+distanceTOprimary+distanceTOmiddle
                +subway*AREA+subway*halls+subway*bedrooms
                +subway*distanceTOsubway+subway*distanceTOprimary+subway*distanceTOmiddle
                +school*AREA+school*halls+school*bedrooms
                +school*distanceTOsubway+school*distanceTOprimary+school*distanceTOmiddle
                +floor*AREA+floor*halls+floor*bedrooms
                +floor*distanceTOsubway+floor*distanceTOprimary+floor*distanceTOmiddle
                +CATE*AREA+CATE*halls+CATE*bedrooms
                +CATE*distanceTOsubway+CATE*distanceTOprimary+CATE*distanceTOmiddle)
meanlogprice=mean(log(price));meanlogprice
predictedlogprice=a%*%x
SSE=sum((log(price)-predictedlogprice)^2)
SST=sum((log(price)-meanlogprice)^2)
RR=1-SSE/SST;RR
summary(lm3lr)

#regression diagnostics
Reg_Diag=function(fm){
  n=nrow(fm$model);df=fm$df.residual
  p=n-df-1;t=c()
  res=residuals(fm);t1=t;t1=which(abs(res)==max(abs(res)))
  sta=rstandard(fm);t2=t;t2=which(abs(sta)>3.5)
  stu=rstudent(fm);t3=t;t3=which(abs(stu)>3.5)
  h=hatvalues(fm);t4=t;t4=which(h>2*(p+1)/n)
  d=dffits(fm);t5=t;t5=which(abs(d)>2*sqrt((p+1)/n))
  c=cooks.distance(fm);t6=t;t6=which(c==max(c))
  co=covratio(fm);abs_co=abs(co-1)
  t7=t;t7=which(abs_co==max(abs_co))
  result=list(res=t1,sta=t2,stu=t3,h=t4,d=t5,c=t6,absco=t7)
  opar=par(mfrow=c(2,2),oma=c(0,0,1.1,0),mar=c(4.1,4.1,2.1,1.1))
  plot(fm,1);plot(fm,2);plot(fm,3);plot(fm,4)
  par(opar)
  return(result)
}
resultlm34step=Reg_Diag(lm34step)
deletedata=c(resultlm34step$res,resultlm34step$sta,resultlm34step$stu,
             resultlm34step$c,resultlm34step$absco)
datacorrect=data[-deletedata,]
lm34datacorrect=lm(data=datacorrect,log(price)~CATE+bedrooms+halls+log(AREA)+floor+subway+school
        +log(distanceTOsubway)+log(distanceTOprimary)+log(distanceTOmiddle)
        +subway*log(AREA)+subway*halls+subway*bedrooms
        +subway*distanceTOsubway+subway*distanceTOprimary+subway*distanceTOmiddle
        +school*AREA+school*halls+school*bedrooms
        +school*distanceTOsubway+school*distanceTOprimary+school*distanceTOmiddle
        +floor*AREA+floor*halls+floor*bedrooms
        +floor*distanceTOsubway+floor*distanceTOprimary+floor*distanceTOmiddle
        +CATE*AREA+CATE*halls+CATE*bedrooms
        +CATE*distanceTOsubway+CATE*distanceTOprimary+CATE*distanceTOmiddle)
summary(lm34datacorrect)
lm34stepdatacorrect=step(lm34datacorrect)
summary(lm34stepdatacorrect)

#hypothetical test
#lm24&lm34
n=nrow(data);p=length(lm34$coefficients)-1;k=length(lm24$coefficients)
SSEFM=sum((lm34$residuals)^2)#计算得全模型的残差平方
SSERM=sum((lm33$residuals)^2)#计算得简模型的残差平方和
F1=((SSERM-SSEFM)/(p+1-k))/(SSEFM/(n-p-1));F1#计算得统计量F的观测值
qf(0.95,p+1-k,n-p-1)#计算得统计量F的临界值
#将统计量F的观测值与统计量F的临界值作比较得拒绝原假设,lm34更恰当
#lm33&lm34
n=nrow(data);p=length(lm34$coefficients)-1;k=length(lm33$coefficients)
SSEFM=sum((lm34$residuals)^2)#计算得全模型的残差平方
SSERM=sum((lm33$residuals)^2)#计算得简模型的残差平方和
F1=((SSERM-SSEFM)/(p+1-k))/(SSEFM/(n-p-1));F1#计算得统计量F的观测值
qf(0.95,p+1-k,n-p-1)#计算得统计量F的临界值
#将统计量F的观测值与统计量F的临界值作比较得拒绝原假设,lm34更恰当
#lm34step&lm34
n=nrow(data);p=length(lm34$coefficients);k=length(lm34step$coefficients)
SSEFM=sum((lm34$residuals)^2)#计算得全模型的残差平方
SSERM=sum((lm34step$residuals)^2)#计算得简模型的残差平方和
F1=((SSERM-SSEFM)/(p+1-k))/(SSEFM/(n-p-1));F1#计算得统计量F的观测值
qf(0.95,p+1-k,n-p-1)#计算得统计量F的临界值
#将统计量F的观测值与统计量F的临界值作比较得接受原假设,lm34step更恰当

#predict
predictdata=data.frame(CATE="xicheng",bedrooms=2,halls=1,AREA=85,
                        floor="low",subway=1,school=1,distanceTOsubway=1000,
                        distanceTOprimary=1000,distanceTOmiddle=1000)
predictdata1=data.frame(CATE=rep("xicheng",1000),bedrooms=rep(2,1000),halls=rep(1,1000),AREA=rep(85,1000),
                       floor=rep("low",1000),subway=rep(1,1000),school=rep(1,1000),distanceTOsubway=seq(1,5000,by=5),
                       distanceTOprimary=rep(1000,1000),distanceTOmiddle=rep(1000,1000))
predictdata2=data.frame(CATE=rep("xicheng",1000),bedrooms=rep(2,1000),halls=rep(1,1000),AREA=rep(85,1000),
                       floor=rep("low",1000),subway=rep(1,1000),school=rep(1,1000),distanceTOsubway=rep(1000,1000),
                       distanceTOprimary=seq(1,5000,by=5),distanceTOmiddle=rep(1000,1000))
predictdata3=data.frame(CATE=rep("xicheng",1000),bedrooms=rep(2,1000),halls=rep(1,1000),AREA=rep(85,1000),
                       floor=rep("low",1000),subway=rep(1,1000),school=rep(1,1000),distanceTOsubway=rep(1000,1000),
                       distanceTOprimary=rep(1000,1000),distanceTOmiddle=seq(1,5000,by=5))
predictdata4=data.frame(CATE=rep("xicheng",1000),bedrooms=rep(2,1000),halls=rep(2,1000),AREA=rep(85,1000),
                       floor=rep("low",1000),subway=rep(1,1000),school=rep(1,1000),distanceTOsubway=seq(1,5000,by=5),
                       distanceTOprimary=rep(1000,1000),distanceTOmiddle=rep(1000,1000))
predictdata5=data.frame(CATE=rep("xicheng",1000),bedrooms=rep(2,1000),halls=rep(2,1000),AREA=rep(85,1000),
                       floor=rep("low",1000),subway=rep(1,1000),school=rep(1,1000),distanceTOsubway=rep(1000,1000),
                       distanceTOprimary=seq(1,5000,by=5),distanceTOmiddle=rep(1000,1000))
predictdata6=data.frame(CATE=rep("xicheng",1000),bedrooms=rep(2,1000),halls=rep(2,1000),AREA=rep(85,1000),
                       floor=rep("low",1000),subway=rep(1,1000),school=rep(1,1000),distanceTOsubway=rep(1000,1000),
                       distanceTOprimary=rep(1000,1000),distanceTOmiddle=seq(1,5000,by=5))
pred=predict(lm34stepdatacorrect,predictdata,interval = "prediction",level = 0.95)
total=pred*85
pred1=predict(lm34stepdatacorrect,predictdata1,interval = "prediction",level = 0.95)
pred2=predict(lm34stepdatacorrect,predictdata2,interval = "prediction",level = 0.95)
pred3=predict(lm34stepdatacorrect,predictdata3,interval = "prediction",level = 0.95)
pred4=predict(lm34stepdatacorrect,predictdata4,interval = "prediction",level = 0.95)
pred5=predict(lm34stepdatacorrect,predictdata5,interval = "prediction",level = 0.95)
pred6=predict(lm34stepdatacorrect,predictdata6,interval = "prediction",level = 0.95)
opar=par(mfrow=c(2,3))
plot(seq(1,5000,by=5),pred1[,1],type="l",xlab = "distance to subway",ylab = "price")
title("price prediction for 1 hall")
plot(seq(1,5000,by=5),pred2[,1],type="l",xlab = "distance to primary school",ylab = "price")
title("price prediction for 1 hall")
plot(seq(1,5000,by=5),pred3[,1],type="l",xlab = "distance to middle school",ylab = "price")
title("price prediction for 1 hall")
plot(seq(1,5000,by=5),pred4[,1],type="l",xlab = "distance to subway",ylab = "price")
title("price prediction for 2 halls")
plot(seq(1,5000,by=5),pred5[,1],type="l",xlab = "distance to primary school",ylab = "price")
title("price prediction for 2 halls")
plot(seq(1,5000,by=5),pred6[,1],type="l",xlab = "distance to middle school",ylab = "price")
title("price prediction for 2 halls")
par(opar)
