#################################################################
###      ESTIMACIÓN DE ESCENARIOS PARA LA SALMONICULTURA      ###  
###                  Analisis de Riesgo                       ###
###      Ajuste Dist. Probabilidad - Método montecarlo        ###  
###                  <Amadeo Guzmán C.>                       ###
#################################################################

#leer datos
coho<- read.csv2("BD Escenarios Aquacapital - Coho.csv", header = TRUE)
head(coho,3)
dim(coho)
str(coho)
names(coho)


library(rriskDistributions)

table(coho$Especie)
table(coho$Especie, coho$Region)


#==============================================  Salmon Coho  =========================================================#

## ==================================
##    MORTALIDAD ACUMULADA (%)    
## ==================================

#---------------
# [ X Región]
#---------------

#Ajustar Distribución
hist(coho[coho$Region=="X",]$Mort.Acump)

#ajustar a dist. de probabilidad
res1.mortX<-fit.cont(data2fit=coho[coho$Region=="X",]$Mort.Acump) 
res1.mortX


#Simular 5000 resultados posibles en base a parametros fijos
simular<- rgamma(5000, shape=2.0506880, rate = 0.2344932) 
hist(simular, freq = TRUE, col="lightsalmon",main="Histograma",sub="Datos simulados de una Dist. Beta",
     breaks = 20)

hist(simular, freq = FALSE, col="lightsalmon",main="Histograma",sub="Datos simulados de una Dist. Beta; Repet=5000",
     breaks = 20)
#agregar curva teorica (gamma)
x<- seq(0,100, by=1)
curve(dgamma(x, shape=2.0506880, rate = 0.2344932),col="blue",lwd=2,add=TRUE)


#F(x) de densidad
dgamma(10, shape=2.0506880, rate = 0.2344932, log=FALSE)*100 #probabilidad de tener un resultado = 10p mort acum
#F(x) de probabilidad
pgamma(10, shape=2.0506880, rate = 0.2344932, lower.tail=TRUE, log.p = FALSE)*100 #Prob resultado <10p

pgamma(10, shape=2.0506880, rate = 0.2344932, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >10p
pgamma(15, shape=2.0506880, rate = 0.2344932, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >15p
pgamma(25, shape=2.0506880, rate = 0.2344932, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >25p

vProbX=pgamma(c(10,20), shape=2.0506880, rate = 0.2344932, lower.tail=TRUE) #Probabilidad de valor entre 10 y 15p
vProbX
miProbX=vProbX[2]-vProbX[1]
miProbX*100


valor_5pct<- qgamma(0.05,shape=2.0506880, rate = 0.2344932, lower.tail=TRUE) #valor que corta el 5% de los datos (percentil 5)
valor_95pct<- qgamma(0.95,shape=2.0506880, rate = 0.2344932, lower.tail=TRUE) #valor que corta el 95% de los datos (percentil 95)

valor_inf<- qgamma(0.0001,shape=2.0506880, rate = 0.2344932, lower.tail=TRUE) #solo ubicar el texto en el gráfico
valor_sup<- qgamma(0.999,shape=2.0506880, rate = 0.2344932, lower.tail=TRUE) #solo ubicar el texto en el gráfico


hist(simular, freq = FALSE, xlim=c(0,100), col="skyblue",main="Histograma",sub="Datos simulados de una Dist. Gamma; Repet=5000",
     breaks = 20)
x<- seq(0,100, by=1)
curve(dgamma(x, shape=2.0506880, rate = 0.2344932),col="darkblue",lwd=2,add=TRUE)
abline(v=valor_5pct, col="red", lty=1, lwd=2)
abline(v=valor_95pct, col="red", lty=1, lwd=2)
text(x=((valor_5pct - valor_inf)/2 + valor_inf), y=0.085, labels="5%     ")
text(x=((valor_sup - valor_95pct)/2 + valor_95pct), y=0.085, labels=" 5% -> ")



#curvas de probabilidad acumulada 
curve(pgamma(x, shape=2.0506880, rate = 0.2344932),xlim=c(-10,100), col="blue",lwd=2,
      xlab="x",ylab="F(x)",main="Función de Distribución N(50,38)")
abline(-12,1)

curve(1-pgamma(x, shape=2.0506880, rate = 0.2344932),xlim=c(-10,100),col="blue",lwd=2,
      xlab="Mort. Acum (%)",ylab="F(x)",main="Función de Distribución")
abline(v=10, col="green", lty=2)
abline(v=15, col="orange", lty=2)
abline(v=25, col="red", lty=2)



#---------------
# [ XI Región]
#---------------

#Ajustar Distribución
hist(coho[coho$Region=="XI",]$Mort.Acump)

#ajustar a dist. de probabilidad
res1.mortXI<-fit.cont(data2fit=coho[coho$Region=="XI",]$Mort.Acump) 
res1.mortXI

#Simular 5000 resultados posibles en base a parametros fijos 
simularXI<- rlnorm(5000, meanlog=1.3899559, sdlog = 0.5371503) 
hist(simularXI,freq = TRUE, xlim=c(0,100), col="lightsalmon",main="Histograma",sub="Datos simulados de una Dist. Log-normal",
     breaks = 25)

hist(simularXI, freq = FALSE, xlim=c(0,100), col="lightsalmon",main="Histograma",sub="Datos simulados de una Dist. Log-normal; Repet=5000",
     breaks = 20)
#agregar curva teorica (gamma)
x<- seq(0,100, by=1)
curve(dlnorm(x, meanlog=1.3899559, sdlog = 0.5371503),col="blue",lwd=2,add=TRUE)


#F(x) de densidad
dlnorm(10, meanlog=1.3899559, sdlog = 0.5371503, log=FALSE)*100 #probabilidad de tener un resultado = 10p mort acum
#F(x) de probabilidad
plnorm(10, meanlog=1.3899559, sdlog = 0.5371503, lower.tail=TRUE, log.p = FALSE)*100 #Prob resultado <10p

plnorm(10, meanlog=1.3899559, sdlog = 0.5371503, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >10p
plnorm(15, meanlog=1.3899559, sdlog = 0.5371503, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >15p
plnorm(25, meanlog=1.3899559, sdlog = 0.5371503, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >25p


vProbXI=plnorm(c(10,20), meanlog=1.3899559, sdlog = 0.5371503, lower.tail=TRUE) #Probabilidad de valor entre 10 y 15p
vProbXI
miProbXI=vProbXI[2]-vProbXI[1]
miProbXI*100


valor_5pct<- qlnorm(0.05, meanlog=1.3899559, sdlog = 0.5371503, lower.tail=TRUE) #valor que corta el 5% de los datos (percentil 5)
valor_95pct<- qlnorm(0.95, meanlog=1.3899559, sdlog = 0.5371503, lower.tail=TRUE) #valor que corta el 95% de los datos (percentil 95)

valor_inf<- qlnorm(0.0001, meanlog=1.3899559, sdlog = 0.5371503, lower.tail=TRUE) #solo ubicar el texto en el gráfico
valor_sup<- qlnorm(0.999, meanlog=1.3899559, sdlog = 0.5371503, lower.tail=TRUE) #solo ubicar el texto en el gráfico


hist(simularXI, freq = FALSE, xlim=c(0,100), col="skyblue",main="Histograma",sub="Datos simulados de una Dist. Log-Normal; Repet=5000",
     breaks = 20)
x<- seq(0,100, by=1)
curve(dlnorm(x, meanlog=1.3899559, sdlog = 0.5371503),col="darkblue",lwd=2,add=TRUE)
abline(v=valor_5pct, col="red", lty=1, lwd=2)
abline(v=valor_95pct, col="red", lty=1, lwd=2)
text(x=((valor_5pct - valor_inf)/2 + valor_inf), y=0.2, labels="5%     ")
text(x=((valor_sup - valor_95pct)/2 + valor_95pct), y=0.2, labels=" 5% -> ")



#curvas de probabilidad acumulada 
curve(plnorm(x, meanlog=1.3899559, sdlog = 0.5371503),xlim=c(-10,100), col="blue",lwd=2,
      xlab="x",ylab="F(x)",main="Función de Distribución")
abline(-10,1)

curve(1-plnorm(x, meanlog=1.3899559, sdlog = 0.5371503),xlim=c(-10,100),col="blue",lwd=2,
      xlab="x",ylab="F(x)",main="Función de Distribución")
abline(v=10, col="green", lty=2)
abline(v=15, col="orange", lty=2)
abline(v=25, col="red", lty=2)




## ==================================
##      CRECIMIENTO - GF3   
## ==================================

#---------------
# [ X Región]
#---------------

#Ajustar Distribución
hist(coho[coho$Region=="X",]$GF3)

#ajustar a dist. de probabilidad
res1.gf3X<-fit.cont(data2fit=coho[coho$Region=="X",]$GF3) 
res1.gf3X


#Simular 5000 resultados posibles en base a parametros fijos
simular.gf3X<- rweibull(5000, shape=8.415993, scale = 2.691221) 
hist(simular.gf3X, freq = TRUE, col="lightsalmon",main="Histograma",sub="Datos simulados de una Dist. Normal",
     breaks = 20)

hist(simular.gf3X, freq = FALSE, col="lightsalmon",main="Histograma",sub="Datos simulados de una Dist. Normal; Repet=5000",
     breaks = 20)
#agregar curva teorica 
x<- seq(0,100, by=1)
curve(dweibull(x, shape=8.415993, scale = 2.691221),col="blue",lwd=2,add=TRUE)


#F(x) de densidad
dweibull(1.8, shape=8.415993, scale = 2.691221, log=FALSE)*100 #probabilidad de tener un resultado = 1.8 gf3
#F(x) de probabilidad
pweibull(1.8, shape=8.415993, scale = 2.691221, lower.tail=TRUE, log.p = FALSE)*100 #Prob resultado <1.8

pweibull(1.7, shape=8.415993, scale = 2.691221, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >1.8
pweibull(1.9, shape=8.415993, scale = 2.691221, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >1.9
pweibull(2.1, shape=8.415993, scale = 2.691221, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >2.1


vProbX=pweibull(c(1.8,2.0), shape=8.415993, scale = 2.691221, lower.tail=TRUE) #Probabilidad de valor entre ....
vProbX
miProbX=vProbX[2]-vProbX[1]
miProbX*100


valor_5pct<- qweibull(0.05,shape=8.415993, scale = 2.691221, lower.tail=TRUE) #valor que corta el 5% de los datos (percentil 5)
valor_95pct<- qweibull(0.95,shape=8.415993, scale = 2.691221, lower.tail=TRUE) #valor que corta el 95% de los datos (percentil 95)

valor_inf<- qweibull(0.0001,shape=8.415993, scale = 2.691221, lower.tail=TRUE) #solo ubicar el texto en el gráfico
valor_sup<- qweibull(0.999,shape=8.415993, scale = 2.691221, lower.tail=TRUE) #solo ubicar el texto en el gráfico

hist(simular.gf3X, freq = FALSE, col="skyblue",main="Histograma",sub="Datos simulados de una Dist. Weibull; Repet=5000",
     breaks = 20)
x<- seq(0,100, by=1)
curve(dweibull(x, shape=8.415993, scale = 2.691221),col="darkblue",lwd=2,add=TRUE)
abline(v=valor_5pct, col="red", lty=1, lwd=2)
abline(v=valor_95pct, col="red", lty=1, lwd=2)
text(x=((valor_5pct - valor_inf)/2 + valor_inf), y=1.19, labels=" <- 5% ")
text(x=((valor_sup - valor_95pct)/2 + valor_95pct), y=1.19, labels=" 5% -> ")



#curvas de probabilidad acumulada 
curve(pweibull(x, shape=8.415993, scale = 2.691221),xlim=c(0,4), col="blue",lwd=2,
      xlab="GF3",ylab="F(x)",main="Función de Distribución")
abline(-1.8,1.1)

curve(1-pweibull(x, shape=8.415993, scale = 2.691221),xlim=c(0,4),col="blue",lwd=2,
      xlab="GF3",ylab="F(x)",main="Función de Distribución")
abline(v= 1.7, col="green", lty=2)
abline(v= 1.9, col="orange", lty=2)
abline(v= 2.1, col="red", lty=2)



#---------------
# [ XI Región]
#---------------

#Ajustar Distribución
hist(coho[coho$Region=="XI",]$GF3)

#ajustar a dist. de probabilidad
res1.gf3XI<-fit.cont(data2fit=coho[coho$Region=="XI",]$GF3) 
res1.gf3XI

#Simular 5000 resultados posibles en base a parametros fijos (dist. gamma "meanlog; sdlog")
simular.gf3XI<- rlnorm(5000, meanlog=1.02776687, sdlog = 0.09662289)
max(simular.gf3XI)
min(simular.gf3XI)
hist(simular.gf3XI,freq = TRUE, col="lightsalmon",main="Histograma",sub="Datos simulados de una Dist. Normal",
     breaks = 25)

hist(simular.gf3XI, freq = FALSE, col="lightsalmon",main="Histograma",sub="Datos simulados de una Dist. Normal; Repet=5000",
     breaks = 20)
#agregar curva teorica (gamma)
x<- seq(0,100, by=1)
curve(dlnorm(x, meanlog=1.02776687, sdlog = 0.09662289),col="blue",lwd=2,add=TRUE)


#F(x) de densidad
dlnorm(1.8, meanlog=1.02776687, sdlog = 0.09662289, log=FALSE)*100 #probabilidad de tener un resultado = 1.8
#F(x) de probabilidad
plnorm(1.8, meanlog=1.02776687, sdlog = 0.09662289, lower.tail=TRUE, log.p = FALSE)*100 #Prob resultado <1.8

plnorm(1.7, meanlog=1.02776687, sdlog = 0.09662289, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >1.7
plnorm(1.9, meanlog=1.02776687, sdlog = 0.09662289, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >1.9
plnorm(2.1, meanlog=1.02776687, sdlog = 0.09662289, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >2.1

vProbXI=plnorm(c(1.8,2.0), meanlog=1.02776687, sdlog = 0.09662289, lower.tail=TRUE) #Probabilidad de valor entre....
vProbXI
miProbXI=vProbXI[2]-vProbXI[1]
miProbXI*100

valor_5pct<- qlnorm(0.05,meanlog=1.02776687, sdlog = 0.09662289, lower.tail=TRUE) #valor que corta el 5% de los datos (percentil 5)
valor_95pct<- qlnorm(0.95,meanlog=1.02776687, sdlog = 0.09662289, lower.tail=TRUE) #valor que corta el 95% de los datos (percentil 95)

valor_inf<- qlnorm(0.0001,meanlog=1.02776687, sdlog = 0.09662289, lower.tail=TRUE) #solo ubicar el texto en el gráfico
valor_sup<- qlnorm(0.999,meanlog=1.02776687, sdlog = 0.09662289, lower.tail=TRUE) #solo ubicar el texto en el gráfico

hist(simular.gf3XI, freq = FALSE, col="skyblue",main="Histograma",sub="Datos simulados de una Dist. Normal; Repet=5000",
     breaks = 20)
x<- seq(0,100, by=1)
curve(dlnorm(x, meanlog=1.02776687, sdlog = 0.09662289),col="darkblue",lwd=2,add=TRUE)
abline(v=valor_5pct, col="red", lty=1, lwd=2)
abline(v=valor_95pct, col="red", lty=1, lwd=2)
text(x=((valor_5pct - valor_inf)/2 + valor_inf), y=1.51, labels=" <- 5% ")
text(x=((valor_sup - valor_95pct)/2 + valor_95pct), y=1.51, labels=" 5% -> ")


#curvas de probabilidad acumulada 
curve(plnorm(x, meanlog=1.02776687, sdlog = 0.09662289),xlim=c(0,4), col="blue",lwd=2,
      xlab="x",ylab="F(x)",main="Función de Distribución")
abline(-10,1)

curve(1-plnorm(x, meanlog=1.02776687, sdlog = 0.09662289),xlim=c(0,4),col="blue",lwd=2,
      xlab="GF3",ylab="F(x)",main="Función de Distribución")
abline(v=1.7, col="green", lty=2)
abline(v=1.9, col="orange", lty=2)
abline(v=2.1, col="red", lty=2)




## ==================================
##      CONVERSIÓN - FCRe  
## ==================================

#---------------
# [ X Región]
#---------------

#Ajustar Distribución
hist(coho[coho$Region=="X",]$FCRe,breaks = 10)

#ajustar a dist. de probabilidad
res1.fcreX<-fit.cont(data2fit=coho[coho$Region=="X",]$FCRe) 
res1.fcreX


#Simular 5000 resultados posibles en base a parametros fijos (dist. gamma "shape-rate")
simular.fcreX<- rlnorm(50000, meanlog=0.21035156, sdlog = 0.08869157)
min(simular.fcreX)
max(simular.fcreX)
hist(simular.fcreX, freq = TRUE, col="lightsalmon",main="Histograma",sub="Datos simulados de una Dist. Log-Normal",
     breaks = 20)

hist(simular.fcreX, freq = FALSE, col="lightsalmon",main="Histograma",sub="Datos simulados de una Dist. Log-Normal;  Repet=5000",
     breaks = 20)
#agregar curva teorica (gamma)
x<- seq(0,100, by=1)
curve(dlnorm(x, meanlog=0.21035156, sdlog = 0.08869157),col="blue",lwd=2,add=TRUE)


#F(x) de densidad
dlnorm(1.4, meanlog=0.21035156, sdlog = 0.08869157, log=FALSE)*100 #probabilidad de tener un resultado = 1.8 gf3
#F(x) de probabilidad
plnorm(1.4, meanlog=0.21035156, sdlog = 0.08869157, lower.tail=TRUE, log.p = FALSE)*100 #Prob resultado <1.8

plnorm(1.3, meanlog=0.21035156, sdlog = 0.08869157, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >1.8
plnorm(1.5, meanlog=0.21035156, sdlog = 0.08869157, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >1.9
plnorm(1.6, meanlog=0.21035156, sdlog = 0.08869157, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >2.1


vProbX=plnorm(c(1.4, 1.6), meanlog=0.21035156, sdlog = 0.08869157, lower.tail=TRUE) #Probabilidad de valor entre ....
vProbX
miProbX=vProbX[2]-vProbX[1]
miProbX*100


valor_5pct<- qlnorm(0.05,meanlog=0.21035156, sdlog = 0.08869157, lower.tail=TRUE) #valor que corta el 5% de los datos (percentil 5)
valor_95pct<- qlnorm(0.95,meanlog=0.21035156, sdlog = 0.08869157, lower.tail=TRUE) #valor que corta el 95% de los datos (percentil 95)

valor_inf<- qlnorm(0.0001,meanlog=0.21035156, sdlog = 0.08869157, lower.tail=TRUE) #solo ubicar el texto en el gráfico
valor_sup<- qlnorm(0.999,meanlog=0.21035156, sdlog = 0.08869157, lower.tail=TRUE) #solo ubicar el texto en el gráfico

hist(simular.fcreX, freq = FALSE, col="skyblue",main="Histograma",sub="Datos simulados de una Dist. Log-Normal; Repet=5000",
     breaks = 20)
x<- seq(0,100, by=1)
curve(dlnorm(x, meanlog=0.21035156, sdlog = 0.08869157),col="darkblue",lwd=2,add=TRUE)
abline(v=valor_5pct, col="red", lty=1, lwd=2)
abline(v=valor_95pct, col="red", lty=1, lwd=2)
text(x=((valor_5pct - valor_inf)/2 + valor_inf), y=3.55, labels=" <- 5% ")
text(x=((valor_sup - valor_95pct)/2 + valor_95pct), y=3.55, labels=" 5% -> ")



#curvas de probabilidad acumulada 
curve(plnorm(x, meanlog=0.21035156, sdlog = 0.08869157),xlim=c(0,4), col="blue",lwd=2,
      xlab="FCRe",ylab="F(x)",main="Función de Distribución")
abline(-1.8,1.1)

curve(1-plnorm(x, meanlog=0.21035156, sdlog = 0.08869157),xlim=c(0,4),col="blue",lwd=2,
      xlab="FCRe",ylab="F(x)",main="Función de Distribución")
abline(v= 1.3, col="green", lty=2)
abline(v= 1.5, col="orange", lty=2)
abline(v= 1.6, col="red", lty=2)




#---------------
# [ XI Región]
#---------------

#Ajustar Distribución
hist(coho[coho$Region=="XI",]$FCRe, breaks=10)

#ajustar a dist. de probabilidad
res1.fcreXI<-fit.cont(data2fit=coho[coho$Region=="XI",]$FCRe) 
res1.fcreXI

#Simular 5000 resultados posibles en base a parametros fijos 
simular.fcreXI<- rweibull(5000, shape=32.901099, scale = 1.195182) 
hist(simular.fcreXI,freq = TRUE, col="lightsalmon",main="Histograma",sub="Datos simulados de una Dist. Logistic",
     breaks = 25)

hist(simular.fcreXI, freq = FALSE, col="lightsalmon",main="Histograma",sub="Datos simulados de una Dist. Logistic; Repet=5000",
     breaks = 20)
#agregar curva teorica
x<- seq(0,100, by=1)
curve(dweibull(x, shape=32.901099, scale = 1.195182),col="blue",lwd=2,add=TRUE)


#F(x) de densidad
dweibull(1.4, shape=32.901099, scale = 1.195182, log=FALSE)*100 #probabilidad de tener un resultado = 1.8
#F(x) de probabilidad
pweibull(1.4, shape=32.901099, scale = 1.195182, lower.tail=TRUE, log.p = FALSE)*100 #Prob resultado <1.8

pweibull(1.3, shape=32.901099, scale = 1.195182, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >1.7
pweibull(1.5, shape=32.901099, scale = 1.195182, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >1.9
pweibull(1.6, shape=32.901099, scale = 1.195182, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >2.1

vProbXI=pweibull(c(1.4,1.6), shape=32.901099, scale = 1.195182, lower.tail=TRUE) #Probabilidad de valor entre....
vProbXI
miProbXI=vProbXI[2]-vProbXI[1]
miProbXI*100

valor_5pct<- qweibull(0.05, shape=32.901099, scale = 1.195182, lower.tail=TRUE) #valor que corta el 5% de los datos (percentil 5)
valor_95pct<- qweibull(0.95, shape=32.901099, scale = 1.195182, lower.tail=TRUE) #valor que corta el 95% de los datos (percentil 95)

valor_inf<- qweibull(0.0001, shape=32.901099, scale = 1.195182, lower.tail=TRUE) #solo ubicar el texto en el gráfico
valor_sup<- qweibull(0.999, shape=32.901099, scale = 1.195182, lower.tail=TRUE) #solo ubicar el texto en el gráfico

hist(simular.fcreXI, xlim=c(0.8,1.4),freq = FALSE, col="skyblue",main="Histograma",sub="Datos simulados de una Dist. Weibull; Repet=5000",
     breaks = 20)
x<- seq(0,100, by=1)
curve(dweibull(x, shape=32.901099, scale = 1.195182),col="darkblue",lwd=2,add=TRUE)
abline(v=valor_5pct, col="red", lty=1, lwd=2)
abline(v=valor_95pct, col="red", lty=1, lwd=2)
text(x=((valor_5pct - valor_inf)/2 + valor_inf), y=10, labels=" <- 5% ")
text(x=((valor_sup - valor_95pct)/2 + valor_95pct), y=10, labels="       5% ->")


#curvas de probabilidad acumulada 
curve(pweibull(x, shape=32.901099, scale = 1.195182),xlim=c(0,4), col="blue",lwd=2,
      xlab="FCRe",ylab="F(x)",main="Función de Distribución")
abline(-10,1)

curve(1-pweibull(x, shape=32.901099, scale = 1.195182),xlim=c(0,4),col="blue",lwd=2,
      xlab="FCRe",ylab="F(x)",main="Función de Distribución")
abline(v=1.3, col="green", lty=2)
abline(v=1.5, col="orange", lty=2)
abline(v=1.6, col="red", lty=2)


## ==================================
##      CONVERSIÓN - FCRb 
## ==================================

#---------------
# [ X Región]
#---------------

#Ajustar Distribución
hist(coho[coho$Region=="X",]$FCRb)

#ajustar a dist. de probabilidad
res1.fcrbX<-fit.cont(data2fit=coho[coho$Region=="X",]$FCRb) 
res1.fcrbX


#Simular 5000 resultados posibles en base a parametros fijos (dist. gamma "shape-rate")
simular.fcrbX<- rlogis(50000, location=1.16664055, scale = 0.04793498) 
hist(simular.fcrbX, freq = TRUE, col="lightsalmon",main="Histograma",sub="Datos simulados de una Dist. Log-Normal",
     breaks = 25)

hist(simular.fcrbX, freq = FALSE, col="lightsalmon",main="Histograma",sub="Datos simulados de una Dist. Log-Normal; Repet=5000",
     breaks = 20)
#agregar curva teorica (gamma)
x<- seq(0,100, by=1)
curve(dlogis(x,  location=1.16664055, scale = 0.04793498),col="blue",lwd=2,add=TRUE)


#F(x) de densidad
dlogis(1.4, location=1.16664055, scale = 0.04793498, log=FALSE)*100 #probabilidad de tener un resultado = 1.8 gf3
#F(x) de probabilidad
plogis(1.4, location=1.16664055, scale = 0.04793498, lower.tail=TRUE, log.p = FALSE)*100 #Prob resultado <1.8

plogis(1.2,  location=1.16664055, scale = 0.04793498, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >1.8
plogis(1.3, location=1.16664055, scale = 0.04793498, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >1.9
plogis(1.4, location=1.16664055, scale = 0.04793498, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >2.1


vProbX=plogis(c(1.3, 1.4), location=1.16664055, scale = 0.04793498, lower.tail=TRUE) #Probabilidad de valor entre ....
vProbX
miProbX=vProbX[2]-vProbX[1]
miProbX*100


valor_5pct<- qlogis(0.05, location=1.16664055, scale = 0.04793498, lower.tail=TRUE) #valor que corta el 5% de los datos (percentil 5)
valor_95pct<- qlogis(0.95, location=1.16664055, scale = 0.04793498, lower.tail=TRUE) #valor que corta el 95% de los datos (percentil 95)

valor_inf<- qlogis(0.0001, location=1.16664055, scale = 0.04793498, lower.tail=TRUE) #solo ubicar el texto en el gráfico
valor_sup<- qlogis(0.999, location=1.16664055, scale = 0.04793498, lower.tail=TRUE) #solo ubicar el texto en el gráfico

hist(simular.fcrbX,freq = FALSE, col="skyblue",main="Histograma",sub="Datos simulados de una Dist. Logistic; Repet=5000",
     breaks = 20)
x<- seq(0,100, by=1)
curve(dlogis(x, location=1.16664055, scale = 0.04793498),col="darkblue",lwd=2,add=TRUE)
abline(v=valor_5pct, col="red", lty=1, lwd=2)
abline(v=valor_95pct, col="red", lty=1, lwd=2)
text(x=((valor_5pct - valor_inf)/2 + valor_inf), y=5, labels=" <- 5% ")
text(x=((valor_sup - valor_95pct)/2 + valor_95pct), y=5, labels=" 5% -> ")



#curvas de probabilidad acumulada 
curve(plogis(x, location=1.16664055, scale = 0.04793498),xlim=c(0,4), col="blue",lwd=2,
      xlab="FCRb",ylab="F(x)",main="Función de Distribución")
abline(-1.8,1.1)

curve(1-plogis(x, location=1.16664055, scale = 0.04793498),xlim=c(0,4),col="blue",lwd=2,
      xlab="FCRb",ylab="F(x)",main="Función de Distribución")
abline(v= 1.2, col="green", lty=2)
abline(v= 1.3, col="orange", lty=2)
abline(v= 1.4, col="red", lty=2)




#---------------
# [ XI Región]
#---------------

#Ajustar Distribución
hist(coho[coho$Region=="XI",]$FCRb)

#ajustar a dist. de probabilidad
res1.fcrbXI<-fit.cont(data2fit=coho[coho$Region=="XI",]$FCRb) 
res1.fcrbXI

#Simular 5000 resultados posibles en base a parametros fijos 
simular.fcrbXI<- rlogis(5000, location=1.15902289, scale = 0.02830813) 
hist(simular.fcrbXI,freq = TRUE, col="lightsalmon",main="Histograma",sub="Datos simulados de una Dist. Log-Normal",
     breaks = 25)

hist(simular.fcrbXI, freq = FALSE, col="lightsalmon",main="Histograma",sub="Datos simulados de una Dist. Log-Normal; Repet=5000",
     breaks = 20)
#agregar curva teorica (gamma)
x<- seq(0,100, by=1)
curve(dlogis(x, location=1.15902289, scale = 0.02830813),col="blue",lwd=2,add=TRUE)


#F(x) de densidad
dlogis(1.4, location=1.15902289, scale = 0.02830813, log=FALSE)*100 #probabilidad de tener un resultado = 1.8
#F(x) de probabilidad
plogis(1.4, location=1.15902289, scale = 0.02830813, lower.tail=TRUE, log.p = FALSE)*100 #Prob resultado <1.8

plogis(1.2, location=1.15902289, scale = 0.02830813, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >1.7
plogis(1.3, location=1.15902289, scale = 0.02830813, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >1.9
plogis(1.4, location=1.15902289, scale = 0.02830813, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >2.1

vProbXI=plogis(c(1.3,1.4), location=1.15902289, scale = 0.02830813, lower.tail=TRUE) #Probabilidad de valor entre....
vProbXI
miProbXI=vProbXI[2]-vProbXI[1]
miProbXI*100

valor_5pct<- qlogis(0.05, location=1.15902289, scale = 0.02830813, lower.tail=TRUE) #valor que corta el 5% de los datos (percentil 5)
valor_95pct<- qlogis(0.95, location=1.15902289, scale = 0.02830813, lower.tail=TRUE) #valor que corta el 95% de los datos (percentil 95)

valor_inf<- qlogis(0.0001, location=1.15902289, scale = 0.02830813, lower.tail=TRUE) #solo ubicar el texto en el gráfico
valor_sup<- qlogis(0.999, location=1.15902289, scale = 0.02830813, lower.tail=TRUE) #solo ubicar el texto en el gráfico

hist(simular.fcrbXI, freq = FALSE, col="skyblue",main="Histograma",sub="Datos simulados de una Dist. Log-Normal; Repet=5000",
     breaks = 20)
x<- seq(0,100, by=1)
curve(dlogis(x, location=1.15902289, scale = 0.02830813),col="darkblue",lwd=2,add=TRUE)
abline(v=valor_5pct, col="red", lty=1, lwd=2)
abline(v=valor_95pct, col="red", lty=1, lwd=2)
text(x=((valor_5pct - valor_inf)/2 + valor_inf), y=9, labels=" <- 5% ")
text(x=((valor_sup - valor_95pct)/2 + valor_95pct), y=9, labels=" 5% -> ")


#curvas de probabilidad acumulada 
curve(plogis(x, location=1.15902289, scale = 0.02830813),xlim=c(0,4), col="blue",lwd=2,
      xlab="FCRb",ylab="F(x)",main="Función de Distribución")
abline(-10,1)

curve(1-plogis(x, location=1.15902289, scale = 0.02830813),xlim=c(0,4),col="blue",lwd=2,
      xlab="FCRb",ylab="F(x)",main="Función de Distribución")
abline(v=1.2, col="green", lty=2)
abline(v=1.3, col="orange", lty=2)
abline(v=1.4, col="red", lty=2)

