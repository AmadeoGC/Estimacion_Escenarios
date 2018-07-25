#################################################################
###      ESTIMACIÓN DE ESCENARIOS PARA LA SALMONICULTURA      ###  
###                  Analisis de Riesgo                       ###
###      Ajuste Dist. Probabilidad - Método montecarlo        ###  
###                  <Amadeo Guzmán C.>                       ###
#################################################################

#leer datos
datos1<- read.csv2("BD Escenarios Aquacapital - SyT.csv", header = TRUE)
head(datos1,3)
dim(datos1)
str(datos1)
names(datos1)


library(rriskDistributions)

table(datos1$Especie)
table(datos1$Especie, datos1$Region)


#============================================== Trucha Arcoiris =========================================================#

trucha<- datos1[datos1$Especie=="Trucha",]
dim(trucha)
names(trucha)

## ==================================
##    MORTALIDAD ACUMULADA (%)    
## ==================================

#---------------
# [ X Región]
#---------------

#Ajustar Distribución
hist(trucha[trucha$Region=="X",]$Mort.Acump)

#ajustar a dist. de probabilidad
res1.mortX<-fit.cont(data2fit=trucha[trucha$Region=="X",]$Mort.Acump) 
res1.mortX


#Simular 5000 resultados posibles en base a parametros fijos (dist. gamma "shape-rate")
simular<- rgamma(5000, shape=3.3562053, rate = 0.2830851) 
hist(simular, freq = TRUE, col="lightsalmon",main="Histograma",sub="Datos simulados de una Dist. Gamma",
     breaks = 20)

hist(simular, freq = FALSE, col="lightsalmon",main="Histograma",sub="Datos simulados de una Dist. Gamma; Repet=5000",
     breaks = 20)
#agregar curva teorica (gamma)
x<- seq(0,100, by=1)
curve(dgamma(x, shape=3.3562053, rate = 0.2830851),xlim=c(-100,200),col="blue",lwd=2,add=TRUE)


#F(x) de densidad
dgamma(10, shape=3.3562053, rate = 0.2830851, log=FALSE)*100 #probabilidad de tener un resultado = 10p mort acum
#F(x) de probabilidad
pgamma(10, shape=2.8467567, rate = 0.1773373, lower.tail=TRUE, log.p = FALSE)*100 #Prob resultado <10p

pgamma(10, shape=3.3562053, rate = 0.2830851, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >10p
pgamma(15, shape=3.3562053, rate = 0.2830851, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >15p
pgamma(25, shape=3.3562053, rate = 0.2830851, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >25p

vProbX=pgamma(c(10,15), shape=3.3562053, rate = 0.2830851, lower.tail=TRUE) #Probabilidad de valor entre 10 y 15p
vProbX
miProbX=vProbX[2]-vProbX[1]
miProbX*100


valor_5pct<- qgamma(0.05,shape=3.3562053, rate = 0.2830851, lower.tail=TRUE) #valor que corta el 5% de los datos (percentil 5)
valor_95pct<- qgamma(0.95,shape=3.3562053, rate = 0.2830851, lower.tail=TRUE) #valor que corta el 95% de los datos (percentil 95)

valor_inf<- qgamma(0.0001,shape=3.3562053, rate = 0.2830851, lower.tail=TRUE) #solo ubicar el texto en el gráfico
valor_sup<- qgamma(0.999,shape=3.3562053, rate = 0.2830851, lower.tail=TRUE) #solo ubicar el texto en el gráfico


hist(simular, freq = FALSE, xlim=c(0,100), col="skyblue",main="Histograma",sub="Datos simulados de una Dist. Gamma; Repet=5000",
     breaks = 20)
x<- seq(0,100, by=1)
curve(dgamma(x, shape=3.3562053, rate = 0.2830851),col="darkblue",lwd=2,add=TRUE)
abline(v=valor_5pct, col="red", lty=1, lwd=2)
abline(v=valor_95pct, col="red", lty=1, lwd=2)
text(x=((valor_5pct - valor_inf)/2 + valor_inf), y=0.071, labels="5%    ")
text(x=((valor_sup - valor_95pct)/2 + valor_95pct), y=0.071, labels=" 5% -> ")



#curvas de probabilidad acumulada 
curve(pgamma(x, shape=3.3562053, rate = 0.2830851),xlim=c(-10,100), col="blue",lwd=2,
      xlab="x",ylab="F(x)",main="Función de Distribución N(50,38)")
abline(-12,1)

curve(1-pgamma(x, shape=3.3562053, rate = 0.2830851),xlim=c(-10,100),col="blue",lwd=2,
      xlab="Mort. Acum (%)",ylab="F(x)",main="Función de Distribución")
abline(v=10, col="green", lty=2)
abline(v=15, col="orange", lty=2)
abline(v=25, col="red", lty=2)



#---------------
# [ XI Región]
#---------------

#Ajustar Distribución
hist(trucha[trucha$Region=="XI",]$Mort.Acump)

#ajustar a dist. de probabilidad
res1.mortXI<-fit.cont(data2fit=trucha[trucha$Region=="XI",]$Mort.Acump) 
res1.mortXI

#Simular 5000 resultados posibles en base a parametros fijos 
simularXI<- rlnorm(5000, meanlog=2.8003109, sdlog = 0.6239332) 
hist(simularXI,freq = TRUE, xlim=c(0,100), col="lightsalmon",main="Histograma",sub="Datos simulados de una Dist. Log-normal",
     breaks = 25)

hist(simularXI, freq = FALSE, xlim=c(0,100), col="lightsalmon",main="Histograma",sub="Datos simulados de una Dist. Log-normal; Repet=5000",
     breaks = 20)
#agregar curva teorica (gamma)
x<- seq(0,100, by=1)
curve(dlnorm(x, meanlog=2.8003109, sdlog = 0.6239332),xlim=c(-100,200),col="blue",lwd=2,add=TRUE)


#F(x) de densidad
dlnorm(10, meanlog=2.8003109, sdlog = 0.6239332, log=FALSE)*100 #probabilidad de tener un resultado = 10p mort acum
#F(x) de probabilidad
plnorm(10, meanlog=2.8003109, sdlog = 0.6239332, lower.tail=TRUE, log.p = FALSE)*100 #Prob resultado <10p

plnorm(10, meanlog=2.8003109, sdlog = 0.6239332, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >10p
plnorm(15, meanlog=2.8003109, sdlog = 0.6239332, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >15p
plnorm(25, meanlog=2.8003109, sdlog = 0.6239332, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >25p


vProbXI=plnorm(c(10,15), meanlog=2.8003109, sdlog = 0.6239332, lower.tail=TRUE) #Probabilidad de valor entre 10 y 15p
vProbXI
miProbXI=vProbXI[2]-vProbXI[1]
miProbXI*100


valor_5pct<- qlnorm(0.05, meanlog=2.8003109, sdlog = 0.6239332, lower.tail=TRUE) #valor que corta el 5% de los datos (percentil 5)
valor_95pct<- qlnorm(0.95, meanlog=2.8003109, sdlog = 0.6239332, lower.tail=TRUE) #valor que corta el 95% de los datos (percentil 95)

valor_inf<- qlnorm(0.0001, meanlog=2.8003109, sdlog = 0.6239332, lower.tail=TRUE) #solo ubicar el texto en el gráfico
valor_sup<- qlnorm(0.999, meanlog=2.8003109, sdlog = 0.6239332, lower.tail=TRUE) #solo ubicar el texto en el gráfico


hist(simularXI, freq = FALSE, xlim=c(0,100), ylim=c(0,0.05), col="skyblue",main="Histograma",sub="Datos simulados de una Dist. Log-Normal; Repet=5000",
     breaks = 20)
x<- seq(0,100, by=1)
curve(dlnorm(x, meanlog=2.8003109, sdlog = 0.6239332),col="darkblue",lwd=2,add=TRUE)
abline(v=valor_5pct, col="red", lty=1, lwd=2)
abline(v=valor_95pct, col="red", lty=1, lwd=2)
text(x=((valor_5pct - valor_inf)/2 + valor_inf), y=0.05, labels="<-5%      ")
text(x=((valor_sup - valor_95pct)/2 + valor_95pct), y=0.05, labels=" 5% -> ")



#curvas de probabilidad acumulada 
curve(plnorm(x, meanlog=2.8003109, sdlog = 0.6239332),xlim=c(-10,100), col="blue",lwd=2,
      xlab="x",ylab="F(x)",main="Función de Distribución")
abline(-10,1)

curve(1-plnorm(x, meanlog=2.8003109, sdlog = 0.6239332),xlim=c(-10,100),col="blue",lwd=2,
      xlab="x",ylab="F(x)",main="Función de Distribución")
abline(v=10, col="green", lty=2)
abline(v=15, col="orange", lty=2)
abline(v=25, col="red", lty=2)



#---------------
# [ XII Región]
#---------------

#Ajustar Distribución
hist(trucha[trucha$Region=="XII",]$Mort.Acump)

#ajustar a dist. de probabilidad
res1.mortXII<-fit.cont(data2fit=trucha[trucha$Region=="XII",]$Mort.Acump) 
res1.mortXII

#Simular 5000 resultados posibles en base a parametros fijos (dist. gamma "shape-rate")
simularXII<- rgamma(5000, shape=1.6373799, rate = 0.2807921) 
hist(simularXII,freq = TRUE, xlim=c(0,100), col="lightsalmon",main="Histograma",sub="Datos simulados de una Dist. Gamma",
     breaks = 25)

hist(simularXII, freq = FALSE, xlim=c(0,100), col="lightsalmon",main="Histograma",sub="Datos simulados de una Dist. Gamma; Repet=5000",
     breaks = 20)
#agregar curva teorica (gamma)
x<- seq(0,100, by=1)
curve(dgamma(x, shape=1.6373799, rate = 0.2807921),xlim=c(-100,200),col="blue",lwd=2,add=TRUE)


#F(x) de densidad
dgamma(10, shape=1.6373799, rate = 0.2807921, log=FALSE)*100 #probabilidad de tener un resultado = 10p mort acum
#F(x) de probabilidad
pgamma(10, shape=1.6373799, rate = 0.2807921, lower.tail=TRUE, log.p = FALSE)*100 #Prob resultado <10p

pgamma(10, shape=1.6373799, rate = 0.2807921, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >10p
pgamma(15, shape=1.6373799, rate = 0.2807921, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >15p
pgamma(25, shape=1.6373799, rate = 0.2807921, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >25p


vProbXI=pgamma(c(10,20), shape=1.6373799, rate = 0.2807921, lower.tail=TRUE) #Probabilidad de valor entre 10 y 15p
vProbXI
miProbXI=vProbXI[2]-vProbXI[1]
miProbXI*100


valor_5pct<- qgamma(0.05,shape=1.6373799, rate = 0.2807921, lower.tail=TRUE) #valor que corta el 5% de los datos (percentil 5)
valor_95pct<- qgamma(0.95,shape=1.6373799, rate = 0.2807921, lower.tail=TRUE) #valor que corta el 95% de los datos (percentil 95)

valor_inf<- qgamma(0.0001,shape=1.6373799, rate = 0.2807921, lower.tail=TRUE) #solo ubicar el texto en el gráfico
valor_sup<- qgamma(0.999,shape=1.6373799, rate = 0.2807921, lower.tail=TRUE) #solo ubicar el texto en el gráfico


hist(simularXII, freq = FALSE, xlim=c(0,100),col="skyblue",main="Histograma",sub="Datos simulados de una Dist. Log-Normal; Repet=5000",
     breaks = 20)
x<- seq(0,100, by=1)
curve(dgamma(x, shape=1.6373799, rate = 0.2807921),col="darkblue",lwd=2,add=TRUE)
abline(v=valor_5pct, col="red", lty=1, lwd=2)
abline(v=valor_95pct, col="red", lty=1, lwd=2)
text(x=((valor_5pct - valor_inf)/2 + valor_inf), y=0.121, labels="5%    ")
text(x=((valor_sup - valor_95pct)/2 + valor_95pct), y=0.121, labels=" 5% -> ")

#curvas de probabilidad acumulada 
curve(pgamma(x, shape=1.6373799, rate = 0.2807921),xlim=c(-10,100), col="blue",lwd=2,
      xlab="x",ylab="F(x)",main="Función de Distribución")
abline(v=10)

curve(1-pgamma(x, shape=1.6373799, rate = 0.2807921),xlim=c(-10,100),col="blue",lwd=2,
      xlab="Mort. Acum (%)",ylab="F(x)",main="Función de Distribución")
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
hist(trucha[trucha$Region=="X",]$GF3)

#ajustar a dist. de probabilidad
res1.gf3X<-fit.cont(data2fit=trucha[trucha$Region=="X",]$GF3) 
res1.gf3X


#Simular 5000 resultados posibles en base a parametros fijos
simular.gf3X<- rnorm(5000, mean=2.0713038, sd = 0.2312781) 
hist(simular.gf3X, freq = TRUE, col="lightsalmon",main="Histograma",sub="Datos simulados de una Dist. Normal",
     breaks = 20)

hist(simular.gf3X, freq = FALSE, col="lightsalmon",main="Histograma",sub="Datos simulados de una Dist. Normal; Repet=5000",
     breaks = 20)
#agregar curva teorica 
x<- seq(0,100, by=1)
curve(dnorm(x, mean=2.0713038, sd = 0.2312781),col="blue",lwd=2,add=TRUE)


#F(x) de densidad
dnorm(1.8, mean=2.0713038, sd = 0.2312781, log=FALSE)*100 #probabilidad de tener un resultado = 1.8 gf3
#F(x) de probabilidad
pnorm(1.8, mean=2.0713038, sd = 0.2312781, lower.tail=TRUE, log.p = FALSE)*100 #Prob resultado <1.8

pnorm(1.7, mean=2.0713038, sd = 0.2312781, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >1.8
pnorm(1.9, mean=2.0713038, sd = 0.2312781, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >1.9
pnorm(2.1, mean=2.0713038, sd = 0.2312781, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >2.1


vProbX=pnorm(c(1.8,2.1), mean=2.0713038, sd = 0.2312781, lower.tail=TRUE) #Probabilidad de valor entre ....
vProbX
miProbX=vProbX[2]-vProbX[1]
miProbX*100


valor_5pct<- qnorm(0.05,mean=2.0713038, sd = 0.2312781, lower.tail=TRUE) #valor que corta el 5% de los datos (percentil 5)
valor_95pct<- qnorm(0.95,mean=2.0713038, sd = 0.2312781, lower.tail=TRUE) #valor que corta el 95% de los datos (percentil 95)

valor_inf<- qnorm(0.0001,mean=2.0713038, sd = 0.2312781, lower.tail=TRUE) #solo ubicar el texto en el gráfico
valor_sup<- qnorm(0.999,mean=2.0713038, sd = 0.2312781, lower.tail=TRUE) #solo ubicar el texto en el gráfico

hist(simular.gf3X, xlim=c(1,3), freq = FALSE, col="skyblue",main="Histograma",sub="Datos simulados de una Dist. Normal; Repet=5000",
     breaks = 20)
x<- seq(0,100, by=1)
curve(dnorm(x, mean=2.0713038, sd = 0.2312781),col="darkblue",lwd=2,add=TRUE)
abline(v=valor_5pct, col="red", lty=1, lwd=2)
abline(v=valor_95pct, col="red", lty=1, lwd=2)
text(x=((valor_5pct - valor_inf)/2 + valor_inf), y=1.7, labels=" <- 5% ")
text(x=((valor_sup - valor_95pct)/2 + valor_95pct), y=1.7, labels=" 5% -> ")



#curvas de probabilidad acumulada 
curve(pnorm(x, mean=2.0713038, sd = 0.2312781),xlim=c(0,4), col="blue",lwd=2,
      xlab="GF3",ylab="F(x)",main="Función de Distribución")
abline(-1.8,1.1)

curve(1-pnorm(x, mean=2.0713038, sd = 0.2312781),xlim=c(0,4),col="blue",lwd=2,
      xlab="GF3",ylab="F(x)",main="Función de Distribución")
abline(v= 1.7, col="green", lty=2)
abline(v= 1.9, col="orange", lty=2)
abline(v= 2.1, col="red", lty=2)




#---------------
# [ XI Región]
#---------------

#Ajustar Distribución
hist(trucha[trucha$Region=="XI",]$GF3)

#ajustar a dist. de probabilidad
res1.gf3XI<-fit.cont(data2fit=trucha[trucha$Region=="XI",]$GF3) 
res1.gf3XI

#Simular 5000 resultados posibles en base a parametros fijos (dist. gamma "meanlog; sdlog")
simular.gf3XI<- rnorm(5000, mean=1.9842612, sd = 0.2339568)
max(simular.gf3XI)
min(simular.gf3XI)
hist(simular.gf3XI,freq = TRUE, col="lightsalmon",main="Histograma",sub="Datos simulados de una Dist. Normal",
     breaks = 25)

hist(simular.gf3XI, freq = FALSE, col="lightsalmon",main="Histograma",sub="Datos simulados de una Dist. Normal; Repet=5000",
     breaks = 20)
#agregar curva teorica (gamma)
x<- seq(0,100, by=1)
curve(dnorm(x, mean=1.9842612, sd = 0.2339568),col="blue",lwd=2,add=TRUE)


#F(x) de densidad
dnorm(1.8, mean=1.9842612, sd = 0.2339568, log=FALSE)*100 #probabilidad de tener un resultado = 1.8
#F(x) de probabilidad
pnorm(1.8, mean=1.9842612, sd = 0.2339568, lower.tail=TRUE, log.p = FALSE)*100 #Prob resultado <1.8

pnorm(1.7, mean=1.9842612, sd = 0.2339568, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >1.7
pnorm(1.9, mean=1.9842612, sd = 0.2339568, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >1.9
pnorm(2.1, mean=1.9842612, sd = 0.2339568, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >2.1

vProbXI=pnorm(c(1.8,2.0), mean=1.9842612, sd = 0.2339568, lower.tail=TRUE) #Probabilidad de valor entre....
vProbXI
miProbXI=vProbXI[2]-vProbXI[1]
miProbXI*100

valor_5pct<- qnorm(0.05,mean=1.9842612, sd = 0.2339568, lower.tail=TRUE) #valor que corta el 5% de los datos (percentil 5)
valor_95pct<- qnorm(0.95,mean=1.9842612, sd = 0.2339568, lower.tail=TRUE) #valor que corta el 95% de los datos (percentil 95)

valor_inf<- qnorm(0.0001,mean=1.9842612, sd = 0.2339568, lower.tail=TRUE) #solo ubicar el texto en el gráfico
valor_sup<- qnorm(0.999,mean=1.9842612, sd = 0.2339568, lower.tail=TRUE) #solo ubicar el texto en el gráfico

hist(simular.gf3XI, xlim=c(1,3),freq = FALSE, col="skyblue",main="Histograma",sub="Datos simulados de una Dist. Normal; Repet=5000",
     breaks = 20)
x<- seq(0,100, by=1)
curve(dnorm(x, mean=1.9842612, sd = 0.2339568),col="darkblue",lwd=2,add=TRUE)
abline(v=valor_5pct, col="red", lty=1, lwd=2)
abline(v=valor_95pct, col="red", lty=1, lwd=2)
text(x=((valor_5pct - valor_inf)/2 + valor_inf), y=1.65, labels=" <- 5% ")
text(x=((valor_sup - valor_95pct)/2 + valor_95pct), y=1.65, labels=" 5% -> ")


#curvas de probabilidad acumulada 
curve(pnorm(x, mean=1.9842612, sd = 0.2339568),xlim=c(0,4), col="blue",lwd=2,
      xlab="x",ylab="F(x)",main="Función de Distribución")
abline(-10,1)

curve(1-pnorm(x, mean=1.9842612, sd = 0.2339568),xlim=c(0,4),col="blue",lwd=2,
      xlab="GF3",ylab="F(x)",main="Función de Distribución")
abline(v=1.7, col="green", lty=2)
abline(v=1.9, col="orange", lty=2)
abline(v=2.1, col="red", lty=2)



#---------------
# [ XII Región]
#---------------

#Ajustar Distribución
hist(trucha[trucha$Region=="XII",]$GF3)

#ajustar a dist. de probabilidad
res1.gf3XII<-fit.cont(data2fit=trucha[trucha$Region=="XII",]$GF3) 
res1.gf3XII

#Simular 5000 resultados posibles en base a parametros fijos (dist. normal "mean-sd")
simular.gf3XII<- rlogis(5000, location=2.2412229, scale = 0.1327168)
max(simular.gf3XII)
hist(simular.gf3XII,freq = TRUE, col="lightsalmon",main="Histograma",sub="Datos simulados de una Dist. Normal",
     breaks = 25)

hist(simular.gf3XII, freq = FALSE, col="lightsalmon",main="Histograma",sub="Datos simulados de una Dist. Normal; Repet=5000",
     breaks = 15)
#agregar curva teorica (normal)
x<- seq(0,100, by=1)
curve(dlogis(x, location=2.2412229, scale = 0.1327168),col="blue",lwd=2,add=TRUE)


#F(x) de densidad
dlogis(1.8, location=2.2412229, scale = 0.1327168, log=FALSE)*100 #probabilidad de tener un resultado = 1.8
#F(x) de probabilidad
plogis(1.8, location=2.2412229, scale = 0.1327168, lower.tail=TRUE, log.p = FALSE)*100 #Prob resultado <1.8

plogis(1.7, location=2.2412229, scale = 0.1327168, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >1.7
plogis(1.9, location=2.2412229, scale = 0.1327168, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >1.9
plogis(2.1, location=2.2412229, scale = 0.1327168, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >2.1


vProbXI=plogis(c(1.8,2.1), location=2.2412229, scale = 0.1327168, lower.tail=TRUE) #Probabilidad de valor entre.....
vProbXI
miProbXI=vProbXI[2]-vProbXI[1]
miProbXI*100


valor_5pct<- qlogis(0.05,location=2.2412229, scale = 0.1327168, lower.tail=TRUE) #valor que corta el 5% de los datos (percentil 5)
valor_95pct<- qlogis(0.95,location=2.2412229, scale = 0.1327168, lower.tail=TRUE) #valor que corta el 95% de los datos (percentil 95)

valor_inf<- qlogis(0.0001,location=2.2412229, scale = 0.1327168, lower.tail=TRUE) #solo ubicar el texto en el gráfico
valor_sup<- qlogis(0.999,location=2.2412229, scale = 0.1327168, lower.tail=TRUE) #solo ubicar el texto en el gráfico

hist(simular.gf3XII, ylim=c(0,2),freq = FALSE, col="skyblue",main="Histograma",sub="Datos simulados de una Dist. Normal; Repet=5000",
     breaks = 20)
x<- seq(0,100, by=1)
curve(dlogis(x, location=2.2412229, scale = 0.1327168),col="darkblue",lwd=2,add=TRUE)
abline(v=valor_5pct, col="red", lty=1, lwd=2)
abline(v=valor_95pct, col="red", lty=1, lwd=2)
text(x=((valor_5pct - valor_inf)/2 + valor_inf), y=2, labels=" <- 5% ")
text(x=((valor_sup - valor_95pct)/2 + valor_95pct), y=2, labels=" 5% -> ")



#curvas de probabilidad acumulada 
curve(plogis(x, location=2.2412229, scale = 0.1327168),xlim=c(-0,4), col="blue",lwd=2,
      xlab="GF3",ylab="F(x)",main="Función de Distribución")

curve(1-plogis(x, location=2.2412229, scale = 0.1327168),xlim=c(0,4),col="blue",lwd=2,
      xlab="GF3",ylab="GF3",main="Función de Distribución")
abline(v= 1.7, col="green", lty=2)
abline(v= 1.9, col="orange", lty=2)
abline(v= 2.1, col="red", lty=2)




## ==================================
##      CONVERSIÓN - FCRe  
## ==================================

#---------------
# [ X Región]
#---------------

#Ajustar Distribución
hist(trucha[trucha$Region=="X",]$FCRe,breaks = 10)

#ajustar a dist. de probabilidad
res1.fcreX<-fit.cont(data2fit=trucha[trucha$Region=="X",]$FCRe) 
res1.fcreX


#Simular 5000 resultados posibles en base a parametros fijos (dist. gamma "shape-rate")
simular.fcreX<- rlnorm(50000, meanlog=0.3605587, sdlog = 0.1014602)
min(simular.fcreX)
max(simular.fcreX)
hist(simular.fcreX, freq = TRUE, col="lightsalmon",main="Histograma",sub="Datos simulados de una Dist. Log-Normal",
     breaks = 20)

hist(simular.fcreX, freq = FALSE, col="lightsalmon",main="Histograma",sub="Datos simulados de una Dist. Log-Normal;  Repet=5000",
     breaks = 20)
#agregar curva teorica (gamma)
x<- seq(0,100, by=1)
curve(dlnorm(x, meanlog=0.3605587, sdlog = 0.1014602),col="blue",lwd=2,add=TRUE)


#F(x) de densidad
dlnorm(1.4, meanlog=0.3605587, sdlog = 0.1014602, log=FALSE)*100 #probabilidad de tener un resultado = 1.8 gf3
#F(x) de probabilidad
plnorm(1.4, meanlog=0.3605587, sdlog = 0.1014602, lower.tail=TRUE, log.p = FALSE)*100 #Prob resultado <1.8

plnorm(1.3, meanlog=0.3605587, sdlog = 0.1014602, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >1.8
plnorm(1.5, meanlog=0.3605587, sdlog = 0.1014602, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >1.9
plnorm(1.6, meanlog=0.3605587, sdlog = 0.1014602, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >2.1


vProbX=plnorm(c(1.4, 1.6), meanlog=0.3605587, sdlog = 0.1014602, lower.tail=TRUE) #Probabilidad de valor entre ....
vProbX
miProbX=vProbX[2]-vProbX[1]
miProbX*100


valor_5pct<- qlnorm(0.05,meanlog=0.3605587, sdlog = 0.1014602, lower.tail=TRUE) #valor que corta el 5% de los datos (percentil 5)
valor_95pct<- qlnorm(0.95,meanlog=0.3605587, sdlog = 0.1014602, lower.tail=TRUE) #valor que corta el 95% de los datos (percentil 95)

valor_inf<- qlnorm(0.0001,meanlog=0.3605587, sdlog = 0.1014602, lower.tail=TRUE) #solo ubicar el texto en el gráfico
valor_sup<- qlnorm(0.999,meanlog=0.3605587, sdlog = 0.1014602, lower.tail=TRUE) #solo ubicar el texto en el gráfico

hist(simular.fcreX, freq = FALSE, col="skyblue",main="Histograma",sub="Datos simulados de una Dist. Log-Normal; Repet=5000",
     breaks = 20)
x<- seq(0,100, by=1)
curve(dlnorm(x, meanlog=0.3605587, sdlog = 0.1014602),col="darkblue",lwd=2,add=TRUE)
abline(v=valor_5pct, col="red", lty=1, lwd=2)
abline(v=valor_95pct, col="red", lty=1, lwd=2)
text(x=((valor_5pct - valor_inf)/2 + valor_inf), y=2.7, labels=" <- 5% ")
text(x=((valor_sup - valor_95pct)/2 + valor_95pct), y=2.7, labels=" 5% -> ")



#curvas de probabilidad acumulada 
curve(plnorm(x, meanlog=0.3605587, sdlog = 0.1014602),xlim=c(0,4), col="blue",lwd=2,
      xlab="FCRe",ylab="F(x)",main="Función de Distribución")
abline(-1.8,1.1)

curve(1-plnorm(x, meanlog=0.3605587, sdlog = 0.1014602),xlim=c(0,4),col="blue",lwd=2,
      xlab="FCRe",ylab="F(x)",main="Función de Distribución")
abline(v= 1.3, col="green", lty=2)
abline(v= 1.5, col="orange", lty=2)
abline(v= 1.6, col="red", lty=2)




#---------------
# [ XI Región]
#---------------

#Ajustar Distribución
hist(trucha[trucha$Region=="XI",]$FCRe)

#ajustar a dist. de probabilidad
res1.fcreXI<-fit.cont(data2fit=trucha[trucha$Region=="XI",]$FCRe) 
res1.fcreXI

#Simular 5000 resultados posibles en base a parametros fijos 
simular.fcreXI<- rlogis(5000, location=1.5264961, scale = 0.1112113) 
hist(simular.fcreXI,freq = TRUE, col="lightsalmon",main="Histograma",sub="Datos simulados de una Dist. Logistic",
     breaks = 25)

hist(simular.fcreXI, freq = FALSE, col="lightsalmon",main="Histograma",sub="Datos simulados de una Dist. Logistic; Repet=5000",
     breaks = 20)
#agregar curva teorica
x<- seq(0,100, by=1)
curve(dlogis(x, location=1.5264961, scale = 0.1112113),col="blue",lwd=2,add=TRUE)


#F(x) de densidad
dlogis(1.4, location=1.5264961, scale = 0.1112113, log=FALSE)*100 #probabilidad de tener un resultado = 1.8
#F(x) de probabilidad
plogis(1.4, location=1.5264961, scale = 0.1112113, lower.tail=TRUE, log.p = FALSE)*100 #Prob resultado <1.8

plogis(1.3, location=1.5264961, scale = 0.1112113, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >1.7
plogis(1.5, location=1.5264961, scale = 0.1112113, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >1.9
plogis(1.6, location=1.5264961, scale = 0.1112113, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >2.1

vProbXI=plogis(c(1.4,1.6), location=1.5264961, scale = 0.1112113, lower.tail=TRUE) #Probabilidad de valor entre....
vProbXI
miProbXI=vProbXI[2]-vProbXI[1]
miProbXI*100

valor_5pct<- qlogis(0.05, location=1.5264961, scale = 0.1112113, lower.tail=TRUE) #valor que corta el 5% de los datos (percentil 5)
valor_95pct<- qlogis(0.95, location=1.5264961, scale = 0.1112113, lower.tail=TRUE) #valor que corta el 95% de los datos (percentil 95)

valor_inf<- qlogis(0.0001, location=1.5264961, scale = 0.1112113, lower.tail=TRUE) #solo ubicar el texto en el gráfico
valor_sup<- qlogis(0.999, location=1.5264961, scale = 0.1112113, lower.tail=TRUE) #solo ubicar el texto en el gráfico

hist(simular.fcreXI, ylim=c(0,2.5),freq = FALSE, col="skyblue",main="Histograma",sub="Datos simulados de una Dist. Logistic; Repet=5000",
     breaks = 20)
x<- seq(0,100, by=1)
curve(dlogis(x, location=1.5264961, scale = 0.1112113),col="darkblue",lwd=2,add=TRUE)
abline(v=valor_5pct, col="red", lty=1, lwd=2)
abline(v=valor_95pct, col="red", lty=1, lwd=2)
text(x=((valor_5pct - valor_inf)/2 + valor_inf), y=2.55, labels=" <- 5% ")
text(x=((valor_sup - valor_95pct)/2 + valor_95pct), y=2.55, labels=" 5% -> ")


#curvas de probabilidad acumulada 
curve(plogis(x, location=1.5264961, scale = 0.1112113),xlim=c(0,4), col="blue",lwd=2,
      xlab="FCRe",ylab="F(x)",main="Función de Distribución")
abline(-10,1)

curve(1-plogis(x, location=1.5264961, scale = 0.1112113),xlim=c(0,4),col="blue",lwd=2,
      xlab="FCRe",ylab="F(x)",main="Función de Distribución")
abline(v=1.3, col="green", lty=2)
abline(v=1.5, col="orange", lty=2)
abline(v=1.6, col="red", lty=2)



#---------------
# [ XII Región]
#---------------

#Ajustar Distribución
hist(trucha[trucha$Region=="XII",]$FCRe)

#ajustar a dist. de probabilidad
res1.fcreXII<-fit.cont(data2fit=trucha[trucha$Region=="XII",]$FCRe) 
res1.fcreXII

#Simular 5000 resultados posibles en base a parametros fijos (dist. normal "mean-sd")
simular.fcreXII<- rlogis(5000, location=1.33637159, scale = 0.04160017 ) 
hist(simular.fcreXII,freq = TRUE, col="lightsalmon",main="Histograma",sub="Datos simulados de una Dist. Logistic",
     breaks = 25)

hist(simular.fcreXII, freq = FALSE, col="lightsalmon",main="Histograma",sub="Datos simulados de una Dist. Logistic; Repet=5000",
     breaks = 20)
#agregar curva teorica (normal)
x<- seq(0,100, by=1)
curve(dlogis(x, location=1.33637159, scale = 0.04160017),col="blue",lwd=2,add=TRUE)


#F(x) de densidad
dlogis(1.4, location=1.33637159, scale = 0.04160017, log=FALSE)*100 #probabilidad de tener un resultado = 1.8
#F(x) de probabilidad
plogis(1.4, location=1.33637159, scale = 0.04160017, lower.tail=TRUE, log.p = FALSE)*100 #Prob resultado <1.8

plogis(1.3, location=1.33637159, scale = 0.04160017, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >1.7
plogis(1.5, location=1.33637159, scale = 0.04160017, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >1.9
plogis(1.6, location=1.33637159, scale = 0.04160017, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >2.1


vProbXI=plogis(c(1.4,1.6), location=1.33637159, scale = 0.04160017, lower.tail=TRUE) #Probabilidad de valor entre.....
vProbXI
miProbXI=vProbXI[2]-vProbXI[1]
miProbXI*100


valor_5pct<- qlogis(0.05, location=1.33637159, scale = 0.04160017, lower.tail=TRUE) #valor que corta el 5% de los datos (percentil 5)
valor_95pct<- qlogis(0.95, location=1.33637159, scale = 0.04160017, lower.tail=TRUE) #valor que corta el 95% de los datos (percentil 95)

valor_inf<- qlogis(0.0001, location=1.33637159, scale = 0.04160017, lower.tail=TRUE) #solo ubicar el texto en el gráfico
valor_sup<- qlogis(0.999, location=1.33637159, scale = 0.04160017, lower.tail=TRUE) #solo ubicar el texto en el gráfico

hist(simular.fcreXII, freq = FALSE, col="skyblue",main="Histograma",sub="Datos simulados de una Dist. Logistic; Repet=5000",
     breaks = 20)
x<- seq(0,100, by=1)
curve(dlogis(x, location=1.33637159, scale = 0.04160017),col="darkblue",lwd=2,add=TRUE)
abline(v=valor_5pct, col="red", lty=1, lwd=2)
abline(v=valor_95pct, col="red", lty=1, lwd=2)
text(x=((valor_5pct - valor_inf)/2 + valor_inf), y=5.9, labels=" <- 5% ")
text(x=((valor_sup - valor_95pct)/2 + valor_95pct), y=5.9, labels=" 5% -> ")



#curvas de probabilidad acumulada 
curve(plogis(x, location=1.33637159, scale = 0.04160017),xlim=c(-0,4), col="blue",lwd=2,
      xlab="FCRe",ylab="F(x)",main="Función de Distribución")

curve(1-plogis(x, location=1.33637159, scale = 0.04160017),xlim=c(0,4),col="blue",lwd=2,
      xlab="FCRe",ylab="F(x)",main="Función de Distribución")
abline(v= 1.3, col="green", lty=2)
abline(v= 1.5, col="orange", lty=2)
abline(v= 1.6, col="red", lty=2)



## ==================================
##      CONVERSIÓN - FCRb 
## ==================================

#---------------
# [ X Región]
#---------------

#Ajustar Distribución
hist(trucha[trucha$Region=="X",]$FCRb)

#ajustar a dist. de probabilidad
res1.fcrbX<-fit.cont(data2fit=trucha[trucha$Region=="X",]$FCRb) 
res1.fcrbX


#Simular 5000 resultados posibles en base a parametros fijos (dist. gamma "shape-rate")
simular.fcrbX<- rlnorm(50000, meanlog=0.28203662, sdlog = 0.08766388) 
hist(simular.fcrbX, freq = TRUE, col="lightsalmon",main="Histograma",sub="Datos simulados de una Dist. Log-Normal",
     breaks = 20)

hist(simular.fcrbX, freq = FALSE, col="lightsalmon",main="Histograma",sub="Datos simulados de una Dist. Log-Normal; Repet=5000",
     breaks = 20)
#agregar curva teorica (gamma)
x<- seq(0,100, by=1)
curve(dlnorm(x, meanlog=0.28203662, sdlog = 0.08766388),col="blue",lwd=2,add=TRUE)


#F(x) de densidad
dlnorm(1.4, meanlog=0.28203662, sdlog = 0.08766388, log=FALSE)*100 #probabilidad de tener un resultado = 1.8 gf3
#F(x) de probabilidad
plnorm(1.4, meanlog=0.28203662, sdlog = 0.08766388, lower.tail=TRUE, log.p = FALSE)*100 #Prob resultado <1.8

plnorm(1.2, meanlog=0.28203662, sdlog = 0.08766388, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >1.8
plnorm(1.3, meanlog=0.28203662, sdlog = 0.08766388, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >1.9
plnorm(1.4, meanlog=0.28203662, sdlog = 0.08766388, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >2.1


vProbX=plnorm(c(1.3, 1.4), meanlog=0.28203662, sdlog = 0.08766388, lower.tail=TRUE) #Probabilidad de valor entre ....
vProbX
miProbX=vProbX[2]-vProbX[1]
miProbX*100


valor_5pct<- qlnorm(0.05,meanlog=0.28203662, sdlog = 0.08766388, lower.tail=TRUE) #valor que corta el 5% de los datos (percentil 5)
valor_95pct<- qlnorm(0.95,meanlog=0.28203662, sdlog = 0.08766388, lower.tail=TRUE) #valor que corta el 95% de los datos (percentil 95)

valor_inf<- qlnorm(0.0001,meanlog=0.28203662, sdlog = 0.08766388, lower.tail=TRUE) #solo ubicar el texto en el gráfico
valor_sup<- qlnorm(0.999,meanlog=0.28203662, sdlog = 0.08766388, lower.tail=TRUE) #solo ubicar el texto en el gráfico

hist(simular.fcrbX, freq = FALSE, col="skyblue",main="Histograma",sub="Datos simulados de una Dist. Log-Normal; Repet=5000",
     breaks = 20)
x<- seq(0,100, by=1)
curve(dlnorm(x, meanlog=0.28203662, sdlog = 0.08766388),col="darkblue",lwd=2,add=TRUE)
abline(v=valor_5pct, col="red", lty=1, lwd=2)
abline(v=valor_95pct, col="red", lty=1, lwd=2)
text(x=((valor_5pct - valor_inf)/2 + valor_inf), y=3.45, labels=" <- 5% ")
text(x=((valor_sup - valor_95pct)/2 + valor_95pct), y=3.45, labels=" 5% -> ")



#curvas de probabilidad acumulada 
curve(plnorm(x, meanlog=0.28203662, sdlog = 0.08766388),xlim=c(0,4), col="blue",lwd=2,
      xlab="FCRb",ylab="F(x)",main="Función de Distribución")
abline(-1.8,1.1)

curve(1-plnorm(x, meanlog=0.28203662, sdlog = 0.08766388),xlim=c(0,4),col="blue",lwd=2,
      xlab="FCRb",ylab="F(x)",main="Función de Distribución")
abline(v= 1.2, col="green", lty=2)
abline(v= 1.3, col="orange", lty=2)
abline(v= 1.4, col="red", lty=2)




#---------------
# [ XI Región]
#---------------

#Ajustar Distribución
hist(trucha[trucha$Region=="XI",]$FCRb)

#ajustar a dist. de probabilidad
res1.fcrbXI<-fit.cont(data2fit=trucha[trucha$Region=="XI",]$FCRb) 
res1.fcrbXI

#Simular 5000 resultados posibles en base a parametros fijos 
simular.fcrbXI<- rnorm(5000, mean=1.30496946, sd = 0.06745951) 
hist(simular.fcrbXI,freq = TRUE, col="lightsalmon",main="Histograma",sub="Datos simulados de una Dist. Log-Normal",
     breaks = 25)

hist(simular.fcrbXI, freq = FALSE, col="lightsalmon",main="Histograma",sub="Datos simulados de una Dist. Log-Normal; Repet=5000",
     breaks = 20)
#agregar curva teorica (gamma)
x<- seq(0,100, by=1)
curve(dnorm(x, mean=1.30496946, sd = 0.06745951),col="blue",lwd=2,add=TRUE)


#F(x) de densidad
dnorm(1.4, mean=1.30496946, sd = 0.06745951, log=FALSE)*100 #probabilidad de tener un resultado = 1.8
#F(x) de probabilidad
pnorm(1.4, mean=1.30496946, sd = 0.06745951, lower.tail=TRUE, log.p = FALSE)*100 #Prob resultado <1.8

pnorm(1.2, mean=1.30496946, sd = 0.06745951, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >1.7
pnorm(1.3, mean=1.30496946, sd = 0.06745951, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >1.9
pnorm(1.4, mean=1.30496946, sd = 0.06745951, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >2.1

vProbXI=pnorm(c(1.3,1.4), mean=1.30496946, sd = 0.06745951, lower.tail=TRUE) #Probabilidad de valor entre....
vProbXI
miProbXI=vProbXI[2]-vProbXI[1]
miProbXI*100

valor_5pct<- qnorm(0.05, mean=1.30496946, sd = 0.06745951, lower.tail=TRUE) #valor que corta el 5% de los datos (percentil 5)
valor_95pct<- qnorm(0.95, mean=1.30496946, sd = 0.06745951, lower.tail=TRUE) #valor que corta el 95% de los datos (percentil 95)

valor_inf<- qnorm(0.0001, mean=1.30496946, sd = 0.06745951, lower.tail=TRUE) #solo ubicar el texto en el gráfico
valor_sup<- qnorm(0.999, mean=1.30496946, sd = 0.06745951, lower.tail=TRUE) #solo ubicar el texto en el gráfico

hist(simular.fcrbXI, freq = FALSE, col="skyblue",main="Histograma",sub="Datos simulados de una Dist. Log-Normal; Repet=5000",
     breaks = 20)
x<- seq(0,100, by=1)
curve(dnorm(x, mean=1.30496946, sd = 0.06745951),col="darkblue",lwd=2,add=TRUE)
abline(v=valor_5pct, col="red", lty=1, lwd=2)
abline(v=valor_95pct, col="red", lty=1, lwd=2)
text(x=((valor_5pct - valor_inf)/2 + valor_inf), y=5.95, labels=" <- 5% ")
text(x=((valor_sup - valor_95pct)/2 + valor_95pct), y=5.95, labels=" 5% -> ")


#curvas de probabilidad acumulada 
curve(pnorm(x, mean=1.30496946, sd = 0.06745951),xlim=c(0,4), col="blue",lwd=2,
      xlab="FCRb",ylab="F(x)",main="Función de Distribución")
abline(-10,1)

curve(1-pnorm(x, mean=1.30496946, sd = 0.06745951),xlim=c(0,4),col="blue",lwd=2,
      xlab="FCRb",ylab="F(x)",main="Función de Distribución")
abline(v=1.2, col="green", lty=2)
abline(v=1.3, col="orange", lty=2)
abline(v=1.4, col="red", lty=2)



#---------------
# [ XII Región]
#---------------

#Ajustar Distribución
hist(trucha[trucha$Region=="XII",]$FCRb,breaks = 10)

#ajustar a dist. de probabilidad
res1.fcrbXII<-fit.cont(data2fit=trucha[trucha$Region=="XII",]$FCRb) 
res1.fcrbXII

#Simular 5000 resultados posibles en base a parametros fijos (dist. normal "mean-sd")
simular.fcrbXII<- rlogis(5000, location=1.29598760, scale = 0.03976553) 
hist(simular.fcrbXII,freq = TRUE, col="lightsalmon",main="Histograma",sub="Datos simulados de una Dist. Normal",
     breaks = 25)

hist(simular.fcrbXII, freq = FALSE, col="lightsalmon",main="Histograma",sub="Datos simulados de una Dist. Normal; Repet=5000",
     breaks = 20)
#agregar curva teorica (normal)
x<- seq(0,100, by=1)
curve(dlogis(x, location=1.29598760, scale = 0.03976553),col="blue",lwd=2,add=TRUE)


#F(x) de densidad
dlogis(1.4, location=1.29598760, scale = 0.03976553, log=FALSE)*100 #probabilidad de tener un resultado = 1.8
#F(x) de probabilidad
plogis(1.4, location=1.29598760, scale = 0.03976553, lower.tail=TRUE, log.p = FALSE)*100 #Prob resultado <1.8

plogis(1.2, location=1.29598760, scale = 0.03976553, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >1.7
plogis(1.3, location=1.29598760, scale = 0.03976553, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >1.9
plogis(1.4, location=1.29598760, scale = 0.03976553, lower.tail=FALSE, log.p = FALSE)*100 #Prob resultado >2.1


vProbXI=plogis(c(1.3,1.4), location=1.29598760, scale = 0.03976553, lower.tail=TRUE) #Probabilidad de valor entre.....
vProbXI
miProbXI=vProbXI[2]-vProbXI[1]
miProbXI*100


valor_5pct<- qlogis(0.05, location=1.29598760, scale = 0.03976553, lower.tail=TRUE) #valor que corta el 5% de los datos (percentil 5)
valor_95pct<- qlogis(0.95, location=1.29598760, scale = 0.03976553, lower.tail=TRUE) #valor que corta el 95% de los datos (percentil 95)

valor_inf<- qlogis(0.0001, location=1.29598760, scale = 0.03976553, lower.tail=TRUE) #solo ubicar el texto en el gráfico
valor_sup<- qlogis(0.999, location=1.29598760, scale = 0.03976553, lower.tail=TRUE) #solo ubicar el texto en el gráfico

hist(simular.fcrbXII, ylim=c(0,7),freq = FALSE, col="skyblue",main="Histograma",sub="Datos simulados de una Dist. Logistic; Repet=5000",
     breaks = 20)
x<- seq(0,100, by=1)
curve(dlogis(x, location=1.29598760, scale = 0.03976553),col="darkblue",lwd=2,add=TRUE)
abline(v=valor_5pct, col="red", lty=1, lwd=2)
abline(v=valor_95pct, col="red", lty=1, lwd=2)
text(x=((valor_5pct - valor_inf)/2 + valor_inf), y=7, labels=" <- 5% ")
text(x=((valor_sup - valor_95pct)/2 + valor_95pct), y=7, labels=" 5% -> ")



#curvas de probabilidad acumulada 
curve(plogis(x, location=1.29598760, scale = 0.03976553),xlim=c(-0,4), col="blue",lwd=2,
      xlab="FCRb",ylab="F(x)",main="Función de Distribución")

curve(1-plogis(x, location=1.29598760, scale = 0.03976553),xlim=c(0,4),col="blue",lwd=2,
      xlab="FCRb",ylab="F(x)",main="Función de Distribución")
abline(v= 1.2, col="green", lty=2)
abline(v= 1.3, col="orange", lty=2)
abline(v= 1.4, col="red", lty=2)
