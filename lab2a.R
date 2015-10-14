#Optimization
f<-function(x){
  yval<-((x-3)^2)+2*((x-3)^2)+3*((x-15)^2)+sin(100*x)
  return(yval)
}
optimise(f,c(0,100))
$minimum
[1] 9.032032

$objective
[1] 215.0062

optimise(f,c(0,15))
$minimum
[1] 9.283052

$objective
[1] 215.4813

optimise(f,c(9,12))
$minimum (y)
[1] 9.534078

$objective (x)
[1] 216.7136

# > optimise(f,c(9,12),maximum = TRUE)
# $maximum
# [1] 11.95739
# 
# $objective
# [1] 269.4118

> optimise(f,c(10,11))
$minimum
[1] 10.16165

$objective
[1] 223.1064

plot(x = c(0:10000),y = sapply(X = c(0:10000),FUN = f),)#Fluctuations!--> Hard to optimize. 
x <- seq(from = 8,to = 10,by = 0.01)
plot(x,f(x))

#Integrating a function.
sinf<-function(x){
  yval<-x*sin(x)
  return(yval)
}

integrate(f = sinf,lower = (-7)*10^5,upper = (7)*10^5,subdivisions = 10^7)
#1356376 with absolute error < 166

system.time(
  integrate(f = sinf,lower = (-7)*10^5,upper = (7)*10^5,subdivisions = 10^7)
)
# user  system elapsed
# 8.023   0.058   8.112 

#parallel
library(parallel)
sinf<-function(x){
  yval<-x*sin(x)
  return(yval)
}
start<-function(x){seq(from = (-7)*10^5,to = (7)*10^5,length.out = 5)[1:4][x]}
end<-function(x){seq(from = (-7)*10^5,to = (7)*10^5,length.out = 5)[2:5][x]}
cl<-makePSOCKcluster(4)
clusterExport(cl,"sinf")
clusterExport(cl,"start")
clusterExport(cl,"end")

sum(unlist(parLapply(cl,1:4,
  function(x){ integrate(f = sinf,lower = start(x),upper = end(x),subdivisions = 10^7)$value}
)))
#[1] 1356376

system.time(
  sum(unlist(parLapply(cl,1:4,
  function(x){ integrate(f = sinf,lower = start(x),upper = end(x),subdivisions = 10^7)$value}
  )))
)

#This is an improvement.
#user  system elapsed 
#0.004   0.002   1.246

#Memoise
install.packages("memoise")
library(memoise)
fib <- function(n) {
  if (n < 2) return(1)
  fib(n - 2) + fib(n - 1)
}

fib2 <- memoise(function(n) {
  if (n < 2) return(1)
  fib2(n - 2) + fib2(n - 1)
})

fib3 <- memoise(fib)

system.time(fib(28))
#user  system elapsed 
#0.798   0.007   0.810
system.time(fib2(28))
#user  system elapsed 
#0.004   0.001   0.005
system.time(fib3(28))
#user  system elapsed 
#0.793   0.007   0.807

###Second round.
system.time(fib(28))
#user  system elapsed 
#0.735   0.003   0.740
system.time(fib2(28))
#user  system elapsed 
#0       0       0 
system.time(fib3(28))
#user  system elapsed 
#0       0       0

#Domain-specific languages
install.packages("ggplot2")
library(ggplot2)
str(mpg)
qplot(x = displ,y = hwy, data = mpg)
qplot(x = displ,y = hwy, data = mpg,color=drv)
qplot(x = displ,y = hwy, data = mpg,geom=c("point","smooth"))
qplot(displ, hwy, data =mpg, geom=c("point","smooth"),method="lm")
qplot(displ, hwy, data =mpg, geom=c("point","smooth"),method="lm",color=drv)

#Modify plots.
gr <- qplot(displ, hwy, data =mpg, geom=c("point","smooth"),method="lm",color=drv)
gr + theme(panel.background = element_rect(colour = "red"))

#Exercise
#Load data
str(diamonds)
str(diamonds$carat)
#num [1:53940] 0.23 0.21 0.23 0.29 0.31 0.24 0.24 0.26 0.22 0.23 ...
str(diamonds$price)
#int [1:53940] 326 326 327 334 335 336 336 337 337 338 ...

#Plot carat vs price.Price goes up with carat.
qplot(x = carat,y = price, data = diamonds)
#Plot differences between colors in colors!More or less different colors for different carats.
str(diamonds$color)
#Ord.factor w/ 7 levels "D"<"E"<"F"<"G"<..: 2 2 2 6 7 7 6 5 2 5 ...
qplot(x = carat,y = price, data = diamonds,color = color)
#Add smoother.
qplot(x = carat,y = price, data = diamonds,color = color, geom = c("point","smooth"))

#Change legend title.
dp<-qplot(x = carat,y = price, data = diamonds,color = color, geom = c("point","smooth"))
dp + scale_colour_brewer(name = "New legends")
