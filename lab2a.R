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
