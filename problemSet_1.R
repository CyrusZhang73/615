myName <- "Hao Zhang"

#vector
#1
v1 <- c(1:20)
v2 <- c(20:1)
v3 <- seq(1, 19, by = 2)
v4 <- rep(c(3,7,11), times=10)
v5 <- c(v4, 3)

#2
a <- seq(3, 6, by = 0.1)
x1 <- exp(a)*sin(a)

#3
b <- 10:100
sum1<- sum(b^3+4*b^2)

#4
str1 <- paste("label", 1:30 )
str2 <- paste("function", 1:30, sep="")

#5
c <- c(1,'function',NA, seq(1,5,2), 0.125)
vs <- paste(c, collapse = ",")
vs

#matrix
#6
A <- matrix(seq(1:9), 3, 3)
m1_ans <- A%*%A%*%A
m1_ans

#7
B <- matrix(c(12, -12, 12), nrow = 17, ncol = 3,byrow = TRUE)
m2_ans <- t(B)%*%B
m2_ans

#8 
y <- c(7, -1, -3, 5, 17)
A <- matrix(0, 5, 5)
A <- abs(col(A)-row(A))+1
m3_ans <- solve(A, y)
m3_ans

#function 
##9a
xv <- seq(0, 1, by=0.1)
function1 <- function(xv){
  xv^(1:length(xv))
  }
func1_ans <- function1(xv)
func1_ans
#9b
function2 <- function(xv){
  n <- length(xv) 
  return((xv^(1:n))/(1:n))
  }
func2_ans <- function2(xv)
func2_ans
##9c
function3 <- function(x,n){
  1+sum(x^(1:n)/(1:n))
  }
func3_ans <- function3(xv, length(xv))
func3_ans

##10
cel_to_far <- function(x) {9*x/5+32}
cel_to_far(27.5)

far_to_cel <- function(x) {(x-32)*5/9}
far_to_cel(78.6)

##11
odd <- function(x) {
  seq(1,x, by=2)
  }
odd_ans <- odd(2000)
odd_ans

##12
  fun <- function(x)
    {
    fun_r <- function(r)
      {
      sum (((1:r)^0.5)/(11+3.5*r^1.2))
           }
    sum(sapply(x,FUN=fun_r))
    }
sum_ans <- fun(10)
sum_ans

##13
modNumber<- function(x, y) {
  n <- x%%y 
if (n==0) 
{return (x)}
  else 
  return (x+y-n)
  }

modNumber(50, 16)
modNumber(500,6)

##14
numberOfWheels <- function(x) {
  switch(x, 
         unicycle=1, 
         bike= , 
         car= , 
         truck=4, 
         tricycle=3, 
         motorcycle=2)
}

##15
myFactorial <- function(number){ X <- c(1:number) 
return(prod(X))}

myFactorial(3)

##16
x=NULL
y=NULL
myCustomFactorial<- function(x,y) {
  return(prod(x:y))
}
myCustomFactorial(2,4)
myCustomFactorial(5,5)

##17
customRiverMean <- function(x){
  riverlength <- rivers < x 
  sum(rivers[riverlength]/sum(riverlength))
}
customRiverMean(400)

##18
for (i in 60){
  l=ToothGrowth$len>=15
  longTeeth=ToothGrowth[l,]$len
  print(longTeeth)
}

##19
apply(mtcars, 2, mean)
averageHorsePower <- c(mean(mtcars$hp))
averageHorsePower
averageWeight <- c(mean(mtcars$wt))
averageWeight


##20
function_20<-function(x,y){
  for (i in 1:length(x)){
    y<x[i]
  }
 # n<-sapply(y<x, sum)
  
}
function_20(c(1:4),c(1:4))
x<-c(1:4)
y<-c(1:4)
n<-y<x
sapply(n, sum)