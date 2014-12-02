#1: function
str <- function(object, ...) UseMethod("str")

#2: if-else
a <- 1:10
if(length(a)) {
 b <- sum(a)
 if (b > 100 && b < 10000)
  c <- sqrt(b)
 else if(b < 500)
  c <- log(b)
}

#3: backtick
`%w/o%` <- function(x,y) x[is.na(match(x,y))]

#4: for loop
for (i in seq_len(10)) {
		    cat(i, "\n", sep = "")
		    e <- # dummy variable
			if(i == 3L) {
			    cat(sqrt(i), "\n", sep = "")
			} # else
		    cat(i^2, "\n", sep = "")
}

#5: while loop
a <- 1
while(a<10) {
 cat("Hello\n")
 a <- a + 1
}

#6: repeat loop
repeat {
       p <- 1
       i <- 1
       error= -0.2*log((1/runif(60, 0, 1))-1)
       z=(p<0.5+error)
       z=replace(z, z==TRUE, 1)

       if (sum(z[(3*i-2):(3*i)]) != 0) {break}
}
#7: command
p <- 1
error= -0.2*log((1/runif(60, 0, 1))-1) # a random component
z=(p<0.5+error) # TRUE/FALSE condition

#8: code block
{
  {
   y <- sqrt(3)
  }
  x <- 1:100
  mean(x)
  sd(x)
}