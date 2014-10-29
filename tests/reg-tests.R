#1: function
str <- function(object, ...) UseMethod("str")

#2: if-else
if(length(larg <- list(...))) {
 nl <- names(larg)
 iGiveHead <- which(nl == "give.head")
 if (any(Bgl <- nl == "give.length"))
  give.length <- larg[[which(Bgl)]]
 else if(length(iGiveHead))
  give.length <- larg[[iGiveHead]]
 if(length(iGiveHead)) # eliminate it from arg.list
  larg <- larg[ - iGiveHead ]
 if(is.numeric(larg[["nest.lev"]]) &&
  is.numeric(larg[["vec.len"]])) # typical call from data.frame
 ## reduce length for typical call:
 larg[["vec.len"]] <-
	min(larg[["vec.len"]],
	    (larg[["width"]]- nchar(larg[["indent.str"]]) -31)%/% 19)
}

#3: backtick
`%w/o%` <- function(x,y) x[is.na(match(x,y))]

#4: for loop
for (i in seq_len(min(list.len,le) ) ) {
		    cat(indent.str, comp.str, nam.ob[i], ":", sep = "")
		    envir <- # pass envir for 'promise' components:
			if(typeof(object[[i]]) == "promise") {
			    structure(object, nam= as.name(nam.ob[i]))
			} # else NULL
		    strSub(object[[i]], give.length=give.length,
                           nest.lev = nest.lev + 1,
                           indent.str = paste(indent.str,".."))
}

#5: while loop
while(a<10) {
 cat("Hello\n")
}

#6: repeat loop
repeat {
       error= -0.2*log((1/runif(60, 0, 1))-1) # a random component
       z=(p<0.5+error) # TRUE/FALSE condition
       z=replace(z, z==TRUE, 1) # replace z to 1 if z is true, else z=0

       if (sum(z[(3*i-2):(3*i)])>0) {break}
}
#7: command
error= -0.2*log((1/runif(60, 0, 1))-1) # a random component
z=(p<0.5+error) # TRUE/FALSE condition