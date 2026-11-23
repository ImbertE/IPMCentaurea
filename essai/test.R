rm(list=ls())
library(spaMM)
library(splines)

IPM_data <- read.csv("donneesIPM.csv") 
centauree_data <- IPM_data[!is.na(IPM_data$Size0Mars) & !is.na(IPM_data$Age),]
centauree_data$Age[centauree_data$Age > 8] <- 8
growthdata <- centauree_data[!is.na(centauree_data$Size1Mars), ]
growthdata <- growthdata[growthdata$Size1Mars != 0, ]


Growthglm1 <- fitme(Size1Mars ~ 1 + poly(Size0Mars,3) + bs(Age,degree=2,knots=6.5) + (Age|year) + (1|Pop),
                    resid.model = ~ log(Size0Mars)+log(Age) + (1|year) + (1|Pop),data=growthdata)


boxcox <- function(object, interval=c(-0.1,0.9), verbose=FALSE) {
  data <- object$data
  Size0Mars <- data$Size0Mars
  Size1Mars <- data$Size1Mars
  objfn <- function(lambda, return_fit=FALSE) {
    data$Size0Mars <- Size0Mars^lambda
    data$Size1Mars <- Size1Mars^lambda
    refit <- update(object, data=data)
    if (return_fit) {
      return(refit)
    } else {
      logjac <- sum((lambda-1)*log(Size1Mars)+log(lambda))
      resu <- logLik(refit)+logjac
      if (verbose) print(resu)
      return( - resu)
    }
  }
  
  optr <- optimize(f=objfn,interval=interval)
  return(list(optr=optr,
              fit=objfn(lambda = optr$minimum, return_fit = TRUE),
              objfn=objfn))
}

essai1 <- boxcox(Growthglm1)
gof(essai1$fit)
essai2 <- essai1$objfn(0.4343354, return_fit = TRUE)
gof(essai2)