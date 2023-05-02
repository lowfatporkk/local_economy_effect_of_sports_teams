## Synthetic Control Model Analysis
#importing libraries
library(Synth)

#filter(data_with_schooling,Code==19)
#length(unique(data_with_schooling$Code))

#getting dataprep
dataprep.out <-
  dataprep(
    foo = as.data.frame(data_with_schooling)
    ,predictors= c("population_growth",
                   "employment_rate",
                   "schooling"
    )
    ,predictors.op = c("mean")
    ,dependent     = c("gdp_per_capita")
    ,unit.variable = c("Code")
    ,time.variable = c("year")
    ,treatment.identifier  = 19
    ,controls.identifier   = c(1:18,20:36)
    ,time.predictors.prior = c(2001:2007)
    ,time.optimize.ssr     = c(2001:2007)
    ,unit.names.variable   = c("Name")
    ,time.plot            = c(2001:2018) 
  )

#running synth
synth.out <- synth(data.prep.obj = dataprep.out)

#getting result tables
synth.tables <- synth.tab(
  dataprep.res = dataprep.out,
  synth.res = synth.out
) 

#printing the result table
print(synth.tables)

#plotting results
path.plot(synth.res = synth.out,
          dataprep.res = dataprep.out,
          Ylab = c("Real GDP per capita"),
          Xlab = c("Year"), 
          Legend = c("Manchester","Synthetic Manchester"),
          Legend.position = "bottomright",
          Ylim = c(30000,40000)
) 

#intervention
Cex.set <- .75
abline(v   = 2008,
       lty = 2)

arrows(2006.5, 32000, 2007.5, 32000,
       col    = "black",
       length = .1)

text(2005.5, 32000,"Acquisition",cex=Cex.set)
gaps.plot(synth.res = synth.out,
          dataprep.res = dataprep.out, 
          Ylab = c("Gap in real GDP per capita"),
          Xlab = c("Year"), 
          Ylim = c(-2000,2000), 
          title()
)

abline(v   = 2008,
       lty = 2)

arrows(2006.5, -1000, 2007.5, -1000,
       col    = "black",
       length = .1)

text(2005.5, -1000,"Acquisition")

Placebo Test in Place

## Placebo Testing
#importing libraries
library(Synth)

#applying the synthetic control method after reassigning the intervention in the data 
#to units and periods where the intervention did not occur
store <- matrix(NA,length(2001:2018),36)
colnames(store) <- unique(data_with_schooling$Name)

#looping the model for all cities
for(iter in 1:36)
{
  dataprep.out <-
    dataprep(
      foo = as.data.frame(data_with_schooling)
             ,predictors= c("population_growth",
                            "employment_rate",
                            "schooling"
             )
             ,predictors.op = c("mean")
             ,dependent     = c("gdp_per_capita")
             ,unit.variable = c("Code")
             ,time.variable = c("year")
             ,treatment.identifier  = iter
             ,controls.identifier   = c(1:36)[-iter]
             ,time.predictors.prior = c(2001:2007)
             ,time.optimize.ssr     = c(2001:2007)
             ,unit.names.variable   = c("Name")
             ,time.plot            = c(2001:2018) 
    )
  
  #running synth
  synth.out <- synth(
    data.prep.obj = dataprep.out,
    method = "BFGS"
  )
  #storing gaps
  store[,iter] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
}

data <- store
rownames(data) <- 2001:2018
#setting bounds in gaps data
gap.start     <- 1
gap.end       <- nrow(data)
years         <- 2001:2018
gap.end.pre  <- which(rownames(data)=="2007")
#MSPE Pre-Treatment
mse        <- apply(data[ gap.start:gap.end.pre,]^2,2,mean)
manchester.mse <- as.numeric(mse[19])
#excluding states with 5 times higher MSPE than Manchester
data <- data[,mse<5*manchester.mse]
Cex.set <- .75
#plotting results
plot(years,data[gap.start:gap.end,which(colnames(data)=="Manchester")],
     ylim=c(-10000,10000),xlab="Year",
     xlim=c(2001,2018),ylab="Gap in real GDP per capita",
     type="l",lwd=2,col="black",
     xaxs="i",yaxs="i")
#adding lines for control states
for (i in 1:ncol(data)) { lines(years,data[gap.start:gap.end,i],col="gray") }
#adding Manchester Line
lines(years,data[gap.start:gap.end,which(colnames(data)=="Manchester")],lwd=2,col="black")

legend("bottomright",legend=c("Manchester","Control cities"),
       lty=c(1,1),col=c("black","gray"),lwd=c(2,1),cex=.8)

#intervention
abline(v=2008,lty=2,col="#404040")
abline(h=0,lty=2,col="#404040")

arrows(2006.5, 3000, 2007.5, 3000,
       col    = "black",
       length = .1)

text(2005.5, 3000,"Acquisition",cex=Cex.set)

abline(v=2001)
abline(v=2018)
abline(h=-10000)
abline(h=10000)

Placebo Test in Time

## Placebo Testing
#importing libraries
library(Synth)
#applying the synthetic control method after reassigning the intervention in the data 
#to units and periods where the intervention did not occur
store <- matrix(NA,length(1:11),length(2006:2013))
colnames(store) <- (2006:2013)
#looping the model for all cities
for(iter in 2006:2013)
{
  dataprep.out <-
    dataprep(
      foo = as.data.frame(data_with_schooling)
      ,predictors= c("population_growth",
                     "employment_rate",
                     "schooling"
      )
      ,predictors.op = c("mean")
      ,dependent     = c("gdp_per_capita")
      ,unit.variable = c("Code")
      ,time.variable = c("year")
      ,treatment.identifier  = 19
      ,controls.identifier   = c(1:18,20:36)
      ,time.predictors.prior = c((iter-5):iter)
      ,time.optimize.ssr     = c((iter-5):iter)
      ,unit.names.variable   = c("Name")
      ,time.plot            = c((iter-5):(iter+5)) 
    )
  #running synth
  synth.out <- synth(
    data.prep.obj = dataprep.out,
    method = "BFGS"
  )
  #storing gaps
  store[,(iter-2005)] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
}
data <- store
rownames(data) <- -5:5
#setting bounds in gaps data
gap.start     <- 1
gap.end       <- nrow(data)
years         <- -5:5
gap.end.pre  <- which(rownames(data)=="0")
#MSPE Pre-Treatment
mse        <- apply(data[ gap.start:gap.end.pre,]^2,2,mean)
treated.mse <- as.numeric(mse[3])
Cex.set <- .75
#plotting results
plot(years,data[gap.start:gap.end,which(colnames(data)=="2008")],
     ylim=c(-10000,10000),xlab="Year",
     ylab="Gap in real GDP per capita",
     type="l",lwd=2,col="black",
     xaxs="i",yaxs="i")
#adding lines for control states
for (i in 1:ncol(data)) { lines(years,data[gap.start:gap.end,i],col="gray") }
#adding Manchester Line
lines(years,data[gap.start:gap.end,which(colnames(data)=="2008")],lwd=2,col="black")
legend("bottomright",legend=c("2008","Other years"),
       lty=c(1,1),col=c("black","gray"),lwd=c(2,1),cex=.8)
#intervention
abline(v=0,lty=2,col="#404040")
abline(h=0,lty=2,col="#404040")
arrows(-0.8, 3000, -0.2, 3000,
       col    = "black",
       length = .1)
text(-1.4, 3000,"Acquisition",cex=Cex.set)
abline(v=2001)
abline(v=2018)
abline(h=-10000)
abline(h=10000)
