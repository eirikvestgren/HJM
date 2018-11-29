#Utility functions

library(zoo)
library(data.table)

#Print: 
printf <- function(...) invisible(cat(sprintf(...)))

removeNAdf = function(dataVector)
#Removes N/A values from a a vector with data
{
  
  return(dataVector[complete.cases(dataVector)])
}

convertZoo = function(prices, dates)
#Converts two vectors with some data and corresponding dates into a zoo object
{
  return(zoo(x = prices, order.by = dates))
}

convertZooList = function(priceList, dateList)
#Converts two lists with data and corresponding dates into a zoo object. Both in form of a list. 
{
  print(typeof(dateList))
  
  convDate = rbind(dateList)
  return(zoo(x = data.frame(priceList), order.by = convDate))
}

rel_vol = function(priceDate)
#Calculates the relative (yearly) volatility of an asset. The input is a zoo object
{
  daysInYear = 252;
  return(sd(diff(log(priceDate))*sqrt(daysInYear) ))
}

abs_vol = function(priceDate)
# Calculates the absolute (yearly) volatility of an interest rate. Input is a zoo object.
{
  daysInYear = 252; 
  return(sd(diff(priceDate))*sqrt(daysInYear))
}

abs_daily_vol = function(priceDate)
{
  return(sd(diff(priceDate)))
}

volatility_scaled_to_time = function(priceDate, dt)
#Returns the volatility when the timestep is not 1. One unit of time is taken to be a day. 
{
  return( abs_daily_vol(priceDate)*sqrt(dt) )
}

#Finish this!!!!
drift_calc = function(priceDate)
#Calculates the average drift per trading day.
#=================================================
# Prove the result? 
# Why is this the average drift rate in percent? 
# Price at t+1: S+x
# Price at t: S
# Return = diff = (S+x) - S => log(return) = log(S+x) - log(S) = log((S+x)/S)
# This is the return in percent
#=================================================  
{
return(mean(diff(log(priceDate))))
}

cleanRateData = function(data)
  # The data is the rate data from Jacob, _raw_
{
  data = as.data.frame(data)
  it = seq(3,39, by = 3)
  for(i in c(it-2, 40)){data[,i] = as.Date(data[,i])}
  #for (i in rev(it)){X = X[,-i]}
  data = data[,-it]
  colnames(data) = c("d1", "p1","d2", "p2","d3", "p3", "d4", "p4","d5", "p5","d6", "p6","d7", "p7",
                  "d8", "p8","d9", "p9","d10", "p10","d11", "p11","d12", "p12","d13", "p13","d14", "p14")
  Xl = as.list(data)
  for (i in 1:length(Xl))
  {
    Xl[[i]] = Xl[[i]][!is.na(Xl[[i]])]
  } #Works, woohoo!! 
  return(Xl)
}

selectRate= function(number, zooList)
{
  return(zooList[[number]])
}

#Yield curve fitting:
yield_curve_fit_to_data = function(rate_frame, date)
  #Check if this is correct. Yes, correct, since we have the interest rates, not the nominal payout value. 
  #When plotting, the non-equal spacing of the different maturity times need to be addressed. 
{
  yield = rep(0, length(rate_frame))
  for (i in 1:length(rate_frame))
  {
    temp = rate_frame[[i]]
    yield[i] = temp[date]
  }
  return(yield)
}
yield_curve_to_absolute= function(yield_curve)
{
  return(yield_curve/100)
}

makeZooList = function(data)
{
  zooList = list()
  for(i in 1:(length(data)/2))
  {
    zooList[[i]] = convertZoo(X[[2*i]], X[[(2*i)-1]])
  }
  return(zooList)
}

raw_to_clean = function(raw_data)
#Not needed at the moment
{
  #Make nice data frame
  semi_clean = cleanRateData(raw_data)
  #Make this to a list of zoo objects as well: 
  cleaner = makeZooList(semi_clean)
  
  return(cleaner)
}
getIndexFromDateZoo = function(date, zooData)
{
  searchElement = zooData[date]
  indCount = 0
  for(i in 1:length(zooData))
  {
    indCount = indCount + 1 
    if (index(zooData[i]) == date)
    {
      break;
    }
  }
  return(indCount)
}

get_data_from_interval = function(initialDate, finalDate, data)
#Both dates need to be trading days.
{
 
  intervalList = list()
  for (i in 1:length(data))
  {
    temp = data[[i]]
    initialIndex = getIndexFromDateZoo(initialDate, temp)
    finalIndex = getIndexFromDateZoo(finalDate, temp)
    intervalList[[i]] = temp[initialIndex:finalIndex]
  }
  return(intervalList)
}

smooth_initial_yield_curve = function(yield_curve, times, today_date, newdates)
  #Make more intermediate points to the yield curve.
  #This allows for valuation of derivatives with expiry between dates. 
  #Also smooths the yield curve.
{
  day_difference_vector = sapply(newdate, FUN = day_diff,  today = today_date) #Number of days between the dates 
  

  return(smoother_curve)
}

day_diff = function(today, future_day)
{
  today = as.Date(today)
  future_day = as.Date(future_day)
  return(future_day - today)
}
