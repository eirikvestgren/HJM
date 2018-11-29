#Cleaning rate data of the format received from Jacob 

#Packages: 
require(readxl)

getwd()
setwd("C:/Users/eirik/OneDrive/Documents/ProsjektMaster") 


#Other scripts: 
source('utility_functions.R')
source('heath-jarrow-morton_implementation.R')



filename = "RDataRaw2015v2.xlsx"

data_raw = read_excel(filename)


X = cleanRateData(data_raw)
zooList = makeZooList(X)

NIBOR = zooList[1:5]
swap = zooList[6:14]



searchDate = as.Date("2010-11-10")
searchDate2 = as.Date("2011-11-01")
b = yield_curve_fit_to_data(NIBOR, searchDate2)
plot(c(0,1/250, 1/4, 1/2, 1), b, type = "l")

timesOfMaturity = c(0,1/250, 1/4, 1/2, 1)
v = get_data_from_interval(searchDate, searchDate2, NIBOR)
bn = yield_curve_to_absolute(b)



swap_maturities = c(1,2,3,4,5,7,10,15,20)
swap_yields = yield_curve_fit_to_data(swap, searchDate2)
swap_yields_abs = yield_curve_to_absolute(swap_yields)
interval_swap = get_data_from_interval(searchDate, searchDate2, swap)
plot(swap_maturities,swap_yields,type = "l")

something = HJM_func(b, timesOfMaturity, v, 2, zero_bond_cash_flow)

something = HJM_func(bn, timesOfMaturity, v, 3, zero_bond_cash_flow_early_mat) #Works correctly now

n_rep = 1000 
value_vec = rep(0, n_rep)
for( i in 1:n_rep)
{
  value_vec[i] = HJM_func(bn, timesOfMaturity, v, nFactors = 3, zero_bond_cash_flow_early_mat, 0.00)
}
hist(value_vec)
summary(value_vec)

# Try HJM-simulation with swap rates
# Cap strike rate 0.02

swap_value_vec = numeric(n_rep)
for (i in 1:n_rep)
{
  swap_value_vec[i] = HJM_func(swap_yields_abs, swap_maturities, interval_swap, 2, zero_bond_cash_flow, 0.02)
}

hist(swap_value_vec)
summary(swap_value_vec)



sw_yc = yield_curve_fit_to_data(swap, searchDate)
plot(sw_yc, type = "l")


b= NIBOR[[2]]
a = NIBOR[[1]]
a[300]
a[searchDate]

getIndexFromDateZoo(searchDate2, a)

b = index(a[searchDate])
p = index(a[searchDate2])

a[b]

notX = raw_to_clean(data_raw) #A collection of lists, without N/A values. Something strange here. Should work. 

#zooList = makeZooList(X)





forward_data_df = data.frame(matrix(unlist(v), nrow = length(v[[1]]), byrow = T))




#Irrelevant, but interesting insights below this line
#===================================================================
#Why doesn't convertZoo work with date argument like X[1].
typeof(X[[1]]) 
typeof(X$d1) 
#X[1] is a list object, which isn't accepted by the ordering procedure of zoo 
a = zooList[1:5]
plot(zooList[[5]])


testDate = as.Date("2009-11-01")

date_diff = testDate - searchDate2


dateVec = as.Date(c("2010-01-01", "2010-02-01", "2010-03-01", "2010-04-01"))
new = sapply(dateVec, FUN = day_diff,  today = testDate)
