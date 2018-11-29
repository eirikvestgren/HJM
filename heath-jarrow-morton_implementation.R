#Heath-Jarrow-Morton framework:
#Working implementation of the HJM framework. 
#Further work: Implement functions to value other contracts than zero coupon bonds (where HJM is not necessary)


#What does the function return: 
#Only the final price of the instrument that is priced. 

#Should more things be returned? If yes, then what? 


source('utility_functions.R')

#Inputs to HJM drift: 
#S_j(k), j = 1...M-i, k = 1...d, the volatility of the forward rates. 
# h_1 ... h_M, h_i = t_i -t_i-1
HJM_drift = function(s_df, maturities, nFactors)
{
  d = nFactors 
  s_df = as.matrix(s_df)
  M = length(maturities) 
  if (dim(s_df)[2] != d)
  {
    s_df = t(s_df)
  }
  n_times = dim(s_df)[1]
  #Find out what the correct is for i, which i have called n_times
  #if(is.null(n_times))
  #{
  #  n_times = length(s_df)/d
  #}
  B_prev = 0;
  B_next = 0;
  final_j = n_times #dim(s_df)[1] = M-i, i.e. the final j
  i = M - final_j
  m = rep(0, final_j)
  for (j in 1:final_j)
  {
    A_prev = rep(0,d) #Inside or outside for(j)? 
    A_next = rep(0,d)
    
    
    for(k in 1:d)
    {
      A_next[k] = A_prev[k] + s_df[j,k]*maturities[i+j]
      
      B_next = B_next + A_next[k]*A_next[k]
      A_prev[k] = A_next[k]
    } #end for k 
    m[j] = (B_next - B_prev)/(2*maturities[i+j])
    B_prev = B_next
  } #end for j 
  return(m)
}

HJM_func = function(forward_yield_curve, times_of_yields, forward_interest_data, nFactors, cashflow, cap_strike)
  # Full implementation of the HJM framework, with the yield curve and corresponding times as inputs. 
  # I might have misunderstood the concepts of forward rates a bit. Hence, a small modification to the 
  # times_of_yields argument may be required
{
  
  n_termins = length(times_of_yields)
  #Constructing the discount intervals.  
  times_of_yields = c(0, times_of_yields)
  diff_times_of_yields = diff(times_of_yields)
  
  
  M = length(forward_yield_curve)
  
  discount_factor = 1; 
  ZCB = 0; 
  C = 0; 
  
  #Cash flows for zero coupon bond and cap
  
  P_ZCB = c(rep(0, n_termins-1), 1) #Vector with cashflow at each maturity 
  P = rep(0, n_termins)
  
  
  #Do better by allowing for cashflows at arbitrary times between each maturity.
  
  #All the skj's, with the number of principal factors preserved equal to the number of factors in the model:
  forward_data_df = data.frame(matrix(unlist(forward_interest_data), nrow = length(forward_interest_data[[1]]), byrow = T))
  all_sjks = s_eval(forward_data_df, nFactors)
  #Does/should the principal components change when different number of dimensions is considered. 
  #Cash flow from cap is found by comparing the level of forward_yield_curve [1] and the strike price. 
  
  for( i in 1:(M-1))
  {
    discount_factor = discount_factor*exp(-forward_yield_curve[i]* diff_times_of_yields[i])
    
    #Now, find the s_j(k)'s. This is done through Principal Component Analysis when we want to fit it to real data. 
    subset_s = all_sjks[1:(M-i), ]
    
    m = HJM_drift(subset_s, diff_times_of_yields, nFactors)
    Z = rnorm(nFactors)
    for(j in 1:(M-i) )
    {
      S = 0;
      if ( (M-i) == 1){ S = sum(subset_s[1:nFactors]*Z[1:nFactors])}
      else { S = sum(subset_s[j, 1:nFactors]*Z[1:nFactors])}
      forward_yield_curve[j] <- forward_yield_curve[j+1] + m[j]*diff_times_of_yields[i] + S*sqrt(diff_times_of_yields[i])
    }
    
    #Evaluate cash flow of the product we want to value given the realization that is done 
    ZCB = ZCB+ discount_factor*P_ZCB[i]
    P[i] = max(forward_yield_curve-cap_strike, 0)
    C = C + discount_factor*P[i]
  }
  #Final payoff: 
  ZCB = ZCB + discount_factor*P_ZCB[M]
  
  
  #Return ZCB if this is desired
  return(ZCB)
  #Want to return ZCB, C and the realization of the spot rate. 
  #return(data.frame(ZCB, C, )
  #return(C)
}

s_eval = function(data, nFactors)
  #Evaluation of the s's through principal components. 
{
  nRates = dim(data)[2]
  prince = princomp(data, cor = FALSE) #Does not work with list
  s_vec = prince$loadings[1:(nFactors*nRates)]
  s = matrix(s_vec, nrow = nRates)
  return(s)
}

zero_bond_cash_flow = function(times)
{
  return(c(rep(0, length(times) -1 ), 1))
  #return(c(0,0,0,0,1))
  #Leads to zero valuation 
}
zero_bond_cash_flow_early_mat = function(times)
{
  return(c(0,0,0,1,0))
}
cap_representation_cash_flow = function(final_maturity, cap_strike)
  #How is it done when caplet is paid between maturity dates? Assume for now that they aren't 
  #Assume payment every 6 months
  #Which rate is should be used for the payment? Spot rate at the given time. 
{
  return()
}
