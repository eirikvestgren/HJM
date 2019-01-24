#Tidy implementation of HJM

source('utility_functions.R')

#Framework that plots the realized rate: 



HJM_rate_simulation_simple = function(maturity_vector_in_years, final_simulation_date, spot_rate_history, todays_rates)
{
  #Parameters for the model: Specify these to the situation
  dt = 1/252; #The time every rate will be predicted ahead for each simulation. 
  number_of_simulations = 100
  number_of_factors = 3 #Number of principal components to be extracted.
  
  
  
  #Calculated quantities obtained from arguments and/or parameters: 
  M = length(maturity_vector_in_years)
  
  #Vectors for simulation times and intervals
  #simulation_times = seq(0, final_simulation_date, by = dt)
  simulation_times = maturity_vector_in_years
  time_intervals = diff(simulation_times)
  
  #Calculating the initial forward curve:
  in_forward_curve= rep(0,M)
  in_forward_curve[1] = todays_rates[1]
  for ( i in 2:M)
  {
    in_forward_curve[i] = forward_transform(maturity_vector_in_years[1], maturity_vector_in_years[i], todays_rates[1], todays_rates[i])
  }

  #Find principal components for the data: Which data should prinicpal components be used on? 
  spot_rate_history_data_frame = data.frame(matrix(unlist(spot_rate_history), nrow = length(spot_rate_history[[1]]), byrow = T))
  #prin = princomp(spot_rate_history_data_frame, cor = FALSE)
  
  volatility_structure = get_PC_loadings(spot_rate_history_data_frame, number_of_factors)
  vol_struc_mat = matrix(volatility_structure, nrow = M)
  
  
  #discount_factors = rep(0, length(time_intervals)) #Make sure that this one fits with the actual times of simulation (if not one day). 
  forward_curve = in_forward_curve
  
  #Generate random numbers Z.
  Z_vec = rnorm(number_of_factors*length(time_intervals), mean = 0, sd = 1)
  Z_mat = matrix(Z_vec, ncol = number_of_factors)
  
  realized_short_rate = rep(0, M); realized_short_rate[1] = forward_curve[1]; # The realized short rate 
  
  for (i in 1:(M-1))
  {
    #Find the drift of the forward curve: 
    n_rates_to_simulate = M-i
    elapsed_time = maturity_vector_in_years[i]
    
    temp_vol = vol_struc_mat[ i:(dim(vol_struc_mat)[1]), ]
    temp_maturities = maturity_vector_in_years - elapsed_time ; #Find the maturities at the time now.
    
    drift = new_HJM_drift(temp_vol, temp_maturities[i:length(temp_maturities)], number_of_factors, dt)
    
    for (j in 1:n_rates_to_simulate) 
    {
      #Working on the line below this one

      S = vol_struc_mat[j, ] %*% Z_mat[j, ] #S is a number when written this way. 
      
      #Might be better ways to make this one
      forward_curve[j] = forward_curve[j+1] + drift[j]*time_intervals[j] + S*sqrt(time_intervals[j])
    }
    realized_short_rate[i+1] = forward_curve[1]
  }
  
  
  #Intent to make a solution working for dt = 1 day:
  #====================================================
  #for (i in 1:length(maturity_vector_in_years))
  #{
  #  
  #  while (elapsed_time < maturity_bucket)
  #  {
  #    #Find intermediate forward curve: 
  #    #How will the many changes in the forward curves affect the rates in the end? 
  #    #Maybe I need to change the initial data to forward data to get the right forward rate volatility, instead of a spot rate volatility. 
  #    #Or may have to transform the volatilities accordingly? 
  #    intermediate_forward_curve = forward_curve
  #    
  #    
  #    discount_factors[i] = exp(-intermediate_forward_curve[1]*time)
  #    elapsed_time = elapsed_time + time_intervals[count_index]
  #    count_index = count_index + 1
  #    
  #  }
  #}
  #======================================================
  #for (i in 1:length(time_intervals))
  #{
  #  discount_factors[i] = exp(-forward_curve[1]*time_intervals[i])
  #  
  #  #Evaluate s_j(k)'s. As it is called s_j(k) they probably change with each j. How? 
  #  
  #  #Find drifts: 
  #  drifts = new_HJM_drift(vol_struc_mat, maturity_vector_in_years, number_of_factors, time_intervals[i] )
  #  #M-i doesn't work! Produces negative values in the loop
  #  for (i in 1:(M-i))
  #  {
  #    S = 0;
  #    if ( (M-i) == 1){ S = sum(vol_struc_mat[1:number_of_factors]*Z_mat[i,])}
  #    else { S = sum(vol_struc_mat[j, ]*Z_mat[i,])}
  #    forward_curve[j] = forward_curve[j+1] + drifts[j]*time_intervals[i] + S*sqrt(time_intervals[i])
      
    #}
    #Glasserman algorithm:
    #for (j in 1: M-i)
    #   S = 0
    #   for (k in 1:d)
    #       S = S + s_j(k)*Z(k)
    #       f[j] = f(j+1) + m[j]*h[i] + S*sqrt()
    
  #}
  
  #Stochastic diff eq for forward rates: 
  # df_i = drift_i*dt + transpose(volatility)*dW
  
  #Transform discount factors to realized spot rates. 
  #rates = ((1/discount_factors)-1)/dt
  #=======================================================
  return(realized_short_rate)
}




#The new drift function (does it work?)
new_HJM_drift = function(s_df, maturities, d, time_increment)
  #The function assumes a constant time increment for the whole simulation.  
{
  s_df = as.matrix(s_df)
  M = length(maturities) 
  
  if (maturities[1] == 0)
  {intervals = diff(maturities)}
  else
  {intervals = diff(c(0, maturities)); M = M+1}
  if (dim(s_df)[2] != d)
  {
    s_df = t(s_df)
  }
  n_times = dim(s_df)[1]
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
      A_next[k] = A_prev[k] + s_df[j,k]*time_increment
      
      B_next = B_next + A_next[k]*A_next[k]
      A_prev[k] = A_next[k]
    } #end for k 
    m[j] = (B_next - B_prev)/(2*time_increment) 
    B_prev = B_next
  } #end for j 
  return(m)
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

forward_transform= function(shortTime, longTime, shortTimeRate, longTimeRate)
  #Returns the forward rate in the interval [shortTime, longTime] when spot rates are given. 
{
  timediff = longTime-shortTime
  return((longTimeRate*longTime- shortTimeRate*shortTime)/timediff)
}

get_all_forward_rates = function(spot_rates, times)
{
  M = length(spot_rates)
  ind = 1
  end = M*(M-1)/2
  forward_curve = rep(0, end)
  for ( i in 1:(M-1))
  {
    for(j in (i+1):M)
    {
      forward_curve[ind] = forward_transform(times[i], times[j], spot_rates[i], spot_rates[j])
      ind = ind+1 
    }
  }
  return(forward_curve)
}

get_PC_loadings = function(data, nFactors)
{
  #Convert to matrix if a list is supplied.
  if (typeof(data) == "list") { data = data.frame(matrix(unlist(data), nrow = length(data[[1]]), byrow =  T))}
  
  PCA = princomp(data, cor= FALSE)
  n_variables_in_data = dim(data)[2]
  return(PCA$loadings[1:(nFactors*n_variables_in_data)])
}

#construct_new_data = function(data, maturities, new_maturities)
#{
#  all_maturities = sort(unique(c(maturities, new_maturities)))
#  n_new_rates = length(all_maturities) - length(maturities)
#  new_rates = list()
#  ind_list_for_new_rates = c()
#  
#  for (i in 1:length(all_maturities))
#  {
#    if (all_maturities[i] %in% maturities)
#    {
#      next; #Same as continue in other languages
#    }
#    else
#    {
#      ind = which(maturities == all_maturities[i-1], arr.ind = TRUE)
#      new_rates = c(new_rates, interpolated_rate_linear(data[[ind]], data[[ ind+1]], maturities[ind], maturities[ind+1], all_maturities[i]) )
#      #Insert the new rates into the old data, and return. 
#      ind_list_for_new_rates = c(ind_list_for_new_rates, i)
#    }
#  }
#  return_rates = list()
#  data_ind = 1
#  new_ind = 1
#  for (i in 1:length(all_maturities) )
#  {
#    if(i %in% ind_list_for_new_rates)
#    {
#      return_rates = c(return_rates, new_rates[new_ind])
#      new = ind = new_ind +1
#    }
#    else{
#      return_rates = c(return_rates, data[data_ind])
#      data_ind = data_ind+1
#    }
#    
#  }
#  return(return_rates)
#}

construct_new_data = function(data, maturities, new_maturities)
#Assume new_maturities and maturities disjoint
{

  all_maturities = sort(unique(c(maturities, new_maturities)))
  n_new_rates = length(all_maturities) - length(maturities)
  new_rates = list()
  ind_list_for_new_rates = c()
  
  for (i in 1:n_new_rates)
  {
    #Find the closest existing maturities to new_maturities[i]
    temp_var = maturities - new_maturities[i]
    ind_above = min(which(temp_var > 0, arr.ind = TRUE))
    ind_below = ind_above - 1 
    new_rates = c(new_rates, interpolated_rate_linear(data[[ind_below]], data[[ind_above]], maturities[ind_below], 
                                                      maturities[ind_above], new_maturities[i]))
    ind_list_for_new_rates = c(ind_list_for_new_rates, which(all_maturities == new_maturities[i], arr.ind = TRUE))
  }
  return_rates = list()
  data_ind = 1
  new_ind = 1
  for (i in 1:length(all_maturities) )
  {
    if(i %in% ind_list_for_new_rates)
    {
      return_rates = c(return_rates, new_rates[new_ind])
      new_ind = new_ind +1
    }
    else{
      return_rates = c(return_rates, data[data_ind])
      data_ind = data_ind+1
    }
    
  }
  return(return_rates)
}
