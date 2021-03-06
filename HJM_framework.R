#Tidy implementation of HJM

source('utility_functions.R')

#Framework that plots the realized rate: 



HJM_rate_simulation = function(maturity_vector_in_years, final_simulation_date, spot_rate_history, todays_rates)
{
  #Parameters for the model:
  dt = 1/252; #The time every rate will be predicted ahead for each simulation. 
  number_of_simulations = 100
  number_of_factors = 1 #Number of principal components to be extracted.
  
  
  
  #Calculated quantities obtained from arguments and/or parameters: 
  M = length(maturity_vector_in_years)
  
  #Vectors for simulation times and intervals
  simulation_times = seq(0, final_simulation_date, by = dt)
  time_intervals = diff(simulation_times)
  
  #Calculating the initial forward curve:
  #(Too many here? Think there should be only M forward rates. Which? f(0,t), t in 1...M)
  #
  number_of_forward_rates = length(maturity_vector_in_years)*(length(maturity_vector_in_years)-1)/2
  in_forward_curve= rep(0, number_of_forward_rates)
  ind = 1; 
  for (i in 1:(M-1))
  {
    for(j in (i+1):M)
    {
      in_forward_curve[ind] = forward_transform(maturity_vector_in_years[i], maturity_vector_in_years[j], todays_rates[i], todays_rates[j])
      ind = ind+1 
    }
  }
  
  #Find principal components for the data: Which data should prinicpal components be used on? 
  spot_rate_history_data_frame = data.frame(matrix(unlist(spot_rate_history), nrow = length(spot_rate_history[[1]]), byrow = T))
  #prin = princomp(spot_rate_history_data_frame, cor = FALSE)
  
  volatility_structure = get_PC_loadings(spot_rate_history_data_frame, number_of_factors)
  vol_struc_mat = matrix(volatility_structure, nrow = M)
  
  discount_factors = rep(0, length(time_intervals))
  forward_curve = in_forward_curve
  
  #Generate random numbers Z.
  Z_vec = rnorm(number_of_factors*length(time_intervals), mean = 0, sd = 1)
  Z_mat = matrix(Z_vec, ncol = number_of_factors)
  
  #Organize the interest rates by maturity buckets
  maturity_bucket = maturity_vector_in_years[1]
  elapsed_time = time_intervals[1];
  count_index = 1; 
  
  #Work in progress here: 
  #====================================================
  for (i in 1:length(maturity_vector_in_years))
  {
    
    while (elapsed_time < maturity_bucket)
    {
      #Find intermediate forward curve: 
      #How will the many changes in the forward curves affect the rates in the end? 
      #Maybe I need to change the initial data to forward data to get the right forward rate volatility, instead of a spot rate volatility. 
      #Or may have to transform the volatilities accordingly? 
      intermediate_forward_curve = forward_curve
      
      
      discount_factors[i] = exp(-intermediate_forward_curve[1]*time)
      elapsed_time = elapsed_time + time_intervals[count_index]
      count_index = count_index + 1
      
    }
  }
  #======================================================
  

  
    for (i in 1:length(time_intervals))
  {
    discount_factors[i] = exp(-forward_curve[1]*time_intervals[i])
    
    #Evaluate s_j(k)'s. As it is called s_j(k) they probably change with each j. How? 
    
    #Find drifts: 
    drifts = new_HJM_drift(vol_struc_mat, maturity_vector_in_years, number_of_factors, time_intervals[i] )
    #M-i doesn't work! Produces negative values in the loop
    for (i in 1:(M-i))
    {
      S = 0;
      if ( (M-i) == 1){ S = sum(vol_struc_mat[1:number_of_factors]*Z_mat[i,])}
      else { S = sum(vol_struc_mat[j, ]*Z_mat[i,])}
      forward_curve[j] = forward_curve[j+1] + drifts[j]*time_intervals[i] + S*sqrt(time_intervals[i])
      
    }
    #Glasserman algorithm:
    #for (j in 1: M-i)
    #   S = 0
    #   for (k in 1:d)
    #       S = S + s_j(k)*Z(k)
    #       f[j] = f(j+1) + m[j]*h[i] + S*sqrt()
    
  }
  
  #Stochastic diff eq for forward rates: 
  # df_i = drift_i*dt + transpose(volatility)*dW
  
  #Transform discount factors to realized spot rates. 
  rates = ((1/discount_factors)-1)/dt
  return(rates)
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
construct_new_data = function(data, maturities, new_maturities)
{
  all_maturities = sort(unique(c(maturities, new_maturities)))
  n_new_rates = length(all_maturities) - length(maturities)
  
  
  for (i in 1:length(all_maturities))
  {
    if (all_maturities[i] %in% maturities)
    {
      next;
    }
    else
    {
      #Find the elements on each side of all_maturities[i] in maturities
      #Make new data based on this
    }
  
  }
}
