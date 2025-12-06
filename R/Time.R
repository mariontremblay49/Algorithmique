one.simu_time_Ranking <- function(k, data, max_par_groupe, algo = "Naif")
{
  if(algo== "Naif")
  {
    start_time <- Sys.time()
    ranking_naif_max(k, data, max_par_groupe)
    end_time <- Sys.time()
  }
  if(algo== "dp")
  {
    start_time <- Sys.time()
    ranking_max(k, data, max_par_groupe)
    end_time <- Sys.time()
  }
  if(algo== "dp_cpp")
  {
    start_time <- Sys.time()
    ranking_max_cpp(k, data, max_par_groupe)
    end_time <- Sys.time()
  }
  if(algo== "dp_tas_cpp")
  {
    start_time <- Sys.time()
    ranking_max_dp_heap_cpp(k, data, max_par_groupe)
    end_time <- Sys.time()
  }
  return(unclass(end_time- start_time)[1])
}



data_summary <- function(data, varname, groupnames)
{
  require(plyr)
  summary_func <- function(x, col)
  {
    c(mean = mean(x[[col]], na.rm=TRUE),
      q1 = quantile(x[[col]], 0.025), q3 = quantile(x[[col]], 0.975))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}
