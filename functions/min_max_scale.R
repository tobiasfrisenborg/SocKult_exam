# define function for min-max scaling
min_max_scale <- function(x){
  return((x-min(x)) / (max(x)-  min(x)))
}