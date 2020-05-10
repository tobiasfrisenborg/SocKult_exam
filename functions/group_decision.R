# define group_decision function
group_decision <- function(a1_answer, a2_answer, a3_answer, a1_weight, a2_weight, a3_weight, seed) {
  
  # set seed for reproducibility
  set.seed(seed)
  
  # sample answer from groups possible answers
  answer <- sample(c(a1_answer, a2_answer, a3_answer),
                   prob = c(a1_weight, a2_weight, a3_weight),
                   size = 1)
  
  return(answer)
}