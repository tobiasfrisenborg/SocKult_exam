# define group_decision function
group_decision <- function(a1_answer, a2_answer, a3_answer, a1_weight, a2_weight, a3_weight, group_strategy, seed) {
  
  # set seed for reproducibility
  set.seed(seed)
  
  answer_vec <- c(a1_answer, a2_answer, a3_answer)
  prob_vec   <- c(a1_weight, a2_weight, a3_weight)
  
  # sample answer from groups possible answers
  if (group_strategy == 'sample_weighted') {
    answer <- sample(answer_vec,
                     prob = prob_vec,
                     size = 1)
  } else if (group_strategy == 'highest_weight') {
    
    # select the answer of highest weighted group member
    answer <- answer_vec[which.max(prob_vec)] }
  
  return(answer)
}