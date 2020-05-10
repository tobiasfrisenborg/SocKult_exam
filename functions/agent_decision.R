# define function for sampling answers
agent_decision <- function(prob_correct) {
  
  # sample answer according to probability of correct answer
  answer <- sample(c(0, 1), 1, prob = c((1 - prob_correct), prob_correct))
  
  return(answer)
}