# define function for calculating that probability is correct
agent_prob_adjust <- function(prob_agent, prob_error) {
  
  # adjust probability floor and ceiling
  prob_agent <- ifelse(prob_agent > 1, 1, prob_agent)
  prob_agent <- ifelse(prob_agent < 0, 0, prob_agent)
  
  # add error to probability
  prob_agent <- ifelse(prob_agent > 0.95, prob_agent - prob_error, prob_agent)
  prob_agent <- ifelse(prob_agent < 0.05, prob_agent + prob_error, prob_agent)
  
  return(prob_agent)
}