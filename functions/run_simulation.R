library(tictoc)

# define function for running simulation
run_simulation <- function(data, condition, n_trials, difficulty_mu, difficulty_sd, prob_error) {
  
  ### SIMULATION PARAMETERS ###
  tic("Simulation")
  difficulty <- rnorm(n_trials, mean = difficulty_mu, sd=difficulty_sd)
  confirmation_weight <- 2
  
  
  
  ### STORING RESULTS ###
  column_names = colnames(data)
  sim_results = as.data.frame(matrix(numeric(),
                                     nrow = 0,
                                     ncol = length(column_names)))
  colnames(sim_results) = column_names
  
  # loop through all agents
  for (agent in 1:length(data$agent_id)) {
    # loop through number of trials, duplicating the current row for the amount of trials
    for (i in 1:n_trials) {
      rows_written <- (agent - 1) * n_trials  # to decide where this loop will create rows, we calculate the number of rows already written
      sim_results[rows_written + i, ] <- data[agent, ]  # write the row
    } }
  
  # assign trial number to a new column
  sim_results$trial_n <- rep(seq(1, n_trials), length(agent_attr$agent_id))
  
  # creating columns for storing data
  sim_results$difficulty        <- NA
  sim_results$agent_correct_sum <- 1  # sum of correct answers for individual, used for calculating reputation
  sim_results$prob_correct      <- NA
  sim_results$agent_answer      <- NA
  
  
  
  ### RUNNING THE FOR EACH AGENT ###
  for (trial in 1:n_trials) {
    
    # save conditional rows for trials in loop
    condition_trial      <- (sim_results$trial_n == trial)
    condition_prev_trial <- (sim_results$trial_n == (trial - 1))
    
    # save difficulty
    sim_results$difficulty[condition_trial] <- difficulty[trial]
    
    # update agents correct response at the start of the trial
    if (trial == 1) {
      sim_results$agent_correct_sum[condition_trial] <- 1
      
    } else {
      sim_results$agent_correct_sum[condition_trial] <- sim_results$agent_correct_sum[condition_prev_trial] + sim_results$agent_answer[condition_prev_trial]
    }
    
    # calculate each agents reputation
    sim_results$agent_reputation[condition_trial]  <- sim_results$agent_correct_sum[condition_trial] / trial
    
    # calculate agent probability of getting a correct response
    sim_results$prob_correct[condition_trial] <- agent_prob_adjust(sim_results$knowledge_scaled[condition_trial] * difficulty[trial],
                                                                   prob_error = prob_error)
    
    # determine agent answer
    for (agent in 1:length(unique(data$agent_id))) {
      
      # save conditional rows for agent in loop
      condition_agent <- (sim_results$agent_id == agent)
      
      sim_results$agent_answer[condition_trial & condition_agent] <- agent_decision(sim_results$prob_correct[condition_trial & condition_agent])
    }
    
    
    # conditional for confirmation condition
    if (condition == 'confirmation') {
      
      sim_results <- sim_results %>%
        group_by(group_id, trial_n) %>%
        mutate(group_answer_sum = sum(agent_answer))
      
      # condition to select if two agents in group answered 0
      condition_0_agree <- ((sim_results$group_answer_sum == 1) & (sim_results$agent_answer == 0))
      
      # condition to select if two agents in group answered 1
      condition_1_agree <- ((sim_results$group_answer_sum == 2) & (sim_results$agent_answer == 0))
      
      # 
      sim_results$agent_reputation[condition_trial & (condition_0_agree | condition_1_agree)] <- 
        sim_results$agent_reputation[condition_trial & (condition_0_agree | condition_1_agree)] * confirmation_weight
    }
    
  }
  
  ######################################## TODO : // Should the agent correct be updated no matter the group response or should it only count if group answers correctly?
  
  ### RUNNING THE SIMULATION ###
  # baseline: only confidence contributes to weight
  if (condition == 'baseline') {
    
    # calculate the sum of confidence
    sim_results <- sim_results %>%
      group_by(group_id, trial_n) %>%
      mutate(confidence_sum = sum(confidence))
    
    # calculate the individual agents answer weight
    sim_results$answer_weight <- sim_results$confidence / sim_results$confidence_sum
    
  } else if (condition == 'reputation' | condition == 'confirmation') {
    
    sim_results <- sim_results %>%
      group_by(group_id, trial_n) %>%
      mutate(weight_sum = sum(confidence * agent_reputation))  # weight of confidence and reputation on group level
    
    sim_results$answer_weight <- (sim_results$confidence * sim_results$agent_reputation) / sim_results$weight_sum  # individual agent weight
  }
  
  # select group decision
  sim_results <- sim_results %>% 
    group_by(group_id, trial_n) %>% 
    mutate(group_answer = group_decision(a1_answer = agent_answer[1],
                                         a2_answer = agent_answer[2],
                                         a3_answer = agent_answer[3],
                                         a1_weight = answer_weight[1],
                                         a2_weight = answer_weight[2],
                                         a3_weight = answer_weight[3] ))
  
  
  
  # print some quick results
  toc()
  
  message(paste('AVG Difficulty: ', mean(sim_results$difficulty)))
  
  group_results <- sim_results %>%
    group_by(group_id) %>%
    mutate(correct_sum = sum(group_answer))

  message(paste('AVG Correct:    ', mean((group_results$correct_sum / 3))))
  
  
  return(sim_results)
  
}