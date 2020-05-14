library(tictoc)

# define function for running simulation
run_simulation <- function(data, condition, group_strategy, n_trials, difficulty_mu, difficulty_sd, prob_error, seed, confirmation_weight = NA) {
  
  tic(paste("simulation")) # timekeeping
  message(paste(condition, group_strategy))
  
  
  ### SIMULATION PARAMETERS ###
  set.seed(seed)
  #difficulty          <- rnorm(n_trials, mean = difficulty_mu, sd=difficulty_sd)
  #difficulty          <- sort(difficulty)
  difficulty          <- seq(difficulty_mu - difficulty_sd, difficulty_mu + difficulty_sd, (difficulty_sd * 2) / n_trials)
  difficulty          <- sample(difficulty)
  difficulty_z        <- scale(difficulty)
  
  
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
  
  
  
  ### RUNNING THE FOR EACH TRIAL ###
  for (trial in 1:n_trials) {
    
    ### SETTING UP TRIAL ###
    #save conditional rows for trials in loop
    condition_trial      <- (sim_results$trial_n == trial)
    condition_prev_trial <- (sim_results$trial_n == (trial - 1))
    
    # save difficulty
    sim_results$difficulty[condition_trial] <- difficulty[trial]
    
    # update agents correct response at the start of the trial
    if (trial == 1) {
      sim_results$agent_correct_sum[condition_trial] <- 1 }
    else {  # add previous agent_answer to the agent_correct_sum column
      sim_results$agent_correct_sum[condition_trial] <-
        sim_results$agent_correct_sum[condition_prev_trial] + sim_results$agent_answer[condition_prev_trial] }
    
    # calculate each agents reputation
    sim_results$agent_reputation[condition_trial]  <- sim_results$agent_correct_sum[condition_trial] / trial
    
    
    ### AGENT DECISION & CONFIDENCE###
    set.seed(seed + trial)
    low_difficulty  <- rnorm(length(data$agent_id), mean = 0.8, sd = 0.1)
    set.seed(seed + trial)
    high_difficulty <- rnorm(length(data$agent_id), mean = 1.8, sd = 0.4)
    
    # calculate the agents confidence
    for (agent in 1:length(unique(data$agent_id))) {
      
      # setup row condition
      condition_agent <- (sim_results$agent_id == agent)
      row_condition   <- (condition_trial & condition_agent)
      
      
      ### AGENT PROBABILITY ###
      sim_results$prob_correct_unadjusted[row_condition] <- sim_results$knowledge_scaled[row_condition] * difficulty[trial]
      sim_results$prob_correct[row_condition] <- agent_prob_adjust(sim_results$prob_correct_unadjusted[row_condition],
                                                                   prob_error = prob_error)
      
      
      ### AGENT CONFIDENCE CALCULATION ###
      if (sim_results$prob_correct[row_condition] >= 0.5) {
        set.seed(seed + agent)
        sim_results$confidence_factor[row_condition] <- 0.8 # sample(low_difficulty, 1)
        
      } else if (sim_results$prob_correct[row_condition] < 0.5) {
        set.seed(seed + agent)
        sim_results$confidence_factor[row_condition] <- 1.8 } # sample(high_difficulty, 1) }
      
      # calculate confidence before adjust error probability
      sim_results$confidence_unadjusted[row_condition] <-
        sim_results$prob_correct[row_condition] * sim_results$confidence_factor[row_condition]

      # adjust for floor, ceiling and error probabilities
      sim_results$confidence[row_condition] <- agent_prob_adjust(sim_results$confidence_unadjusted[row_condition],
                                                                 prob_error = prob_error)
      
      ### AGENT DECISION ###
      sim_results$agent_answer[row_condition] <- agent_decision(sim_results$prob_correct[row_condition],
                                                                seed = (seed + (agent * trial)) )
    }  # agent loop end
  }  # trial loop end
  
  
  
  ### CONDITIONS ###
  # baseline condition
  if (condition == 'baseline') {
    
    # calculate the sum of confidence
    sim_results <- sim_results %>%
      group_by(group_id, trial_n) %>%
      mutate(weight_sum = sum(prob_correct))
    
    # calculate the individual agents answer weight
    sim_results$answer_weight <- sim_results$prob_correct / sim_results$weight_sum
    
    # reputation- and confirmation conditions
  } else if (condition == 'equality') {
    
    # calculate the sum of confidence
    sim_results <- sim_results %>%
      group_by(group_id, trial_n) %>%
      mutate(confidence_sum = sum(confidence))
    
    # calculate the individual agents answer weight
    sim_results$answer_weight <- sim_results$confidence / sim_results$confidence_sum
    
    # reputation- and confirmation conditions
  } else if (condition == 'reputation') {
    
    sim_results <- sim_results %>%
      group_by(group_id, trial_n) %>%
      mutate(weight_sum = sum(confidence * agent_reputation))  # weight of confidence and reputation on group level
    
    sim_results$answer_weight <- (sim_results$confidence * sim_results$agent_reputation) / sim_results$weight_sum  # individual agent weight
    
  } else if (condition == 'confirmation') {
    
    sim_results <- sim_results %>%
      group_by(group_id, trial_n) %>%
      mutate(group_answer_sum = sum(agent_answer))
    
    # condition to select if two agents in group answered 0
    condition_0_agree <- ((sim_results$group_answer_sum == 1) & (sim_results$agent_answer == 0))
    
    # condition to select if two agents in group answered 1
    condition_1_agree <- ((sim_results$group_answer_sum == 2) & (sim_results$agent_answer == 1))
    
    sim_results$confirmation_bias <- 1
    sim_results$confirmation_bias[condition_0_agree | condition_1_agree] <- confirmation_weight
    
    sim_results <- sim_results %>%
      group_by(group_id, trial_n) %>%
      mutate(weight_sum = sum(confidence * confirmation_bias * agent_reputation))  # weight of confidence and reputation on group level
    
    sim_results$answer_weight <- (sim_results$confidence * sim_results$confirmation_bias * sim_results$agent_reputation) / sim_results$weight_sum  # individual agent weight
  }
  
  
  
  ### GROUP DECISION ###
  sim_results <- sim_results %>% 
    group_by(group_id, trial_n) %>% 
    mutate(group_answer = group_decision(a1_answer      = agent_answer[1],
                                         a2_answer      = agent_answer[2],
                                         a3_answer      = agent_answer[3],
                                         a1_weight      = answer_weight[1],
                                         a2_weight      = answer_weight[2],
                                         a3_weight      = answer_weight[3],
                                         group_strategy = group_strategy,
                                         seed           = (seed + trial_n[1])))
  
  # assign condition
  sim_results$condition <- condition
  
  
  ### PRINT RESULTS ###
  toc()  # timekeeping
  
  message(paste('  AVG Difficulty:', mean(sim_results$difficulty) ))
  
  group_results <- sim_results %>%
    group_by(group_id) %>%
    mutate(correct_sum = sum(group_answer))
  
  message(paste('  AVG Correct:   ', mean(group_results$correct_sum / 3), 'out of', n_trials, '\n',
                ' (%) Correct:   ', sum((mean(group_results$correct_sum / 3) / n_trials) * 100), '\n'))
  
  
  return(sim_results)
  
}