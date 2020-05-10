# define function for creating agents
create_agents <- function(n_groups, group_size, conf_influence_factor, seed) {
  
  # setup number parameters for calculations
  n_agents              <- group_size * n_groups
  
  # create data frame
  agents <- as.data.frame(seq(from = 1, to = n_agents))
  colnames(agents)[1] <- "agent_id"
  
  agents$group_id   <- sort(rep(1:n_groups, group_size))  # define agents groups
  agents$group_size <- group_size  # define group size column
  
  # defining knowledge distribution
  set.seed(seed)
  agents$knowledge <- round(rnorm(length(agents$agent_id), mean=100, sd=15))
  
  # scale knowledge for use in future calculations
  agents$knowledge_scaled  <- min_max_scale(agents$knowledge)
  agents$knowledge_z_score <- scale(agents$knowledge)
  
  # creating the two distributions for over- and underconfidence levels
  set.seed(seed)
  above_average <- rnorm(length(agents$agent_id), mean =(-0.2), sd = 0.2)
  set.seed(seed)
  below_average <- rnorm(length(agents$agent_id), mean =  0.4 , sd = 0.3)
  
  # loop through all agents, calculating confidence depending on proximity to the mean knowledge
  # sample from distribution depending on above or below average knowledge
  for (i in 1:length(agents$agent_id)) {
    
    if (agents$knowledge[i] >= 100) {
      set.seed(seed + i)
      agents$confidence_value[i] <- sample(above_average, 1)
      agents$confidence_level[i] <- 1 + (agents$confidence_value[i] * abs(agents$knowledge_z_score[i]) * conf_influence_factor)
      # error handling as the simulation doesn't work with negative confidence values
      if (agents$confidence_level[i] <= 0.001) {
        agents$confidence_level[i] <- 0.001 }
      agents$confidence[i]       <- agents$knowledge[i] * agents$confidence_level[i] }
    
    else if (agents$knowledge[i] < 100) {
      set.seed(seed + i)
      agents$confidence_value[i] <- sample(below_average, 1)[1]
      agents$confidence_level[i] <- 1 + (agents$confidence_value[i] * abs(agents$knowledge_z_score[i]) * conf_influence_factor)
      # error handling
      if (agents$confidence_level[i] <= 0.001) {
        agents$confidence_level[i] <- 0.001 }
      agents$confidence[i]       <- agents$knowledge[i] * agents$confidence_level[i] }
    
    
  }
  
  
  return(agents)
}