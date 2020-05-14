# define function for creating agents
create_agents <- function(n_groups, group_size, conf_influence_factor, seed) {
  
  # setup number parameters for calculations
  n_agents                 <- group_size * n_groups
  
  # create data frame
  agents                   <- as.data.frame(seq(from = 1, to = n_agents))
  colnames(agents)[1]      <- "agent_id"
  
  agents$group_id          <- sort(rep(1:n_groups, group_size))  # define agents groups
  
  # defining knowledge distribution
  set.seed(seed)
  agents$knowledge         <- round(rnorm(length(agents$agent_id), mean=100, sd=15))
  
  # scale knowledge for use in future calculations
  agents$knowledge_z       <- scale(agents$knowledge)
  agents$knowledge_scaled  <- (agents$knowledge_z * 0.125) + 0.5

  return(agents)
}