# backup
# create_agents confidence creation
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






