# define plotting function
plot_results <- function(baseline, equality, reputation, confirmation) {
  
  # bind data together
  data <- rbind(sim_baseline, sim_equality, sim_reputation, sim_confirmation)
  
  ### CALCULATIONS ###
  # calculate overall group performance
  data <- data %>% 
    group_by(agent_id, condition) %>% 
    mutate(group_performance = sum(group_answer),
           cum_performance = cumsum(group_answer))
  
  data <- data %>% 
    group_by(group_id, condition) %>% 
    mutate(knowledge_sd = sd(knowledge),
           knowledge_iqr = IQR(knowledge))
  
  # calculate performance averaged by trial
  data_summary <- data %>%
    group_by(trial_n, difficulty, condition) %>% 
    summarize(mean_group_answer = mean(group_answer))
  
  # make conditions pretty
  data$condition <- tools::toTitleCase(data$condition)
  data$condition <- as.factor(data$condition)
  data$condition <- ordered(data$condition, levels = c("Baseline",
                                                       "Equality",
                                                       "Reputation",
                                                       "Confirmation"))
  
  
  
  
  ### PLOTS ###
  # plot performance averaged by trial and condition
  difficulty_plot <- ggplot(data_summary, aes(x = difficulty, y = mean_group_answer, color = condition)) +
    geom_smooth(se = FALSE) + 
    theme_minimal() +
    scale_color_manual(values=c("#eb5146", "#3b44ed", "#981dd1", "#23de8d")) +
    labs(title = 'Mean Performance over difficulty',
         x = 'Trial Difficulty',
         y = 'Mean Group Performance')
  
  # plot cumulative group performance by condition
  cumulative_trial_plot <- ggplot(data, aes(x = trial_n, y = cum_performance, color = condition)) +
    geom_smooth(method = 'loess', se = FALSE) + 
    theme_minimal() +
    scale_color_manual(values=c("#eb5146", "#3b44ed", "#981dd1", "#23de8d")) +
    labs(title = 'Cumulative Performance over Trials',
         x = 'Number of Trials',
         y = 'Mean Group Performance')
  
  # plot the effect of knowledge spread over group performance
  knowledge_spread_plot <- ggplot(data, aes(x = knowledge_sd, y = group_performance, color = condition)) +
    geom_smooth(method = 'loess', se = FALSE, size = 1.3) + 
    scale_color_manual(values=c("#eb5146", "#3b44ed", "#981dd1", "#23de8d")) +
    theme_minimal() +
    labs(title = 'Group Performance over Spread of Knowledge (SD)',
         x = "Group Knowledge SD",
         y = "Mean Group Performance")
  
  # plot ridgeline
  ridgeline_performance_plot <- ggplot(data, aes(x = group_performance, y = condition, fill = condition)) +
    geom_density_ridges(rel_min_height = 0.005, scale = 1.5, alpha=.95) +
    scale_fill_cyclical(values = c("#eb5146", "#3b44ed", "#981dd1", "#23de8d")) +
    theme_ridges() +
    theme(text = element_text(size = 10), legend.position = "none") +
    labs(title = "Group Performance Ridgeline Plot",
         x = "Mean Group Performance",
         y = "")
  
  
  return(list(difficulty_plot,
              cumulative_trial_plot,
              knowledge_spread_plot,
              ridgeline_performance_plot))
  
}