# define plotting function
plot_results <- function(baseline, reputation, confirmation) {
  
  baseline_summary <- baseline %>% 
    group_by(trial_n) %>%
    summarize(performance = mean(group_answer), difficulty = mean(difficulty))
  baseline_summary[,"cum_performance"] <- cumsum(baseline_summary$performance)
  
  reputation_summary <- reputation %>% 
    group_by(trial_n) %>%
    summarize(performance = mean(group_answer), difficulty = mean(difficulty))
  reputation_summary[,"cum_performance"] <- cumsum(reputation_summary$performance)
  
  confirmation_summary <- confirmation %>% 
    group_by(trial_n) %>%
    summarize(performance = mean(group_answer), difficulty = mean(difficulty))
  confirmation_summary[,"cum_performance"] <- cumsum(confirmation_summary$performance)
  
  
  baseline_summary$condition <- "baseline"
  reputation_summary$condition <- "reputation"
  confirmation_summary$condition <- "confirmation"
  
  summary <- rbind(baseline_summary, reputation_summary, confirmation_summary)
  
  difficulty_plot <- ggplot(summary, aes(x = difficulty, y = performance, color = condition)) +
    geom_smooth() + 
    labs(title = 'Mean Performance over difficulty')
  
  cumulative_trial_plot <- ggplot(summary, aes(x = trial_n, y = cum_performance, color = condition)) +
    geom_smooth() + 
    labs(title = 'Cumulative Performance over trials')
  
  
  #return(difficulty_plot)
  #return(cumulative_trial_plot)
  return(list(difficulty_plot, cumulative_trial_plot))
  
}