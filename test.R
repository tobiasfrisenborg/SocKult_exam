sigmoid <- function(x) {
  1 / (1 + exp(-x)) }

x <- scale(agent_attr$knowledge)







confidence_function <- function(x) {
  (2 * exp(x)) - 2 * exp(-x)) / (3 * exp(x)) + 3 * exp(-x))
}


ggplot(as.data.frame(x), aes(x = x, y = confidence_function(x))) +
  geom_point() +
  labs(x = "knowledge_scaled",
       y = "confidence_level") 
