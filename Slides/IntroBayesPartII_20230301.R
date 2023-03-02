# Setup ####
rm(list = ls())

## Libraries ####
library(dplyr)

# Sample ####

set.seed(202303)
n <- 100
age_groups <- c("0-14", "15-64", "65+")
data <- data.frame(ID = 1:n,
                   Age = age_groups[sample(1:3, size = 100,
                                           replace = TRUE,
                                           prob = c(0.182, 0.65,
                                                    0.168))])

## Beta posterior quantiles ####
qbeta(c(0.025, 0.975), shape1 = 17, shape2 = 74)

## Beta posterior samples ####
post_prob <- rbeta(n = 1000, shape1 = 17, shape2 = 74)
hist(post_prob, main = "", xlab = "Proportion Above 15 who are 65+",
     border = FALSE, col = "navy", freq = FALSE)


## Exceedance probabilities ####
sum(post_prob > 15/87)/length(post_prob)
sum(post_prob > 0.25)/length(post_prob)


## Calculting the support ratio ####
support_ratio <- (1 - post_prob)/post_prob
c(mean(support_ratio), sd(support_ratio))
quantile(support_ratio, probs = c(0.025, 0.975))


## Posterior predictive interval ####
post_pred <- rbinom(1000, size = 87, prob = post_prob)
c(mean(post_pred), sd(post_pred))
quantile(post_pred, probs = c(0.025, 0.975))


## Grid approximation ####
p_grid <- seq(0.001, 0.999, .001)
prior_eval <- dbeta(p_grid, shape1 = 2, shape2 = 2)
likelihood_eval <- dbinom(15, size = 87, prob = p_grid)
marg_calc <- sum(likelihood_eval*prior_eval)
post_eval <- (1/marg_calc)*likelihood_eval*prior_eval


## Plot ####
y_lims <- round(c(min(c(prior_eval/10, likelihood_eval, post_eval)),
                  max(c(prior_eval/10, likelihood_eval, post_eval))),
                digits = 3)

par(lend = 1)
plot(p_grid, prior_eval/10,
     xlab = "p", ylab = "Density",
     xlim = c(0,1), ylim = y_lims,
     lwd = 3, type = "l", col = "grey75")
lines(p_grid, likelihood_eval,
      lwd = 3, col = "navy")
lines(p_grid, post_eval,
      lwd = 3, col = "firebrick")
legend("topright", bty = "n", lwd = 3, cex = .5,
       col = c("grey75", "navy", "firebrick"),
       legend = c("Prior", "Likelihood", "Posterior"))



## Posterior mode ####
max_val_idx <- which.max(post_eval)
p_grid[max_val_idx]


## Posterior samples ####
post_samp_grid <- sample(p_grid, size = 1000, replace = TRUE,
                         prob = post_eval)
quantile(post_samp_grid, c(0.025, .5, 0.975))

