setwd("~/Dropbox/Workshops/IntroBayesPartII/Figures/")
library(scales)
set.seed(1234)
prior_p <- rbeta(1000, 2, 2)

pdf("Gamma22_Prior.pdf", height = 3, width = 3)
hist(prior_p, main = "", xlab = "p",
     border = FALSE, col = "navy", freq = FALSE)
dev.off()

set.seed(202303)
n <- 100
age_groups <- c("0-14", "15-64", "65+")
data <- data.frame(ID = 1:n,
                   Age = age_groups[sample(1:3, size = 100,
                                           replace = TRUE,
                                           prob = c(0.182, 0.65,
                                                    0.168))])
post_prob <- rbeta(n = 1000, shape1 = 17, shape2 = 74)
support_ratio <- (1 - post_prob)/post_prob

post_pred <- rbinom(1000, size = 87, prob = post_prob)

p_grid <- seq(0.001, 0.999, .001)
prior_eval <- dbeta(p_grid, shape1 = 2, shape2 = 2)
likelihood_eval <- dbinom(15, size = 87, prob = p_grid)
marg_calc <- sum(likelihood_eval*prior_eval)
post_eval <- (1/marg_calc)*likelihood_eval*prior_eval

pdf("Posterior_GridApprox_Mode.pdf", height = 3, width = 3)
par(mfrow = c(1,1), lend = 1)
plot(p_grid, post_eval,
     xlab = "p", ylab = "Density",
     xlim = c(0.1, 0.3), lwd = 3, type = "l",
     col = "firebrick")
abline(h = 0)
abline(v = p_grid[which.max(post_eval)], lty = 2, lwd = 2)
legend("topright", bty = "n",
       lwd = 2, lty = c(1,2), cex = 0.5,
       col = c("firebrick", "black"),
       legend = c("Posterior", "Posterior Mode"))
dev.off()
.

post_prob_grid <- sample(p_grid, size = 1000, replace = TRUE,
                         prob = post_eval)

true_post <- dbeta(p_grid, 17, 74)
pdf("Posterior_Compare.pdf", height = 3, width = 3)
par(mfrow = c(1,1), lend = 1)
hist(post_prob, freq = FALSE,
     main = "", xlab = "p",
     ylab = "Density",
     border = FALSE, col = alpha("navy", 0.4))
hist(post_prob_grid, freq = FALSE,
     main = "", xlab = "p",
     ylab = "Density", add = TRUE,
     border = FALSE, col = alpha("firebrick", 0.4))
lines(p_grid, true_post, lwd = 2)
legend("topright", bty = "n",
       border = FALSE, cex = 0.5,
       fill = alpha(c("navy", "firebrick"), 0.4),
       legend = c("Posterior", "Grid Approximation"))
dev.off()