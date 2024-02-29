# Geometric Distribution
# Set the probability of success
p <- 0.2

# Generate 1000 random variables from the geometric distribution
x <- rgeom(1000, p)

# Calculate basic statistics
mean_x <- mean(x)
var_x <- var(x)
sd_x <- sd(x)

# Print the results
cat("Number of trials required to achieve first success:\n")
cat("Mean (in 2 decimal places): ", sprintf("%.2f", mean_x), "\n")
cat("Variance (in 2 decimal places): ", sprintf("%.2f", var_x), "\n")
cat("Standard deviation (in 2 decimal places): ", sprintf("%.2f", sd_x), "\n")

# Plot histogram
hist(x, main = "Histogram of Geometric Distribution", xlab = "Number of Trials", ylab = "Frequency", col = "lightblue")

# Hypergeometric Distribution
# Probability that the sample contains more than 10% defectives
# A sample of 10 is selected from a box of 40
prob_more_than_10_percent_1 <- 1 - phyper(0.1 * 10 - 1, 40 * 0.1, 40 * (1 - 0.1), 10)

# A sample of 10 is selected from a box of 5000
prob_more_than_10_percent_2 <- 1 - phyper(0.1 * 10 - 1, 5000 * 0.1, 5000 * (1 - 0.1), 10)

cat("Probability that the sample contains more than 10% defectives:\n")
cat("For a sample of 10 from a box of 40: ", sprintf("%.4f", prob_more_than_10_percent_1), "\n")
cat("For a sample of 10 from a box of 5000: ", sprintf("%.4f", prob_more_than_10_percent_2), "\n")