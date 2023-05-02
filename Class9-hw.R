# In cognitive studies on number, a common model is that people represent a number N with a distribution Normal(N,W*N), where W is a parameter (“weber fraction”) that describes how accurate their representations are. Note that whatever W is, the standard deviation of people’s representation still scales linearly with N: W is like the slope saying how it changes. 
# 
# Assignment7.csv has data from number discrimination experiments with the Tsimane’, an indigenous Bolivian group that I work with. Each row has some demographics, as well as two columns, n1 and n2, which were numbers presented to people as dot patterns, and their accuracy at selecting which one has more. 
# 
# 1. First, make a plot of the accuracies to visualize the pattern of accuracy as the numbers vary.

library(dplyr)
library(rstan)
library(ggplot2)

d <- read.csv("Assignment7.csv")
d <- subset(d, !is.na(correct))

plt <- ggplot(d, aes(x=n1, y=correct)) +
  stat_summary(fun.data=mean_se, geom="errorbar") + 
  theme_bw()
plt

# 
# Modeling requires a tiny bit of math to start. If people represent n1 as Normal(n1,W*n1) and n2 as Normal(n2,W*n2), then the representation of their difference is Normal(n1-n2, sqrt((W*n1)^2+(W*n2)^2)). Suppose that people’s probability of choosing n1 is the probability that this difference distribution is positive (which you can get from the cdf of that normal, in stan).
# 
# 2. Write a stan model to infer W from the accuracy data. For now, we’ll assume that everyone has the same W. Plot the posterior distribution on W from this data.

stan.data <- list(N=nrow(d)
                  ,n1=d$n1
                  ,n2=d$n2
                  ,correct=d$correct)
fit <- stan(file="Class9-hw.stan", data=stan.data, iter=1000, chains=2, cores=2)


# 3. At a typical W value, what number discrimination (e.g. what n1 and n2) would people be 75% accurate? Is there more than one or just one?
# 
# 4. Fit W separately for subjects with each education level. Plot the results (means and posterior 50% quantiles) all together in one figure – what patterns do you see and why?

stan.data <- list(N=nrow(d)
                  ,EDU_MAX=max(d$education)
                  ,n1=d$n1
                  ,n2=d$n2
                  ,correct=d$correct
                  ,education=d$education)
fit <- stan(file="Class9-hw-b.stan", data=stan.data, iter=1000, chains=2, cores=2)

