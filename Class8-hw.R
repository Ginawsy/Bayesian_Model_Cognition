# 1. This is real data. Load it, check it out (e.g. look for outliers etc), and
# visualize it in a way that shows what you are likely to find. You can use the
# fact that later items tend to be harder to provide a visual answer. What
# result do you expect to find?

d <- read.csv("Assignment6.csv")
d <- subset(d, rt < 2*60*1000)  #keep between 2s and 2m
d <- subset(d, rt > 2*1000)
hist(d$rt, breaks=100)


q <- subset(d, is.element(uniqueid, unique(d$uniqueid)[1:30]))
library(ggplot2)
# Let's look at item (kinda difficulty) vs RT
plt <- ggplot(q, aes(x=which_item, y=rt/1000, color=correct)) + 
      geom_point() + 
      facet_wrap(~uniqueid) +
      stat_smooth(method="lm", formula=y~x)
plt

# rt      = b0 + b1*x
# log(rt) = b0 + b1*x
#     rt  = exp(b0)*exp(b1*x)
#
#To do this, we’ll assume that each item (question) has a latent difficulty, with a Normal(0,3) prior. You
#can set this up just like how we set up subject parameters in class. Assume that difficulty determines
#accuracy via bernoulli(inv_logit(the item’s difficulty)), as in a logistic regression intercept 
# (if you need  to review logistic regression, see Gelman & Hill or rstan’s documentation). 

library(rstan)

stan.data <- list(N=nrow(d)
                  ,NITEMS=max(d$which_item)
                  ,which_item=d$which_item
                  ,accuracy=d$correct
                  ,rt=d$rt/1000)
fit <- stan(file="Class8-hw.stan", data=stan.data, iter=1000, chains=2, cores=2)

# Then, also assume that
#difficulty has some linear relationship to RT (this relationship is what we want to know). Thus, your
#model will have two likelihood components, one for the accuracy and one for the RT, with the inferred
# difficulty simultaneously affecting both both.

# 2. Implement the model, being sure you pay attention to the scaling of
# variables, what priors you choose, etc. Make a plot of the inferred difficulty
# of each item (item number on the x-axis, posterior difficulty on the y-axis).
# Does what you find make sense? Why or why not?
#
# 3. Show a histogram of the posterior on linear slope relating difficulty to
# RT. Does what you find make sense? Why or why not?
