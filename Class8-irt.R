
d <- read.csv("Assignment6.csv")
d <- subset(d, rt < 2*60*1000)  #keep between 2s and 2m
d <- subset(d, rt > 2*1000)
hist(d$rt, breaks=100)

d$subjectFrom1 <- as.numeric(as.factor(d$uniqueid))

q <- subset(d, is.element(uniqueid, unique(d$uniqueid)[1:30]))
library(ggplot2)
# Let's look at item (kinda difficulty) vs RT
plt <- ggplot(q, aes(x=which_item, y=rt/1000, color=correct)) + 
      geom_point() + 
      facet_wrap(~uniqueid) +
      stat_smooth(method="lm", formula=y~x)
plt

library(rstan)

stan.data <- list(N=nrow(d)
                  ,NITEMS=max(d$which_item)
                  ,NSUBJ=max(d$subjectFrom1)
                  ,accuracy=d$correct
                  ,which_item=d$which_item
                  ,which_subject=d$subjectFrom1
                )
fit <- stan(file="Class8-irt.stan", data=stan.data, iter=5000, chains=4, cores=4)

