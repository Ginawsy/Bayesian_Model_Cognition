library(ggplot2)
library(lme4)

d <- sleepstudy

# Analyze the relationship between Days and Reaction

plt <- ggplot(d, aes(x=Days, y=Reaction)) + 
    geom_jitter() + 
    stat_smooth(method="lm", formula=y~x) + ylim(0,700)
plt

# R's standard regression:
# We are fitting Reaction = b0 + b1*Days
# fit b0, b1 with the assumption that (Reaction - (b0+b1*Days)) ~ Normal
# Generative model version:
#   P(Reaction | b0,b1,Days) ~ Normal(b0+b1*Days,noise)
#l <- lm(Reaction ~ Days, data=d)
#summary(l)

library(rstan)

stan.data <- list(N=nrow(d)
                  ,x=d$Days
                  ,y=as.vector(scale(d$Reaction)) # standardizing
                  )
fit <- stan(file="Class7a.stan", data=stan.data, iter=5000, chains=2, cores=2)

###########################################################
## Let's do an analysis with individual subject differences:
###########################################################

plt <- ggplot(d, aes(x=Days, y=Reaction)) + 
  geom_point() + 
  facet_wrap(~Subject) +
  stat_smooth(method="lm", formula=y~x) + ylim(0,700)
plt

d$SubjectFrom1 <- as.numeric(d$Subject)

stan.data <- list(N=nrow(d)
                  ,NSUBJ=max(d$SubjectFrom1)
                  ,x=d$Days
                  ,y=as.vector(scale(d$Reaction)) # standardizing
                  ,subject=d$SubjectFrom1
                  )
fit <- stan(file="Class7b.stan", data=stan.data, iter=5000, chains=2, cores=2)
