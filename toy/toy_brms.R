library(tidyverse)
library(brms)

## a source to read: https://ourcodingclub.github.io/tutorials/brms/

## 10 games
## fake data that doesn't really make sense but is useful as an example
fake_data <- tibble(time = rep(1:100, 15), 
                    score_diff = sign(runif(100 * 15, -1, 1)) * rpois(100 * 15, 3),
                    win = rep(rbinom(15, 1, 0.5), each = 100))
fake_data


fake_model <- brm(win ~ score_diff + time + time:score_diff,
  data = fake_data,
  family = bernoulli(),
  prior = c(prior(normal(0, 2), class = "Intercept")) ## 
)

plot(fake_model)

# compare with frequentist logistic regression
fake_glm_model <- glm(win ~ score_diff + time + time:score_diff,
                      data = fake_data, family = "binomial")
plot(fake_glm_model)

## should see these distribution's centers match (approximately)
## with what you find from the basic logistic regression.

## this takes quite a while to run if you use all 2400 rows per game. you might try
## using a subset of the data where you haven't expanded each game to 2400 rows to 
## save computation time. 
## 
## the "prior" part is where prior information from the betting line would go.
## this prior says the teams are evenly matched (centered at 0 on log odds scale).
## 
## the prior is also on the log odds scale
## 
## can try scaling time and score differential to be between 0 and 1, which may
## speed up the alrogithm. 
## 
## this model also assumes independence, which is not an accurate assumption to make.
## That is, this model assumes that each row in the data set is providing
## independent information. But we have many rows per game, and observations
## within a game should have some dependence. No need to deal with this now
## but we will talk about it at our next meeting!
## 
## also the model could incorporate a pregame effect based on the betting line. But,
## we can ignore this for now and get this working in the more simple setting. 






