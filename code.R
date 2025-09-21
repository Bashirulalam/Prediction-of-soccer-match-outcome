
library(tidyverse)
library(dplyr)
library(rpart)
library(rpart.plot)
library(party)
library(skellam)
library(DescTools)


train = read.csv("EuroTraining.csv")
test = read.csv("EuroTest.csv")

head(train)
head(test)


#########Explanatory data analysis
str(train)
str(test)

#Average Goals Per Tournament 

mean(train$Goals, na.rm = TRUE)
mean(test$Goals, na.rm = TRUE)

# Distribution of goals
table(train$Goals)
table(test$Goals)

# Average goal difference per Tournament
avg_goals_year <- train %>%
  group_by(Year) %>%
  summarise(avg_goals = mean(Goals, na.rm = TRUE)) %>%
  ungroup()

print(avg_goals_year)

# Plot
ggplot(avg_goals_year, aes(x = Year, y = avg_goals)) +
  geom_line(color = "blue") +
  geom_point(size = 2) +
  labs(title = "Average Goal Difference per Tournement",
       x = "Tournament Year", y = "Average Goal difference")


# Relationship between  goal difference and market value

ggplot(train, aes(x = MarketValue, y = Goals)) +
  geom_point(alpha = 0.6) +
  geom_smooth(aes(x = MarketValue, y = Goals), 
              method = "lm", se = FALSE, color = "red") +
  labs(title = "Relationship between Market Value Difference and Goals",
       x = "Market Value Difference (log-transformed)", y = "Goals")


# Relationship between goals and CL players

ggplot(train, aes(x = CLPlayers , y = Goals)) +
  geom_point(alpha = 0.6) +
  geom_smooth( aes(x = CLPlayers , y = Goals ),
               method = "lm" , se = F , color = "darkgreen") +
  labs(title = "Relationship between CLPlayers and Goal DIfference" ,
       X = "Number of CL Payers" , Y = " Goals")



######## poission model for prediction

pred_model = glm(Goals ~ GDP + MarketValue + FifaRank + UefaPoints + CLPlayers,
                 data = train , family = poisson(link = "log"))

summary(pred_model)



###### prediction on training data

train$pred_goals = predict(pred_model, newdata = train, type = "response")

train$abs_error <- abs(train$Goals - train$pred_goals)

# Mean Absolute Error (MAE)
mae_train <- mean(train$abs_error)
mae_train

#### prediction on test data

test$pred_goals = predict(pred_model ,  newdata = test ,  type = "response")

test$abs_error <- abs(test$Goals - test$pred_goals)

mae_test <- mean(test$abs_error)

mae_test




######### extra

library(boot)

mae_fn <- function(data, indices) {
  d <- data[indices, ]
  fit <- glm(Goals ~ GDP + MarketValue + FifaRank + UefaPoints + CLPlayers,
             data = d, family = poisson(link = "log"))
  preds <- predict(fit, newdata = d, type = "response")
  mean(abs(d$Goals - preds))
}

# 10-fold cross-validation
cv_results <- cv.glm(train, pred_model , cost = function(y, yhat) mean(abs(y - yhat)), K = 10)
cv_results$delta

############# extra end 

######### drop the features that are not important

pred_model_new = glm(Goals ~ GDP + MarketValue,
                 data = train , family = poisson(link = "log"))

summary(pred_model_new)

###### prediction on training data without features that are not important

train$pred_goals_new = predict(pred_model_new, newdata = train, type = "response")

train$abs_error_new <- abs(train$Goals - train$pred_goals_new)

# Mean Absolute Error (MAE)
mae_train_new <- mean(train$abs_error_new)
mae_train_new

#### prediction on test data

test$pred_goals_new = predict(pred_model_new ,  newdata = test ,  type = "response")

test$abs_error_new <- abs(test$Goals - test$pred_goals_new)

mae_test_new <- mean(test$abs_error_new)

mae_test_new


############## fitting regression tree

train$GroupStage <- as.factor(train$GroupStage)

# Fit regression tree 
reg_tree_model <- rpart(
  Goals ~ GroupStage + GDP + MarketValue + FifaRank + UefaPoints + CLPlayers,
  data = train,
  method = "anova"   # regression tree for numeric response
)

#Inspect model 
summary(reg_tree_model)

####ploting the tree

rpart.plot(reg_tree_model, type = 2, extra = 1, fallen.leaves = TRUE,
           main = "Regression Tree for Euro Goals")

# Variable importance from the regression tree
reg_tree_model$variable.importance


####### predicting using regression tree model and computation of MAE and RMSE

#
train$pred_reg_goals_tree <- predict(reg_tree_model, newdata = train)

# Compute errors
mae_reg_train_tree <- mean(abs(train$Goals - train$pred_reg_goals_tree))
rmse_reg_train_tree <- sqrt(mean((train$Goals - train$pred_reg_goals_tree)^2))

mae_reg_train_tree
rmse_reg_train_tree

# --- Predictions on test data ---
test$GroupStage <- as.factor(test$GroupStage)

test$pred_reg_goals_tree <- predict(reg_tree_model, newdata = test)

# Compute errors
mae_reg_test_tree <- mean(abs(test$Goals - test$pred_reg_goals_tree))
rmse_reg_test_tree <- sqrt(mean((test$Goals - test$pred_reg_goals_tree)^2))

mae_reg_test_tree
rmse_reg_test_tree


####performing grid serach over tree parameter

# Define parameter grid
cp_vals <- c(0.0005, 0.001, 0.002)
minsplit_vals <- c(3, 5, 7)
maxdepth_vals <- c(3, 5, 7)

# Empty results data frame
results <- data.frame(
  cp = numeric(),
  minsplit = numeric(),
  maxdepth = numeric(),
  mae_train = numeric(),
  mae_test = numeric()
)

# Grid search
for (cp in cp_vals) {
  for (minsplit in minsplit_vals) {
    for (maxdepth in maxdepth_vals) {
      
      # Fit tree with given hyperparameters
      tree_model <- rpart(
        Goals ~ GroupStage + GDP + MarketValue + FifaRank + UefaPoints + CLPlayers,
        data = train,
        method = "anova",
        control = rpart.control(cp = cp, minsplit = minsplit, maxdepth = maxdepth)
      )
      
      # Predictions
      pred_train <- predict(tree_model, newdata = train)
      pred_test  <- predict(tree_model, newdata = test)
      
      # Compute MAE
      mae_train <- mean(abs(train$Goals - pred_train))
      mae_test  <- mean(abs(test$Goals - pred_test))
      
      # Store results
      results <- rbind(results, data.frame(
        cp = cp,
        minsplit = minsplit,
        maxdepth = maxdepth,
        mae_train = mae_train,
        mae_test = mae_test
      ))
    }
  }
}

print(results)


#####finding the best tree
best_tree <- results[which.min(results$mae_test), ]
best_tree

####plotting the best tree


# --- 1. Identify best tree (lowest test MAE) ---
best_tree_row <- results[which.min(results$mae_test), ]
best_tree_row

# Fit best tree
best_tree_model <- rpart(
  Goals ~ GroupStage + GDP + MarketValue + FifaRank + UefaPoints + CLPlayers,
  data = train,
  method = "anova",
  control = rpart.control(
    cp = best_tree_row$cp,
    minsplit = best_tree_row$minsplit,
    maxdepth = best_tree_row$maxdepth
  )
)

# --- 2. Identify most overfitting tree (largest gap between train and test MAE) ---
results$gap <- results$mae_test - results$mae_train
overfit_tree_row <- results[which.max(results$gap), ]
overfit_tree_row

# Fit overfitting tree
overfit_tree_model <- rpart(
  Goals ~ GroupStage + GDP + MarketValue + FifaRank + UefaPoints + CLPlayers,
  data = train,
  method = "anova",
  control = rpart.control(
    cp = overfit_tree_row$cp,
    minsplit = overfit_tree_row$minsplit,
    maxdepth = overfit_tree_row$maxdepth
  )
)

# --- 3. Visualize both trees ---
par(mfrow = c(1,2))   # plot side by side

rpart.plot(best_tree_model, type = 2, extra = 1, fallen.leaves = TRUE,
           main = "Best Tree (Lowest Test MAE)")

rpart.plot(overfit_tree_model, type = 2, extra = 1, fallen.leaves = TRUE,
           main = "Most Overfitting Tree")


########## conditional inference model

set.seed(123)  # reproducibility

cf_ctrl <- cforest_unbiased(mtry = 3, ntree = 1000)  # mtry ≈ sqrt(#predictors)
cforest_model <- cforest(
  Goals ~ GroupStage + GDP + MarketValue + FifaRank + UefaPoints + CLPlayers,
  data = train,
  controls = cf_ctrl
)

# --- Step 4: Check variable importance ---
varimp(cforest_model)


#### combing data and find k

Euro_full <- rbind(train, test)

# --- Step 3: Count distinct European Championships (Year) ---
K <- length(unique(Euro_full$Year))
K



##### performing k fold


Euro_full$GroupStage <- as.factor(Euro_full$GroupStage)

# --- Distinct tournaments (Years) ---
years <- sort(unique(Euro_full$Year))
K <- length(years)

# --- Rank Probability Score function ---
rank_prob_score <- function(pred_matrix, obs) {
  n <- nrow(pred_matrix)
  C <- ncol(pred_matrix)
  
  # One-hot encode observations
  obs_mat <- matrix(0, n, C)
  obs_mat[cbind(1:n, as.integer(obs))] <- 1
  
  # Cumulative sums
  P_cum <- t(apply(pred_matrix, 1, cumsum))
  O_cum <- t(apply(obs_mat, 1, cumsum))
  
  # RPS
  mean(rowSums((P_cum - O_cum)^2) / (C - 1))
}

# --- Helper: outcome probs from Skellam ---
get_probs <- function(lambda_home, lambda_away, max_goals = 10) {
  goal_diff <- -max_goals:max_goals
  probs <- dskellam(goal_diff, lambda_home, lambda_away)
  p_home <- sum(probs[goal_diff > 0])
  p_draw <- probs[goal_diff == 0]
  p_away <- sum(probs[goal_diff < 0])
  return(c(HomeWin = p_home, Draw = p_draw, AwayWin = p_away))
}

# --- Storage for results ---
results <- data.frame(
  Year = integer(),
  Model = character(),
  RPS = numeric(),
  stringsAsFactors = FALSE
)

# --- Cross-validation loop ---
set.seed(123)
for (yr in years) {
  train_data <- subset(Euro_full, Year != yr)
  test_data  <- subset(Euro_full, Year == yr)
  
  # Fit models
  tree_model <- rpart(
    Goals ~ GroupStage + GDP + MarketValue + FifaRank + UefaPoints + CLPlayers,
    data = train_data, method = "anova"
  )
  
  cf_model <- cforest(
    Goals ~ GroupStage + GDP + MarketValue + FifaRank + UefaPoints + CLPlayers,
    data = train_data,
    controls = cforest_unbiased(mtry = 3, ntree = 500)
  )
  
  # Predict expected goals
  test_data$pred_tree <- predict(tree_model, newdata = test_data)
  test_data$pred_cf   <- unlist(predict(cf_model, newdata = test_data, OOB = TRUE))
  
  # Identify matches: each match = two rows (team vs opponent)
  match_ids <- unique(paste0(test_data$Year, "_", pmin(test_data$Team, test_data$Opponent),
                             "_", pmax(test_data$Team, test_data$Opponent)))
  
  # Storage for probabilities
  probs_tree <- matrix(NA, nrow = length(match_ids), ncol = 3)
  probs_cf   <- matrix(NA, nrow = length(match_ids), ncol = 3)
  obs_vec    <- integer(length(match_ids))
  
  row_idx <- 1
  for (m in match_ids) {
    match_rows <- which(paste0(test_data$Year, "_", 
                               pmin(test_data$Team, test_data$Opponent), "_",
                               pmax(test_data$Team, test_data$Opponent)) == m)
    if (length(match_rows) != 2) next
    
    # Extract true goals
    g1 <- test_data$Goals[match_rows[1]]
    g2 <- test_data$Goals[match_rows[2]]
    
    # Determine outcome
    if (g1 > g2) outcome <- 1   # HomeWin
    else if (g1 == g2) outcome <- 2  # Draw
    else outcome <- 3   # AwayWin
    
    # Predicted lambdas
    lambda1_tree <- test_data$pred_tree[match_rows[1]]
    lambda2_tree <- test_data$pred_tree[match_rows[2]]
    
    lambda1_cf <- test_data$pred_cf[match_rows[1]]
    lambda2_cf <- test_data$pred_cf[match_rows[2]]
    
    # Compute outcome probabilities
    probs_tree[row_idx, ] <- get_probs(lambda1_tree, lambda2_tree)
    probs_cf[row_idx, ]   <- get_probs(lambda1_cf, lambda2_cf)
    obs_vec[row_idx] <- outcome
    
    row_idx <- row_idx + 1
  }
  
  # Compute RPS for this fold
  rps_tree <- rank_prob_score(probs_tree, obs_vec)
  rps_cf   <- rank_prob_score(probs_cf, obs_vec)
  
  results <- rbind(results,
                   data.frame(Year = yr, Model = "Tree", RPS = rps_tree),
                   data.frame(Year = yr, Model = "Cforest", RPS = rps_cf))
}

# --- Final average RPS per model ---
aggregate(RPS ~ Model, data = results, mean)