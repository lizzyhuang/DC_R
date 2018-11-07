require(glmnet)

training = function(training.df) {
  game_type_bin = model.matrix(training.df$result ~ training.df$game_type)[, -1]
  xMatrix = as.matrix(data.frame(training.df[, c("team_foul", "opponent_foul", "team_corner", "opponent_corner", "team_cross", "opponent_cross", "team_shoton", "opponent_shoton", "team_shotoff", "opponent_shotoff", "team_pos", "opponent_pos")], game_type_bin))
  glmnet.res = cv.glmnet(xMatrix, y = as.factor(training.df$result), alpha = 1,
                      family = "multinomial",type.multinomial = "grouped", intercept = FALSE)
  
  return (list(glmnet.res = glmnet.res, coefs = coef(glmnet.res), best.lambda = glmnet.res$lambda.1se))
}

# test.df = KOP.testing
# glmnet.object = MU.res$glmnet.res
# lambda = 0.07
testing = function(test.df, glmnet.object, lambda) {
  test_game_type_bin = model.matrix(test.df$result ~ test.df$game_type)[, -1]
  test_xMatrix = as.matrix(data.frame(test.df[, c("team_foul", "opponent_foul", "team_corner", "opponent_corner", "team_cross", "opponent_cross", "team_shoton", "opponent_shoton", "team_shotoff", "opponent_shotoff", "team_pos", "opponent_pos")], test_game_type_bin))
  glmnet.pred = predict(glmnet.object, newx = test_xMatrix, s = lambda, type = "response")
  pred = colnames(glmnet.pred)[max.col(glmnet.pred[, , 1], ties.method = "first")]
  
  error = ifelse(pred == test.df$result, 0, 1)
  return (list(prediction = pred, error = error))
} 
