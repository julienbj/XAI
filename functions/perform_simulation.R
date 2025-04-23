source("functions/generate_data.R")
source("functions/compute_shap.R")

perform_simulation <- function(N=1000, M, dependence=F, include.shapr=T){
  train <- generate_data(N, M, dependence = dependence)
  test <- generate_data(500, M, dependence = dependence)

  X_train <- train$data[, grep("^V", names(train$data)), drop = FALSE]
  X_test <- test$data[, grep("^V", names(test$data)), drop = FALSE]

  model = lm(y ~ ., data = train$data)
  true_shap <- compute_shap(model=model, test_data = X_test, train=train, dependence=dependence)

  mae <- list()

  if (include.shapr) {
    library(shapr)

    p0 <- mean(train$data$y)

    explanation <- explain(
      model = model,
      x_explain = X_test,
      x_train = X_train,
      approach = "empirical",
      phi0 = p0
    )

    shapr_vals <- explanation$shapley_values_est
    shapr_matrix <- as.matrix(shapr_vals[, .(V1, V2, V3, V4, V5)])

    mae$shapr <- mean(abs(shapr_matrix - true_shap))
  }

  return(mae)
}

perform_simulation(M=5)
