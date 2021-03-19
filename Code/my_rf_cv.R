#' generate a random forest cross-validation for penguins
#'
#' we will predict output body_mass_g using covariates bill_length_mm,
#' bill_depth_mm, and flipper_length_mm.
#'
#' @param k a number for folds
#' @param data_pen data for penguins
#'
#'
#' @return A numeric with the cross-validation error
#'
#' @keywords inference
#' @keywords prediction
#'
#' @importFrom dplyr filter
#' @importFrom stats predict
#' @importFrom randomForest randomForest
#' @importFrom stats na.omit
#'
#'
#' @examples
#'
#'my_rf_cv(5, my_penguins)
#'
#'
#' @export




my_rf_cv <- function(k, data_pen) {
  data_p <- data_pen
  # remove rows with any column with NA
  penguins_clean <- na.omit(data_p)
  # select the data we want
  penguins_clean <- penguins_clean %>%
    select("body_mass_g", "bill_length_mm", "bill_depth_mm", "flipper_length_mm")
  n <- nrow(penguins_clean)
  inds <- sample(rep(1:k, length = n))
  penguins_clean$fold <- inds
  sum_err <- 0

  for(i in 1:k) {
    # select train data
    fold_train <- penguins_clean %>% filter(penguins_clean$fold != i)

    # select test data
    fold_test <- penguins_clean %>% filter(penguins_clean$fold == i)

    # get a clean test data
    fold_test <- fold_test %>% select("body_mass_g", "bill_length_mm",
                                      "bill_depth_mm", "flipper_length_mm")
    fold_test_p <- fold_test %>% select("bill_length_mm",
                                        "bill_depth_mm", "flipper_length_mm")

    # train with random forest
    train_model <- randomForest(body_mass_g ~ bill_length_mm +
                                  bill_depth_mm + flipper_length_mm,
                                data = fold_train, ntree = 100)

    # predict using trained model
    pred <- predict(train_model, fold_test_p)
    # calculate error and add to the sum of error
    sum_err <- sum_err + mean((fold_test$body_mass_g - pred)^2)
  }
  avg_err <- sum_err / k
  return(avg_err)
}
