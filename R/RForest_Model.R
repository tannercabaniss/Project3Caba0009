#' Random Forest Model
#'
#' This function builds and trains a Random Forest classifier for the provided features using the provided parameters.
#'
#' @param data The name of the dataset to be analyzed.
#' @param features The features to include in the analysis.
#' @param scaling The scaling scheme to be applied to the data.
#' @param parameters The parameters for the Random Forest model.
#'
#' @return a named list containing heatmaps of the confusion matrices and relevant model evaluation metrics
#' @export
#'
#' @importFrom stats predict
#'
#' @examples
#' RForest_Model(data="Obesity Dataset", features=c(TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,
#' TRUE,TRUE,TRUE,TRUE,TRUE,TRUE), scaling="Robust Scaling", parameters=c(10, 3, 10, 10))
RForest_Model <- function(data, features, scaling, parameters) {

  # Import Data
  if (data == "Obesity Dataset") {
    OR_Train <- Project3Caba0009::OR_Train
    OR_Test <- Project3Caba0009::OR_Test
    train_original <- OR_Train
    test_original <- OR_Test
  }

  # Transform categorical variables to numeric
  decategorize_2 <- c("Female" = 0, "Male" = 1)
  decategorize_6 <- c("no" = 0, "yes" = 1)
  decategorize_7 <- c("no" = 0, "yes" = 1)
  decategorize_10 <- c("no" = 0, "Sometimes" = 1, "Frequently" = 2, "Always" = 3)
  decategorize_11 <- c("no" = 0, "yes" = 1)
  decategorize_13 <- c("no" = 0, "yes" = 1)
  decategorize_16 <- c("no" = 0, "Sometimes" = 1, "Frequently" = 2)
  decategorize_17 <- c("Automobile" = 0, "Motorbike" = 1, "Public_Transportation" = 2, "Walking" = 3, "Bike" = 4)
  decategorize_18 <- c("Insufficient_Weight" = 0, "Normal_Weight" = 1, "Overweight_Level_I" = 2,
                       "Overweight_Level_II" = 3, "Obesity_Type_I" = 4, "Obesity_Type_II" = 5, "Obesity_Type_III" = 6)

  train_original[,2] <- decategorize_2[train_original[,2]]
  train_original[,6] <- decategorize_6[train_original[,6]]
  train_original[,7] <- decategorize_7[train_original[,7]]
  train_original[,10] <- decategorize_10[train_original[,10]]
  train_original[,11] <- decategorize_11[train_original[,11]]
  train_original[,13] <- decategorize_13[train_original[,13]]
  train_original[,16] <- decategorize_16[train_original[,16]]
  train_original[,17] <- decategorize_17[train_original[,17]]
  train_original[,18] <- decategorize_18[train_original[,18]]

  test_original[,2] <- decategorize_2[test_original[,2]]
  test_original[,6] <- decategorize_6[test_original[,6]]
  test_original[,7] <- decategorize_7[test_original[,7]]
  test_original[,10] <- decategorize_10[test_original[,10]]
  test_original[,11] <- decategorize_11[test_original[,11]]
  test_original[,13] <- decategorize_13[test_original[,13]]
  test_original[,16] <- decategorize_16[test_original[,16]]
  test_original[,17] <- decategorize_17[test_original[,17]]

  print("Decategorization successful")

  # Include selected features for scaling
  feature_list <- which(features)+1 # Plus one adapts for not including ID as available feature
  subset_train <- train_original[,feature_list]
  target_train <- train_original[,ncol(train_original)]

  subset_test <- test_original[,feature_list]

  print("Select features successful")

  # Helper function for determining if a feature is binary
  is.binary <- function(column) {
    unique_values <- unique(column)
    if (dim(unique_values)[1] == 2) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }

  # Apply selected scaling method for training data
  for (col_name in names(subset_train)) {
    column <- as.data.frame(subset_train[[col_name]])
    if (!is.binary(column) && scaling == "Min-Max") {
      preprocess <- caret::preProcess(column, method = "range")
      scaled_column <- predict(preprocess, column)
      colnames(scaled_column) <- col_name
      subset_train[[col_name]] <- unlist(scaled_column)
    }
    else if(!is.binary(column) && scaling == "Standardization") {
      scaled_column <- scale(column)
      colnames(scaled_column) <- col_name
    }
    else if(!is.binary(column) && scaling == "Robust Scaling") {
      preprocess <- caret::preProcess(column, method = c("center", "scale"))
      scaled_column <- predict(preprocess, column)
      colnames(scaled_column) <- col_name
      subset_train[[col_name]] <- unlist(scaled_column)
    }
  }

  # Apply selected scaling method for submission data
  for (col_name in names(subset_test)) {
    column <- as.data.frame(subset_test[[col_name]])
    if (!is.binary(column) && scaling == "Min-Max") {
      preprocess <- caret::preProcess(column, method = "range")
      scaled_column <- predict(preprocess, column)
      colnames(scaled_column) <- col_name
      subset_test[[col_name]] <- unlist(scaled_column)
    }
    else if(!is.binary(column) && scaling == "Standardization") {
      scaled_column <- scale(column)
      colnames(scaled_column) <- col_name
    }
    else if(!is.binary(column) && scaling == "Robust Scaling") {
      preprocess <- caret::preProcess(column, method = c("center", "scale"))
      scaled_column <- predict(preprocess, column)
      colnames(scaled_column) <- col_name
      subset_test[[col_name]] <- unlist(scaled_column)
    }
  }

  print("Scaling successful")

  # Split data into train and test set for model
  set.seed(12345)
  index <- sample(1:nrow(subset_train), size = 0.75 * nrow(subset_train), replace = FALSE)

  training_data_X <- subset_train[index,]
  training_data_Y <- target_train[index]
  training_data_Y <- factor(training_data_Y, levels = c(0,1,2,3,4,5,6))

  testing_data_X <- subset_train[-index,]
  testing_data_Y <- target_train[-index]
  testing_data_Y <- factor(testing_data_Y, levels = c(0,1,2,3,4,5,6))

  print("Factor definition successful")

  # Setup parameters for classifier
  parameter_list <- parameters

  # Setup Random Forest Classifier and Train Model on Training Data
  rf_model <- randomForest::randomForest(training_data_Y ~ .,
                                         data = training_data_X,
                                         ntree = parameter_list[1],
                                         mtry = parameter_list[2],
                                         max_depth = parameter_list[3],
                                         nodesize = parameter_list[4],
                                         importance = TRUE)

  print("Modelling successful")

  # Make predictions on train and test datasets
  predictions_train <- predict(rf_model, training_data_X)
  predictions_test <- predict(rf_model, testing_data_X)

  print("Predictions successful")

  custom_labels <- c("Insufficient_Weight", "Normal_Weight", "Overweight_Level_I",
                     "Overweight_Level_II", "Obesity_Type_I", "Obesity_Type_II", "Obesity_Type_III")

  # Convert predicted class labels to factors with custom levels
  predictions_train <- factor(predictions_train, levels = 0:6, labels = custom_labels)
  predictions_test <- factor(predictions_test, levels = 0:6, labels = custom_labels)

  # Convert target class labels to factors with custom levels
  training_data_Y <- factor(training_data_Y, levels = 0:6, labels = custom_labels)
  testing_data_Y <- factor(testing_data_Y, levels = 0:6, labels = custom_labels)

  # Extract results from predictions
  conf_matrix_train <- caret::confusionMatrix(predictions_train, training_data_Y)
  conf_matrix_test <- caret::confusionMatrix(predictions_test, testing_data_Y)

  # Get confusion matrix tables for heatmaps
  conf_matrix_train_df <- as.data.frame(conf_matrix_train$table)
  conf_matrix_test_df <- as.data.frame(conf_matrix_test$table)

  Reference <- conf_matrix_train_df$Reference
  Prediction <- conf_matrix_train_df$Prediction
  Freq <- conf_matrix_train_df$Freq

  # Generate heatmaps for confusion matrices
  train_heatmap <- ggplot2::ggplot(conf_matrix_train_df, ggplot2::aes(x=factor(Reference, levels = rev(levels(Reference))),y=Prediction,fill=Freq)) +
    ggplot2::geom_tile(color="black") +
    ggplot2::theme_bw() + ggplot2::coord_equal() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank()) +
    ggplot2::scale_fill_distiller(palette="Reds", direction=1) +
    ggplot2::labs(title = "Training Data - Confusion Matrix", x = "Actual") +
    ggplot2::geom_text(ggplot2::aes(label=Freq), color="black") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1)) +
    ggplot2::theme(plot.margin = ggplot2::unit(c(0, 0, 1.45, 0), "inches"))

  Reference <- conf_matrix_test_df$Reference
  Prediction <- conf_matrix_test_df$Prediction
  Freq <- conf_matrix_test_df$Freq

  test_heatmap <- ggplot2::ggplot(conf_matrix_test_df, ggplot2::aes(x=factor(Reference, levels = rev(levels(Reference))),y=Prediction,fill=Freq)) +
    ggplot2::geom_tile(color="black") +
    ggplot2::theme_bw() + ggplot2::coord_equal() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank()) +
    ggplot2::scale_fill_distiller(palette="Reds", direction=1) +
    ggplot2::labs(title = "Testing Data - Confusion Matrix", x = "Actual") +
    ggplot2::geom_text(ggplot2::aes(label=Freq), color="black") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1)) +
    ggplot2::theme(plot.margin = ggplot2::unit(c(0, 0, 1.45, 0), "inches"))

  # Generate formatted table of model metrics
  metrics_train_df <- conf_matrix_train$byClass[,c(1,2,3,4,5,6,7,11)]
  metrics_test_df <- conf_matrix_test$byClass[,c(1,2,3,4,5,6,7,11)]

  # Feature Importance
  importance_values <- randomForest::importance(rf_model)
  colnames(importance_values) <- c("Insufficient_Weight", "Normal_Weight", "Overweight_LevelI", "Overweight_LevelII",
                                   "Obesity_Type_I", "Obesity_Type_II", "Obesity_Type_III", "MeanDecreaseAccuracy",
                                   "MeanDecreaseGini")

  # Write out results list
  results_list <- list("Train_hmap" = train_heatmap, "Test_hmap" = test_heatmap, "Metrics_train" = metrics_train_df,
                       "Metrics_test" = metrics_test_df, "Import_Vals" = importance_values)

  return (results_list)
}
