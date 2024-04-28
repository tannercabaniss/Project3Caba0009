#' Feature Analysis
#'
#' This function performs helpful feature analysis to aid in feature engineering for the RF model.
#'
#' @param features The features to include in the analysis.
#' @param scaling The scaling scheme to be applied to the data.
#'
#' @return a named list containing correlation, covariance matrices alongside other informative plots.
#' @export
#'
#' @importFrom stats cor cov princomp
#'
#' @examples
#' Feat_Anal(features=c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,
#' TRUE,TRUE,TRUE,TRUE,TRUE,TRUE), scaling="None")
Feat_Anal <- function(features, scaling) {
  # Import Data
  OR_Train <- Project3Caba0009::OR_Train
  OR_Test <- Project3Caba0009::OR_Test
  train_original <- OR_Train
  train_analysis <- OR_Train
  test_original <- OR_Test

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

  # Include selected features for scaling
  # TODO Add target variable to feature list
  feature_list <- which(features)+1 # Plus one adapts for not including ID as available feature
  subset_train <- train_original[,feature_list]

  # Helper function for determining if a feature is binary
  is.binary <- function(column) {
    unique_values <- unique(column)
    if (dim(unique_values)[1] == 2) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }

  # Helper function for determining if a feature is categorical
  is.categorical <- function(column) {
    unique_values <- unique(column)
    if (dim(unique_values)[1] < 8) {
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

  # Generate correlation and covariance matrices
  cor_mat <- cor(subset_train)
  cov_mat <- cov(subset_train)

  cor_df <- reshape2::melt(cor_mat)
  cov_df <- reshape2::melt(cov_mat)

  # Generate correlation and covariance heatmaps
  Var1 <- cor_df$Var1
  Var2 <- cor_df$Var2
  value <- cor_df$value

  cor_heatmap <- ggplot2::ggplot(cor_df, ggplot2::aes(x=factor(Var1, levels = rev(levels(Var1))),y=Var2,fill=value)) +
    ggplot2::geom_tile(color="black") +
    ggplot2::theme_bw() + ggplot2::coord_equal() +
    ggplot2::scale_fill_distiller(palette="Reds", direction=1) +
    ggplot2::labs(title = "Correlation Matrix Heatmap", x = "", y="") +
    ggplot2::geom_text(ggplot2::aes(label=round(value,2)), color="black", size=30/(sqrt(dim(cor_df)[1]))) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1)) +
    ggplot2::theme(plot.margin = ggplot2::unit(c(0, 0, 1.45, 0), "inches"))

  Var1 <- cov_df$Var1
  Var2 <- cov_df$Var2
  value <- cov_df$value

  cov_heatmap <- ggplot2::ggplot(cov_df, ggplot2::aes(x=factor(Var1, levels = rev(levels(Var1))),y=Var2,fill=value)) +
    ggplot2::geom_tile(color="black") +
    ggplot2::theme_bw() + ggplot2::coord_equal() +
    ggplot2::scale_fill_distiller(palette="Reds", direction=1) +
    ggplot2::labs(title = "Covariance Matrix Heatmap", x = "", y="") +
    ggplot2::geom_text(ggplot2::aes(label=round(value,2)), color="black", size=30/(sqrt(dim(cov_df)[1]))) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1)) +
    ggplot2::theme(plot.margin = ggplot2::unit(c(0, 0, 1.45, 0), "inches"))

  # Generate summaries for selected features
  plot_list <- list()
  summary_list <- list()
  for (col_name in names(subset_train)) {
    column <- as.data.frame(subset_train[[col_name]])
    # Binary and categorical features
    if (is.binary(column) || is.categorical(column)) {
      column <- train_analysis[[col_name]]
      column_table <- table(column)
      Values = names(column_table)
      Frequency = as.numeric(column_table)
      column_df <- data.frame(Values = names(column_table), Frequency = as.numeric(column_table))
      column_plot <- ggplot2::ggplot(column_df, ggplot2::aes(x = Values, y = Frequency)) +
        ggplot2::geom_bar(stat = "identity", fill = "blue", col="black", alpha=0.5) +
        ggplot2::labs(title = col_name, x = "Values", y = "Frequency")
      plot_list[[col_name]] <- column_plot
      summary_list[[col_name]] <- column_table
    }
    # Numerical features
    else {
      column <- data.frame(value = subset_train[[col_name]])
      column_table <- column
      column_plot <- ggplot2::ggplot(column, ggplot2::aes(x=value)) +
        ggplot2::geom_histogram(bins=10, fill = "blue", alpha=0.5, col="black")
      plot_list[[col_name]] <- column_plot
      summary_list[[col_name]] <- summary(column_table)
    }
  }

  # PCA analysis for selected features
  PCA_results <- princomp(subset_train)$loadings

  results_list <- list("Cov_HM" = cov_heatmap, "Cor_HM" = cor_heatmap,
                       "Feat_Plots" = plot_list, "Sum_List" = summary_list,
                       "PCA" = PCA_results)
  return(results_list)
}
