

#----------------------------------------------------------#
# 1. Classification per taxon list all the same time 
#----------------------------------------------------------#

method_data_class <- 
  purrr::map(.x = neo_taxa_vec_1,
             .f = ~ get_classification(taxa_vec = .x,
                                       use_only_exact_match = FALSE),
             .progress = TRUE) 

method_data_class
method_data_class[[30]]$classification


safe_classification <- purrr::safely(get_classification)   # capture errors 
class_taxa <- purrr::map(neo_taxa_vec, ~ safe_classification
                         (taxa_vec = .x, use_only_exact_match = FALSE), 
                         .progress = TRUE) 

class_taxa

View(class_taxa)

#----------------------------------------------------------#
# 2. Failed classification 
#----------------------------------------------------------#

library(purrr)

# Get index positions of failed classifications

failed_indices <- keep(seq_along(class_taxa), function(i) {
  class_df <- class_taxa[[i]]$result$classification[[1]]
  is.null(class_df) || !inherits(class_df, "data.frame") || nrow(class_df) == 0 # conditions: (class_df) is either NULL, not a data frame, or an empty data frame
})

#----------------------------------------------------------#
# 3. Remove failed classification 
#----------------------------------------------------------#

class_taxa_success <- class_taxa[-c(failed_indices)] # 1401-30 = 1370 successful classifications
View(class_taxa_success)




