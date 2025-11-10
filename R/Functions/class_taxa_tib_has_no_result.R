

class_taxa_tib_has_no_result <- function(x) {   # function to id taxon with failed classifications

name_cols <- colnames(x$result) 

is_classification_present <- "classification" %in% name_cols

return(!is_classification_present)

}
