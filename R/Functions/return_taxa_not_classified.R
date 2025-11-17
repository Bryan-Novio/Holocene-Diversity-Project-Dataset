

return_taxa_not_classified <- function(x) {   # function to id taxon with failed classifications

name_cols <- colnames(x$result) 

is_classification_present <- "classification" %in% name_cols

return(!is_classification_present)

}
