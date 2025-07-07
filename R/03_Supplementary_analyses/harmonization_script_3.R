
#----------------------------------------------------------#
# 1. Extract taxon list from classified taxa using taxospace
#----------------------------------------------------------#

class_taxa_success <- class_taxa[-c(failed_indices)]
View(class_taxa_success)


#----------------------------------------------------------#
# 2. Load up taxon list for each study
#----------------------------------------------------------#

method_data_files <- list.files("Data/Processed/Other/", pattern ="[.]rds$", full.names = TRUE)
method_data_files

method_data <- purrr::map(method_data_files,readr::read_rds) 
method_data
method_data_taxa <- purrr::map(method_data, ~ pull(.x, taxa))
method_data_taxa
str(method_data_taxa)

#----------------------------------------------------------#
# 3. Select only unique taxa per taxon list  -------------
#----------------------------------------------------------#

method_data_taxa_unique <- purrr::map(method_data_taxa, ~ unique(.x))
str(method_data_taxa_unique[[1]])
str(method_data_taxa_unique[[2]])
str(method_data_taxa_unique[[3]])
str(method_data_taxa_unique[[4]])


typeof(method_data_taxa_unique[[1]])
nrow(method_data_taxa_unique[[1]])

#----------------------------------------------------------#
# 4. Fix format of taxon names (i.e. capitalize first letter) 
#----------------------------------------------------------#

# study 1 taxon list

method_data_taxa_unique_cap_1 <- str_to_sentence(method_data_taxa_unique[[1]])
str(method_data_taxa_unique_cap_1)  # a subset(from one method)
method_data_taxa_unique_cap_re_1 <- str_replace_all(method_data_taxa_unique_cap_1,"_"," ")
str(method_data_taxa_unique_cap_re_1) 

# study 2 taxon list

method_data_taxa_unique_cap_2 <- str_to_sentence(method_data_taxa_unique[[2]])
str(method_data_taxa_unique_cap_2)  # a subset(from one method)
method_data_taxa_unique_cap_re_2 <- str_replace_all(method_data_taxa_unique_cap_2,"_"," ")
str(method_data_taxa_unique_cap_re_2)


# study 3 taxon list

method_data_taxa_unique_cap_3 <- str_to_sentence(method_data_taxa_unique[[3]])
str(method_data_taxa_unique_cap_3)  # a subset(from one method)
method_data_taxa_unique_cap_re_3 <- str_replace_all(method_data_taxa_unique_cap_3,"_"," ")
str(method_data_taxa_unique_cap_re_3)


# study4 taxon list

method_data_taxa_unique_cap_4 <- str_to_sentence(method_data_taxa_unique[[4]])
str(method_data_taxa_unique_cap_4)  # a subset(from one method)
method_data_taxa_unique_cap_re_4 <- str_replace_all(method_data_taxa_unique_cap_4,"_"," ")
str(method_data_taxa_unique_cap_re_4)

#----------------------------------------------------------#
# 5. Create harmonization table per study
#----------------------------------------------------------#


# Extract sel_name and classification and combine into a tibble


extracted_data <- purrr::map_dfr(class_taxa_success, function(item) {
  if (!is.null(item$result)) {            # check if  checks if the result component within the current item is not NULL
    tibble(
      sel_name = item$result$sel_name,
      classification = list(item$result$classification) # Use list() to preserve list structure
    )
  } else {
    tibble(
      sel_name = NA_character_,
      classification = list(NULL)
    )
  }
})

View(extracted_data)

class_neotoma <- print(extracted_data)

class_neotoma_re <- class_neotoma %>% rename(taxon_name = sel_name)

class_neotoma_re %>% unlist(classification)

View(class_neotoma_re)

class_neotoma_re_unlisted <- class_neotoma_re %>%
  mutate(classification = map(classification, ~ .x[[1]]))

neotoma_class_final <- print(class_neotoma_re_unlisted)
glimpse(class_neotoma_re_unlisted)


# harmonization table for study 1
study_1_tib <- method_data_taxa_unique_cap_re_1 %>%  as_tibble() %>% rename(taxon_name = value)
harm_table_study_1 <- inner_join(study_1_tib, neotoma_class_final, by = "taxon_name")
harm_table_study_1

# harmonization table for study 2
study_2_tib <- method_data_taxa_unique_cap_re_2 %>%  as_tibble() %>% rename(taxon_name = value)
harm_table_study_2 <- inner_join(study_2_tib, neotoma_class_final, by = "taxon_name")
harm_table_study_2 

# harmonization table for study 3
study_3_tib <- method_data_taxa_unique_cap_re_3 %>%  as_tibble() %>% rename(taxon_name = value)
harm_table_study_3 <- inner_join(study_3_tib, neotoma_class_final, by = "taxon_name")
harm_table_study_3 

# harmonization table for study 4
study_4_tib <- method_data_taxa_unique_cap_re_4 %>%  as_tibble() %>% rename(taxon_name = value)
harm_table_study_4 <- inner_join(study_4_tib, neotoma_class_final, by = "taxon_name")
harm_table_study_4

class_neotoma_re_temp <-  harm_table_study_4 %>%
  mutate(classification = map(classification, ~ .x[[1]]))

class_neotoma_re_unnested <- class_neotoma_re_temp %>%
  unnest(classification)

final_harm_study_4 <- print(class_neotoma_re_unnested)

final_harm_study_4 

