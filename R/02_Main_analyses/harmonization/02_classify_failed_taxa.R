
#PART 2: CLASSIFY FAILED TAXA ----


#----------------------------------------------------------#
# 1. Load data classified taxa rds file  ------------------
#----------------------------------------------------------#

class_taxa <- read_rds(here("Data/Processed/Data_harmonised/classified_taxa_neotoma_p1_all_subset.rds"))

class_taxa_tib <- tibble::tibble(class_taxa)  # classification as tibble
class_taxa_tib %>% unnest(class_taxa)

#----------------------------------------------------------#
# 2. Identify taxa with successful or failed classifications
#----------------------------------------------------------#

class_taxa_tib_has_no_result <- function(x) {   # function to id taxon with failed classifications
  is.null(x$result)
}

class_taxa_checked <-                           # produce checklist of successful or failed classifications: TRUE -> failed
  class_taxa_tib %>% 
  dplyr::mutate(
    has_no_results = purrr::map_lgl(
      .x = class_taxa,.f =
        class_taxa_tib_has_no_result
    )
  ) 

class_taxa_checked %>%  print(n = 500)

class_taxa_has_no_res <- class_taxa_checked %>%
  select(has_no_results)  # select on classification results column as lgl

class_taxa_has_no_res_chr <- class_taxa_has_no_res %>% 
  as_vector() %>% 
  as.character() %>% 
  as_tibble() # as chr

class_taxa_names <- bind_cols(datasubset_all_unique_re,class_taxa_has_no_res_chr) %>%
  rename(class_res = value)  # bind neotoma_name with classification results 

class_taxa_failed <- class_taxa_names %>% 
  filter(class_res == TRUE)  # list of failed classifications

class_taxa_success <- class_taxa_names %>%
  filter(class_res == FALSE)   # list of succesful classifications

#----------------------------------------------------------#
# 3. Filter out if the classification is plant or an animal 
#----------------------------------------------------------#  

class_taxa_succeeded <- class_taxa_checked %>% # successful classification with tabulate results
  filter(has_no_results == FALSE) %>% 
  unnest(class_taxa) %>% 
  unnest_longer(class_taxa) %>% 
  unpack(cols = class_taxa) %>% 
  unnest_longer(class_taxa$classification) %>% 
  filter(!is.na(id))

class_taxa_animals <- class_taxa_succeeded %>%  # with classification Animalia 
  select(sel_name,classification) %>% 
  unnest_wider(classification) %>% 
  select(sel_name, name, rank) %>% 
  unnest(c(name,rank)) %>% 
  filter(rank == "kingdom") %>% 
  filter(name == "Animalia")

class_taxa_plants <- class_taxa_succeeded %>%  # with classification Plantae 
  select(sel_name,classification) %>% 
  unnest_wider(classification) %>% 
  select(sel_name, name, rank) %>% 
  unnest(c(name,rank)) %>% 
  filter(rank == "kingdom") %>% 
  filter(name == "Plantae")


write_rds(class_taxa_succeeded, here("Data/Processed/Data_harmonised/class_taxa_succeeded.rds"))
write_csv(class_taxa_plants, here("Data/Processed/Data_harmonised/class_taxa_plants.csv"))

#----------------------------------------------------------#
# 4. Manual classification ; 0 failed ; 108 classified as animals initially)
#----------------------------------------------------------#  

animals <- class_taxa_animals %>%
  rename(taxon_name = sel_name)

animals <- write_csv(animals,here("Data/Processed/Data_harmonised/animals.csv"))

animals_classified <- write_csv(animals_classified,here("Data/Processed/Data_harmonised/animals_classified.csv"))


animals_classified_names <- names((animals_classified))
get_animals_classified_names <- animals_classified  %>%  rename(level_= animals_classified_names, -1) 
new_name <- paste0("level_", 1:8)
animals_reclass <- get_animals_classified_names %>% set_names("taxon_name", new_name)

write_csv(animals_reclass, here("Data/Processed/Data_harmonised/additional_class_taxa.csv")) # classified as animals before then reclassified as animals
