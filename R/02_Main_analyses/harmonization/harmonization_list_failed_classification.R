library(tidyverse)
library(here)
library(purrr)
library(dplyr)

#----------------------------------------------------------#
# 1. Load data classified taxa rds file  ------------------
#----------------------------------------------------------#

class_taxa <- read_rds(here("Data/Processed/Data_harmonised/classified_taxa_neotoma_paper_1.rds"))

class_taxa_tib <- tibble::tibble(class_taxa)  # classification as tibble
class_taxa_tib %>% unnest(class_taxa)
View(class_taxa_tib)


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


class_taxa_has_no_res <- class_taxa_checked %>% select(has_no_results)  # select on classification results column as lgl
View(class_taxa_has_no_res)


class_taxa_has_no_res_chr <- class_taxa_has_no_res %>% as_vector() %>% as.character() %>% as_tibble() # as chr
typeof(class_taxa_has_no_res_chr)


class_taxa_names <- bind_cols(method_data_join_neotoma_re,class_taxa_has_no_res_chr) %>% rename(class_res = value)                       #bind neotoma_name with classification results 

class_taxa_failed <- class_taxa_names %>% filter(class_res == TRUE)   # list of failed classifications
class_taxa_success <- class_taxa_names %>% filter(class_res == FALSE)   # list of failed classifications

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
# 4. Manual classification of 100 taxa (27 failed ; 73 animals)
#----------------------------------------------------------#  

animals <- class_taxa_animals %>% rename(taxon_name = sel_name)

class_taxa_failed_animal <- full_join(class_taxa_failed, animals, by = "taxon_name") %>% select(taxon_name)

write_csv(class_taxa_failed_animal, here("Data/Processed/Data_harmonised/class_taxa_failed_animal.csv"))

class_taxa_failed_animal_re <- read_csv(here("Data/Processed/Data_harmonised/class_taxa_failed_animal_re.csv"), show_col_types = FALSE)

class_taxa_failed_animal_re_names <- names((class_taxa_failed_animal_re ))

class_taxa_failed_animal_re_names_new <- class_taxa_failed_animal_re %>%  rename(level_= class_taxa_failed_animal_re_names, -1) 
new_name <- paste0("level_", 1:8)

class_taxa_100 <- class_taxa_failed_animal_re_names_new %>% set_names("taxon_name", new_name)

write_csv(class_taxa_100, here("Data/Processed/Data_harmonised/class_taxa_100.csv"))
