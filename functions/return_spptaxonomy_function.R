#function to get spp taxonomy

library(worrms)
library(taxize)
library(dplyr)

get_taxa <- function(taxon_list){
# Get WoRM's id for sourcing
wrm_alg <- taxize::gnr_datasources() %>% 
  dplyr::filter(title %in% c("World Register of Marine Species","AlgaeBase")) %>% 
  dplyr::pull(id)

# If scientific names are provided, check synonyms and get correct name and ID
# NOTE: it takes longer because it goes trough a
# series of checkpoints a taxon validation


# If scientific names are provided, check misspelling
fix_taxon <- taxize::gnr_resolve(taxon_list,
                                 data_source_ids = wrm_alg,
                                 best_match_only = TRUE,
                                 canonical = TRUE,
                                 ask = FALSE) %>%
  # dplyr::filter(score > 0.98) %>%
  dplyr::select(
    query = user_supplied_name,
    taxa = matched_name2)

# # Missing in fix_taxon
missing_misspelling <- tibble::tibble(
  query = taxon_list) %>%
  dplyr::filter(!query %in% fix_taxon$query)

# Make a batch loop to deal with curl_fetch_memory HTTP and internal errors of webpage

# Initialize an empty list to store the results
all_results <- list()

# Process in batches
for (i in seq(1, length(fix_taxon$taxa), by = 50)) {
  batch <- fix_taxon$taxa[i:min(i + 50 - 1, length(fix_taxon$taxa))]
  results <- worrms::wm_records_names(batch)
  all_results <- append(all_results, results)
}


alphaid <- dplyr::bind_rows(all_results) %>%
  dplyr::select(scientificname,status,AphiaID = valid_AphiaID,taxa = valid_name,kingdom:genus, isMarine,rank) %>% 
  left_join(fix_taxon,
            by = "taxa",
            relationship = "many-to-many") %>% 
  dplyr::mutate(query = ifelse(is.na(query),scientificname,query)) %>% 
  select(-scientificname)

# Missing in fix_taxon
missing_misspelling_wrms <- alphaid %>% 
  # For when the name has multiple wrong outputs
  arrange(status) %>% 
  dplyr::filter(status == "unaccepted") %>% 
  dplyr::distinct(query,.keep_all = T) %>%
  dplyr::select(query)

# Missing in AphiaIDs
missing_alphaid <- alphaid %>% 
  dplyr::filter(is.na(AphiaID) | AphiaID == -999) %>% 
  dplyr::select(query)

worms_db <- alphaid %>% 
  # Select only marine species (NOTE: These are not exclusively marine species)
  dplyr::filter(
    isMarine > 0) %>% 
  arrange(status) %>% 
  dplyr::select(-isMarine,
                -status) %>% 
  dplyr::distinct(query,.keep_all = T) 

missing_salt <- alphaid %>% 
  # Select only marine species (NOTE: These are not exclusively marine species)
  dplyr::filter(is.na(isMarine) | isMarine != 1) %>% 
  dplyr::select(-isMarine) # we don't really need this 

fishbase_id <- data.frame(taxa = worms_db$taxa)

# Final clean output data frame
fishbase_id <- unique(fishbase_id)
output_df <- dplyr::left_join(worms_db, fishbase_id, by = "taxa") %>% 
  # rest of selection
  dplyr::distinct() %>% 
  dplyr::select(
    query,
    worms_id = AphiaID,
    everything()
  )

#get common name
#list of taxa
taxa_list <- output_df$taxa
#empty list of common names
common_name_list <- c()

for(i in 1:length(taxa_list)){
  common_name <- sci2comm(sci = taxa_list[i], simplify = F)
  common_name_list[i] <- ifelse(length(common_name[[1]]) > 0, common_name[[1]], NA)
}
  
output_df$common_name <- unlist(common_name_list)

return(data.table(output_df))

} #close function
