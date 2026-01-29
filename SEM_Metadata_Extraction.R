# Corpus of Singapore English Messages ------------------------------------

get_sem_metadata <- function(df){
  # Pass the data frame to the following functions
  df %>%
    # Convert to tibble (otherwise nothing works)
    as_tibble() %>% 
    # Data is actually not grouped, but not doing this causes problems
    dplyr::ungroup() %>% 
    # Extract the corpus tags
    mutate(metadata = str_extract_all(pre, "< <.*?> >")) %>%  # Extract all metadata
    # Grab the one that is closest to the keyword in the "pre" column and handle empty lists
    mutate(metadata = map_chr(metadata, ~ if (length(.x) == 0) NA_character_ else tail(.x, 1))) %>% 
    # Replace "NA" strings with actual NA
    mutate(metadata = ifelse(metadata == "NA", NA, metadata)) %>%
    # Remove both < < and > >
    mutate(metadata = str_remove_all(metadata, "< <|> >")) %>%
    # Get rid of superfluous spaces
    mutate(metadata = str_squish(metadata)) %>%
    # Generate new columns based on data within the corpus tags
    mutate(
      # Year (collection)
      year = str_extract(metadata, "(?<=COSEM : )\\d{2}"),
      
      # Race of collect
      race_collector = str_extract(metadata, "(?<=COSEM : \\d{2})\\w"),
      
      # Gender (of the person the data was collected from)
      gender_collector = str_extract(metadata, "(?<=COSEM : \\d{2}\\w)\\w"),
      
      # Extract ID
      speaker_ID = str_extract(metadata, "(?<=COSEM : \\d{2}\\w{2})\\d{2}"),
      
      # Line
      corpus_line = str_extract(metadata, "(?<=COSEM : \\d{2}\\w{2}\\d{2}-)\\d*"),
      
      # Age
      age_speaker = str_extract(metadata, "(?<=COSEM : \\d{2}\\w{2}\\d{2}-\\d{1,5}-)\\d{2}"),
      
      # Nationality (speaker)
      nationality_speaker = str_extract(metadata, "(?<=COSEM : \\d{2}\\w{2}\\d{2}-\\d{1,5}-\\d{2})\\w{2}"),
      
      # Race (speaker)
      race_speaker = str_extract(metadata, "(?<=COSEM : \\d{2}\\w{2}\\d{2}-\\d{1,5}-\\d{2}\\w{2})\\w{2}"),
      
      # Age (speaker)
      gender_speaker = str_extract(metadata, "(?<=COSEM : \\d{2}\\w{2}\\d{2}-\\d{1,5}-\\d{2}\\w{4})\\w"),
      
      # Year (utterance)
      year_utterance = str_extract(metadata, "(?<=COSEM : \\d{2}\\w{2}\\d{2}-\\d{1,5}-\\d{2}\\w{1,8}-)\\d*")
      
    )
}


# btw <- kwic(SEM, "btw", window = 20)
# btw_metadata <- get_sem_metadata(btw)