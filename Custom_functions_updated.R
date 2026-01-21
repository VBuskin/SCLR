
# Custom functions --------------------------------------------------------

## Sampling ----------------------------------------------------------------

### Function for drawing a random stratified sample from an ICE query

# (1) Requires the following libraries
library(tidyverse)
library(quanteda)
library(sampling)
library(data.table)

# (2) "data": Requires the data frame produced by a kwic() query
# (3) "size": Requires the size the of the stratified sample.

# Definition
stratified_sample_ICE <- function(data, size) {
  # Set seed for reproducibility
  set.seed(1234)
  
  # Required sample size
  sample_size_required <- size
  
  # Seprate the file numbers from the text categories
  verb_data <- separate_wider_delim(data,
                                    cols = docname, 
                                    delim = "-",
                                    names = c("Text_category",
                                              "File_number"))
  
  # Compute proportions in the population
  verb_data_prop <- table(verb_data$Text_category) / length(verb_data$Text_category)
  
  # Compute sizes of the stratified sample
  strat_sample_sizes <- round(sample_size_required * (verb_data_prop))
  
  # Draw the stratified sample
  verb_strat_sample <- tibble(strata(verb_data,
                                     "Text_category",
                                     strat_sample_sizes,
                                     method = "srswor"))
  
  
  # Generate output df
  output_sample <- tibble(getdata(verb_data, verb_strat_sample)) 
  
  # Some post-hoc clean-up
  output_sample %>% 
    dplyr::select(-ID_unit, -Prob, -Stratum) %>% # remove unnecessary columns
    relocate(Text_category) -> output_sample_cleaned # move some columns around
  
  # Return the desired sample
  return(output_sample_cleaned)
  
}

## Corpora -----------------------------------------------------------------

### Corpus of Singapore English Messages ------------------------------------

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


### ICE ---------------------------------------------------------------------

# Functions to retrieve ICE biodata based on Martin Schweinberger's scripts (http://www.martinschweinberger.de/blog/resources/)

library(quanteda)
library(dplyr)
library(tibble)

# ------------------------------------------------------------------
# 1. Function: prepare_metadata
# Downloads Schweinberger's ICE-IR biodata, collapses speaker-level
# info to document-level, and aligns with a quanteda tokens object
# ------------------------------------------------------------------

prepare_metadata <- function(tokens_obj, meta_url = "http://martinschweinberger.de/docs/data/BiodataIceIreland.txt") {
  
  # Load speaker-level metadata
  ice_meta <- read.table(
    meta_url,
    header = TRUE,
    sep = "\t",
    stringsAsFactors = FALSE,
    na.strings = c("", "NA")
  )
  
  # Map text IDs to docnames
  doc_map <- tibble(
    docname = docnames(tokens_obj),
    text.id = sub("^(S\\d+[A-Z]-\\d{3}).*", "\\1", docnames(tokens_obj))
  )
  
  # Collapse speaker metadata to document-level
  ice_meta_doc <- ice_meta %>%
    left_join(doc_map, by = "text.id") %>%
    group_by(docname) %>%
    summarise(
      n_speakers = n_distinct(spk.ref, na.rm = TRUE),
      speakers   = paste(unique(spk.ref), collapse = ","),
      sex        = paste(unique(na.omit(sex)), collapse = ","),
      age        = paste(unique(na.omit(age)), collapse = ","),
      zone       = paste(unique(na.omit(zone)), collapse = ","),
      date       = paste(unique(na.omit(date)), collapse = ","),
      .groups = "drop"
    )
  
  # Ensure all documents are present (fill missing with NA)
  ice_meta_doc_full <- tibble(
    docname = docnames(tokens_obj)
  ) %>%
    left_join(ice_meta_doc, by = "docname")
  
  # Attach as docvars
  docvars(tokens_obj) <- ice_meta_doc_full %>% select(-docname)
  
  return(tokens_obj)
}

# ------------------------------------------------------------------
# 2. Function: kwic_metadata
# Runs KWIC on a tokens object and automatically joins document-level metadata
# ------------------------------------------------------------------

kwic_metadata <- function(tokens_obj, pattern, window = 10) {
  
  # 2a. Run KWIC
  kw <- kwic(tokens_obj, pattern, window = window)
  
  # 2b. Prepare metadata table
  docvars_df <- data.frame(
    docname = docnames(tokens_obj),
    docvars(tokens_obj),
    row.names = NULL
  )
  
  # 2c. Join KWIC and metadata
  kw_meta <- kw %>%
    as_tibble() %>%
    left_join(docvars_df, by = "docname")
  
  return(kw_meta)
}


# Example:
# Suppose ICE_IR is your tokens object
#ICE_IR <- readRDS("ICE_IR.RDS")  # your tokens object

# Prepare docvars automatically
#ICE_IR <- prepare_metadata(ICE_IR)

# Run KWIC with metadata automatically attached
#kwic_results <- kwic_metadata(ICE_IR, "eat")
#kwic_results











