
# Functions to retrieve ICE biodata based on Martin Schweinberger's scripts (http://www.martinschweinberger.de/blog/resources/)

library(quanteda)
library(tidyverse)

# 1. Function: prepare_metadata
# Downloads Schweinberger's ICE-IR biodata, collapses speaker-level
# info to document-level, and aligns with a quanteda tokens object

##### Ireland -----------------------------------------------------------------

prepare_metadata_ireland <- function(tokens_obj, meta_url = "http://martinschweinberger.de/docs/data/BiodataIceIreland.txt") {
  
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

# 2. Function: kwic_metadata
# Runs KWIC on a tokens object and automatically joins document-level metadata

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


#### New Zealand -------------------------------------------------------------

prepare_metadata_nz <- function(tokens_obj,
                                meta_url = "http://martinschweinberger.de/docs/data/BiodataIceNewZealand.txt") {
  
  # Load metadata
  ice_meta_nz <- readr::read_tsv(url(meta_url), show_col_types = FALSE)
  
  # Standardize join key
  ice_meta_nz <- ice_meta_nz %>%
    mutate(docname = toupper(gsub("[^A-Z0-9]", "", text.id))) %>%
    group_by(docname) %>%
    summarise(
      n_speakers = n(),
      speakers   = paste(spk.ref[!is.na(spk.ref)], collapse = ","),
      sex        = paste(unique(na.omit(sex)), collapse = ","),
      age        = paste(unique(na.omit(age)), collapse = ","),
      ethnicity  = paste(unique(na.omit(ethnicity)), collapse = ","),
      occupation = paste(unique(na.omit(occupation)), collapse = ","),
      .groups = "drop"
    ) %>%
    mutate(
      # Replace empty strings with NA
      speakers   = dplyr::na_if(speakers, ""),
      sex        = dplyr::na_if(sex, ""),
      age        = dplyr::na_if(age, ""),
      ethnicity  = dplyr::na_if(ethnicity, ""),
      occupation = dplyr::na_if(occupation, "")
    )
  
  # Standardize docnames in tokens object: remove extension
  token_docnames <- toupper(gsub("\\.TXT$", "", docnames(tokens_obj)))
  
  # Join metadata
  doc_meta <- tibble(docname = token_docnames) %>%
    left_join(ice_meta_nz, by = "docname")
  
  # Assign docvars, keeping order
  docvars(tokens_obj) <- doc_meta
  
  return(tokens_obj)
}
