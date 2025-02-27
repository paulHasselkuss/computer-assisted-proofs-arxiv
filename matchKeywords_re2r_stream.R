library(tidyverse)
library(jsonlite)
library(re2r)

raw <- "raw/arxiv-metadata.origin-snapshot120225.json"
#raw <- "raw/dummy.json"
keywords <- "keywords.txt"

# Keywords
pattern <- read.delim("keywords.txt", comment.char = '#') %>% 
  pull(keywords) %>% 
  paste(collapse = "|")
regexKeywords <- str_glue("\\b({pattern})\\b") %>% re2(case_sensitive = F) #match whole words only
regexDate <- re2("\\d{4}")

# Files
fileRaw <- file(raw)
fileMatches <- file("processed/matches.json", open = "wb")
fileStats <- file("processed/stats_total.json", open = "wb")
fileStatsCategories <- file("processed/stats_categories.json", open = "wb")

# Handle chunks
handleChunks <- function(d){
  d <- d %>%
    unite("txt", title, abstract, sep=" ", na.rm = T, remove = F) %>% #for matching
    mutate(
      id.arxiv = id,
      categories.secondary = str_split(categories, " "),
      categories.primary = map(categories.secondary, ~ str_remove_all(.x, "\\..*|-.*")), #?
      date.last.updated = map_chr(versions, ~ re2_match(.x$created[length(.x$created)], regexDate)[1, 1]), #last version
      date.submitted = map_chr(versions, ~ re2_match(.x$created[1], regexDate)[1, 1]),#first version (v1)
      year = date.submitted,
      authors = str_split(authors, ", | and "),
      no.author = lengths(authors),
      no.categories = lengths(categories),
      match = re2_match_all(txt, regexKeywords),
      has.match = map_lgl(match, ~ !is.null(.) && nrow(.) > 0) %>% as.integer()
    ) %>%
    select(
      -id,
      -update_date,
      -authors_parsed,
      -submitter,
      -comments, 
      -`journal-ref`,
      -license,
      -`report-no`,
      -txt,
      -categories
    )
  
  #preprints by year
  d %>%
    distinct(id.arxiv, .keep_all = T) %>%
    group_by(year) %>%
    summarise(total=n(), matches=sum(has.match)) %>%
    stream_out(fileStats)
  
  #preprints by year and category. One preprint can have several categories
  d %>%
    unnest_longer(categories.primary) %>%
    distinct(id.arxiv, categories.primary, .keep_all = T) %>% #remove doubles
    group_by(year, categories.primary) %>%
    summarise(total=n(), matches=sum(has.match)) %>%
    stream_out(fileStatsCategories)
  
  #matches only, one line per match
  d %>%
    filter(lengths(match) > 0) %>% #remove lines without matches
    mutate(match = map(match, ~ .x[1, 1])) %>% #extract matches from the matrix returned by re2r
    unnest_longer(match) %>% #one row per matching term
    mutate(match=str_to_lower(match), match_id = NULL) %>%
    distinct() %>%
    stream_out(fileMatches)
}

# Run it...
#Rprof()

stream_in(fileRaw, pagesize = 10000, handler = handleChunks)
close(fileMatches)
close(fileStats)
close(fileStatsCategories)

#Rprof(NULL)

# Note: Found 2.661.459 records
