library(tidyverse)
library(jsonlite)
library(re2r)

rawFile <- "arxiv-metadata.origin"
#rawFile <- "dummy" #for testing

# Keywords
keywords <- read.delim("keywords.txt", comment.char = '#')
pattern <- keywords %>% 
  pull(keywords) %>% 
  paste(collapse = "|")

# Pattern
regexKeywords <- str_glue("\\b({pattern})\\b") %>% re2(case_sensitive = F) #match whole words only
regexDate <- re2("\\d{4}")

# Files & Connections
inRaw <- str_glue("raw/{rawFile}.json") %>% file()
outMatches <- str_glue("processed/matches.json") %>% file(open = "wb")
outTotal <- str_glue("processed/stats_total.json") %>% file(open = "wb")
outCategories <- str_glue("processed/stats_categories.json") %>% file(open = "wb")

# Run it...
stream_in(inRaw, pagesize = 1000, handler = function(data){
  data <- data %>%
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
  data %>%
    distinct(id.arxiv, .keep_all = T) %>%
    group_by(year) %>%
    summarise(total=n(), matches=sum(has.match)) %>%
    stream_out(outTotal)
  
  #preprints by year and category. One preprint can have several categories
  data %>%
    unnest_longer(categories.primary) %>%
    distinct(id.arxiv, categories.primary, .keep_all = T) %>% #remove doubles
    group_by(year, categories.primary) %>%
    summarise(total=n(), matches=sum(has.match)) %>%
    stream_out(outCategories)
  
  #matches only, one line per match
  data %>%
    filter(lengths(match) > 0) %>% #remove lines without matches
    mutate(match = map(match, ~ .x[1, 1])) %>% #extract matches from the matrix returned by re2r
    unnest_longer(match) %>% #one row per matching term
    mutate(match=str_to_lower(match), match_id = NULL) %>%
    distinct() %>%
    stream_out(outMatches)
})
close(outMatches)
close(outTotal)
close(outCategories)

# Note: Found 2.496.754 records
