# Sun Mar  2 17:46:36 2025 ------------------------------
# Processing JSTOR / Constellate Data for each journal
# Social Forces

# load packages
require(tidyverse)
require(stopwords)
require(SnowballC)

# functions
`%nin%` = Negate(`%in%`)

# load each data type
df_meta <- read.csv(file.choose())
df_uni <- read.csv(file.choose())
df_bi <- read.csv(file.choose())
# df_tri <- read.csv(file.choose())

# create id / number conversion 
df_meta <- arrange(df_meta, datePublished) # sort
df_meta$idno <- 1:nrow(df_meta) # add row numbers as idno
df_meta <- select(df_meta, idno, everything()) # move column to front
df_ids <- select(df_meta, idno, id) # create new conversion df
df_uni <- inner_join(df_ids, df_uni) # join with unigrams, etc.
df_bi <- inner_join(df_ids, df_bi)
# df_tri <- inner_join(df_ids, df_tri)

# add total word counts to meta
df_wc <- df_uni %>% 
  group_by(idno) %>% 
  summarize(wordCount2 = sum(count))
df_meta <- df_meta %>% 
  full_join(df_wc)
ggplot(df_meta, aes(wordCount, wordCount2)) + 
  geom_point() + geom_abline(intercept = 0, slope = 1)

filter(df_meta, wordCount != wordCount2) %>% 
  ggplot(aes(wordCount, wordCount2)) + 
  geom_point() + geom_abline(intercept = 0, slope = 1)

# parse docSubType for useable articles
# explore distribution
count(df_meta, docSubType)
count(df_meta, docSubType, publicationYear) %>% 
  ggplot(aes(publicationYear, n)) + 
  geom_line(aes(color = docSubType)) +
  facet_wrap(~docSubType)
df_meta$docSubType <- tolower(df_meta$docSubType) # lowercase
df_meta$docSubType <- gsub("[[:punct:]]", "", df_meta$docSubType) # punctuation
count(df_meta, docSubType)
filter(df_meta, docSubType == "") %>% count(str_detect(title, "(review)"))

# remove (review) from "" category
filter(df_meta, docSubType == "") %>%
  count(str_detect(title, "(review)"))
df_meta <- df_meta %>% 
  filter(!str_detect(title, "(review)"))
count(df_meta2, docSubType)
# only keep "", "article", and "researcharticle"
df_meta <- filter(df_meta, docSubType %in% c("", "article", "researcharticle"))

# use meta to filter word lists
df_uni <- filter(df_uni, idno %in% df_meta$idno)
df_bi <- filter(df_bi, idno %in% df_meta$idno)
#df_tri <- filter(df_tri, idno %in% df_meta$idno)

# stopped here ______________

# cleaning text unigrams -------------------------------------------------------
df_uni$ngram <- gsub("[[:punct:]]", "", df_uni$ngram) # remove punctuation
df_uni$ngram <- gsub("[[:digit:]]", "", df_uni$ngram) # Remove numbers
df_uni$ngram <- tolower(df_uni$ngram) # lowercase
sw <- stopwords("en", source = "stopwords-iso") # remove stopwords
df_uni <- df_uni %>% filter(ngram %nin% sw)
df_uni <- arrange(df_uni, ngram) # alphabetize
count(df_uni, ngram == "")
df_uni$ngram[df_uni$ngram == ""] <- NA # replace blanks with NA
df_uni <- df_uni %>% filter(!is.na(ngram)) # remove NA
df_uni$len <- str_length(df_uni$ngram) # calculate length
df_uni <-  df_uni %>% filter(len > 2) # remove words < 3 chars
df_uni <- select(df_uni, -len) # remove length variable

# then merge duplicate rows and add counts together
# ------------------------------------------------------------------------------
# # test example
# df <- data.frame(idno = rep(1:3, 12),
#                  ngram = rep(c("hello", "mrs", "jones", "dear"), 3),
#                  count = rep(1:6, 2))
# # in doc 1, hello appears 3 times.
# # in doc 2, hello appears 15 times.
# # i think this works:
# df2 <-  df %>% 
#   group_by(idno, ngram) %>%
#   summarize(count = sum(count))
# ------------------------------------------------------------------------------
df_uni <-  df_uni %>% 
  group_by(idno, id, ngram) %>% 
  summarize(count = sum(count)) %>% 
  ungroup() # removed almost 3m rows

# stemming words 
# The SnowballC package function "wordStem" uses Porter (1980) algorithm as
# cited in Silge text. (https://smltar.com/stemming.html)
df_uni <- df_uni %>% mutate(stem = wordStem(ngram))

# stem dictionary contains original-stem counts. Use to lookup as needed.
stem_dict <- df_uni %>% 
  select(ngram, stem, count) %>% 
  group_by(ngram, stem) %>% 
  summarize(count = sum(count)) %>% 
  ungroup() %>% 
  arrange(count, stem)



# cleaning text bigrams --------------------------------------------------------
df_bi$ngram <- gsub("[[:punct:]]", "", df_bi$ngram) # remove punctuation
df_bi$ngram <- tolower(df_bi$ngram) # lowercase
df_bi$ngram <- gsub("[[:digit:]]", "", df_bi$ngram) # Remove numbers
# did NOT remove stopwords
count(df_bi, ngram == "") # no blank cells
count(df_bi, ngram == " ") # 1.7m spaces
df_bi$ngram[df_bi$ngram == ""] <- NA # replace blanks with NA
df_bi$ngram[df_bi$ngram == " "] <- NA # replace blanks with NA
df_bi <- df_bi %>% filter(!is.na(ngram)) # remove NA
df_bi$len <- str_length(df_bi$ngram) # calculate length
df_bi <-  df_bi %>% filter(len > 7) # remove strings shorter than 7
# this removed about 8m rows
# did not remove length var
# df_bi1 <- select(df_bi1, -len) # remove length var
# did NOT alphabetize
# df_bi <- arrange(df_bi, ngram) # alphabetize


# then merge duplicate rows and add counts together
df_bi <-  df_bi %>% 
  group_by(idno, id, ngram) %>% 
  summarize(count = sum(count)) %>% 
  ungroup() # 35m to 32.1m






# # cleaning text trigrams --------------------------------------------------------
# df_tri$ngram[1:20] # 56.7m rows
# df_tri$ngram <- gsub("[[:punct:]]", "", df_tri$ngram) # remove punctuation
# df_tri$ngram <- tolower(df_tri$ngram) # lowercase
# df_tri$ngram <- gsub("[[:digit:]]", "", df_tri$ngram) # Remove numbers
# # did NOT remove stopwords
# count(df_tri, ngram == "") # no blank cells
# count(df_tri, ngram == " ") # no spaces
# count(df_tri, is.na(ngram))
# # df_tri$ngram[df_tri$ngram == ""] <- NA # replace blanks with NA
# # df_tri$ngram[df_tri$ngram == " "] <- NA # replace blanks with NA
# # df_tri <- df_tri %>% filter(!is.na(ngram)) # remove NA
# df_tri$len <- str_length(df_tri$ngram) # calculate length
# df_tri <-  df_tri %>% filter(len > 11) # remove strings shorter than 11
# # 56.7m to 48.2m rows
# 
# # did not remove length var
# # df_tri1 <- select(df_tri1, -len) # remove length var
# # did NOT alphabetize
# # df_tri <- arrange(df_tri, ngram) # alphabetize
# 
# # then merge duplicate rows and add counts together
# df_tri <-  df_tri %>% 
#   group_by(idno, id, ngram) %>% 
#   summarize(count = sum(count)) %>% 
#   ungroup() # 48.2m to 46.6m

# saving data ------------------------------------------------------------------

# saving metadata
sf_meta <- df_meta
save(sf_meta, file = "~/Desktop/sf_meta.RData")

# saving unigrams
sf_uni <- df_uni
save(sf_uni, file = "~/Desktop/sf_uni.RData")

# save bigrams
sf_bi <- df_bi
save(sf_bi, file = "~/Desktop/sf_bi.RData")

# save trigrams
# sf_tri <- df_tri
# save(sf_tri, file = "~/Desktop/sf_tri.RData")


