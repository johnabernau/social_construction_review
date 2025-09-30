# Tue Mar  4 08:48:52 2025 -----------------------------------------------------
# Summary statistics for each journal

# load packages
require(tidyverse)
require(stopwords)
require(SnowballC)
# functions
`%nin%` = Negate(`%in%`)

# load each data type
#file.path(file.choose())

# annual review
# load("/Users/jberna5/Desktop/JSTOR_ConstellateData/annual_review_sociology/ars_meta.RData")
# load("/Users/jberna5/Desktop/JSTOR_ConstellateData/annual_review_sociology/ars_uni.RData")
# load("/Users/jberna5/Desktop/JSTOR_ConstellateData/annual_review_sociology/ars_bi.RData")

# american soc review
load("/Users/jberna5/Desktop/JSTOR_ConstellateData/american_soc_review/asr_meta.RData")
load("/Users/jberna5/Desktop/JSTOR_ConstellateData/american_soc_review/asr_uni.RData")
load("/Users/jberna5/Desktop/JSTOR_ConstellateData/american_soc_review/asr_bi.RData")

# american journal of sociology
load("/Users/jberna5/Desktop/JSTOR_ConstellateData/american_journal_sociology/ajs_meta.RData")
load("/Users/jberna5/Desktop/JSTOR_ConstellateData/american_journal_sociology/ajs_uni.RData")
load("/Users/jberna5/Desktop/JSTOR_ConstellateData/american_journal_sociology/ajs_bi.RData")

# social forces
load("/Users/jberna5/Desktop/JSTOR_ConstellateData/social_forces/sf_meta.RData")
load("/Users/jberna5/Desktop/JSTOR_ConstellateData/social_forces/sf_uni.RData")
load("/Users/jberna5/Desktop/JSTOR_ConstellateData/social_forces/sf_bi.RData")



# annual review summary stats
# ------------------------------------------------------------------------------
# names(ars_meta)
# # filter articles less than 500 words.
# # This removes 3 short articles, and 17 from 1994 with missing data
# ars_meta <- filter(ars_meta, wordCount2 > 500) 
# 
# # wordcount
# ggplot(ars_meta, aes(publicationYear, wordCount2)) + 
#   geom_point(position = "jitter", alpha = 0.5, 
#              size = 3, color = "royalblue") +
#   labs(title = "Annual Review of Sociology 1975-2018 (N = 927)") +
#   theme_minimal()
# # there were some long ones and a 8 short ones in 2000
# 
# # bar graph year
# count(ars_meta, publicationYear) %>% 
#   ggplot(aes(x = publicationYear, y = n)) + 
#   geom_bar(stat = "identity", fill = "royalblue") +
#   labs(title = "Annual Review of Sociology 1975-2018 (N = 927)") +
#   theme_minimal()
# 
# 
# 
# filter(ars_meta, str_detect(title, "Book Review")) %>% View()


# ASR summary stats
# ------------------------------------------------------------------------------
names(asr_meta)
# filter those less than 500.
# started with 7043, ended with 6714 = removed 329 rows.
#nrow(asr_meta) - nrow(filter(asr_meta, wordCount2 > 500))
asr_meta <- filter(asr_meta, wordCount2 > 500) 


# begin finding duplicates------------
# only keep rows with distinct titles and authors?
asr_meta %>%
  group_by(title, creator) %>%
  filter(n()>1) %>%
  ungroup() %>% View() # there are 911 duplicates (so ~1/2 are duplicated). 

nrow(asr_meta) # 6714
# first remove based on URL (N = -165)
nrow(asr_meta %>% distinct(url, .keep_all = TRUE)) # 6549
asr_meta <- asr_meta %>% distinct(url, .keep_all = TRUE) # removed 165
# clean title var
asr_meta$title2 <- tolower(asr_meta$title)
asr_meta$title2 <- gsub("[[:punct:]]", "", asr_meta$title2)
# count title, creator, year duplicates
nrow(asr_meta %>% distinct(title2, creator, publicationYear, .keep_all = TRUE)) # 6108
# examine
asr_meta %>%
  group_by(title2, creator, publicationYear) %>%
  filter(n()>1) %>%
  ungroup() %>% 
  ggplot(aes(publicationYear)) + geom_histogram()
# remove
asr_meta <- asr_meta %>% distinct(title2, creator, publicationYear, .keep_all = TRUE)
# removed 441 duplicate rows
# new count is 6108

# count for duplicates, allowing title and URL to vary
nrow(asr_meta %>% distinct(creator, pageStart, pageEnd, 
                           datePublished, isPartOf, publicationYear,
                           issueNumber, volumeNumber,
                           .keep_all = TRUE)) # 5943
# removed 165
asr_meta <- asr_meta %>% distinct(creator, pageStart, pageEnd, 
                                  datePublished, isPartOf, publicationYear,
                                  issueNumber, volumeNumber,
                                  .keep_all = TRUE)
# now allow author to vary
nrow(asr_meta %>% distinct(pageStart, pageEnd, 
                           datePublished, isPartOf, publicationYear,
                           issueNumber, volumeNumber,
                           .keep_all = TRUE)) # 5533
# removed 410
asr_meta <- asr_meta %>% distinct(pageStart, pageEnd, 
                           datePublished, isPartOf, publicationYear,
                           issueNumber, volumeNumber,
                           .keep_all = TRUE)
# new count is 5533

# end finding duplicates ------------

# word count plot
ggplot(asr_meta, aes(publicationYear, wordCount2)) + 
  geom_point(position = "jitter", alpha = 0.5, 
             size = 3, color = "royalblue") +
  labs(title = "American Sociological Review 1936-2024 (N = 5533)") +
  theme_minimal()

# asr_meta %>% arrange(wordCount2) %>% View()

# bar graph year
count(asr_meta, publicationYear) %>% 
  ggplot(aes(x = publicationYear, y = n)) + 
  geom_bar(stat = "identity", fill = "royalblue") +
  labs(title = "American Sociological Review 1936-2024 (N = 5533)") +
  theme_minimal()

asr_meta %>% 
  count(provider)


# American Journal of Sociology summary stats
# ------------------------------------------------------------------------------
names(ajs_meta)
# filter those less than 500. This leaves in some comments and replies, 
# but so be it: communities of discourse, etc. 
# started 5397, ended with 5345. #Removed 52 rows.
# nrow(ajs_meta) - nrow(filter(ajs_meta, wordCount2 > 500))
ajs_meta <- filter(ajs_meta, wordCount2 > 500) 

# remove Masthead and Book Review
count(ajs_meta, title == "Masthead") # remove 10
ajs_meta <- filter(ajs_meta, title != "Masthead")
count(ajs_meta, title == "Book Review") # remove 71
ajs_meta <- filter(ajs_meta, title != "Book Review")
count(ajs_meta, str_detect(title, "Book Review")) # remove 5
ajs_meta <- filter(ajs_meta, !str_detect(title, "Book Review"))
count(ajs_meta, title == "Acknowledgments to Referees") # remove 3
ajs_meta <- filter(ajs_meta, title != "Acknowledgments to Referees")



# only keep rows with distinct titles and authors
ajs_meta %>%
  group_by(title, creator) %>%
  filter(n()>1) %>%
  ungroup() %>% View()
# there are only 125 duplicate rows, but further examination below. 

# Group by title, and author / date published.
# No duplicates. All from JSTOR with unique URLS. 
nrow(ajs_meta) # 5256
nrow(ajs_meta %>% 
       distinct(title, datePublished, creator, .keep_all = TRUE)) #5256
nrow(ajs_meta %>% 
       distinct(url, .keep_all = TRUE)) #5256


# word count plot
ggplot(ajs_meta, aes(publicationYear, wordCount2)) + 
  geom_point(position = "jitter", alpha = 0.5, 
             size = 3, color = "royalblue") +
  labs(title = "American Journal of Sociology 1895-2018 (N = 5256)") +
  theme_minimal()

# ajs_meta %>% arrange(wordCount2) %>% View()

# bar graph year
count(ajs_meta, publicationYear) %>% 
  ggplot(aes(x = publicationYear, y = n)) + 
  geom_bar(stat = "identity", fill = "royalblue") +
  labs(title = "American Journal of Sociology 1895-2018 (N = 5256)") +
  theme_minimal()

ajs_meta %>% 
  count(provider)




# Social Forces summary stats
# ------------------------------------------------------------------------------
names(sf_meta)
# filter those less than 500. 
# 7580 to 7464 = Removed 116 rows.
# nrow(sf_meta) - nrow(filter(sf_meta, wordCount2 > 500))
sf_meta <- filter(sf_meta, wordCount2 > 500) 

# sf_meta %>% arrange(-wordCount2) %>% View()

# remove miscellaneous titles
count(sf_meta, title == "Errata") # remove 2
sf_meta <- filter(sf_meta, title != "Errata")
count(sf_meta, str_detect(tolower(title), "erratum")) # remove 5
sf_meta <- filter(sf_meta, !str_detect(tolower(title), "erratum"))

# count(sf_meta, title == "Masthead") # remove 0
# sf_meta <- filter(sf_meta, title != "Masthead")
# count(sf_meta, title == "Book Review") # remove 0
# sf_meta <- filter(sf_meta, title != "Book Review")
count(sf_meta, title == "Book Notes") # remove 4
sf_meta <- filter(sf_meta, title != "Book Notes")
# count(sf_meta, title == "Acknowledgments to Referees") # remove 0
# sf_meta <- filter(sf_meta, title != "Acknowledgments to Referees")

# looking for duplicates
sf_meta %>%
  group_by(title) %>%
  filter(n()>1) %>%
  ungroup() %>% View() # 1971 duplicate titles

# looking for duplicates
nrow(sf_meta) # 7453
# Most strict = title, date, creator
nrow(sf_meta %>% 
       distinct(title, datePublished, creator, .keep_all = TRUE)) # 7442
sf_meta <- sf_meta %>% 
  distinct(title, datePublished, creator, .keep_all = TRUE) # removed 11

# Looser = title, year, creator
sf_meta %>%
  group_by(title, publicationYear, creator) %>%
  filter(n()>1) %>%
  ungroup() %>% View() # 262 duplicate

sf_meta <- sf_meta %>% 
  distinct(title, publicationYear, creator, .keep_all = TRUE) # removed 131


# bar graph year
count(sf_meta, publicationYear) %>% 
  ggplot(aes(x = publicationYear, y = n)) + 
  geom_bar(stat = "identity", fill = "royalblue") +
  labs(title = "Social Forces 1925-2024 (N = 7311)") +
  theme_minimal()

# bar graph year
count(sf_meta, publicationYear, publisher) %>% 
  filter(publicationYear > 1980) %>% 
  ggplot(aes(x = publicationYear, y = n)) + 
  geom_bar(stat = "identity", fill = "royalblue") +
  theme_minimal() +
  facet_wrap(~publisher)


# You could try to parse titles (see below), but easier and more realistic 
# problem is that Project MUSE is the problem. 
count(sf_meta, publisher)
sf_meta %>% filter(publisher != "Project MUSE") %>% 
  count(publicationYear) %>% 
  ggplot(aes(x = publicationYear, y = n)) + 
  geom_bar(stat = "identity", fill = "royalblue") +
  theme_minimal()

# BUT 2021-2024 is only project muse.
count(sf_meta, publisher)
count(sf_meta, publicationYear, publisher) %>% 
  ggplot(aes(x = publicationYear, y = n)) + 
  geom_bar(stat = "identity", aes(fill = publisher)) +
  theme_minimal()

# separate publisher, only keep 2021+ from Muse
sf_meta_oxford <- sf_meta %>% filter(publisher != "Project MUSE") # N = 5705
sf_meta_muse <- sf_meta %>% filter(publisher != "Oxford University Press") # N = 1606
sf_meta_muse <- sf_meta_muse %>% filter(publicationYear > 2020) # N = 297
# join to create final
sf_meta <- bind_rows(sf_meta_oxford, sf_meta_muse) # N = 6002

# confirm visually
count(sf_meta, publicationYear, publisher) %>% 
  ggplot(aes(x = publicationYear, y = n)) + 
  geom_bar(stat = "identity", aes(fill = publisher)) +
  theme_minimal()

# # 2003-2021 has some republished articles from earlier years?
# sf_meta %>% filter(publicationYear > 2003 & publicationYear < 2021) %>%
#   arrange(title) %>% 
#   View()
# # lowercase and remove punctuation for matching purposes
# sf_meta$title2 <- str_trim(tolower(sf_meta$title)) # lowercase
# sf_meta$title2 <- gsub("[[:punct:]]", "", sf_meta$title2) # remove punctuation
# # sf_meta %>% arrange(title) %>% View()
# # okay years = 5029
# nrow(sf_meta %>% filter(publicationYear <= 2003 | publicationYear >= 2021))
# # problem years = 2286
# nrow(sf_meta %>% filter(publicationYear > 2003 & publicationYear < 2021))
# # corrected problem years = 1323
# nrow(sf_meta %>%
#        filter(publicationYear > 2003 & publicationYear < 2021) %>%
#        distinct(title2, .keep_all = TRUE)) # 1323
# # final dataset should be 5029 + 1323 = 6352
# df1 <- sf_meta %>% filter(publicationYear <= 2003 | publicationYear >= 2021)
# df2 <- sf_meta %>% 
#   filter(publicationYear > 2003 & publicationYear < 2021) %>% 
#   distinct(title2, .keep_all = TRUE)
# sf_meta <- bind_rows(df1, df2)
# # now cleaned SF data should be N = 6352


# word count plot
ggplot(sf_meta, aes(publicationYear, wordCount2)) + 
  geom_point(position = "jitter", alpha = 0.5, 
             size = 3, color = "royalblue") +
  labs(title = "Social Forces 1925-2024 (N = 6002)") +
  theme_minimal()


# bar graph year
count(sf_meta, publicationYear) %>% 
  ggplot(aes(x = publicationYear, y = n)) + 
  geom_bar(stat = "identity", fill = "royalblue") +
  labs(title = "Social Forces 1925-2024 (N = 6002)") +
  theme_minimal()



# saving new metadata files

save(ars_meta, file = "/Users/jberna5/Desktop/JSTOR_ConstellateData/annual_review_sociology/ars_meta2.RData") # 927

save(asr_meta, file = "/Users/jberna5/Desktop/JSTOR_ConstellateData/american_soc_review/asr_meta2.RData") # 5533

save(ajs_meta, file = "/Users/jberna5/Desktop/JSTOR_ConstellateData/american_journal_sociology/ajs_meta2.RData") # 5256

save(sf_meta, file = "/Users/jberna5/Desktop/JSTOR_ConstellateData/social_forces/sf_meta2.RData") # 6002


