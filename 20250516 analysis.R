# Thu May 15 10:48:50 2025 ------------------------------
# Analysis with clean journal files 

# load packages
require(tidyverse)
require(stopwords)
require(SnowballC)
# functions
`%nin%` = Negate(`%in%`)

# load each data type
#file.path(file.choose())
# 
# # american soc review
load("/Users/jberna5/Desktop/JSTOR_ConstellateData/american_soc_review/asr_meta2.RData") # 5533
load("/Users/jberna5/Desktop/JSTOR_ConstellateData/american_soc_review/asr_uni2.RData")
load("/Users/jberna5/Desktop/JSTOR_ConstellateData/american_soc_review/asr_bi2.RData")
# 
# # american journal of sociology
load("/Users/jberna5/Desktop/JSTOR_ConstellateData/american_journal_sociology/ajs_meta2.RData") # 5256
load("/Users/jberna5/Desktop/JSTOR_ConstellateData/american_journal_sociology/ajs_uni2.RData")
load("/Users/jberna5/Desktop/JSTOR_ConstellateData/american_journal_sociology/ajs_bi2.RData")
# 
# # social forces
load("/Users/jberna5/Desktop/JSTOR_ConstellateData/social_forces/sf_meta2.RData") # 6002
load("/Users/jberna5/Desktop/JSTOR_ConstellateData/social_forces/sf_uni2.RData")
load("/Users/jberna5/Desktop/JSTOR_ConstellateData/social_forces/sf_bi2.RData")

ajs_meta$issueNumber <- as.numeric(ajs_meta$issueNumber)
asr_meta$issueNumber <- as.numeric(asr_meta$issueNumber)
sf_meta$issueNumber <- as.numeric(sf_meta$issueNumber)

all_meta <- bind_rows(ajs_meta, sf_meta, asr_meta) %>% 
  arrange(datePublished) %>% 
  mutate(idno2 = row_number()) %>% 
  select(idno2, everything())
# ------------------------------------------------------------------------------
# Analysis
# ------------------------------------------------------------------------------


# # finding all "social* construct*" in bigrams
# ars_test <- filter(ars_bi, str_detect(ngram, "social[:alpha:]* construct")) %>%
#   full_join(ars_meta) # keeps all rows in this df
# ars_test$count[is.na(ars_test$count)] <- 0 # recode NA to 0
# # graph it?
# ars_test %>%
#   filter(publicationYear != 1994) %>% # removed for missing data
#   group_by(publicationYear) %>%
#   summarize(count = sum(count)) %>%
#   ungroup() %>%
#   ggplot(aes(x = publicationYear, y = count)) +
#   geom_point() +
#   geom_smooth(se = FALSE)


# finding all "social* construct*" in bigrams
asr_test <- filter(asr_bi, str_detect(ngram, "social[:alpha:]* construct")) %>%
  full_join(asr_meta) # keeps all rows in this df
asr_test$count[is.na(asr_test$count)] <- 0 # recode NA to 0
# why more rows than normal? Because articles can appear more than once for use of different words
# graph it?
asr_test %>%
  group_by(publicationYear) %>%
  summarize(count = sum(count)) %>%
  ungroup() %>%
  ggplot(aes(x = publicationYear, y = count)) +
  geom_point() +
  geom_smooth(se = FALSE)


# finding all "social* construct*" in bigrams
ajs_test <- filter(ajs_bi, str_detect(ngram, "social[:alpha:]* construct")) %>%
  full_join(ajs_meta) # keeps all rows in this df
ajs_test$count[is.na(ajs_test$count)] <- 0 # recode NA to 0
# graph it?
ajs_test %>%
  group_by(publicationYear) %>%
  summarize(count = sum(count)) %>%
  ungroup() %>%
  ggplot(aes(x = publicationYear, y = count)) +
  geom_point() +
  geom_smooth(se = FALSE)

# finding all "social* construct*" in bigrams
sf_test <- filter(sf_bi, str_detect(ngram, "social[:alpha:]* construct")) %>%
  full_join(sf_meta) # keeps all rows in this df
sf_test$count[is.na(sf_test$count)] <- 0 # recode NA to 0
# graph it?
sf_test %>%
  group_by(publicationYear) %>%
  summarize(count = sum(count)) %>%
  ungroup() %>%
  ggplot(aes(x = publicationYear, y = count)) +
  geom_point() +
  geom_smooth(se = FALSE)

# ars_test <- ars_test %>% select(-issueNumber)
# doing whole search in one

# before doing this, maybe you have to add manual journal ID?
#ars_test$journal <- "Annual Review of Sociology"
ajs_test$journal <- "American Journal of Sociology"
asr_test$journal <- "American Sociological Review"
sf_test$journal <- "Social Forces"

# sure...but what happened to all the other variables? title / year, etc.
df <- bind_rows(asr_test, ajs_test, sf_test)

scr_df <- df
# file.path(file.choose())
save(scr_df, file = "/Users/jberna5/Library/CloudStorage/GoogleDrive-bernau.john@gmail.com/My Drive/_cloudlocal/1. Desktop/2. Publications/scr_review/scr_df.RData")

load("/Users/jberna5/Library/CloudStorage/GoogleDrive-bernau.john@gmail.com/My Drive/_cloudlocal/1. Desktop/2. Publications/scr_review/scr_df.RData")

# ------------------------------------------------------------------------------
# Finished SCR count, saved as DF
# ------------------------------------------------------------------------------

scr_df$isPartOf <- factor(scr_df$isPartOf, levels = c("American Journal of Sociology", "Social Forces", "American Sociological Review"))
df2 <- scr_df %>% 
  group_by(isPartOf, publicationYear) %>% 
  summarize(count = sum(count))

# Graph #1: all with smoothed
df2 %>% 
  filter(!is.na(isPartOf)) %>% 
  #filter(publicationYear > 1960) %>% 
  ggplot(aes(x = publicationYear, y = count)) + 
  geom_point(aes(color = isPartOf), size = 3, alpha = 0.7) +
  geom_smooth(aes(color = isPartOf), se = FALSE) +
  theme_minimal()
  # geom_vline(xintercept = 1966, linewidth = 2)
  #geom_smooth(method = "lm", se = FALSE) +
  #scale_color_discrete(guide = F)
  #facet_wrap(~isPartOf, ncol = 4)

# Graph #2: facet wrap by journal
df2 %>% 
  filter(!is.na(isPartOf)) %>% 
  filter(publicationYear > 1960) %>% 
  ggplot(aes(x = publicationYear, y = count)) + 
  geom_point(aes(color = isPartOf), size = 3, alpha = 0.7) +
  geom_smooth(aes(color = isPartOf), span = 0.9, se = FALSE) +
  scale_y_continuous(limits = c(0, 80)) +
  theme_minimal() +
#geom_smooth(method = "lm", se = FALSE) +
  scale_color_discrete(guide = F) +
  facet_wrap(~isPartOf, ncol = 4)


# Graph #3: single smoothed line
# use this to change scope of slope line. By 2005. 
df2 %>% 
  group_by(isPartOf, publicationYear) %>% 
  summarize(count = sum(count)) %>% 
  filter(publicationYear > 1960) %>% 
  ggplot(aes(x = publicationYear, y = count)) + 
  labs(y = "SC Word Count") +
  geom_point(size = 3, alpha = 0.8, aes(color = isPartOf)) +
  geom_smooth(span = 1, se = FALSE, aes(color = isPartOf)) +
  # geom_smooth(span = 1, se = FALSE) +
  theme_minimal()


# --------------------------------------------------------------
# TABLE of word matches and frequencies
# --------------------------------------------------------------
sum(scr_df$count) # N = 2112 total uses of "social* construct"

# this produces table of phrase matches (Table 2)
t <- scr_df %>% 
  select(ngram, count) %>% 
  group_by(ngram) %>% 
  summarise(count = sum(count)) %>% 
  arrange(-count)
sum(t$count)

# --------------------------------------------------------------
# Proportion not just raw counts
# --------------------------------------------------------------

# making count proportional to total words
scr_df$scprop <- scr_df$count / scr_df$wordCount2

# graphing all articles
scr_df %>% 
  filter(publicationYear > 1960) %>% 
ggplot(aes(x = publicationYear, y = scprop)) +
  geom_point(size = 3, alpha = 0.5, aes(color = isPartOf)) +
  #facet_wrap(~isPartOf)
  geom_smooth(span = 1, se = FALSE)


# calculating "journal-year averages"
# the proportion accounts for different length articles (denom = total words)
# the mean accounts for number/availability of articles (denom = total articles)
df3 <- scr_df %>% 
  group_by(isPartOf, publicationYear) %>% 
  summarize(count = mean(scprop))

# graphing journal year averages (FINAL #1)
df3 %>% 
  #filter(publicationYear > 1960) %>% 
  ggplot(aes(x = publicationYear, y = count)) + 
  labs(y = "\"social* construct\" word proportion", x = NULL) +
  scale_color_discrete(name = NULL) +
  geom_point(size = 3, alpha = 0.7, aes(color = isPartOf)) +
  # geom_smooth(span = 1, se = FALSE, aes(color = isPartOf)) +
  geom_smooth(span = 0.6, se = FALSE, color = "black") +
  #facet_wrap(~isPartOf) +
  scale_x_continuous(breaks = c(1900, 1920, 1940, 1960, 1980, 2000, 2020)) +
  theme_minimal() +
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5), 'cm'))

ggsave("~/Desktop/scr1.jpg", width = 7.5, height = 4, units = "in")

# graphing journal year averages (FINAL #2)
df3 %>% 
  filter(publicationYear > 1950) %>% 
  ggplot(aes(x = publicationYear, y = count)) + 
  labs(y = "\"social* construct\" word proportion", x = NULL) +
  scale_color_discrete(guide = NULL, name = NULL) +
  geom_point(size = 3, alpha = 0.8, aes(color = isPartOf)) +
  # geom_smooth(span = 1, se = FALSE, aes(color = isPartOf)) +
  geom_smooth(span = 1, se = FALSE, color = "black") +
  #geom_smooth(span = 1, se = FALSE) +
  facet_wrap(~isPartOf) +
  scale_x_continuous(breaks = c(1960, 1980, 2000, 2020)) +
  theme_minimal() +
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5), 'cm'))

ggsave("~/Desktop/scr2.jpg", width = 7.5, height = 4, units = "in")



# ---------------------------------------------------------------------
# Useful stats for writing
scr_df %>% filter(count > 0) %>% arrange(publicationYear) %>% View()
scr_df %>% filter(count > 0) %>% arrange(publicationYear) %>% 
  filter(publicationYear < 1967) %>%
  # summarize(x = sum(count)) %>% # N = 43
  View()
scr_df %>% filter(count > 0) %>% arrange(publicationYear) %>% 
  filter(publicationYear >= 1967) %>% View()

scr_df %>% 
  group_by(idno) %>% 
  mutate(total = sum(count)) %>% 
  ungroup() %>% 
  arrange(-total) %>% 
  distinct(idno, total, .keep_all = T) %>% 
  # slice_max(total, n=5) %>% 
  View()
  
