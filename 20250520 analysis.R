# Tue May 20 10:28:09 2025 ------------------------------
# Analysis of other trends with clean journal files 

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


# finding all "epistem" in unigrams
asr_test <- filter(asr_uni, str_detect(ngram, "\\bepistem[:alpha:]*")) %>%
  full_join(asr_meta) # keeps all rows in this df
asr_test$count[is.na(asr_test$count)] <- 0 # recode NA to 0
asr_test$root <- "epistem"
# graph it?
asr_test %>%
  group_by(publicationYear) %>%
  summarize(count = sum(count)) %>%
  ungroup() %>%
  ggplot(aes(x = publicationYear, y = count)) +
  geom_point() +
  geom_smooth(se = FALSE)

# finding all "ontolog" in unigrams
asr_test2 <- filter(asr_uni, str_detect(ngram, "\\bontolog[:alpha:]*")) %>% full_join(asr_meta) # keeps all rows in this df
asr_test2$count[is.na(asr_test2$count)] <- 0 # recode NA to 0
asr_test2$root <- "ontolog"
# graph it?
asr_test2 %>%
  group_by(publicationYear) %>%
  summarize(count = sum(count)) %>%
  ungroup() %>%
  ggplot(aes(x = publicationYear, y = count)) +
  geom_point() +
  geom_smooth(se = FALSE)

asr_wd <- bind_rows(asr_test, asr_test2)




# finding all "epistem" in unigrams
ajs_test <- filter(ajs_uni, str_detect(ngram, "\\bepistem[:alpha:]*")) %>%
  full_join(ajs_meta) # keeps all rows in this df
ajs_test$count[is.na(ajs_test$count)] <- 0 # recode NA to 0
ajs_test$root <- "epistem"
# graph it?
ajs_test %>%
  group_by(publicationYear) %>%
  summarize(count = sum(count)) %>%
  ungroup() %>%
  ggplot(aes(x = publicationYear, y = count)) +
  geom_point() +
  geom_smooth(se = FALSE)

# finding all "ontolog" in unigrams
ajs_test2 <- filter(ajs_uni, str_detect(ngram, "\\bontolog[:alpha:]*")) %>% full_join(ajs_meta) # keeps all rows in this df
ajs_test2$count[is.na(ajs_test2$count)] <- 0 # recode NA to 0
ajs_test2$root <- "ontolog"
# graph it?
ajs_test2 %>%
  group_by(publicationYear) %>%
  summarize(count = sum(count)) %>%
  ungroup() %>%
  ggplot(aes(x = publicationYear, y = count)) +
  geom_point() +
  geom_smooth(se = FALSE)

ajs_wd <- bind_rows(ajs_test, ajs_test2)
count(ajs_wd, root)

# finding all "epistem" in unigrams
sf_test <- filter(sf_uni, str_detect(ngram, "\\bepistem[:alpha:]*")) %>%
  full_join(sf_meta) # keeps all rows in this df
sf_test$count[is.na(sf_test$count)] <- 0 # recode NA to 0
sf_test$root <- "epistem"
# graph it?
sf_test %>%
  group_by(publicationYear) %>%
  summarize(count = sum(count)) %>%
  ungroup() %>%
  ggplot(aes(x = publicationYear, y = count)) +
  geom_point() +
  geom_smooth(se = FALSE)

# finding all "ontolog" in unigrams
sf_test2 <- filter(sf_uni, str_detect(ngram, "\\bontolog[:alpha:]*")) %>% full_join(sf_meta) # keeps all rows in this df
sf_test2$count[is.na(sf_test2$count)] <- 0 # recode NA to 0
sf_test2$root <- "ontolog"
# graph it?
sf_test2 %>%
  group_by(publicationYear) %>%
  summarize(count = sum(count)) %>%
  ungroup() %>%
  ggplot(aes(x = publicationYear, y = count)) +
  geom_point() +
  geom_smooth(se = FALSE)

sf_wd <- bind_rows(sf_test, sf_test2)


# join all
eo_all <- bind_rows(ajs_wd, sf_wd, asr_wd)

eo_all$isPartOf <- factor(eo_all$isPartOf, levels = c("American Journal of Sociology", "Social Forces", "American Sociological Review"))

eo_group <- eo_all %>% 
  group_by(idno, root, publicationYear, 
           isPartOf, wordCount2, title) %>% 
  summarize(count = sum(count)) %>% 
  ungroup()
# making count proportional to total words
eo_group$countprop <- eo_group$count / eo_group$wordCount2

ggplot(eo_group, aes(publicationYear, countprop)) +
  geom_point(aes(color = root)) +
  geom_smooth(aes(color = root)) +
  facet_grid(isPartOf ~ root)


eo2 <- eo_all %>% 
  group_by(isPartOf, publicationYear, root) %>% 
  summarize(count = sum(count))

ggplot(eo2, aes(publicationYear, count)) +
  geom_point(aes(color = root))

# spread and find correlation?


# STOPPED HERE


# ------------------------------------------------------------------------------
# Finished SCR count, saved as DF
# ------------------------------------------------------------------------------



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



