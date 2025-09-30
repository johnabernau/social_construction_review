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
# # annual review
# load("/Users/jberna5/Desktop/JSTOR_ConstellateData/annual_review_sociology/ars_meta2.RData") # 927
# load("/Users/jberna5/Desktop/JSTOR_ConstellateData/annual_review_sociology/ars_uni.RData")
# load("/Users/jberna5/Desktop/JSTOR_ConstellateData/annual_review_sociology/ars_bi.RData")
# 
# # american soc review
load("/Users/jberna5/Desktop/JSTOR_ConstellateData/american_soc_review/asr_meta2.RData") # 5533
load("/Users/jberna5/Desktop/JSTOR_ConstellateData/american_soc_review/asr_uni.RData")
load("/Users/jberna5/Desktop/JSTOR_ConstellateData/american_soc_review/asr_bi.RData")
# 
# # american journal of sociology
load("/Users/jberna5/Desktop/JSTOR_ConstellateData/american_journal_sociology/ajs_meta2.RData") # 5256
load("/Users/jberna5/Desktop/JSTOR_ConstellateData/american_journal_sociology/ajs_uni.RData")
load("/Users/jberna5/Desktop/JSTOR_ConstellateData/american_journal_sociology/ajs_bi.RData")
# 
# # social forces
load("/Users/jberna5/Desktop/JSTOR_ConstellateData/social_forces/sf_meta2.RData") # 6002
load("/Users/jberna5/Desktop/JSTOR_ConstellateData/social_forces/sf_uni.RData")
load("/Users/jberna5/Desktop/JSTOR_ConstellateData/social_forces/sf_bi.RData")

# ------------------------------------------
# first have to pare down uni / bigrams to match the cleaned meta2 files
# ------------------------------------------
# use meta to filter word lists
asr_uni <- filter(asr_uni, idno %in% asr_meta$idno) # N = 7,364,621
asr_bi <- filter(asr_bi, idno %in% asr_meta$idno) # N = 25,314,385
save(asr_uni, file = "/Users/jberna5/Desktop/JSTOR_ConstellateData/american_soc_review/asr_uni2.RData")
save(asr_bi, file = "/Users/jberna5/Desktop/JSTOR_ConstellateData/american_soc_review/asr_bi2.RData")


ajs_uni <- filter(ajs_uni, idno %in% ajs_meta$idno) # N = 6986710
ajs_bi <- filter(ajs_bi, idno %in% ajs_meta$idno) # N = 24399702
save(ajs_uni, file = "/Users/jberna5/Desktop/JSTOR_ConstellateData/american_journal_sociology/ajs_uni2.RData")
save(ajs_bi, file = "/Users/jberna5/Desktop/JSTOR_ConstellateData/american_journal_sociology/ajs_bi2.RData")

sf_uni <- filter(sf_uni, idno %in% sf_meta$idno) # N = 6987525
sf_bi <- filter(sf_bi, idno %in% sf_meta$idno) # N = 23927445
save(sf_uni, file ="/Users/jberna5/Desktop/JSTOR_ConstellateData/social_forces/sf_uni2.RData")
save(sf_bi, file ="/Users/jberna5/Desktop/JSTOR_ConstellateData/social_forces/sf_bi2.RData")
