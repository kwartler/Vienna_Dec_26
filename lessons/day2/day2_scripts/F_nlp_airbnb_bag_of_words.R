#' Ted Kwartler
#' Dec 12, 2026
#' Purpose: Bag of words text mining on Boston Airbnb guest reviews.
#'          Keyword scanning, cleaning, the document term matrix, term
#'          frequency, word association, bigram word clouds, commonality and
#'          comparison clouds, and dictionary sentiment with a radar chart.
#'          After the break you run the same steps on your GEO brand responses.
#' Data: https://github.com/kwartler/teaching-datasets (Airbnb-boston_only.zip)

# library & options
library(tm)
library(qdapRegex)
library(ggplot2)
library(ggthemes)
library(wordcloud)
library(RColorBrewer)
library(tidytext)
library(dplyr)
library(tidyr)
library(radarchart)
options(stringsAsFactors = FALSE, timeout = 300)

# ---------------------------------------------------------------------------
# 0. CUSTOM FUNCTIONS
# ---------------------------------------------------------------------------
# Lower case that returns NA instead of stopping on a strange character
tryTolower <- function(x){
  tryError <- tryCatch(tolower(x), error = function(e) e)
  if (!inherits(tryError, 'error')){
    y <- tolower(x)
  } else {
    y <- NA
  }
  return(y)
}

# Every cleaning step in one place, applied in this order
cleanCorpus <- function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  corpus <- tm_map(corpus, removePunctuation, ucp = TRUE) # ucp also drops curly quotes
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

# Two word tokens, ie "great location"
bigramTokens <- function(x){
  y <- unlist(lapply(NLP::ngrams(words(x), 2), paste, collapse = ' '),
              use.names = FALSE)
  return(y)
}

# ---------------------------------------------------------------------------
# 1. GET THE DATA: download the zip straight from GitHub
# ---------------------------------------------------------------------------
# Open Vienna_Dec_26.Rproj first so the backup paths below work
zipURL    <- 'https://github.com/kwartler/teaching-datasets/raw/refs/heads/main/Airbnb-boston_only.zip'
nrcURL    <- 'https://raw.githubusercontent.com/kwartler/teaching-datasets/refs/heads/main/nrc.csv'
backupCSV <- 'lessons/data/airbnb_boston_reviews_3k.csv' # Wi-Fi backup: the 3,000 reviews we sample below
backupNRC <- 'lessons/data/nrc.csv'

tmpZip <- tempfile(fileext = '.zip')
gotZip <- tryCatch({
  download.file(zipURL, tmpZip, mode = 'wb') # mode = 'wb' matters on Windows
  TRUE
}, error = function(e) FALSE)

if(gotZip){
  unzip(tmpZip, exdir = tempdir())
  reviews <- read.csv(file.path(tempdir(), 'Airbnb-boston_only.csv'))
} else {
  message('Download failed, reading the backup CSV in the course repo')
  reviews <- read.csv(backupCSV)
}

# One row per review; the listing columns repeat for every review of a listing
dim(reviews)
names(reviews)
reviews$comments[1]
length(unique(reviews$listing_id))
range(reviews$date)

# We only need a few columns today
keeps   <- c('review_id', 'listing_id', 'date', 'comments',
             'neighbourhood_cleansed', 'room_type', 'review_scores_rating')
reviews <- reviews[, keeps]

# ---------------------------------------------------------------------------
# 2. KEYWORD SCANNING: no cleaning needed, just grepl()
# ---------------------------------------------------------------------------
# grepl returns TRUE/FALSE per review; the mean of TRUE/FALSE is a share
mean(grepl('clean', reviews$comments, ignore.case = TRUE))
mean(grepl('dirty', reviews$comments, ignore.case = TRUE))

# grep returns the row positions; look at a few
idx <- grep('dirty', reviews$comments, ignore.case = TRUE)
head(idx)
reviews$comments[idx[1]]

# Some rows are not guest reviews at all
reviews$comments[grep('automated posting', reviews$comments)[1]]

# Drop empty, very short and automated rows
reviews <- reviews[!is.na(reviews$comments), ]
reviews <- reviews[nchar(reviews$comments) > 20, ]
reviews <- reviews[!grepl('automated posting', reviews$comments), ]

# A crude language filter: English reviews almost always contain "the" or "and"
# \\b is a word boundary so "theme" or "band" do not count
english <- grepl('\\b(the|and)\\b', reviews$comments, ignore.case = TRUE)
table(english)
head(reviews$comments[!english], 3)
reviews <- reviews[english, ]

# The business question: what separates top rated listings from the rest?
# review_scores_rating is the listing's overall score, repeated on each review
summary(reviews$review_scores_rating)
reviews$ratingGroup <- NA
reviews$ratingGroup[reviews$review_scores_rating >= 98] <- 'top_rated'
reviews$ratingGroup[reviews$review_scores_rating <= 86] <- 'lower_rated'
table(reviews$ratingGroup)

# Scan for a few complaints by group
complaints <- c('dirty', 'nois', 'smell', 'bug', 'broken', 'cancel')
scanDF <- data.frame()
for(i in 1:length(complaints)){
  hits <- grepl(complaints[i], reviews$comments, ignore.case = TRUE)
  tmp  <- aggregate(hits ~ ratingGroup, data = data.frame(hits, ratingGroup = reviews$ratingGroup), mean)
  tmp$keyword <- complaints[i]
  scanDF <- rbind(scanDF, tmp)
}
scanDF$pct <- round(scanDF$hits * 100, 1)
scanDF

ggplot(scanDF, aes(x = pct, y = keyword, fill = ratingGroup)) +
  geom_col(position = 'dodge') +
  theme_gdocs() +
  labs(x = '% of reviews mentioning the keyword', y = '')

# ---------------------------------------------------------------------------
# 3. SAMPLE & CLEAN: tm is slow, so we work with 1,500 reviews per group
# ---------------------------------------------------------------------------
set.seed(1234)
topIdx   <- sample(which(reviews$ratingGroup == 'top_rated'), 1500)
lowerIdx <- sample(which(reviews$ratingGroup == 'lower_rated'), 1500)
txt      <- reviews[c(topIdx, lowerIdx), ]
table(txt$ratingGroup)

# This sample is what lessons/data/airbnb_boston_reviews_3k.csv holds
#write.csv(txt, 'lessons/data/airbnb_boston_reviews_3k.csv', row.names = FALSE)

# Substitutions before cleaning
# Phones type curly apostrophes; make them straight so "it’s" matches the stop word "it's"
txt$comments <- gsub('[‘’]', "'", txt$comments)

# "the T" is how Bostonians say the subway; unify it before lower case
txt$comments <- gsub('\\bthe T\\b', 'the subway', txt$comments)

# Stop words: common words plus words that are true of every Airbnb review
stopwords('english')
customStopwords <- c(stopwords('english'), 'boston', 'airbnb', 'stay', 'place')

# A volatile corpus: one "document" per review
txtCorpus <- VCorpus(VectorSource(txt$comments))
txtCorpus <- cleanCorpus(txtCorpus, customStopwords)

# Before and after
txt$comments[1]
content(txtCorpus[[1]])

# Cleaning takes time; keep a copy of the clean text
txt$cleanText <- sapply(txtCorpus, content)

# ---------------------------------------------------------------------------
# 4. THE DOCUMENT TERM MATRIX: one row per review, one column per word
# ---------------------------------------------------------------------------
txtDTM <- DocumentTermMatrix(txtCorpus)
txtDTM  # notice the sparsity: most cells are 0
dim(txtDTM)

# Fill in the zeros as a regular matrix so we can look at it
txtDTMm <- as.matrix(txtDTM)
txtDTMm[1:5, c('clean', 'location', 'great', 'host', 'subway')]

# ---------------------------------------------------------------------------
# 5. TERM FREQUENCY: sum each column to get a word frequency matrix
# ---------------------------------------------------------------------------
termFreq <- colSums(txtDTMm)
termFreq <- data.frame(word      = names(termFreq),
                       frequency = termFreq,
                       row.names = NULL)
termFreq <- termFreq[order(termFreq$frequency, decreasing = TRUE), ]
head(termFreq, 15)

# Bar chart of the top terms
topWords <- termFreq[1:15, ]
ggplot(topWords, aes(x = frequency, y = reorder(word, frequency))) +
  geom_col(fill = 'darkred') +
  theme_gdocs() +
  geom_text(aes(label = frequency), colour = 'white', hjust = 1.25, size = 3.0) +
  labs(x = 'frequency', y = '')

# Your turn: are any of the top words uninformative?
# Add them to customStopwords, rerun section 3, then this section

# ---------------------------------------------------------------------------
# 6. WORD ASSOCIATION: when "dirty" appears, what else appears?
# ---------------------------------------------------------------------------
associations <- findAssocs(txtDTM, 'dirty', 0.15)
associations # lots of words that appear only once or twice

assocDF <- data.frame(terms     = names(associations[[1]]),
                      value     = unlist(associations[[1]]),
                      row.names = NULL)

# A word seen once next to "dirty" scores high by chance; keep words used 5+ times
assocDF <- merge(assocDF, termFreq, by.x = 'terms', by.y = 'word')
assocDF <- assocDF[assocDF$frequency >= 5, ]
assocDF <- assocDF[order(assocDF$value, decreasing = TRUE), ]
head(assocDF, 12)

assocDF <- assocDF[1:12, ]
ggplot(assocDF, aes(x = value, y = reorder(terms, value))) +
  geom_point(col = '#c00c00') +
  theme_gdocs() +
  geom_text(aes(x = value, label = value), colour = '#c00c00',
            hjust = 'inward', vjust = 'inward') +
  labs(x = 'association with "dirty"', y = '')

# Your turn: try 'location', 'host' or 'subway'

# ---------------------------------------------------------------------------
# 7. TOKENIZATION & A BIGRAM WORD CLOUD
# ---------------------------------------------------------------------------
# Same corpus, but each token is now two words
bigramDTM  <- DocumentTermMatrix(txtCorpus, control = list(tokenize = bigramTokens))
bigramDTMm <- as.matrix(bigramDTM)
dim(bigramDTMm) # many more columns than the unigram DTM

bigramFreq <- colSums(bigramDTMm)
bigramFreq <- data.frame(word      = names(bigramFreq),
                         frequency = bigramFreq,
                         row.names = NULL)
bigramFreq <- bigramFreq[order(bigramFreq$frequency, decreasing = TRUE), ]
head(bigramFreq, 15)

# Pick a palette and drop the lightest colors
pal <- brewer.pal(8, 'Blues')
pal <- pal[-(1:3)]

# Word clouds are frequency charts; expand your plot pane first
set.seed(1234)
wordcloud(bigramFreq$word,
          bigramFreq$frequency,
          max.words    = 75,
          random.order = FALSE,
          colors       = pal,
          scale        = c(2.5, 0.6))

# ---------------------------------------------------------------------------
# 8. TWO CORPORA: collapse each group into one document
# ---------------------------------------------------------------------------
topText   <- paste(txt$cleanText[txt$ratingGroup == 'top_rated'], collapse = ' ')
lowerText <- paste(txt$cleanText[txt$ratingGroup == 'lower_rated'], collapse = ' ')

# A corpus of 2 documents
groupCorpus <- VCorpus(VectorSource(c(topText, lowerText)))
groupDTM    <- DocumentTermMatrix(groupCorpus)
groupDTMm   <- as.matrix(groupDTM)
rownames(groupDTMm) <- c('top_rated', 'lower_rated') # same order as above!
groupDTMm[, c('clean', 'dirty', 'location', 'great', 'host')]

# The cloud functions expect terms as rows, so transpose with t()
# Commonality: words BOTH groups use a lot
commonality.cloud(t(groupDTMm),
                  max.words    = 100,
                  random.order = FALSE,
                  colors       = '#17223B',
                  scale        = c(3, 0.5))

# Comparison: words that set each group apart
comparison.cloud(t(groupDTMm),
                 max.words    = 100,
                 random.order = FALSE,
                 colors       = c('#0F8B8D', '#E07A2F'),
                 title.size   = 1.2,
                 scale        = c(3, 0.4))

# ---------------------------------------------------------------------------
# 9. SENTIMENT: join the words to a lexicon
# ---------------------------------------------------------------------------
# tidy() turns the 2 document DTM into one row per document and term
tidyText <- tidy(groupDTM)
tidyText$document <- ifelse(tidyText$document == '1', 'top_rated', 'lower_rated')
head(tidyText)

# Polarity with the bing lexicon: each word is positive or negative
bing     <- get_sentiments('bing')
bingSent <- inner_join(tidyText, bing, by = c('term' = 'word'))
head(bingSent)

polarity <- aggregate(count ~ document + sentiment, bingSent, sum)
polarity

# Share of polarized words that are negative, by group
totals <- aggregate(count ~ document, bingSent, sum)
negs   <- polarity[polarity$sentiment == 'negative', ]
negs$pctNegative <- round(negs$count / totals$count[match(negs$document, totals$document)] * 100, 1)
negs

# Which negative words drive the lower rated group?
lowerNeg <- bingSent[bingSent$document == 'lower_rated' & bingSent$sentiment == 'negative', ]
head(lowerNeg[order(lowerNeg$count, decreasing = TRUE), ], 10)

# Emotion with the NRC lexicon: 8 emotions plus positive and negative
nrc <- tryCatch(read.csv(nrcURL), error = function(e) read.csv(backupNRC))
nrcSent <- inner_join(tidyText, nrc,
                      by           = c('term' = 'word'),
                      relationship = 'many-to-many')

# Drop positive and negative to keep only the emotions
nrcSent <- nrcSent[!grepl('positive|negative', nrcSent$sentiment), ]

# Sum by group and emotion, then one column per group for the radar chart
nrcRadar <- aggregate(count ~ document + sentiment, nrcSent, sum) %>%
  pivot_wider(names_from = document, values_from = count) %>%
  as.data.frame()
nrcRadar

# Groups write different amounts, so compare shares, not counts
tmp    <- as.matrix(nrcRadar[, 2:ncol(nrcRadar)])
tmp    <- prop.table(tmp, 2)
plotDF <- data.frame(labels = nrcRadar[, 1], round(tmp * 100, 1), row.names = NULL)
plotDF

chartJSRadar(scores = plotDF, labelSize = 10, showLegend = TRUE)

# ---------------------------------------------------------------------------
# NEXT: the GEO brand activity uses these same steps on your LLM responses.
# Your responses become txt$comments, your brand becomes a custom stop word,
# and the groups become models or prompts instead of ratings.
# ---------------------------------------------------------------------------

# End
