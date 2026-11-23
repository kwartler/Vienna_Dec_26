#' Ted Kwartler
#' Dec 12, 2026
#' Purpose: GEO (generative engine optimization) brand activity. When people
#'          ask an AI assistant for a recommendation, which brands come up?
#'          You pick a brand, ask 5 models 10 questions twice (100 calls),
#'          then measure your brand's visibility with keyword scanning, bag of
#'          words, and a second LLM acting as an analyst.
#' Builds on: E_openrouter_basics.R (askLLM) and F_nlp_airbnb_bag_of_words.R
#' Cost: roughly 200 short calls on cheap models, well under 1 USD

# library & options
library(httr)
library(jsonlite)
library(tm)
library(qdapRegex)
library(ggplot2)
library(ggthemes)
library(wordcloud)
library(RColorBrewer)
options(stringsAsFactors = FALSE)

# ---------------------------------------------------------------------------
# 0. YOUR KEY & CUSTOM FUNCTIONS
# ---------------------------------------------------------------------------
# Keys live in your .Renviron file, never in a script. See keys.example.md
nchar(Sys.getenv('OPENROUTER_API_KEY')) > 0 # TRUE means R found your key

# The same askLLM() from E_openrouter_basics.R
askLLM <- function(prompt,
                   model       = 'openai/gpt-4o-mini',
                   system      = NULL,
                   temperature = 0.7,
                   maxTokens   = 400,
                   key         = Sys.getenv('OPENROUTER_API_KEY')) {
  if (is.null(system)) {
    msgs <- list(list(role = 'user', content = prompt))
  } else {
    msgs <- list(list(role = 'system', content = system),
                 list(role = 'user',   content = prompt))
  }
  body <- list(model       = model,
               messages    = msgs,
               temperature = temperature,
               max_tokens  = maxTokens)
  resp <- POST('https://openrouter.ai/api/v1/chat/completions',
               add_headers(Authorization  = paste('Bearer', key),
                           `Content-Type` = 'application/json'),
               body = toJSON(body, auto_unbox = TRUE))
  if (status_code(resp) == 200) {
    answer <- content(resp)$choices[[1]]$message$content
  } else {
    warning(paste('Status', status_code(resp), 'from', model, ':',
                  content(resp)$error$message))
    answer <- NA
  }
  return(answer)
}

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

# Same cleaning steps as the Airbnb script
cleanCorpus <- function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  corpus <- tm_map(corpus, removePunctuation, ucp = TRUE)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

# Which brand is named first in a response? NA when none are named
firstBrand <- function(answer, brands){
  pos <- rep(NA, length(brands))
  for (i in 1:length(brands)) {
    hit <- regexpr(brands[i], answer, ignore.case = TRUE)
    if (hit > 0) {
      pos[i] <- hit
    } else {
      pos[i] <- NA
    }
  }
  if (all(is.na(pos))) {
    winner <- NA
  } else {
    winner <- brands[which.min(pos)]
  }
  return(winner)
}

# ---------------------------------------------------------------------------
# 1. YOUR BRAND: change these three lines
# ---------------------------------------------------------------------------
myBrand     <- 'Hoka'
myCategory  <- 'running shoes'
competitors <- c('Nike', 'Adidas', 'Brooks', 'Asics', 'New Balance', 'Saucony')
# Tip: avoid brand names that are everyday words (ie "On", "Gap"); every
# "on" in a response would count as a mention

allBrands <- c(myBrand, competitors)

# ---------------------------------------------------------------------------
# 2. THE QUESTIONS & MODELS: 10 questions x 5 models x 2 repeats = 100 calls
# ---------------------------------------------------------------------------
# Unbranded questions are the GEO test: does the AI bring your brand up
# on its own? Branded questions show how the AI describes your brand.
questions <- data.frame(
  type   = c(rep('unbranded', 8), rep('branded', 2)),
  prompt = c('What are the best CATEGORY brands right now?',
             'I am a beginner. Which CATEGORY should I buy?',
             'Which CATEGORY brand gives the best value for money?',
             'Which CATEGORY brands are the most comfortable?',
             'Which CATEGORY brands are the most sustainable?',
             'Recommend a premium CATEGORY brand and explain why.',
             'What CATEGORY brands do experts recommend?',
             'Which CATEGORY brands should I avoid and why?',
             'What is BRAND known for?',
             'What are the downsides of BRAND?'))
questions$prompt <- gsub('CATEGORY', myCategory, questions$prompt)
questions$prompt <- gsub('BRAND', myBrand, questions$prompt)
questions$questionID <- paste0('Q', 1:nrow(questions))
questions

# Five cheap models from different companies
modelList <- c('openai/gpt-4o-mini',
               'google/gemini-2.5-flash',
               'mistralai/mistral-small-3.2-24b-instruct',
               'meta-llama/llama-3.3-70b-instruct',
               'deepseek/deepseek-chat')

# Check every model id is still offered; swap any that print FALSE using
# the model list code in E_openrouter_basics.R
liveModels <- fromJSON(content(GET('https://openrouter.ai/api/v1/models'),
                               as = 'text', encoding = 'UTF-8'))$data$id
modelList %in% liveModels

# Every combination of question, model and repeat: one row per call
calls <- expand.grid(questionID = questions$questionID,
                     model      = modelList,
                     repeatNum  = 1:2,
                     stringsAsFactors = FALSE)
calls <- merge(calls, questions, by = 'questionID')
calls$answer <- NA
nrow(calls) # 100

# ---------------------------------------------------------------------------
# 3. COLLECT: 100 calls, saved as we go so a crash never costs you twice
# ---------------------------------------------------------------------------
dir.create('personalFiles', showWarnings = FALSE)
saveFile <- 'personalFiles/geo_responses.rds'

# If you already started, pick up where you left off
if (file.exists(saveFile)) {
  calls <- readRDS(saveFile)
} else {
  saveRDS(calls, saveFile)
}
sum(is.na(calls$answer)) # calls still to make

for (i in 1:nrow(calls)) {
  if (is.na(calls$answer[i])) {
    print(paste(i, 'of', nrow(calls), calls$model[i], calls$questionID[i]))
    calls$answer[i] <- askLLM(calls$prompt[i], model = calls$model[i])
    saveRDS(calls, saveFile)
    Sys.sleep(0.5) # be polite to the server
  } else {
    next
  }
}

# Rows that still failed (rerun the loop above to retry them)
sum(is.na(calls$answer))
calls <- calls[!is.na(calls$answer), ]

# Read a few answers before measuring anything
calls$answer[1]
calls$answer[sample(1:nrow(calls), 1)]

# ---------------------------------------------------------------------------
# 4. KEYWORD SCANNING: share of answers that mention each brand
# ---------------------------------------------------------------------------
# Only unbranded questions count; a branded question names your brand for you
unbranded <- calls[calls$type == 'unbranded', ]
nrow(unbranded)

visibility <- data.frame(brand = allBrands, mentionRate = NA)
for (i in 1:length(allBrands)) {
  hits <- grepl(allBrands[i], unbranded$answer, ignore.case = TRUE)
  visibility$mentionRate[i] <- round(mean(hits) * 100, 1)
}
visibility <- visibility[order(visibility$mentionRate, decreasing = TRUE), ]
visibility

visibility$isMine <- visibility$brand == myBrand
ggplot(visibility, aes(x = mentionRate, y = reorder(brand, mentionRate), fill = isMine)) +
  geom_col() +
  scale_fill_manual(values = c('grey60', 'darkred'), guide = 'none') +
  theme_gdocs() +
  labs(x = '% of unbranded answers that mention the brand', y = '',
       title = 'AI visibility: who gets recommended?')

# Your brand's mention rate by model: do some models know you better?
unbranded$mine <- grepl(myBrand, unbranded$answer, ignore.case = TRUE)
byModel <- aggregate(mine ~ model, data = unbranded, mean)
byModel$mine <- round(byModel$mine * 100, 1)
byModel[order(byModel$mine, decreasing = TRUE), ]

# ...and by question: which shopping situations include you?
byQuestion <- aggregate(mine ~ prompt, data = unbranded, mean)
byQuestion$mine <- round(byQuestion$mine * 100, 1)
byQuestion[order(byQuestion$mine, decreasing = TRUE), ]

# Being named first matters; people read the top of the answer
unbranded$firstNamed <- NA
for (i in 1:nrow(unbranded)) {
  unbranded$firstNamed[i] <- firstBrand(unbranded$answer[i], allBrands)
}
sort(table(unbranded$firstNamed, useNA = 'ifany'), decreasing = TRUE)

# Same question, same model, asked twice: did the answer change?
repeatCheck <- aggregate(mine ~ questionID + model, data = unbranded, sum)
table(repeatCheck$mine) # 1 means your brand showed up in only one of the two tries

# ---------------------------------------------------------------------------
# 5. BAG OF WORDS: top words and a word cloud
# ---------------------------------------------------------------------------
# Every answer that mentions your brand, branded and unbranded questions
mineTxt <- calls[grepl(myBrand, calls$answer, ignore.case = TRUE), ]
nrow(mineTxt)

# LLM answers use markdown (**bold**, ### headers); strip the symbols
mineTxt$answer <- gsub('[*#_`>|-]', ' ', mineTxt$answer)
mineTxt$answer <- gsub('[‘’]', "'", mineTxt$answer)

# Stop words: common words, your brand name (every answer here mentions it,
# so it would dominate the cloud), the category words and words every
# answer uses. Competitor names stay in.
customStopwords <- c(stopwords('english'),
                     strsplit(tolower(myBrand), ' ')[[1]],
                     strsplit(tolower(myCategory), ' ')[[1]],
                     'brand', 'brands', 'also', 'known', 'can', 'may', 'well',
                     'many', 'often', 'however', 'overall', 'options', 'choice')

mineCorpus <- VCorpus(VectorSource(mineTxt$answer))
mineCorpus <- cleanCorpus(mineCorpus, customStopwords)
mineDTM    <- DocumentTermMatrix(mineCorpus)
mineDTMm   <- as.matrix(mineDTM)

# Term frequency
termFreq <- colSums(mineDTMm)
termFreq <- data.frame(word = names(termFreq), frequency = termFreq, row.names = NULL)
termFreq <- termFreq[order(termFreq$frequency, decreasing = TRUE), ]
head(termFreq, 20)

ggplot(termFreq[1:15, ], aes(x = frequency, y = reorder(word, frequency))) +
  geom_col(fill = 'darkred') +
  theme_gdocs() +
  labs(x = 'Frequency', y = '', title = paste('Top words in answers mentioning', myBrand))

# Word cloud
wordcloud(termFreq$word,
          termFreq$frequency,
          max.words    = 75,
          random.order = FALSE,
          colors       = brewer.pal(8, 'Reds')[4:8])

# ---------------------------------------------------------------------------
# 6. A SECOND LLM AS ANALYST: read every answer and score it
# ---------------------------------------------------------------------------
# Keyword scanning cannot tell praise from criticism. Ask a model to read
# each answer and return structured JSON we can count.
judgeSystem <- 'You are a careful brand analyst. Reply with JSON only, no other text.'
judgePrompt <- function(answer, brand){
  p <- paste0('Brand: ', brand, '\n',
              'Text to analyze:\n"""', answer, '"""\n',
              'Return JSON with these keys:\n',
              'mentioned: true or false, is the brand named in the text\n',
              'rank: the position of the brand among all brands named (1 = first), or null\n',
              'sentiment: "positive", "neutral", "negative" or "not mentioned" toward the brand\n',
              'attributes: up to 3 short phrases the text associates with the brand, or an empty list')
  return(p)
}

judgeFile <- 'personalFiles/geo_judged.rds'
if (file.exists(judgeFile)) {
  judged <- readRDS(judgeFile)
} else {
  judged <- calls[, c('questionID', 'type', 'model', 'repeatNum', 'answer')]
  judged$mentioned  <- NA
  judged$rank       <- NA
  judged$sentiment  <- NA
  judged$attributes <- NA
}

for (i in 1:nrow(judged)) {
  if (is.na(judged$sentiment[i])) {
    print(paste('Judging', i, 'of', nrow(judged)))
    raw <- askLLM(judgePrompt(judged$answer[i], myBrand),
                  model = 'openai/gpt-4o-mini', system = judgeSystem,
                  temperature = 0, maxTokens = 200)
    raw <- gsub('```json|```', '', raw) # some models wrap JSON in code fences
    parsed <- tryCatch(fromJSON(raw), error = function(e) NULL)
    if (!is.null(parsed)) {
      judged$mentioned[i]  <- isTRUE(parsed$mentioned)
      judged$rank[i]       <- ifelse(is.null(parsed$rank), NA, parsed$rank)
      judged$sentiment[i]  <- parsed$sentiment
      judged$attributes[i] <- paste(unlist(parsed$attributes), collapse = '; ')
    } else {
      warning(paste('Could not read the JSON for row', i))
    }
    saveRDS(judged, judgeFile)
    Sys.sleep(0.5)
  } else {
    next
  }
}

# Sentiment toward your brand, by question type
table(judged$type, judged$sentiment)

# Average rank when mentioned on unbranded questions (1 = named first)
judgedUnbranded <- judged[judged$type == 'unbranded' & judged$mentioned %in% TRUE, ]
mean(judgedUnbranded$rank, na.rm = TRUE)
aggregate(rank ~ model, data = judgedUnbranded, mean)

# What the models associate with your brand
attrs <- unlist(strsplit(tolower(judged$attributes), '; '))
attrs <- attrs[!is.na(attrs) & attrs != '']
head(sort(table(attrs), decreasing = TRUE), 15)

# Check the analyst: does it agree with keyword scanning?
judged$keywordHit <- grepl(myBrand, judged$answer, ignore.case = TRUE)
table(keyword = judged$keywordHit, llm = judged$mentioned)
# Read the disagreements. Who was wrong, the regex or the LLM?
disagree <- which(judged$keywordHit != judged$mentioned)
if (length(disagree) > 0) {
  judged$answer[disagree[1]]
} else {
  print('No disagreements')
}

# ---------------------------------------------------------------------------
# 7. LET AN LLM DRAFT THE BRIEF, THEN CHECK IT AGAINST YOUR NUMBERS
# ---------------------------------------------------------------------------
myRate <- visibility$mentionRate[visibility$brand == myBrand]
facts <- paste0(
  'Brand: ', myBrand, ' (', myCategory, ')\n',
  'Mention rate on unbranded questions by brand (%): ',
  paste(visibility$brand, visibility$mentionRate, sep = ' ', collapse = ', '), '\n',
  myBrand, ' mention rate by model (%): ',
  paste(byModel$model, byModel$mine, sep = ' ', collapse = ', '), '\n',
  'Sentiment counts toward ', myBrand, ': ',
  paste(names(table(judged$sentiment)), table(judged$sentiment), collapse = ', '), '\n',
  'Top associated words: ', paste(head(termFreq$word, 15), collapse = ', '), '\n',
  'Top attributes from the analyst model: ',
  paste(names(head(sort(table(attrs), decreasing = TRUE), 8)), collapse = ', '), '\n')
cat(facts)

brief <- askLLM(paste0('Using only these facts, write a 150 word brief for the ',
                       myBrand, ' chief marketing officer on how AI assistants ',
                       'portray the brand and two actions to improve its visibility.\n\n',
                       facts),
                model = 'google/gemini-2.5-flash', maxTokens = 600)
cat(brief)
# Before you use it: does every number in the brief match the facts above?

# ---------------------------------------------------------------------------
# 8. YOUR SUBMISSION: answer in a few sentences each
# ---------------------------------------------------------------------------
# 1. On unbranded questions, how visible is your brand compared to competitors?
# 2. Which model and which shopping situation include your brand least often?
# 3. How do the models describe your brand (bag of words and analyst attributes)?
# 4. Where did keyword scanning and the analyst LLM disagree, and who was right?
# 5. Did the LLM brief state anything your numbers do not support?
# 6. One recommendation for the brand, and what data you would want next.

# End
