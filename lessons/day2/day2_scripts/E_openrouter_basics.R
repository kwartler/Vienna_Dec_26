#' Ted Kwartler
#' Dec 12, 2026
#' Purpose: Call large language models through the OpenRouter API, then wrap
#'          the call in a function we reuse for the GEO brand activity.
#' Docs: https://openrouter.ai/docs

# library & options
library(httr)
library(jsonlite)

# ---------------------------------------------------------------------------
# 0. YOUR KEY
# ---------------------------------------------------------------------------
# Keys live in your .Renviron file, never in a script. See keys.example.md
apiKey <- Sys.getenv('OPENROUTER_API_KEY')
nchar(apiKey) > 0 # TRUE means R found your key; FALSE means restart R

# ---------------------------------------------------------------------------
# 1. ASK THE SERVER WHAT IT HAS: the model list (no key needed)
# ---------------------------------------------------------------------------
modelResp <- GET('https://openrouter.ai/api/v1/models')
status_code(modelResp) # 200 means OK

# The server answers in JSON; fromJSON turns it into a data frame
models <- fromJSON(content(modelResp, as = 'text', encoding = 'UTF-8'))$data
nrow(models)

# Prices are in USD per token; multiply to get USD per million tokens
models$inputPerM  <- as.numeric(models$pricing$prompt) * 1000000
models$outputPerM <- as.numeric(models$pricing$completion) * 1000000

# Cheapest paid models (free models have tight rate limits in a full classroom)
paid <- models[models$inputPerM > 0, ]
head(paid[order(paid$inputPerM), c('id', 'inputPerM', 'outputPerM')], 15)

# Search for a provider
paid[grep('openai|google|mistral', paid$id), c('id', 'inputPerM', 'outputPerM')]

# ---------------------------------------------------------------------------
# 2. ONE REQUEST, ONE RESPONSE
# ---------------------------------------------------------------------------
llmModel <- 'openai/gpt-4o-mini' # any id from the list above
url      <- 'https://openrouter.ai/api/v1/chat/completions'

# The request: who we are (header) and what we want (body)
headers <- add_headers(Authorization  = paste('Bearer', apiKey),
                       `Content-Type` = 'application/json')
body <- list(model       = llmModel,
             messages    = list(list(role    = 'user',
                                     content = 'In one sentence, what is Vienna known for?')),
             temperature = 0.7,
             max_tokens  = 200)

# Look at the JSON we are about to send
toJSON(body, auto_unbox = TRUE, pretty = TRUE)

# Send it
resp <- POST(url, headers, body = toJSON(body, auto_unbox = TRUE))
status_code(resp) # 200 OK, 401 bad key, 402 out of credit, 429 too many requests

# The response is nested JSON; content() turns it into an R list
out <- content(resp)
names(out)
out$choices[[1]]$message$content # the answer
out$usage                        # tokens in and out, which is what you pay for

# ---------------------------------------------------------------------------
# 3. CHANGE THE REQUEST, NOT THE CODE
# ---------------------------------------------------------------------------
# A system message sets the role; temperature sets how much answers vary
body$messages <- list(list(role = 'system', content = 'You are a cynical travel critic.'),
                      list(role = 'user',   content = 'In one sentence, what is Vienna known for?'))
body$temperature <- 1.2
resp <- POST(url, headers, body = toJSON(body, auto_unbox = TRUE))
content(resp)$choices[[1]]$message$content

# Swap the model: same request, a different company's model
body$model <- 'google/gemini-2.5-flash'
resp <- POST(url, headers, body = toJSON(body, auto_unbox = TRUE))
content(resp)$choices[[1]]$message$content

# ---------------------------------------------------------------------------
# 4. WRAP IT IN A FUNCTION: askLLM()
# ---------------------------------------------------------------------------
askLLM <- function(prompt,
                   model       = 'openai/gpt-4o-mini',
                   system      = NULL,
                   temperature = 0.7,
                   maxTokens   = 300,
                   key         = Sys.getenv('OPENROUTER_API_KEY')) {

  # Build the messages; the system message is optional
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

  # Return the text when it worked, NA with a warning when it did not
  if (status_code(resp) == 200) {
    answer <- content(resp)$choices[[1]]$message$content
  } else {
    warning(paste('Status', status_code(resp), 'from', model, ':',
                  content(resp)$error$message))
    answer <- NA
  }
  return(answer)
}

askLLM('Name three things Vienna is famous for.')
askLLM('Name three things Vienna is famous for.', system = 'Answer like a pirate.')

# ---------------------------------------------------------------------------
# 5. SAME QUESTION, MANY MODELS: the pattern behind the GEO activity
# ---------------------------------------------------------------------------
modelList <- c('openai/gpt-4o-mini',
               'google/gemini-2.5-flash',
               'mistralai/mistral-small-3.2-24b-instruct')
question  <- 'What are the best running shoe brands for beginners?'

results <- data.frame(model = modelList, answer = NA)
for (i in 1:length(modelList)) {
  print(paste('Asking', modelList[i]))
  results$answer[i] <- askLLM(question, model = modelList[i])
  Sys.sleep(0.5) # be polite to the server
}
results

# Which models mention a brand at all?
results$mentionsNike <- grepl('nike', results$answer, ignore.case = TRUE)
results[, c('model', 'mentionsNike')]

# Save the answers so you never pay for the same call twice
dir.create('personalFiles', showWarnings = FALSE)
saveRDS(results, 'personalFiles/openrouter_results.rds')

# End
