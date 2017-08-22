# Extract key phrases uses Microsoft Cognitive Services API for Key Phrase

library(httr)
library(rjson)


cogAuth <- function(key) {

    # Access key assignment for use in REST calls
    assign("keyCogServices", key, envir = envCogServices)

}

# Create the empty environment to store the key
envCogServices <- new.env(parent = emptyenv())



# Function for using Cognitive Services API
# Note: Can ONLY be used with keyPhrases OR sentiment
fnCogServicesBatch <- function(text.inputs, phrase.language = "en", endpoint = "keyPhrases") {

    # Coerce to character
    text.inputs <- as.character(text.inputs)

    # The URL for Key Phrases cognitive service
    url.cog.service <- paste("https://westus.api.cognitive.microsoft.com/text/analytics/v2.0/", endpoint, sep = "")

    # Create empty list in proper structure for request
    list.docs <- list(documents = list(list()))
    num.max <- length(text.inputs)

    # For loop (unfortunately); 
    for (i in 1:num.max) {
        list.docs$documents[[i]] <- list(language = phrase.language, id = i, text = text.inputs[i])
    }

    # Convert the list to JSON for posting
    json.body <- toJSON(list.docs)

    # Post the call to the REST API
    raw.response <- POST(url.cog.service, add_headers(.headers = c("Ocp-Apim-Subscription-Key" = envCogServices$keyCogServices, "Content-Type" = "application/json")), body = json.body)

    # Read in the response as character
    json.response <- readBin(raw.response$content, "character")

    # Convert the character, now JSON, response back to a list
    list.response <- fromJSON(json.response)

    # Extract the first element of each of these
    list.phrases <- lapply(list.response$documents, "[[", 1)

    # Unlist to flatten all topics (does this break with score?)
    vec.words <- unlist(list.phrases)

    # Important!
    tolower(vec.words)

}
