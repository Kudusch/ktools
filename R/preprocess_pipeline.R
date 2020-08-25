#' @importFrom stats filter

f.removeWords <- function(str, stopwords) {
    x <- unlist(stringr::str_split(str, regex("[\\s/]")))
    return(paste(x[!x %in% stopwords], collapse = " "))
}
f.escapeRegex <- function(r) {
    stringi::stri_replace_all_regex(r, "\\(|\\)|\\[|\\]", "\\\\$0")
}

#' Remove Non-Word Characters
#'
#' This function preprocesses a character vector by removing non-word characters
#' and reports the mean number of characters before and after preprocessing.
#'
#' By default URLs, html-entities (&nbsp), digits-words, apostrophized words, and all punctuation are removed.
#'
#' Other preprocessing steps can be controlled via the arguments of the function.
#'
#' @param text A character vector that will be preprocessed.
#' @param rm.hashtags A logical, defining if #hashtags should be removed.
#' @param rm.mentions A logical, defining if @mentions should be removed.
#' @param rm.emoji A logical, defining if emoji should be removed.
#' @param rm.digitwords A logical, defining if all digits should be removed, including digitwords (e.g. 5G, T3, etc.)
#' @param join.hyphenation A logical, defining if hyphenated words should be joined.
#'
#' @return A preprocessed character vector.
#'
#' @examples
#'\dontrun{
#' preprocess.removeNonWordChars(
#' text,
#' rm.hashtags=FALSE,
#' rm.mentions=FALSE,
#' rm.emoji=FALSE,
#' rm.digitwords=FALSE,
#' join.hyphenation=FALSE)
#'}
#'
#' @export
preprocess.removeNonWordChars <- function(text, rm.hashtags=FALSE, rm.mentions=FALSE, rm.emoji=FALSE, rm.digitwords=FALSE, join.hyphenation=FALSE) {
    message("Mean number of chars per document at start: ", round(mean(str_length(text)), 1))

    # Remove URLS
    message("Removing URLS")
    text <- gsub("https?://.*?(\\s|$)", " ", text, ignore.case = TRUE)
    text <- gsub("(www.)?\\w+\\.de", " ", text, ignore.case = TRUE)
    text <- gsub("(www.)?\\w+\\.com", " ", text, ignore.case = TRUE)
    text <- gsub("(www.)?\\w+\\.org", " ", text, ignore.case = TRUE)

    if (rm.mentions) {
        # Remove @mentions
        message("Removing @mentions")
        text <- gsub("@\\S*", " ", text, ignore.case = TRUE)
    } else {
        message("Keeping @mentions")
        text <- gsub("@(\\S*)", " at_\\1 ", text, ignore.case = TRUE)
    }

    if (rm.hashtags) {
        # Remove #hashtags
        message("Removing #hashtags")
        text <- gsub("#\\S*", " ", text, ignore.case = TRUE)
    } else {
        message("Keeping #hashtags")
        text <- gsub("#(\\S*)", " ht_\\1 ", text, ignore.case = TRUE)
    }

    # Join apostrophe-words
    message("Joining apostrophe-words")
    text <- gsub("(\\w)['\u00B4\u2019]([trslm]\\w*)", "\\1", text, ignore.case = TRUE)

    if (join.hyphenation) {
        # Join hyphenated words
        message("Joining hyphenated words")
        text <- gsub("([a-zA-Z_])-([a-zA-Z_])", "\\1_\\2", text, ignore.case = TRUE)
    }

    # Remove html-characters
    message("Removing html-entities")
    text <- gsub("&(\\w+|\\#\\d+);", " ", text, ignore.case = TRUE)

    # Remove digits-words
    message("Removing digits")
    if (rm.digitwords) {
        message("Keeping digit-words")
        text <- gsub("(\\s|\\W)[0-9]+", " ", text, ignore.case = TRUE)
    } else {
        text <- gsub("(?<=\\W)[0-9]+(?=\\W)",
                     " ",
                     text,
                     ignore.case = TRUE,
                     perl = T)
    }

    # Remove punctuation
    message("Removing punctuation")
    if (rm.emoji) {
        regex_pattern <- paste("[^a-zA-Z\\x7f-\\xff0-9_" ,"]", sep = "")
    } else {
        message("Keeping emoji")
        df.emojis <- ktools::Emojis
        # Remove asterix and pound emoji as they interfere with regex
        df.emojis <- df.emojis[-(1366:1367),]
        df.emojis$Code_alt <- gsub("\\+", "000", df.emojis$Code)
        text <- gsub(pattern = "\uFE0F", " ", text, perl = TRUE, ignore.case = TRUE)
        text <- gsub(pattern = paste("(" ,paste(df.emojis$Emoji, collapse = "|"), ")", sep = ""), " \\1 ", text, perl = TRUE, ignore.case = TRUE)
        regex_pattern <- paste("[^a-zA-Z\\x7f-\\xff0-9_\\s", paste(df.emojis$Emoji, collapse = ""), "]+", sep = "")
        rm(df.emojis)
    }

    text <- gsub(pattern = regex_pattern, " ", text, perl = TRUE, ignore.case = TRUE)

    message("Mean number of chars per document at end: ", round(mean(str_length(text)), 1))
    return(text)
}

#' Remove Words From Text
#'
#' This function preprocesses a character vector by removing the words defined in \code{stopwords}.
#' and reports the mean number of characters before and after preprocessing.
#'
#' @param text A character vector that will be preprocessed.
#' @param stopwords A character vector containing words and phrases to be removed.
#'
#' @return A preprocessed character vector.
#'
#' @examples
#'\dontrun{
#' stopwords <- c("in", "and", "per se")
#' preprocess.removeStopwords(text, stopwords)
#'}
#'
#' @export
preprocess.removeStopwords <- function(text, stopwords) {
    message("Mean number of chars per document at start: ", round(mean(str_length(text)), 1))
    message("Removing stopwords (", length(stopwords), ")")
    text <- unlist(lapply(text, f.removeWords, stopwords = stopwords))
    message("Mean number of chars per document at end: ", round(mean(str_length(text)), 1))
    return(text)
}

#' Tidy a data.frame()
#'
#' This function is a wrapper for \code{unnest_tokens()} that filters emptpy tokens.
#'
#' @param df A dataframe that will be tidied.
#' @param text_col The column containing the text to be tidied.
#'
#' @return A tidy dataframe.
#'
#' @examples
#'\dontrun{
#' preprocess.genTidyDF(df, text_col)
#'}
#'
#' @export
preprocess.genTidyDF <- function(df, text_col) {
    text_col = enquo(text_col)
    df %>%
        unnest_tokens(
            word_token,
            !!text_col,
            token = "regex",
            pattern = "\\s") %>%
        filter(!str_detect(word_token, pattern = "^\\s*$"),
               !word_token == "",
               !grepl(pattern = "\\x{200D}", word_token)) %>%
        return(.)
}


