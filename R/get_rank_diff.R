#' Calculate the if-idf rank differences.
#'
#' @description Implementation of a simple algorithm to find the difference between two corpora. The relative importance of the terms in \code{tidy_x} are compared to the importance \code{tidy_y}.
#' @param tidy_x A data frame created by tidytext::unnest_tokens()
#' @param tidy_y A data frame created by tidytext::unnest_tokens()
#' @param id string. Name of id variable. Defaults to "id".
#' @param token string. Name of token variable. Defaults to "token".
#' @return Returns a data frame.
#' @examples
#'\dontrun{
#' rank_diff <- get_rank_diff(tidy_x, tidy_y, id = "id", token = "token")
#'}
#'
#' @importFrom stats sd
#' @importFrom utils flush.console
#' @importFrom magrittr %>%
#' @importFrom dplyr n
#'
#' @export
get_rank_diff <- function(tidy_x, tidy_y, id = "id", token = "token") {
    rank_dif_raw <- get_corpus_rank_diff(tidy_x, tidy_y, id = id, token = token)
    rank_dif_raw <- as.data.frame(t(matrix(unlist(rank_dif_raw), nrow=length(unlist(rank_dif_raw[1])))))
    rank_dif <- data.frame(id = as.numeric(rank_dif_raw$V1), rank_dif = as.numeric(levels(rank_dif_raw$V2))[rank_dif_raw$V2], stringsAsFactors=FALSE)
    rank_dif <- rank_dif[order(rank_dif$id), ]
    row.names(rank_dif) <- rank_dif$id
    message("")
    message("Rank differences of ", round(mean(rank_dif$rank_dif), 2), " \u00B1", round(sd(rank_dif$rank_dif), 2), " were calculated.")
    return(rank_dif)
}

get_token_rank_diff <- function(token_row, y_df, rmax) {
    rank_dif <- as.numeric(token_row["rank"]) - y_df[y_df$token == as.character(token_row["token"]), ]$rank
    if (length(rank_dif) == 0) {
        rank_dif <- rmax
    }
    rank_dif <- abs(rank_dif) / rmax
    return(rank_dif)
}

get_doc_rank_diff <- function(doc_id, freq_df) {
    freq_by_rank <- freq_df
    sub <- freq_by_rank[which(freq_by_rank$doc_id == doc_id),]
    x <- sub[which(sub$corpus_id == "x"),]
    y <- sub[which(sub$corpus_id == "y"),]
    rmax <- max(abs(c(max(x$rank) - 1, 1 - max(y$rank))))
    rank_diffs <- apply(x, MARGIN = 1, FUN = get_token_rank_diff, y_df = y, rmax = rmax)
    message(round(((match(doc_id, unique(freq_by_rank$doc_id)))/(length(unique(freq_by_rank$doc_id))))*100, 1), "%", "\r",appendLF=FALSE)
    flush.console()
    return(c(doc_id, mean(unlist(rank_diffs))))
}

get_corpus_rank_diff <- function(tidy_x, tidy_y, id = "id", token = "token") {
    tidy_x$id <- paste(tidy_x[[id]], "x", sep = "_")
    tidy_x$token <- tidy_x[[match(token, names(tidy_x))]]
    tidy_y$id <- paste(tidy_y[[id]], "y", sep = "_")
    tidy_y$token <- tidy_y[[match(token, names(tidy_y))]]
    tidy_combined <- rbind(tidy_x, tidy_y) %>% dplyr::count(id, token, sort = TRUE)
    tidy_sum <- tidy_combined %>%
        dplyr::group_by(id) %>%
        dplyr::summarize(total = sum(n))
    tidy_combined <- dplyr::left_join(tidy_combined, tidy_sum, by = "id")
    tidy_combined <- tidy_combined %>%
        tidytext::bind_tf_idf(token, id, n)
    freq_by_rank <- tidy_combined %>%
        dplyr::group_by(id) %>%
        dplyr::mutate(rank = rank(-tf_idf))
    id_vec <- stringr::str_split(freq_by_rank$id, "_")
    id_vec <- unlist(id_vec)
    freq_by_rank$doc_id <- id_vec[seq(1,length(id_vec), 2)]
    freq_by_rank$corpus_id <- id_vec[seq(2,length(id_vec), 2)]
    message("Finding rank differences for ", length(unique(freq_by_rank$doc_id)), " documents.")
    return(lapply(unique(freq_by_rank$doc_id), get_doc_rank_diff, freq_df = freq_by_rank))
}
