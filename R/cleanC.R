urlregex <- "(?i)\\b((?:[a-z][\\w-]+:(?:/{1,3}|[a-z0-9%])|www\\d{0,3}[.]|[a-z0-9.\\-]+[.][a-z]{2,4}/)(?:[^\\s()<>]+|\\(([^\\s()<>]+|(\\([^\\s()<>]+\\)))*\\))+(?:\\(([^\\s()<>]+|(\\([^\\s()<>]+\\)))*\\)|[^\\s`!()\\[\\]{};:\\'\".,<>?]))"


#' @rdname clean
#' @importFrom Rcpp evalCpp
#' @useDynLib boostTest
#' @examples
#' cleanC("This is 1 sentence with 2.0 numbers in it, and one comma.")
#' cleanC("This is a $1,500,000 budget and $20bn cash and a $5 cigar.")
#' 
#' # for a vector of texts
#' cleanC(c("This is 1 sentence with 2.0 numbers in it, and one comma.", 
#'         "$1.2 billion was spent on text analysis in 2014."))
#'
#' \dontrun{# on a single long text
#' mobydick <- texts(corpus(textfile("~/Dropbox/QUANTESS/corpora/project_gutenberg/pg2701.txt")))
#' system.time(tmp <- cleanC(mobydick)) # .218 seconds
#' system.time(tmp <- clean(mobydick))  # .776 seconds}
#' 
#' @export
cleanC <- function(x) as.character(cleanCpp(x)[, 1])


#' simple cleaning of text before processing
#' 
#' @details minimal test \code{clean} that only removes digits and punctuation
#' for testing.
#' @rdname clean
#' @param x character vector to be cleaned
#' @return A character vector equal in length to the original texts, after cleaning.
#' @examples
#' clean("This is 1 sentence with 2.0 numbers in it, and one comma.")
#' clean("This is a $1,500,000 budget and $20bn cash and a $5 cigar.")
#' 
#' # for a vector of texts
#' clean(c("This is 1 sentence with 2.0 numbers in it, and one comma.", 
#'         "$1.2 billion was spent on text analysis in 2014."))
#' @export
clean <- function(x) {
    UseMethod("clean")
}


#' @rdname clean
#' @export
clean.character <- function(x) {

    # use "negative lookahead" to keep Twitter symbols, always keep "_"
    # remove other punctuation from POSIX [:punct:]
    x <- gsub("(?![_])[[:punct:]]", "", x, perl=TRUE)
    
    # amended regex removes currency stuff better, e.g.
    # clean("This is a $5m watch and $20bn budget and $100,000 in cash plus a $5 cigar.")
    #
    # 2nd part in alternation removes 1st 2nd 31st 43rd 3bis 101th etc.
    #
    # third part in alternation means don't remove digits if in a word, e.g. 4sure, crazy8
    # the third stuff is to remove thousands separators, e.g. 1,000,000 or 1.000.000
    # clean("nodigits crazy8 4sure 67 89b 1,000,000 1.023.496")
    # note: \u00A3 is pound sign, \u20AC is euro sign, \u00A2 is the cent sign
    x <- gsub("[$\u00A3\u20AC\u00A2][[:digit:]]\\w*|\\b[[:digit:]]+(st|nd|rd|d|th|bis)\\b|\\b([[:digit:]]+[,.]?)+\\b", "", x)
    x <- tolower(x)

    # convert 2+ multiple whitespaces into one
    x <- gsub("\\s{2,}", " ", x, perl=TRUE)
    # remove leading and trailing whitespace and return
    gsub("^\\s+|\\s+$", "", x)
}


