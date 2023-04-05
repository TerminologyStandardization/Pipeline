# .pipeline: A character vector specifying the sequence of string operations to be performed 
# in the string_operations() function.
.pipeline <- c(
  "asciify", "remove control characters (space)", "standardize quotes",
  "standardize hyphens (space)", "standardize slash (space)",
  "standardize back slash (space)", "standardize round bracket left",
  "standardize round bracket right", "standardize square bracket left",
  "standardize square bracket right", "standardize plus", "standardize ampersand",
  "standardize comma", "standardize colon", "standardize semicolon",
  "standardize currencies", "remove 's", "remove s'", "to lower",
  "standardize letter number (1)", "standardize letter number (2)",
  "remove space characters (space)", "trim"
)

#  string_ops(.string, .table_string): Function that performs a series of string operations 
#  on the input string .string using the operations listed in the .table_string argument. 
#  Returns the modified string.
string_ops <- function(.string, .table_string) {
  # Assign the input string to variable s
  s <- .string
  
  # Assign the input table of string operations to variable t
  t <- .table_string
  
  # Replace all occurrences of one or more whitespace characters with a single space in s
  s <- gsub("\\s+", " ", s)
  
  # Iterate through each row of the table t
  for (i in 1:nrow(t)) {
    # Check if the search column value in the current row is NA
    if (is.na(t$search[i])) {
      # If it is NA, call the corresponding function in the call column for the current row,
      # passing only s as the argument
      s <- t$call[[i]](s)
    } else {
      # If it is not NA, call the corresponding function in the call column for the current row,
      # passing s, the search value, and the replace value as arguments
      s <- t$call[[i]](s, t$search[i], t$replace[i])
    }
  }
  # Replace all occurrences of one or more whitespace characters with a single space in s again
  s <- gsub("\\s+", " ", s)
  
  # Return the modified string
  return(s)
}

# replace_words(.string, .table_replace, .tokenized = FALSE, .regex = FALSE): 
# Function that replaces words in .string based on the replacement rules specified in .table_replace. 
# If .tokenized is TRUE, the function assumes that the input string is already tokenized. 
# If .regex is TRUE, the function uses regular expressions for replacement. 
# Returns the modified string with replaced words.
replace_words <- function(.string, .table_replace, .tokenized = FALSE, .regex = FALSE) {
  
  # Check if the input string is not tokenized
  if (!.tokenized) {
    if (!.regex) {
      # Add space at the beginning and end of the input string and store it in variable s
      s <- paste0(" ", trimws(.string), " ")
      
      # Add space at the beginning and end of the first column of .table_replace and store it in t1
      t1 <- paste0(" ", trimws(.table_replace[[1]]), " ")
      
      # Add space at the beginning and end of the second column of .table_replace and store it in t2
      t2 <- paste0(" ", trimws(.table_replace[[2]]), " ")
      
      # Perform fixed string replacement of t1 with t2 in s, then remove leading/trailing whitespaces
      trimws(stringi::stri_replace_all_fixed(s, t1, t2,
        vectorize_all = FALSE
      ))
    } else {
      # Remove leading/trailing whitespaces of the input string and store it in variable s
      s <- trimws(.string)
      
      # Create regex pattern with word boundaries for the first column of .table_replace and store it in t1
      t1 <- paste0(
        "\\b", trimws(.table_replace[[1]]),
        "\\b"
      )
      
      # Remove leading/trailing whitespaces of the second column of .table_replace and store it in t2
      t2 <- trimws(.table_replace[[2]])
      
      # Perform regex-based string replacement of t1 with t2 in s, then remove leading/trailing whitespaces
      trimws(stringi::stri_replace_all_regex(s, t1, t2,
        vectorize_all = FALSE
      ))
    }
  } else {
    # If the input string is tokenized, assign it to variable s
    s <- .string
    # Assign the first and second columns of .table_replace to t1 and t2, respectively
    t1 <- .table_replace[[1]]
    t2 <- .table_replace[[2]]
    
    # Use qdapTools::lookup function to perform token-based replacement using t1 and t2
    qdapTools::lookup(s, t1, t2, missing = NULL)
  }
}

# .tab_string_ops: A data frame containing the string operations table loaded from a file.
.tab_string_ops  <- readr::read_rds("02_additional_lists/string_operations.rds")

# .str_op_pipeline: A data frame containing the specified string operations from the 
# .tab_string_ops table, based on the operations listed in the .pipeline vector.
.str_op_pipeline <- .tab_string_ops[purrr::map_int(.pipeline, ~ which(.tab_string_ops == .x)), ]

# string_operations(.string): Function that performs a series of string operations on the input 
# .string using the operations listed in the .str_op_pipeline data frame. Returns the modified string.
string_operations <- function(.string) {
  
  # The function is a pipeline of operations:
  # 
  .string %>%
    # 1. Remove any asterisk (*) at the end of the string using gsub
    gsub("\\*$", "", .) %>%
    # 2. Call the string_ops function with the input string and the predefined .str_op_pipeline
    string_ops(., .str_op_pipeline) %>%
    # 3. Replace the Cyrillic "a" character (Unicode code point \u0430) with the Latin "a" character
    gsub("\u0430", "a", .) %>%
    # 4. Remove leading and trailing whitespaces from the resulting string using trimws()
    trimws()
}

# get_adjustment_lists(): Function that reads multiple files and processes the data to 
# create a list of adjustment rules for text preprocessing. Returns a list of data frames 
# with adjustment rules.
get_adjustment_lists <- function() {
  
  ilst1 <- list(
    man_term_adj = "02_additional_lists/term_adjustment.xlsx" %>%
      openxlsx::read.xlsx() %>%
      dplyr::mutate(dplyr::across(c(term_orig, term_adj), ~ string_operations(.))) %>%
      dplyr::distinct(term_orig, term_adj, .keep_all = TRUE),
    us_uk = "02_additional_lists/us_uk.xlsx" %>%
      openxlsx::read.xlsx() %>%
      dplyr::mutate(dplyr::across(c(us, uk), ~ string_operations(.))) %>%
      dplyr::distinct(us, uk, .keep_all = TRUE),
    split = "02_additional_lists/split_terms.xlsx" %>%
      openxlsx::read.xlsx() %>%
      dplyr::mutate(across(c(unsplit, split), ~ string_operations(.))) %>%
      dplyr::distinct(unsplit, split, .keep_all = TRUE)
  )
  
  ilst2 <- list(
    lemma_raw = "02_additional_lists/20201207_195750_lemma.xlsx" %>%
      openxlsx::read.xlsx(),
    lemma = "02_additional_lists/20201207_195750_lemma.xlsx" %>%
      openxlsx::read.xlsx() %>% 
      dplyr::mutate(lemma = ifelse(!lemma_new == "0", lemma_new, lemma)) %>%
      dplyr::select(token, lemma) %>%
      dplyr::distinct() %>%
      dplyr::filter(!token == lemma),
    stop = "02_additional_lists/stopwords.xlsx" %>%
      openxlsx::read.xlsx() %>%
      dplyr::mutate(
        stop = stop %>%
          trimws() %>%
          string_operations() %>%
          replace_words(., ilst1[["man_term_adj"]]) %>%
          replace_words(., ilst1[["us_uk"]]) %>%
          replace_words(., ilst1[["split"]]) %>%
          string_operations()
      ) %>% 
      dplyr::distinct(stop, .keep_all = TRUE),
    non_gen_dic = openxlsx::read.xlsx("02_additional_lists/non_general_terms.xlsx")
  )
  
  ilst3 <- list(
    gen_dic = "02_additional_lists/2of12inf.txt" %>%
      readr::read_delim("\t", col_names = "term", col_types = cols("c")) %>%
      mutate(
        term = term %>%
          gsub("%", "", ., fixed = TRUE) %>%
          trimws() %>%
          string_operations() %>%
          replace_words(., ilst1[["man_term_adj"]]) %>%
          replace_words(., ilst1[["us_uk"]]) %>%
          replace_words(., ilst1[["split"]]) %>%
          string_operations()
      ) %>%
      dplyr::distinct(term, .keep_all = FALSE) %>%
      dplyr::filter(!term %in% ilst2[["non_gen_dic"]]$non_general_term) 
  )
  
  c(ilst1, ilst2, ilst3)
  
}


# tokenize_corpus(.txt, .lst_adj): Function that tokenizes the input text .txt using the 
# adjustment rules specified in the .lst_adj argument. Returns a tokenized and processed text data frame.
tokenize_corpus <- function(.txt) {
  
  # Call get_adjustment_lists() function and store the result in .lst_adj
  .lst_adj <- get_adjustment_lists()
  
  # Check if .txt is a path
  if (is.character(.txt)) {
    # If .txt is a path, read the text using readtext::readtext()
    tab_ <- readtext::readtext(.txt)
  } else {
    # If .txt is not a path, directly assign it to tab_
    tab_ <- .txt
  }
  
  tab_ %>%
    dplyr::mutate(
      # Apply string_ops function to the 'text' column
      text = string_ops(text, .str_op_pipeline),
      # Remove the .txt extension from the 'doc_id' column
      doc_id = gsub("\\.txt$", "", doc_id),
      # Replace words in the 'text' column using the 'split' list from .lst_adj
      text = replace_words(text, .lst_adj[["split"]], .regex = TRUE)
    ) %>%
    # Prepare the document for term counting using rTermCount::prep_document()
    rTermCount::prep_document(.fun_std = rTermCount::std_str) %>%
    dplyr::mutate(
      # Replace words in the 'token' column using the 'us_uk' list from .lst_adj
      token = replace_words(token, .lst_adj[["us_uk"]], .tokenized = TRUE),
      # Replace words in the 'token' column using the 'lemma' list from .lst_adj
      token = replace_words(token, .lst_adj[["lemma"]], .tokenized = TRUE)
    ) %>%
    dplyr::mutate(doc_id = gsub(".txt", "", doc_id, fixed = TRUE))
}

