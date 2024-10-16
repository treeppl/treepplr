#' Prepare input for [tp_go()]
#'
#' @description
#' `tp_write` writes an JSON file to be used by [tp_go()].
#'
#' @param model_data_strlist a structured list coming from [tp_import()].
#'
#' @details
#' This function takes a structured list of TreePPL code and data files (more details in [tp_import()]) and write 2 files from them.
#'
#' @return None.
#' @export

tp_write <- function(model_data_strlist, hostrep_cor = FALSE){

  dir <- tp_tempdir()

  cat(model_data_strlist[[1]], file = paste0(dir, "input.tppl"))

  # write json with input data
  input_json <- RJSONIO::toJSON(model_data_strlist[[2]])
  if(hostrep_cor) {
    input_json <- .cor_export_num(input_json)
  }

  write(input_json, paste0(dir, "input.json"))
}

.cor_export_num <- function(JSON_str) {
  JSON_str <- str_replace_all(JSON_str, "age\":", "age\":!")
  JSON_str <-str_split_fixed(JSON_str, "!", n = Inf)
  JSON_res <- JSON_str[1]
  for (i in 2:length(JSON_str)) {
    if(!str_detect(str_sub(JSON_str[i],1,10), "\\.")){
      JSON_str[i] <- str_c(str_sub(JSON_str[i],1,9), ".0", str_sub(JSON_str[i],start=10))
    }
    JSON_res <- str_c(JSON_res, JSON_str[i])
  }
  JSON_res
}

#' Convert a phyjson_tree object to phyjson_tree list
#'
#' @description
#' `tp_phyjson_list` takes an object of class "phyjson_tree" and return a phyjson_tree list ready to be export as a JSON
#'
#' @param phyjson_tree an object of class "phyjson_tree".
#'
#' @return A structured list how contain a phyjson_tree
#' @export
#'
tp_phyjson_list <- function(phyjson_tree) {
  pjs_list <- list(.rec_phyjson_list(phyjson_tree$tree, phyjson_tree$root_index))
  names(pjs_list) = phyjson_tree$name

  return(pjs_list)
}

.rec_phyjson_list <- function(tree, row_index) {
  row <- tree[row_index, ]

  sub_pjs_list <- list("label" = row$Label, "age" = row$Age)
  if (row$Type != "Leaf") {
    sub_pjs_list <- c(sub_pjs_list,
                     list(
                       "left" = .rec_phyjson_list(tree, row$Left),
                       "right" = .rec_phyjson_list(tree, row$Right)
                     ))
  }

  pjs_list <- list("__constructor__" = row$Type, "__data__" = sub_pjs_list)

  return(pjs_list)
}
