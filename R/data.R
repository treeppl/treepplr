
#' List expected input variables for a model
#'
#' @param model A TreePPL model
#'
#' @returns The expected input data for a given TreePPL model.
#' @export
#'
tp_expected_input <- function(model) {

  #### under development here and in treeppl ####

}



#' Import data for TreePPL program
#'
#' @description
#' Prepare data input for [treepplr::tp_run()].
#'
#' @param data_input One of the following options:
#'   * A named list (or structured list) containing TreePPL data, OR
#'   * The full path of a multiple sequence alignment in fasta (.fasta, .fas)
#'   or nexus (.nexus, .nex) format, OR
#'   * For test data, a string with the name of a model supported by treepplr
#' (see [treepplr::tp_model_library()]).
#' @param data_file_name An optional name for the file created. Ignored if `data_input`
#' is the name of a model from the TreePPL library.
#' @param dir The directory where you want to save the data file in JSON format.
#' Default is [base::tempdir()]. Ignored if `data_input` is the name of a model
#' from the TreePPL library.
#'
#' @return The path for the data file that will be used by [treepplr::tp_run()].
#'
#' @details
#' `data_input`: The name of each list element has to match the name of a model
#' input, which is defined in the TreePPL model code.
#'
#' @export
#' @examples
#' \donttest{
#' # Example using a model name supported by TreePPL
#' input <- tp_data("tree_inference")
#' input
#'
#' # Example using an internal FASTA file (same input data as before, but in fasta format)
#' fasta_file <- system.file("extdata", "tree_inference.fasta", package = "treepplr")
#' input <- tp_data(fasta_file)
#' input
#'}
#'
tp_data <- function(data_input, data_file_name = "tmp_data_file", dir = tp_tempdir()) {

  #### TODO data inputs have to be named as it is expected in the model ####

  if (assertthat::is.string(data_input)) {

    if (grepl("\\.(fasta|fas|nexus|nex)$", data_input, ignore.case = TRUE)) {
      data <- read_aln(data_input)
      data_list <- tp_list(list(data))
      data_path <- tp_write_data(data_list, data_file_name, dir)

    } else {

      res_lib <- tp_find_data(data_input)

      # model_input has the name of a known model
      if (length(res_lib) != 0) {
        data_path <- res_lib

      } else {
        stop("Invalid input string.")
      }
    }

    # OR data_input is a list (or a structured list)
  } else if (is.list(data_input)) {

    # flatten the list
    data_list <- tp_list(data_input)
    # write json with input data
    data_path <- tp_write_data(data_list, data_file_name, dir)

  } else {
    stop("Unknow R type (not a valid path, known data model, or list")
  }

  return(data_path)

}


#' Write data to file
#'
#' @param data_list A named list of data input
#' @param data_file_name An optional name for the file created
#' @param dir The directory where you want to save the data file in JSON format.
#' Default is [base::tempdir()].
#'
#' @returns The path to the created file
#'
#' @export
tp_write_data <- function(data_list, data_file_name = "tmp_data_file", dir = tp_tempdir()) {

  input_json <- jsonlite::toJSON(data_list, auto_unbox=TRUE)
  path <- paste0(dir, data_file_name, ".json")
  write(input_json, file = path)

  return(path)

}


#' Create a flat list
#'
#' @description
#' `tp_list` takes a variable number of arguments and returns a list.
#'
#' @param ... Variadic arguments (see details).
#'
#' @details
#' This function takes a variable number of arguments, so that users can pass as
#' arguments either independent lists, or a single structured
#' list of list (name_arg = value_arg).
#'
#' @return A list.
tp_list <- function(...) {
  dotlist <- list(...)

  if (length(dotlist) == 1L && is.list(dotlist[[1]])) {
    dotlist <- dotlist[[1]]
  }

  dotlist
}



# UTILS: Data conversion


## Sequence data

# Read alignment in FASTA or NEXUS (for tree inference)
read_aln <- function(file) {
  # define the encoding
  # NB: everything that is not ACTG will be replace with gap ("-") within the function
  base_code <- c(
    "A" = 0,
    "C" = 1,
    "G" = 2,
    "T" = 3,
    "-" = 4
  )

  # Print an error message if the input is not in fasta or nexus
  if (!grepl("\\.(fasta|fas|nexus|nex)$", file, ignore.case = TRUE)) {
    stop("Please, provide an input file in fasta or nexus format")
  }

  # If the input is a FASTA file
  else if (grepl("\\.(fasta|fas)$", file, ignore.case = TRUE)) {
    raw <- readLines(file, warn = FALSE)
    raw <- raw[nzchar(raw)] # remove empty lines, if any
    #nm <- gsub(">", "", raw[grepl(">", raw)]) # sequence names
    sq <- raw[!grepl(">", raw)] # sequences
    # sequence matrix
    sq_list <- strsplit(sq, "")
    sq_mat <- do.call(rbind, sq_list)
    sq_mat <- toupper(sq_mat)
    sq_mat[] <- gsub("[^ACGT]", "-", sq_mat, ignore.case = TRUE) # replace everything that is not ACTG with "-"
    # numerical matrix
    num_mat <- matrix(
      base_code[sq_mat],
      nrow = nrow(sq_mat),
      ncol = ncol(sq_mat)
      #dimnames = list(nm, NULL)
    )
    # final list
    res <- list(data = num_mat)
    return(res)

    # If the input is a NEXUS file
  } else if (grepl("\\.(nexus|nex)$", file, ignore.case = TRUE)) {
    # nexus matrix block
    raw <- readLines(file, warn = FALSE)
    raw <- raw[nzchar(raw)] # remove empty lines, if any
    start <- grep("^[[:space:]]*matrix[[:space:]]*$", tolower(raw))
    end <- grep(";", raw)
    end <- end[end > start][1]
    mat <- raw[(start + 1):(end - 1)] # extract contents
    mat <- trimws(mat)
    mat <- mat[nzchar(mat)] # remove empty entries
    #nm <- sub("\\s+.*$", "", mat) # sequence names (split on the 1st white space)
    sq <- sub("^\\S+\\s+", "", mat) # sequences
    # sequence matrix
    sq_list <- strsplit(sq, "")
    sq_mat <- do.call(rbind, sq_list)
    sq_mat <- toupper(sq_mat)
    sq_mat[] <- gsub("[^ACGT]", "-", sq_mat, ignore.case = TRUE) # replace everything that is not ACTG with "-"
    # numerical matrix
    num_mat <- matrix(
      base_code[sq_mat],
      nrow = nrow(sq_mat),
      ncol = ncol(sq_mat)
      #dimnames = list(nm, NULL)
    )
    # final list
    res <- list(data = num_mat)
    return(res)
  }
}

# Read alignment in FASTA or NEXUS (for tree inference)
read_aln <- function(file) {
  # define the encoding
  # NB: everything that is not ACTG will be replace with gap ("-") within the function
  base_code <- c(
    "A" = 0,
    "C" = 1,
    "G" = 2,
    "T" = 3,
    "-" = 4
  )

  # Print an error message if the input is not in fasta or nexus
  if (!grepl("\\.(fasta|fas|nexus|nex)$", file, ignore.case = TRUE)) {
    stop("Please, provide an input file in fasta or nexus format")
  }

  # If the input is a FASTA file
  else if (grepl("\\.(fasta|fas)$", file, ignore.case = TRUE)) {
    raw <- readLines(file, warn = FALSE)
    raw <- raw[nzchar(raw)] # remove empty lines, if any
    #nm <- gsub(">", "", raw[grepl(">", raw)]) # sequence names
    sq <- raw[!grepl(">", raw)] # sequences
    # sequence matrix
    sq_list <- strsplit(sq, "")
    sq_mat <- do.call(rbind, sq_list)
    sq_mat <- toupper(sq_mat)
    sq_mat[] <- gsub("[^ACGT]", "-", sq_mat, ignore.case = TRUE) # replace everything that is not ACTG with "-"
    # numerical matrix
    num_mat <- matrix(
      base_code[sq_mat],
      nrow = nrow(sq_mat),
      ncol = ncol(sq_mat)
      #dimnames = list(nm, NULL)
    )
    # final list
    res <- list(data = num_mat)
    return(res)

    # If the input is a NEXUS file
  } else if (grepl("\\.(nexus|nex)$", file, ignore.case = TRUE)) {
    # nexus matrix block
    raw <- readLines(file, warn = FALSE)
    raw <- raw[nzchar(raw)] # remove empty lines, if any
    start <- grep("^[[:space:]]*matrix[[:space:]]*$", tolower(raw))
    end <- grep(";", raw)
    end <- end[end > start][1]
    mat <- raw[(start + 1):(end - 1)] # extract contents
    mat <- trimws(mat)
    mat <- mat[nzchar(mat)] # remove empty entries
    #nm <- sub("\\s+.*$", "", mat) # sequence names (split on the 1st white space)
    sq <- sub("^\\S+\\s+", "", mat) # sequences
    # sequence matrix
    sq_list <- strsplit(sq, "")
    sq_mat <- do.call(rbind, sq_list)
    sq_mat <- toupper(sq_mat)
    sq_mat[] <- gsub("[^ACGT]", "-", sq_mat, ignore.case = TRUE) # replace everything that is not ACTG with "-"
    # numerical matrix
    num_mat <- matrix(
      base_code[sq_mat],
      nrow = nrow(sq_mat),
      ncol = ncol(sq_mat)
      #dimnames = list(nm, NULL)
    )
    # final list
    res <- list(data = num_mat)
    return(res)
  }
}





## Phylogenetic trees

#' Convert phylo obj to TreePPL tree
#'
#' @description
#' `tp_phylo_to_tpjson` takes an object of class `phylo` and returns
#' a TreePPL json str ready to print in a data file for TreePPL use.
#'
#' @param phylo_tree an object of class [ape::phylo].
#' @param age a string that determines the way the age of the nodes are
#' calculated (default "branch-length").
#'
#' * "branch-length" : Root's age = NA, branch-length from the parent branch
#' * "root-to-tip" : Root's age = 0.0, cumulative branch-length from root
#' * "tip-to-root" : Leaf's age = 0.0, cumulative branch-length from leaf
#'
#' @return A TreePPL json str
#'
#' @export
tp_phylo_to_tpjson <- function(phylo_tree, age = "") {

  name <- "tree"
  root_tree <- tp_phylo_to_tppl_tree(phylo_tree)

  if (age != "branch-length") {
    tree <- tree_age_cumul(root_tree[[2]], root_tree[[1]], age)
  }

  json_df <- tp_list(name = name,
                     tree = tree,
                     root_index = root_tree[[1]])

  tp_treeppl_json(json_df)
}

#' Convert phylo to a tppl_tree
#'
#' @description
#' `tp_phylo_to_tppl_tree` takes an object of class `phylo` and returns
#' a tppl_tree.
#'
#' @param phylo_tree an object of class [ape::phylo].
#'
#' @return A pair (root index, tppl_tree)
tp_phylo_to_tppl_tree <- function(phylo_tree) {
  name <- deparse(substitute(phylo_tree))

  df_ <- tidytree::as_tibble(phylo_tree)

  tree <- data.frame(matrix(
    c(NA, NA, 0.0, NA, NA),
    ncol = 5,
    nrow = nrow(df_),
    byrow = TRUE
  ))
  colnames(tree) <- c("Type", "Label", "Age", "Left", "Right")

  num_leaf <- nrow(df_) - phylo_tree$Nnode

  for (i in seq_len(nrow(df_))) {
    row <- df_[i, ]
    tree[row$node, "Label"] <- row$node
    tree[row$node, "Age"] <- row$branch.length

    if (row$parent == row$node) {
      tree[row$node, "Type"] <- "Node"
      root_index <- row$node
    } else {
      if (i <= num_leaf) {
        tree[row$node, "Type"] <- "Leaf"
      } else {
        tree[row$node, "Type"] <- "Node"
      }
      if (is.na(tree[row$parent, "Left"])) {
        tree[row$parent, "Left"] <- row$node
      } else {
        tree[row$parent, "Right"] <- row$node
      }
    }
  }
  list(root_index,tree)
}

#' Calculate age in a tppl_tree
#'
#' @description
#' `tree_age_cumul` takes an tppl_tree and returns a json with
#' branch length accumulation as node's age.
#'
#' @param tree an tppl_tree create with [treepplr::tp_phylo_to_tppl_tree].
#' @param root_index the index of the root in the tppl_tree.
#' @param age a string that determine the way the age of the node are
#' calculated (default "branch-length").
#'
#' * "root-to-tip" : Root's age = 0.0, cumulative branch-length from root
#' * "tip-to-root" : Leaf's age = 0.0, cumulative branch-length from leaf
#'
#' @return A tppl_tree

tree_age_cumul <- function(tree, root_index, age = "branch-length") {
  age_cumul <- rep(length(tree$Type), 0.0)
  down <- TRUE
  going_left <- c(TRUE, NA)
  i <- root_index
  if (age == "root-to-tip") {
    tree[i, "Age"] <- 0.0
    age_cumul[i] <- 0.0
  }
  parent <- c()
  while (down || !is.na(going_left[1]) || (i != root_index)) {
    if (down) {
      if (tree[i, "Type"] == "Node") {
        parent <- c(i, parent)
        if (going_left[1]) {
          i <- tree[i, "Left"]
        } else {
          i <- tree[i, "Right"]
        }
        going_left <- c(TRUE, going_left)
        if (age == "root-to-tip") {
          age_cumul[i] <- age_cumul[parent[1]] + tree[i, "Age"]
        }
      } else {
        down <- FALSE
        going_left <- going_left[-1]
        if (age == "tip-to-root") {
          age_cumul[i] <- 0.0
        }
      }
    } else {
      i <- parent[1]
      parent <- parent[-1]
      if (going_left[1]) {
        down <- TRUE
        going_left[1] <- FALSE
        if (age == "tip-to-root") {
          age_cumul[i] <- age_cumul[tree[i, "Left"]] + tree[tree[i, "Left"], "Age"]
        }
      } else {
        down <- FALSE
        going_left <- going_left[-1]
      }
    }
  }
  tree$Age <- age_cumul
  tree
}

#' Convert a tppl_tree to TreePPL json str
#'
#' @description
#' `tp_treeppl_json` takes an tppl_tree create with [treepplr::tp_phylo_to_tppl_tree]
#'  and returns a list ready to be exported with JSON
#'
#' @param tree an object of class json.
#'
#' @return A TreePPL json str
#' @export
#'
tp_treeppl_json <- function(tree) {
  pjs_list <- list(rec_tree_list(tree$tree, tree$root_index))
  names(pjs_list) <- tree$name

  pjs_list
}

rec_tree_list <- function(tree, row_index) {
  row <- tree[row_index, ]

  sub_pjs_list <- list("label" = row$Label, "age" = row$Age)
  if (row$Type != "Leaf") {
    sub_pjs_list <- c(sub_pjs_list,
                      list(
                        "left" = rec_tree_list(tree, row$Left),
                        "right" = rec_tree_list(tree, row$Right)
                      ))
  }

  pjs_list <- list("__constructor__" = row$Type, "__data__" = sub_pjs_list)

  pjs_list
}



#' Convert TreePPL multi-line JSON to R phylo/multiPhylo object with associated
#' weights
#'
#' @description
#' `tp_json_to_phylo` takes the path to a TreePPL json output and returns an
#' object of class `phylo`.
#'
#' @param json_out One of two options:
#'   * A list of TreePPL output in parsed JSON format (output from
#'   [treepplr::tp_run()]), OR
#'   * The full path of the json file containing the TreePPL output.
#'
#' @return A list with two elements:
#'         $trees: A 'phylo' object (if one tree) or 'multiPhylo' object (if multiple).
#'         $weights: A numeric vector of sample weights.
#' @export
tp_json_to_phylo <- function(json_out) {

  res <- try(file.exists(json_out), silent = TRUE)
  # If path exists, import output from file
  if (!is(res, "try-error") && res) {

    ## Read lines and parse each line as a separate JSON object
    raw_lines <- readLines(json_out, warn = FALSE)

    # Filter out empty lines just in case
    raw_lines <- raw_lines[raw_lines != ""]

    json_list <- lapply(raw_lines, function(x) {
      jsonlite::fromJSON(x, simplifyVector = FALSE)})

    # If path doesn't exist, then it should be a list
  } else if (is.list(json_out)) {
    json_list <- json_out
  } else {
    stop("Incorrect input format")
  }

  ## Process each tree in the list
  newick_strings <- sapply(json_list, function(sweep) {

    # loop over all particles within the sweep
    samples <- sweep$samples

    sweep_string <- sapply(samples, function(particle) {

      # Extract the root age
      root_data <- particle[[1]]$`__data__`
      root_age  <- root_data$age

      # Start recursion from the children of the root.
      # The root itself does not have a branch length in standard Newick,
      # but it provides the 'parent_age' for its immediate children.

      left_str  <- build_newick_node(root_data$left, root_age)
      right_str <- build_newick_node(root_data$right, root_age)

      # Construct final Newick string: (ChildL, ChildR);
      paste0("(", left_str, ",", right_str, ");")
    })

  })

  # 4. Convert to phylo
  trees <- ape::read.tree(text = newick_strings)

  # 5. Get weight for each tree
  nweight_matrix <- sapply(json_list, function(sweep) {

    nconst <- sweep$normConst
    logweights <- unlist(sweep$weights)
    log_nw <- nconst + logweights

  })

  nweights <- as.vector(nweight_matrix)
  norm_weights <- exp(nweights - max(nweights))

  return(list(trees = trees, weights = norm_weights))
}


# 2. Recursive function to build Newick string
# 'parent_age' is passed down from the caller
build_newick_node <- function(node, parent_age) {

  type <- node[["__constructor__"]]
  data <- node[["__data__"]]

  # Ensure ages are numeric
  parent_age <- as.numeric(parent_age)

  if (type == "Leaf") {
    # Rule: Leaf branch length is the age of its parent node
    len <- parent_age

    if (is.null(data$label)){
      label <- data$index
    } else {
      label <- data$label
    }

    # Return "Label:Length"
    return(paste0(label, ":", len))

  } else if (type == "Node") {
    # Rule: Internal node branch length = Parent Age - Own Age
    current_age <- as.numeric(data$age)
    len <- parent_age - current_age

    # Recursively process children, passing CURRENT age as their 'parent_age'
    left_str <- build_newick_node(data$left, current_age)
    right_str <- build_newick_node(data$right, current_age)

    # Return "(Left,Right):Length"
    return(paste0("(", left_str, ",", right_str, "):", len))
  }
}


# Function to ladderize tree and correct tip label sequence
ladderize_tree <- function(tree, temp_file = "temp", orientation = "left"){
  if(file.exists(paste0("./", temp_file))){
    stop("The chosen temporary file exists! Please choose an other temp_file name")
  }
  if(orientation == "left"){
    right <- FALSE
  }else{
    right <- TRUE
  }
  tree_temp <- ape::ladderize(tree, right = right)
  ape::write.tree(tree_temp, file = paste0("./", temp_file, ".tre"))
  tree_lad <- ape::read.tree(paste0("./", temp_file, ".tre"))
  file.remove(paste0("./", temp_file, ".tre"))
  return(tree_lad)
}





