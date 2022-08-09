#' Parse an html table into a list.
#' @description Largely the same as \code{rvest::html_table()} with additional code to
#' allow multiple lines & changed so parsed into a list.
#' @param x A node, node set or document.
#' @param header Use first row as header? If `NA`, will use first row
#'   if it consists of `<th>` tags.
#' @param trim Remove leading and trailing whitespace within each cell?
#' @param fill If `TRUE`, automatically fill rows with fewer than
#'   the maximum number of columns with `NA`s.
#' @param dec The character used as decimal mark.
#' @keywords internal
#' @noRd


html_table <- function(x, header = NA, trim = TRUE, fill = FALSE, dec = ".") {
  UseMethod("html_table")
}

#' @export
html_table.xml_document <- function(x, header = NA, trim = TRUE, fill = FALSE,
                                    dec = ".") {
  tables <- xml2::xml_find_all(x, ".//table")
  lapply(tables, html_table, header = header, trim = trim, fill = fill, dec = dec)
}


#' @export
html_table.xml_nodeset <- function(x, header = NA, trim = TRUE, fill = FALSE,
                                   dec = ".") {
  # FIXME: guess useful names
  lapply(x, html_table, header = header, trim = trim, fill = fill, dec = dec)
}

#' @export
# ML - section used for reptiledb
html_table.xml_node <- function(x, header = NA, trim = TRUE,
                                fill = FALSE, dec = ".") {

  stopifnot(rvest::html_name(x) == "table")

  # Throw error if any rowspan/colspan present
  rows <- rvest::html_nodes(x, "tr")
  n <- length(rows)
  cells <- lapply(rows, rvest::html_nodes, xpath = ".//td|.//th")

  ncols <- lapply(cells, rvest::html_attr, "colspan", default = "1")
  ncols <- lapply(ncols, as.integer)
  nrows <- lapply(cells, rvest::html_attr, "rowspan", default = "1")
  nrows <- lapply(nrows, as.integer)

  p <- unique(vapply(ncols, sum, integer(1)))
  maxp <- max(p)

  if (length(p) > 1 & maxp * n != sum(unlist(nrows)) &
      maxp * n != sum(unlist(ncols))) {
    # then malformed table is not parsable by smart filling solution
    if (!fill) { # fill must then be specified to allow filling with NAs
      stop("Table has inconsistent number of columns. ",
           "Do you want fill = TRUE?", call. = FALSE)
    }
  }
  cells <- cells[1:11] # exclude external links

  # ML old voodoo code
  values <-
    cells %>%
    lapply(
      function(z){
        z <-
          z %>%
          lapply(
            function(zz){
              zz <-
                zz %>%
                as.character() %>%
                stringr::str_split('\n', simplify = T) %>%
                lapply(
                  function(zzz){
                    zzz <-
                      zzz %>%
                      gsub("[<>]", "QQQ", .) %>%
                      stringr::str_split('QQQbrQQQ', simplify = T)
                  }
                ) %>%
                lapply(
                  function(zzz){
                    zzz <-
                      zzz %>%
                      gsub("QQQ.QQQ", "", .) %>%
                      gsub("QQQ..QQQ", "", .) %>%
                      gsub("QQQ...QQQ", "", .) %>%
                      gsub("amp;", "", .) %>%
                      stringr::str_trim()
                  }
                ) %>%
                .[. != ""]
            }
          )
        if(z[[1]] != "References"){
          z <-
            z %>%
            unlist(recursive = F) %>%
            .[. != ""]
        }else{
          z[[2]] <-
            z[[2]] %>%
            unlist() %>%
            sapply(
              function(zz){
                zz %>%
                  gsub("QQQa href=", "", .) %>%
                  gsub(" target=\"_blank\"QQQ", "", .) %>%
                  gsub("get paper here", "", .)%>%
                  gsub("\"", "'", .) %>%
                  gsub("amp;", "", .)
              }
            ) %>%
            unname()
          z[[1]] <-
            z[[1]] %>%
            unlist()
        }
        return(z)
      }
    ) %>%
    lapply(
      function(z){
        out <- z[2]
        names(out) <-
          z[1] %>%
          gsub(" ", "_", .)
        return(out)
    }) %>%
    # get rid of top level of list
    unlist(recursive = F)

  return(values)
}
