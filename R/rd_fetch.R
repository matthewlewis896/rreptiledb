#' Fetch Data for a Species
#' @description Pull data from the Reptiles Database for a specified species
#' @param binomial \code{character}. The species's binomial name.
#' @return A list of attributes and information about the species.
#' @author Matt Lewis
#' @export

rd_fetch <-
  function(
    binomial
  ){
    assertthat::assert_that(is.character(binomial))

    split_name <-
      binomial %>%
      stringr::str_to_sentence() %>%
      stringr::str_split(' ',simplify = T)

    assertthat::assert_that(length(split_name) == 2L,
                            msg = "The supplied binomial doesn't contain only 2 words.")

    search_url <-
      paste0(
        "http://reptile-database.reptarium.cz/species?",
        "genus=", split_name[1],
        "&species=", split_name[2]
      )

    sp_data <-
      search_url %>%
      xml2::read_html() %>%
      rvest::html_element(".species") %>%
      {ifelse(length(.) > 0L,list(html_table(.)),0)} %>%
      unlist(recursive = F)

    assertthat::assert_that(is.list(sp_data), msg = 'Species not found.')

    return(sp_data)
  }



