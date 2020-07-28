#' Fetch Data for a Species
#' @description Pull data from the Reptiles Database for a specified species
#' @author Matt Lewis, \email{matthewlewis896@@gmail.com}
#'
#' @param binomial Character. The species's binomial name.
#'
#' @return A list of attributes and information about the species.
#' @export

rd_fetch <-
  function(
    binomial
  ){

    if(!is.character(binomial)){
      stop("Please supply a valid character input for 'binomial'.")
    }else{
      split_name <-
        binomial %>%
        strsplit(" ") %>%
        unlist()

      if(length(split_name) != 2L){
        stop("Please supply a valid binomial name in the form binomial = 'Genus species'.")
      }

      genus_name <-
        split_name[1] %>%
        tolower() %>%
        strsplit("") %>%
        unlist()

      genus_name[1] <-
        genus_name[1] %>%
        toupper()

      genus_name <-
        genus_name %>%
        paste(collapse = "")

      species_name <-
        split_name[2] %>%
        tolower()
    }

    search_url <-
      paste0(
        "http://reptile-database.reptarium.cz/species?",
        "genus=",
        genus_name,
        "&",
        "species=",
        species_name
      )

    sp_data <-
      search_url %>%
      xml2::read_html() %>%
      rvest::html_node(
        ".species"
        ) %>%
      html_table()

    return(sp_data)
  }



