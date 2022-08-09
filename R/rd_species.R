#' Obtain basic info on all species present on the reptile database
#' @description Downloads the most recent species checklist present on the reptile
#' database.
#' @param outdir (optional) \code{character} directory in which to save species list.
#' Defaults to a temporary file present only in memory.
#' @return A \code{tibble}.
#' @author Matt Lewis
#' @export

rd_species <-
  function(outdir){
    if(!missing(outdir)){
      assertthat::is.dir(outdir)
    }
    data_link <- 'http://www.reptile-database.org/data/'

    mr_data <-
      data_link %>%
      rvest::read_html() %>%
      rvest::html_elements('a') %>%
      .[1] %>%
      rvest::html_attr('href') %>%
      paste0(data_link, .)

    if(!missing(outdir)){
      outfile <- file.path(outdir, basename(mr_data))
    }else{
      outfile <- tempfile(fileext = '.xlsx')
    }

    utils::download.file(mr_data, outfile, mode = 'wb')

    df <-
      outfile %>%
      readxl::read_xlsx(na = c('', ' ', '\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035')) %>%
      dplyr::mutate(dplyr::across(tidyselect::everything(), ~gsub('\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035\035', '',.)))

    return(df)
  }
