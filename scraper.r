library(tibble)
library(httr)
library(rvest)
library(dplyr)
library(ggplot2)

cjsg_session <- function() {
  rvest::html_session('http://esaj.tjsp.jus.br/cjsg/consultaCompleta.do')
}


dvec <- function(fun, itens, ..., verbose = TRUE, p = .05) {
  f <- dplyr::failwith(tibble::data_frame(result = 'erro'), fun)
  tibble::data_frame(item = itens) %>%
    dplyr::distinct(item) %>%
    dplyr::group_by(item) %>%
    dplyr::do({
      if (runif(1) < p && verbose) print(.$item)
      d <- f(.$item, ...)
      if (tibble::has_name(d, 'result')) d$result <- 'OK'
      d
    }) %>%
    dplyr::ungroup()
}

cjsg <- function(s, parms = cjsg_parms(s), path = './cjsg', 
                 max_pag = 10, overwrite = FALSE,
                 verbose = TRUE, p = .05) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  if (!file.exists(path)) stop(sprintf('Pasta não "%s" pôde ser criada', path))
  r0 <- s %>% 
    rvest::submit_form(parms)
  n_pags <- if (is.na(max_pag) || is.infinite(max_pag)) cjsg_npags(r0) else max_pag
  dvec(cjsg_pag, 1:n_pags, path = path, ow = overwrite, s = s)
}

parse_cjsg <- function(file, cores = 1) {
  
  # Set names for .id
  names(file) <- file
  file <- file[file.size(file) > 0]
  
  # Run either with progress bar or on parallel
  if (cores == 1) {
    pb <- progress::progress_bar$new(total = length(file))
    purrr::map_dfr(file, parse_cjsg_, pb, .id = "file")
  } else {
    file %>%
      parallel::mclapply(parse_cjsg_, mc.cores = cores) %>%
      dplyr::bind_rows(.id = "file")
  }
}

#' Parse a page of CJSG results
#'
#' @param file The path to the file to be parsed
#' @param pb Progress bar created by [parse_cjsg()]
#' @return A tibble with the parsed information
parse_cjsg_ <- function(file, pb = NULL) {
  
  # Safely parse everything
  parse <- purrr::possibly(parse_cjsg_lawsuit, tibble::tibble(), quiet = FALSE)
  
  # Iterate over xml nodes to parse every lawsuit
  table <- file %>%
    xml2::read_html("UTF-8") %>%
    rvest::html_nodes(".fundocinza1") %>%
    purrr::map_dfr(parse)
  
  if (!is.null(pb)) { pb$tick() }
  return(table)
}

#' Parse one lawsuit from a CJSG page
#'
#' @param node A `.fundocinza1` node extracted from the page
#' @return One row with the data concerning the lawsuit
parse_cjsg_lawsuit <- function(node) {
  
  # Auxiliary function to fill in missing columns in table
  fill_in_columns <- function(data) {
    
    # Fill in ementa and publicacao
    if (!tibble::has_name(data, "ementa"))
      data <- dplyr::mutate(data, ementa = NA_character_)
    if (!tibble::has_name(data, "data_publicacao"))
      data <- dplyr::mutate(data, data_publicacao = NA_character_)
    
    return(data)
  }
  
  # Auxiliary function to create a column that doesn't exist
  fncols <- function(data, cname) {
    add <-cname[!cname%in%names(data)]
    
    if(length(add)!=0) data[add] <- NA_character_
    data
  }
  
  # Get information from lawsuit
  tmp <- rvest::html_node(node, ".downloadEmenta")
  infos <- tibble::tibble(
    id_lawsuit = stringr::str_trim(rvest::html_text(tmp)),
    id_decision = rvest::html_attr(tmp, "cdacordao"))
  
  # Get complicated variables
  id <- node %>%
    rvest::html_node(".ementaClass") %>%
    rvest::html_text() %>%
    stringr::str_trim() %>%
    stringr::str_replace_all("[^0-9]", "")
  cs <- node %>%
    rvest::html_node(".assuntoClasse") %>%
    rvest::html_text() %>%
    stringr::str_trim()
  ts <- node %>%
    rvest::html_node(".ementaClass2") %>%
    rvest::html_text()
  
  # Create final table
  node %>%
    rvest::html_nodes(".ementaClass2") %>%
    rvest::html_text() %>%
    stringr::str_split_fixed(":", 2) %>%
    tibble::as_tibble() %>%
    purrr::set_names(c("key", "val")) %>%
    dplyr::mutate_all(stringr::str_trim) %>%
    dplyr::mutate(
      key = key %>%
        rm_accent() %>%
        stringr::str_to_lower() %>%
        stringr::str_replace_all(" +", "_") %>%
        stringr::str_replace_all("[^a-z_]", "") %>%
        stringr::str_replace_all("_d[eo]_", "_")) %>%
    tidyr::spread(key, val) %>%
    dplyr::bind_cols(infos) %>%
    fill_in_columns() %>%
    dplyr::mutate(id = id, cs = cs, ts = ts) %>%
    fncols("data_julgamento") %>%
    dplyr::select(
      id_page = id, id_decision, id_lawsuit, class_subject = cs,
      district = comarca, date_decision = data_julgamento,
      date_publication = data_publicacao, date_registration = data_registro,
      rapporteur = relatora, summary = ementa, txt_summary = ts)
}






cjsg_pag <- function(pag, path, ow, s) {
  Sys.sleep(1)
  u <- 'http://esaj.tjsp.jus.br/cjsg/trocaDePagina.do?tipoDeDecisao=A&pagina=%d'
  u_pag <- sprintf(u, pag)
  arq <- sprintf('%s/%05d.html', path, pag)
  if (!file.exists(arq) || ow) {
    httr::GET(u_pag, httr::write_disk(arq, overwrite = ow), 
              handle = s$handle)
    tibble::tibble(result = 'OK')
  } else {
    tibble::tibble(result = 'já existe')
  }
}

arqs <- dir('data/cjsg', full.names = TRUE)
httr::BROWSE(arqs[1])

cjsg_parms <- function(s, livre = '', data_inicial = NULL, data_final = NULL, secoes = '') {
  secoes <- paste(secoes, collapse = ',')
  dt_inicial <- ''
  if (!is.null(data_inicial)) {
    dt_inicial <- sprintf('%02d/%02d/%d', lubridate::day(data_inicial),
                          lubridate::month(data_inicial),
                          lubridate::year(data_inicial))
  }
  dt_final <- ''
  if (!is.null(data_final)) {
    dt_final <- sprintf('%02d/%02d/%d', lubridate::day(data_final),
                        lubridate::month(data_final),
                        lubridate::year(data_final))
  }
  suppressWarnings({
    s %>% 
      rvest::html_form() %>% 
      dplyr::first() %>% 
      rvest::set_values('dados.buscaInteiroTeor' = livre,
                        'secoesTreeSelection.values' = secoes,
                        'dados.dtJulgamentoInicio' = dt_inicial,
                        'dados.dtJulgamentoFim' = dt_final)
  })
}

cjsg_npags <- function(req, parms = NULL) {
  if (!is.null(parms)) req <- req %>% rvest::submit_form(parms)
  num <- req$response %>% 
    httr::content('text') %>% 
    xml2::read_html() %>%
    rvest::html_node('#nomeAba-A') %>% 
    rvest::html_text() %>% 
    tidyr::extract_numeric()
  (num %/% 20) + 1
}



session <- cjsg_session()
parms <- session %>% 
  cjsg_parms(livre = 'processo', data_inicial = '2019-05-01', 
             data_final = '2019-07-01')

# numero de paginas a serem baixadas
session %>% cjsg_npags(parms)

d_result <- session %>% 
  cjsg(parms, path = 'data/cjsg', max_pag = 500)

parse_cjsg_um <- function(i, nodes) {
  node <- nodes[[i]]
  trim <- stringr::str_trim
  id <- node %>%
    rvest::html_node('.ementaClass') %>%
    rvest::html_text() %>%
    trim() %>%
    stringr::str_replace_all('[^0-9]', '')
  infos <- node %>%
    rvest::html_node('.downloadEmenta') %>% {
      tibble::tibble(n_processo = trim(rvest::html_text(.)),
                     cd_acordao = rvest::html_attr(., 'cdacordao'))
    }
  ca <- node %>%
    rvest::html_node('.assuntoClasse') %>%
    rvest::html_text() %>%
    trim()
  tsf <- node %>%
    rvest::html_node('textarea') %>%
    rvest::html_text()
  tab_infos <- node %>%
    rvest::html_nodes('.ementaClass2') %>%
    rvest::html_text() %>%
    stringr::str_split_fixed(':', 2) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    setNames(c('key', 'val')) %>%
    dplyr::mutate_all(dplyr::funs(trim(.))) %>%
    dplyr::mutate(key = tolower(abjutils::rm_accent(key)),
                  key = stringr::str_replace_all(key, ' +', '_'),
                  key = stringr::str_replace_all(key, '[^a-z_]', ''),
                  key = stringr::str_replace_all(key, '_d[eo]_', '_')) %>%
    tidyr::spread(key, val) %>%
    dplyr::bind_cols(infos) %>%
    dplyr::mutate(id = id, classe_assunto = ca, txt_ementa = tsf) %>%
    dplyr::select(id, cd_acordao, n_processo, dplyr::everything(), txt_ementa)
  tab_infos
}

parse_cjsg_arq <- function(arq) {
  itens <- xml2::read_html(arq, encoding = 'UTF-8') %>%
    rvest::html_nodes('.fundocinza1')
  dvec(parse_cjsg_um, 1:length(itens), nodes = itens, verbose = FALSE) %>%
    dplyr::select(-item)
}

parse_cjsg <- function(arqs) {
  dvec(parse_cjsg_arq, arqs) %>%
    dplyr::rename(arq = item)
}

arqs <- dir('data/cjsg', full.names = TRUE)
d_cjsg <- parse_cjsg(arqs)
saveRDS(d_cjsg, 'data/d_cjsg.rds')

d_cjsg <- readRDS('data/d_cjsg.rds')
d_cjsg

View(d_cjsg)