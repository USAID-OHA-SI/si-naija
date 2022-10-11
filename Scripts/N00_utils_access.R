#' @title Generic credentials management
#'
#' @param name    Name of the key
#' @param service Name of the Service
#' @param update  Value of the key
#'
#' @return status as Boolean
#' @export
#' @family authentication
#'
#' @examples
#' \dontrun{
#' set_key(name = "username", service = "pano")
#' }
#'
set_key <- function(name,
                    service = "usaid",
                    update = FALSE) {
  # Check if key exist
  key <- get_key(name = name, service = service, verbose = FALSE)

  # Check if Key exist and update set to false
  if (!is.null(key) & !update) {
    cat(crayon::yellow(glue::glue("Waring - Key exist. Set update = TRUE to override it.")))
    cat("\n")

    return(NULL)
  }

  # Get secret from prompt and save under name/service
  keyring::key_set(service = service,
                   username = name,
                   prompt = glue::glue("Enter secret for your key[{name}]:"))

  print("Success - Key/Secret pair saved.")
}


#' @title Get value of key
#'
#' @param service Name of the Service
#' @param key     Name of the key
#' @param value   Value of the key
#'
#' @return status as Boolean
#' @export
#' @family authentication
#'
#' @examples
#' \dontrun{
#' get_key(key = "email", service = "pano")
#' }
#'
get_key <- function(name = "email",
                    service = "usaid",
                    verbose = TRUE) {

  # Get list of keys under specified service
  keys <- keyring::key_list(service = service)

  # Check and get records
  record <- NULL

  if (is.null(keys) | nrow(keys) == 0) {
    if (verbose) {
      cat(crayon::red(glue::glue("Error - no service named [{service}]")))
      cat("\n")
    }

    return(NULL)
  }

  record <- dplyr::filter(keys, username == name)

  if (is.null(record) | nrow(record) == 0) {
    if (verbose) {
      cat(crayon::red(glue::glue("Warning - username [{name}] is not available")))
      cat("\n")
    }

    return(NULL)
  }

  # Get secret of the key
  key <- keyring::key_get(service = service, username = name)

  return(key)
}

