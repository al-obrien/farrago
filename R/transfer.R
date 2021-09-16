#' Check for contents via winSCP
#'
#' \code{check_winscp} will connect to the secure file transfer protocol available with winSCP and check for the presence of a file with the provided pattern.
#'
#' This function is used as part of the \code{\link{transfer_winscp}} function to ensure the file of interest is actually available for download.
#'
#' @param pattern Character string, typically a file name or portion thereof, that will be used in the search. Default is today's date.
#' @param connection Character string that is used to connect to the desired file system. Usually the password and username can be provided as part of the address.
#' @param rmt_path Character string describing the file path to search in, default is the home directory ('.').
#' For example: sftp://USERNAME:PASSWORD@@SERVERADDRESS.CA/
#' @param winscp_location Character string, defining the location of the install location for winSCP. Default is C:/Program Files (x86)/WinSCP/winSCP.com.
#' @param detailed_output Logical value, if set to \code{TRUE} then all the console output is provided. Default is \code{FALSE}.
#' @return A logical value of whether or not the pattern was found.
#' @examples
#' \dontrun{
#' # Test to see if the pattern is present in the server of interest.
#' check_winscp(pattern = 'June_22_2020', connection = 'sftp://######:######@@hostlocation.ca/',
#'                  rmt_path = './location', detailed_output = TRUE)
#'}
#'
#' @export
check_winscp <- function(pattern = NULL, connection = NULL, rmt_path = '.', winscp_location = 'C:/Program Files (x86)/WinSCP/winSCP.com', detailed_output = FALSE) {

  # Input check
  if(is.null(rmt_path)) stop('No valid location input to check for contents.'); # To put to location, try something like "\"./TO LOC\"" (needs to be double quotes for CLI)
  if(is.null(pattern)) {warning('No pattern input, default used'); pattern <- format(lubridate::today(), "%B_%d_%Y");}
  if(!file.exists(winscp_location)) stop('The winscp location does not exist. Please ensure you have it installed.')

  # Error if no connection
  if(is.null(connection)) stop('No connection value was provided. Try to provide both the username and password as part of the connection.');

  # Format input for command line (if already quoted, then skip)
  connection <- dplyr::if_else(stringr::str_detect(connection, '^".*"$'), connection, shQuote(connection))
  rmt_path <- dplyr::if_else(stringr::str_detect(rmt_path, '^".*"$'), rmt_path, shQuote(rmt_path))
  pattern <- dplyr::if_else(stringr::str_detect(pattern, '^".*"$'), stringr::str_replace_all(pattern, '(^")(.*)("$)', '\\2'), pattern) # This likely isn't needed

  # Temp files
  winscp_command_file <- tempfile(fileext = '.txt')

  # winscp code
  winscp_listcontent <- paste0('
  open ', connection, '
  cd ', rmt_path, '
  ls
  exit')

  # Input into temp file
  writeLines(winscp_listcontent, winscp_command_file)

  # Pass winscp through system
  winscp_env <- system2(command = normalizePath(winscp_location, '\\'),
                        args = paste0(' /script=', winscp_command_file), stdout = TRUE)

  check_scp <- any(stringr::str_detect(winscp_env, pattern))

  if(detailed_output) cat('**OUTPUT FROM LS COMMAND: **', winscp_env, '***END OF OUTPUT**', sep = '\n');

  if(check_scp) {
    print(paste('Happy days! The pattern (', pattern , ') input has been detected in',
                stringr::str_replace(connection, '(.*@)(.*)', '\\2')))
  } else {
    print('No pattern was found')
  }
  check_scp
}

#' Transfer content between file systems via winSCP
#'
#' \code{transfer_winscp} will connect to the secure file transfer protocol available with winSCP and transfer (upload or download) a specific file of interest.
#'
#' This function uses \code{\link{check_winscp}} during the download operation to ensure the file actually exists.
#' The remote location requires the directory to end in '/', the \code{drop_location} must end in a backslash as well; several checks exist in the code to ensure this.
#' If the code is not operating, try to add double quotes around a string that contains spaces. The paths are normalized to windows when passing to the windows command
#' prompt; however, the commands in winSCP console follow a UNIX style. The function will create a batch file temporarily that is passed to winSCP through command prompt, the
#' subsequent output is captured and the operation of data transfer is performed.
#'
#' Options can be provided to the transfer, which may be helpful for changing permissions (e.g. \code{-permissions=640} to allow group read). Note that only
#' some options are available for upload (\code{put}) as compared to download (\code{get}). See more at: https://winscp.net/eng/docs/scriptcommand_put and https://winscp.net/eng/docs/scriptcommand_get
#'
#' @param file Character string. When uploading, should be the file path and name, when downloading it should just be the file name and extension.
#' @param direction Character string, either 'upload' or 'download'.
#' @param connection Character string that is used to connect to the desired file system. Usually the password and username can be provided as part of the address.
#' @param rmt_path Character string describing the file path to upload to or download from. If none provided, default to home directory.
#' For example: sftp://USERNAME:PASSWORD@@SERVERADDRESS.CA/
#' @param drop_location Character string that provides the path to place the downloaded file from winSCP.
#' @param options Character string for command line options to winSCP (e.g. for permissions).
#' @param winscp_location Character string, defining the location of the install location for winSCP. Default is C:/Program Files (x86)/WinSCP/winSCP.com.
#' @param ... Additional parameters that will be passed to \code{\link{check_winscp}} during download operations.
#' @return Dataset will be saved in the file system.
#' @examples
#' \dontrun{
#' # Download a file
#' transfer_winscp(file ='filename_in_remote_location.xlsx'),
#'                 direction = 'download',
#'                 connection = 'sftp://######:######@@hostlocation.ca/'
#'                 rmt_path = './location/',
#'                 drop_location = 'C:/PATH/TO/DESIRED/FOLDER/')
#'
#' # Upload a file
#' transfer_winscp(file = 'C:/LOCATION/OF/FILE/UPLOAD.csv, direction = 'upload',
#'                 connection = 'sftp://######:######@@hostlocation.ca/',
#'                 rmt_location = './location/')
#'
#' }
#' @export
transfer_winscp <- function(file = NULL, direction = NULL, connection = NULL, rmt_path = NULL, drop_location = NULL, options = NULL, winscp_location = 'C:/Program Files (x86)/WinSCP/winSCP.com', ...) {

  # Input checks (short circuit)
  if(is.null(direction) || !(direction %in% c('upload', 'download'))) stop('Invalid direction input, use \'upload\' or \'download\' keywords');
  if(is.null(file)) stop('Please provide a file to upload or download')

  # Error if no connection
  if(is.null(connection)) stop('No connection value was provided. Try to provide both the username and password as part of the connection.');

  # Drop location check (also checks to make sure ends in slashes)
  if(is.null(drop_location) && direction == 'download'){
    stop('Drop location must be provided for downloading.')
  } else if (!is.null(drop_location) && !stringr::str_detect(drop_location, '[.]*[/|/"|\\\\|\\\\"]{1}$')) {
    stop('Drop location must end in slash')
  } else if(!is.null(drop_location) && !file.exists(stringr::str_replace(drop_location, '^(.*)(/|/"|\\\\|\\\\")$', '\\1'))) {
    stop('Input drop location for the download file is not valid')
  }

  # Temp files
  winscp_transfercontent_file <- tempfile()

  # If uploading...
  if(direction == 'upload') {

    # Format input for command line
    connection <- shQuote(connection)
    file <- shQuote(normalizePath(file))

    # Check location to upload
    if(is.null(rmt_path)) {
      rmt_path <- './';
      warning('No default file location found, setting to "', rmt_path, '"');
    } else {
      rmt_path <- shQuote(rmt_path)
    }

    # Create code to pass to winscp
    winscp_listcontent <- paste0('open ', connection,
                                 '\n put ', options, ' ', file, ' ', rmt_path,
                                 '\n exit')

    # Input into temp file
    writeLines(winscp_listcontent, winscp_transfercontent_file)

    # Pass winscp through system
    system2(command = normalizePath(winscp_location, '\\'),
            args = paste0(' /script=', winscp_transfercontent_file))

    # If downloading...
  } else if (direction == 'download') {

    # Format input for command line
    connection <- shQuote(connection)
    drop_location <- shQuote(normalizePath(drop_location))

    # Check location to download
    if(is.null(rmt_path)) {
      rmt_path <- './';
      warning('No default file location found, setting to "', rmt_path, '"');
    } else {
      rmt_path <- rmt_path # shQuote(rmt_path) # (removed due to quote issues)
    }

    # Check for file presence
    check_scp <- check_winscp(pattern = file,
                              rmt_path = rmt_path,
                              connection = connection,
                              winscp_location = winscp_location,
                              ... = ...)

    # Stop process if file cannot be found at location
    if(!check_scp) stop('The file being searched at "', stringr::str_replace(connection,
                                                                             '(.*@)(.*)', '\\2'),'/',
                        rmt_path, '" cannot be found.')

    # Create code to pass to winscp (paste the rmt location and file searched)
    winscp_listcontent <- paste0('open ', connection,
                                 '\n get ', options, ' ', shQuote(paste0(rmt_path, file)), ' ', drop_location,
                                 '\n exit')

    # Input into temp file
    writeLines(winscp_listcontent, winscp_transfercontent_file)

    # Pass winscp through system
    system2(command = normalizePath(winscp_location, '\\'),
            args = paste0(' /script=', winscp_transfercontent_file))
  }
}


#' Find (n-latest) file
#'
#' Find particular file within a directory and return its name/path.
#'
#' There are two ways to discover the n-th file of interest. Based upon the file name search pattern, the date is sorted either by
#' creation date (\code{ctime}) or a provided pattern (\code{date_pattern}) and format (\code{date_format}) within the file name itself.
#'
#' @param path File path that contains files of interest (can look into child folders using \code{recursive = TRUE}).
#' @param name_pattern Basic regular expression to discover names within a file path.
#' @param slice_n Grab the n-th file from the list (descending order by date). Default set to \code{NULL}, entire list is returned. Set to \code{1} if want just the latest match returned.
#' @param date_filter Reduce set of discovered file based upon a date threshold (default: \code{NULL}, no filtering occurs).
#' @param date_pattern Regular expression to search file name for date, parsed with \code{date_format} parameter.
#' @param date_format POSIX formatted dates or date/times (e.g. \code{"\%Y-\%m-\%d \%H-\%M"}).
#' @param ... Additional parameters passed to \code{\link{list.files}}.
#'
#' @return Returns character vector, file name.
#' @export
#' @examples
#' \dontrun{
#' # Find latest file for particular excel file using basic pattern
#' latest_file <- find_file("/taget_folder",
#'                  "^target_name.*\\.xlsx",
#'                  recursive = TRUE)
#'
#' # Find latest .rds file by the date in the file name itself
#' latest_file <- find_file(path = "./taget_folder",
#'                  name_pattern = "^target_data_(\\d{4}\\-\\d{2}\\-\\d{2})[ ]({1}\\d{2}\\-\\d{2})\\.rds$",
#'                  date_pattern = "(\\d{4}-\\d{2}-\\d{2}\\s\\d{2}-\\d{2})",
#'                  date_format = "%Y-%m-%d %H-%M",
#'                  date_filter = lubridate::today()-10, # Only keep less than today and slice the top value
#'                  full.names=FALSE)
#' }
find_file <- function(path, name_pattern, slice_n = NULL, date_filter = NULL, date_pattern = NULL, date_format = NULL, ...){

  # Grab all files by pattern and path
  extract_date <- data.frame(file_name = list.files(path,
                                                    pattern=name_pattern, # need grep if want perl compatible regex
                                                    ...),
                             stringsAsFactors = FALSE)

  if(extract_date$file_name == 0) return(NA_character_)

  # Extract the dates from the files found depending on method...
  if(!is.null(date_pattern) && !is.null(date_format)) {
    extract_date <- dplyr::mutate(extract_date,
                                  time_create = as.POSIXct(regmatches(file_name, regexpr(date_pattern, file_name)),
                                                           format = date_format, tz = 'UTC'))
  } else if (all(sapply(file.path(path, extract_date$file_name), file.exists))) {
    extract_date <-  dplyr::mutate(extract_date, time_create = lubridate::ymd_hms((file.info(file.path(path, file_name))$ctime))) # If more than one created that day, will take the latest
  } else {
    extract_date <- dplyr::mutate(extract_date, time_create = lubridate::ymd_hms((file.info(file_name)$ctime))) # If more than one created that day, will take the latest
  }

  extract_date %>%
    {
      if(!is.null(date_filter)) {
        dplyr::filter(., time_create < date_filter)
      } else .
    } %>%
    dplyr::arrange(dplyr::desc(time_create)) %>%
    {
      if(!is.null(slice_n)){
        dplyr::slice(., slice_n)
      } else .
    } %>%
    dplyr::pull(file_name)
}


#' Store an object (temporarily).
#'
#' \code{stow} will save an object from the R environment to a location on the hard-drive,
#' preferably a temporary location (i.e. \code{\link{tempfile}}).
#'
#' This function will export an object to the hard-drive as a '.rds' or '.fst' file. As such, the 'fst' package is required for this function.
#' Typically, the purpose of this is to remove a large objects (a few Giga-bytes) from the R session to reclaim RAM and to use \code{\link{retrieve}}
#' at the exact time it is required later in a workflow. \code{stow} outputs an object of the S3 class that contains pertinent information on how to
#' easily retrieve the object. It is recommended to name the output as the same as the object but appended with '_stowed'. The option to output as a 'csv'
#' file is available to easily stow and pickup in non R workflows.
#'
#' @param object Dataset from the R environment; if a list is being stowed, use the 'rds' method.
#' @param path Character string, location to stow object on hard-drive, default is \code{NULL} and will be placed in a temporary location.
#' @param new_name Character string that is used to name the temporary file; default is \code{NULL} and uses the object name.
#' @param method Character string for method of export, either 'rds', 'fst', 'csv'; used to determine the file extension.
#' @param compress Logical value; default is \code{TRUE}, will compress the outputs.
#' @param cleanup  Logical value; default is \code{FALSE}, will remove the object from the provided environment.
#' @param envir Location from which to export/cleanup the object of interest; default is \code{.GlobalEnv}.
#' @param ... Additinal arguments based to the write function of the respective method.
#' @return S3 object of class \code{stow_temp}, to be used when retrieving data.
#' @examples
#' \dontrun{
#' # Create fake data
#' temp_data <- data.frame(col1 = rpois(100, 1), col2 = runif(100))
#'
#' # Stow an object
#' temp_data_stowed <- stow(object = temp_data, method = 'fst', cleanup = TRUE)
#'
#' # Retrieve an object
#' temp_data <- retrieve(stowed_object = temp_data_stowed, cleanup = TRUE)
#' }
#' @export
stow <- function(object, path = NULL, new_name = NULL , method = c('rds', 'fst', 'csv'), compress = TRUE, cleanup = FALSE , envir = .GlobalEnv, ...) {

  # Basic checks
  if(is.null(path) & !is.null(new_name)) stop('You must provide a path if you want to rename the output object. The method selected will determine the file type.');
  if(!is.null(path)) if(!dir.exists(path)) stop('The path provided does not exist. Please check and try again');
  if(all(is.null(path), is.null(new_name))) warning('A temporary file will be created for the object');
  if(!is.null(new_name) && !is.character && length(new_name == 1)) stop('new_name must be a character of length 1');

  # Method check
  method <-  match.arg(method)
  #if(method == 'fst' & !('fst' %in% installed.packages()[,1])) stop('fst package was not found, please install.')

  # Create path string, if no path provided, use temp location
  path_out <- if(is.null(path)) tempfile() else normalizePath(path);

  # For custom new name, assign to path or else use object name
  if(!is.null(path) & is.null(new_name)){

    # Create from new name
    name <- substitute(object)
    path_out <- file.path(path_out, name, fsep = '\\')


  } else if(!is.null(path) & !is.null(new_name)) {

    # Take object name
    path_out <- file.path(path_out, new_name, fsep = '\\')
  }

  # Save to location by method (add more methods like fst as needed)
  switch(method,
         rds = {saveRDS(object, paste0(path_out, '.rds'), compress = compress, ...)},
         fst = {fst::write_fst(object, paste0(path_out, '.fst'), ...)},
         csv = {readr::write_csv(object, paste0(path_out, '.csv'), ...)})

  # Create returned object list
  out_list <- list(path= paste0(path_out, '.', method),
                   name = gsub('^(.*[\\\\/])(.*$)', '\\2', path_out),
                   method = method)

  # Set as specific class
  class(out_list) <- "stow_temp"

  # Remove object from environment
  if(cleanup) {
    warning('Removing ', substitute(object), ' from the following environment: ', substitute(envir))
    rm(list = deparse(substitute(object)), envir = envir)
  }
  out_list
}

#' Retrieve a 'stowed' object.
#'
#' \code{retrieve} will load an object that was previously stowed via \code{\link{stow}} back to the provided R environment.
#'
#' This function will load an object, initially created by \code{\link{stow}}, from the hard-drive ('.rds', '.csv', or '.fst' file). \code{retrieve}
#' requires the S3 object output from \code{\link{stow}} in order to load the object. This is to ensure the correct location, name, and method are used.
#' File types of '.rds', '.csv', and '.fst' can be loaded quickly into an R session.
#'
#' @param stowed_object S3 object created by \code{\link{stow}}.
#' @param keep_name Logical value; default is \code{FALSE}, used if want the object name originally used to be automatically
#' \code{\link{assign}}ed to the provided environment.
#' @param cleanup  Logical value; default is \code{FALSE}, will remove the object from the hard-drive.
#' @param as.data.table Logical value; if 'fst' method used, returns as a data.table object (requires data.table installed).
#' @param envir Location for imported object to be saved.
#' @param ... Additional arguments based to the read function of the respective method.
#' @return R data object previously stored on hard drive.
#' @examples
#' \dontrun{
#' # Create fake data
#' temp_data <- data.frame(col1 = rpois(100, 1), col2 = runif(100))
#'
#' # Stow an object
#' temp_data_stowed <- stow(object = temp_data, method = 'fst', cleanup = TRUE)
#'
#' # Retrieve an object
#' temp_data <- retrieve(stowed_object = temp_data_stowed, cleanup = TRUE)
#' }
#' @export
retrieve <- function(stowed_object, keep_name = FALSE, cleanup = FALSE, as.data.table = FALSE, envir = .GlobalEnv, ...) {
  if(class(stowed_object)!='stow_temp') stop('Input needs to be created by the function `store_temp`')

  out <- switch(stowed_object$method,
                rds = {readRDS(stowed_object$path)},
                fst = {fst::read_fst(stowed_object$path, as.data.table = as.data.table, ...)},
                csv = {readr::read_csv(stowed_object$path, ...)})

  if(keep_name){
    assign(x = stowed_object$name, value = out, envir = envir)

    if(cleanup){
      warning('Removing original file from the following location: ', stowed_object$path)
      file.remove(stowed_object$path)
    }

    return(paste0('Variable assigned to environment automatically with the name: ', stowed_object$name))
  }

  if(cleanup) {
    warning('Removing original file from the following location: ', stowed_object$path)
    file.remove(stowed_object$path)
  }

  return(out)
}

#' Load multiple CSV files from file directories
#'
#' \code{load_csv} will load take an input path which contains multiple csv files, based upon the pattern provided,
#' all target csv files will be loaded into the global environment.
#'
#' @param path A character string for the file path where csv files are located.
#' @param pattern A character vector passed to \code{list.files} to determine which files to load from the path.
#' @param ... Additional parameters passed to \code{read_csv}.
#' @return Datasets for all csv files that match desired pattern.
#'
#' @export
load_csv <- function(path, pattern, ...) {
  # readr version may have more stable output, can swap back to base version if issues arise

  tmp.list.1 <- list.files(path, pattern = pattern) # Find the files
  tmp.list.2 <- list(length = length(tmp.list.1)) # Assign the length
  for (i in 1:length(tmp.list.1)) {
    tmp.list.2[[i]] <- readr::read_csv(
      paste0(path, "/", tmp.list.1[i]), # Important to have paste esp if it isn't in current WD!
      ...
    )
  }
  names(tmp.list.2) <- tmp.list.1
  tmp.list.2
}

#' Pass code from R to SAS
#'
#' \code{pass_code_to_sas} will pass SAS code (typed in R and/or list of SAS
#' files) from R to an installed local version of SAS.
#'
#' @param sas_file_list A list of strings defining the location and name of SAS
#'   code files.
#' @param inputstring A string vector of SAS code.
#' @param sas_path File path to SAS 9.4 executable; default set to a rough standard of: \code{C:/Program Files/SASHome/SASFoundation/9.4/sas.exe}.
#' @param config_file File path for SAS 9.4 configuration file (e.g. C:/.../SAS9.4_Prod/SASV9.CFG).
#' @examples
#' \dontrun{
#' sas_code_string <- 'DATA TEMP; do i = 1 to 10; age = i+1; output; end; run;'
#' pass_code_to_sas(inputstring = sas_code_string, sas_path = "C:/Program Files/SASHome/SASFoundation/9.4/sas.exe")
#' }
#' @export
pass_code_to_sas <- function(sas_file_list = NULL, inputstring = NULL, sas_path = "C:/Program Files/SASHome/SASFoundation/9.4/sas.exe", config_file = NULL) {

  # If provided list of scripts, check they are all valid
  if(!is.null(sas_file_list)){
    if(any(purrr::map_lgl(sas_file_list, file.exists)) == FALSE | is.list(sas_file_list) == FALSE){
      stop("You entered an invalid file location or did not provide the locations as a list of characters")
    }
  }

  sink(file.path(R.home(), "temp_codePass.sas"))

  if(!is.null(sas_file_list)){
    for(i in 1:length(sas_file_list)){
      cat(readLines(sas_file_list[[i]]), sep = "\n")
    }
  }

  cat(inputstring)
  sink()

  # Output message to view what code was sent...
  message(paste0("The above info was passed to SAS: ",
                 if(!is.null(sas_file_list)){for(i in 1:length(sas_file_list)){cat(readLines(sas_file_list[[i]]), sep = "\n")}},
                 print(inputstring)))

  # Run SAS
  system2(sas_path,
          args = paste0(
            "\"", file.path(R.home(), "temp_codePass.sas"), "\"",
            if(!is.null(configFile)) { paste0(" -config \"", config_file, "\"")}
          )
  )

  # Delete the SAS file
  file.remove(file.path(R.home(), "temp_codePass.sas"))
}

#' Load data from SAS 9.4 to R
#'
#' \code{load_from_sas} will load data tables from SAS to R; various library connections and pre-processing code can be
#' included prior to data loading to allow SAS to conduct formatting and calculations on its side. Tables are temporarily extracted
#' to the default R installation location, if there are no errors, the table will be deleted from this location upon loading into memory for R.
#' The SAS script and log will have a random number attached to ensure multiple users dont clash onto the same file.
#' Requires a local version of SAS 9.4 which must be accessible from the RStudio instance.
#'
#' @param datalist A string vector of library and tables names in SAS (libname.tablename).
#' @param sas_path File path to SAS 9.4 executable; default set to a rough standard of: \code{C:/Program Files/SASHome/SASFoundation/9.4/sas.exe}.
#' @param config_file File path for SAS 9.4 configuration file (e.g. C:/.../SAS9.4_Prod/SASV9.CFG)
#' @param add_library Character string (SAS code) for additional libraries to be mapped (see \code{\link{connect_sas_libraries}}).
#' @param add_code Character string (SAS code) for additional data formatting and calculations to perform before loading data.
#' @param convert2csv Logical variable (TRUE/FALSE) to indicate if the data exported from SAS should be in csv format.
#' @param keep_attr Logical variable (TRUE/FALSE) to indicate if attribute values should be stripped from the output tables upon loading from SAS.
#' @return Dataset(s) based upon the input parameters.
#' @examples
#' \dontrun{
#' load_from_sas(c('LIBNAME1.TBLNAME1', 'LIBNAME2.TBLNAME1'))
#' }
#' @export
load_from_sas <- function(datalist,
                          sas_path = "C:/Program Files/SASHome/SASFoundation/9.4/sas.exe",
                          config_file = NULL,
                          add_library = "",
                          add_sas_code = "",
                          convert2csv = FALSE,
                          keep_attr = FALSE) {
  message(
    "Function Notes: \n(1) This function requires SAS 9.4 Classic to be installed and the readr, haven, stringr packages",
    "\n(2) Variable types may be different if loading as .csv or as SAS file",
    "\n(3) The datalist must include the library name and table name (e.g. libname.tablename)"
  )

  # Initialize random extract name
  random_suffix <- create_randomstring(5L)
  extract_name <- paste0('temp_extract_', random_suffix)


  # 1. Loop if want to convert to CSV
  if (convert2csv) {

    # Create the file
    sink(file.path(R.home(), paste0(extract_name, ".sas")))
    cat(paste0(add_library, "\n", add_sas_code), "\n")
    for (i in unique(datalist)) {
      cat(
        "PROC EXPORT DATA =", i, "\n",
        "OUTFILE = ", paste0("\"", file.path(R.home(), paste0(i, ".csv")), "\""), "\n",
        "DBMS = CSV LABEL REPLACE;\n",
        "PUTNAMES = YES;\n",
        "RUN;\n"
      )
    }
    sink()

    # Run SAS
    message("Extracting from SAS, please wait...")
    system2(sas_path,
            args = paste0(
              "\"", file.path(R.home(), paste0(extract_name, '.sas')), "\"",
              if(!is.null(configFile)) { paste0(" -config \"", config_file, "\"")}
            )
    )

    # Read file from SAS
    for (i in unique(datalist)) {
      message(paste0("Now loading: ", i))
      tryCatch(error = function(cnd) stop('The data from SAS could not be read/parsed, this is probably due to the selected table not existing.'),
               temp <- readr::read_csv(file.path(R.home(), paste0(i, ".csv")))) # Read the csv from the temp location
      assign(i, temp, envir = .GlobalEnv) # Could be improved by adding check to ensure no other objects named the same in .GlobalEnv
    }
  }

  # 2. If string > 32 and not CSV converted
  else if (convert2csv == FALSE & any(stringr::str_count(datalist) > 32)) {

    # Create SAS file
    sink(file.path(R.home(), paste0(extract_name, '.sas')))
    cat(paste0(add_library, "\n", add_sas_code), "\n")
    for (i in unique(datalist)) {
      temp_sasname <- stringr::str_trunc(gsub("\\.", "_", i), 32, ellipsis = "") # Truncate the saved file name to this; doesnt index >32 strings, loops for all strings that are passed; 23 for name to include file ext.
      temp_path <- gsub("/", "\\\\", file.path(R.home(), paste0(temp_sasname, ".sas7bdat"))) # Convert to Windows file path; SAS is not case sensitive when running script
      cat(
        "DATA", paste0("\"", temp_path, "\""), "; \n", # Will be truncated if over 32, will pull into R with full name though
        "SET", paste0(i), ";\n",
        "RUN;\n"
      )
    }
    sink()

    # Run SAS
    message("\nExtracting from SAS, please wait...")
    system2(sas_path,
            args = paste0(
              "\"", file.path(R.home(), paste0(extract_name, '.sas')), "\"",
              if(!is.null(configFile)) { paste0(" -config \"", config_file, "\"")}
            )
    )

    # Pass data from SAS
    for (i in unique(datalist)) {
      message(paste0("Now loading: ", i))
      temp_sasname <- stringr::str_trunc(gsub("\\.", "_", i), 32, ellipsis = "") # As prior comment stated...
      tryCatch(error = function(cnd) stop('The data from SAS could not be read/parsed, this is probably due to the selected table not existing.'), # Provide more informative error...
               temp <- haven::read_sas(file.path(R.home(), paste0(stringr::str_to_lower(temp_sasname), ".sas7bdat")))) # SAS file output is in lowercase only...

      if (keep_attr == FALSE) {
        for (var in colnames(temp)) {
          attr(temp[[deparse(as.name(var))]], "format.sas") <- NULL # Remove this attribute as it causes some merge issues in certain circumstances
        }
        for (var in colnames(temp)) {
          attr(temp[[deparse(as.name(var))]], "label") <- NULL # Remove this attribute as well!
        }
      }
      assign(i, temp, envir = .GlobalEnv) # Assign it with dot for consistency on front end
    }
  }

  # 3. if <= 32 and not CSV converted
  else if (convert2csv == FALSE & all(stringr::str_count(datalist) <= 32)) {

    # Create the file
    sink(file.path(R.home(), paste0(extract_name, '.sas')))
    cat(paste0(add_library, "\n", add_sas_code), "\n")
    for (i in unique(datalist)) {
      temp_sasname <- gsub("\\.", "_", i) # SAS cannot have spaces, and only a "." if part of library ref... Cannot save unless removed
      temp_path <- gsub("/", "\\\\", file.path(R.home(), paste0(temp_sasname, ".sas7bdat"))) # Convert to Windows file path; SAS is not case sensitive when running script
      cat(
        "DATA", paste0("\"", temp_path, "\""), "; \n", # If longer than 32, will carry only table name forward now
        "SET", paste0(i), ";\n",
        "RUN;\n"
      )
    }
    sink()

    # Run SAS
    message("Extracting from SAS, please wait...")
    system2(sas_path,
            args = paste0(
              "\"", file.path(R.home(), paste0(extract_name, '.sas')), "\"",
              if(!is.null(configFile)) { paste0(" -config \"", config_file, "\"")}
            )
    )

    # Pass data from SAS
    for (i in unique(datalist)) {
      message(paste0("Now loading: ", i))
      temp_sasname <- gsub("\\.", "_", i) # As prior comment stated...
      tryCatch(error = function(cnd) stop('The data from SAS could not be read/parsed, this is probably due to the selected table not existing.'), # Provide more informative error
               temp <- haven::read_sas(file.path(R.home(), paste0(stringr::str_to_lower(temp_sasname), ".sas7bdat")))) # SAS file output is in lowercase only...
      if (keep_attr == F) {
        for (var in colnames(temp)) {
          attr(temp[[deparse(as.name(var))]], "format.sas") <- NULL # Remove this attribute as it causes some merge issues in certain circumstances
        }
        for (var in colnames(temp)) {
          attr(temp[[deparse(as.name(var))]], "label") <- NULL # Remove this attribute as well! zap_label is an alternative.
        }
      }
      assign(i, temp, envir = .GlobalEnv) # Assign it with dot for consistency on front end
    }
  }


  # Delete the temp data files
  for (i in unique(datalist)) {
    if (convert2csv) {
      file.remove(file.path(R.home(), paste0(i, ".csv")))
    } else if (convert2csv == FALSE & any(stringr::str_count(datalist) > 32)) {
      temp_sasname <- stringr::str_trunc(gsub("\\.", "_", i), 32, ellipsis = "")
      file.remove(file.path(R.home(), paste0(stringr::str_to_lower(temp_sasname), ".sas7bdat"))) # SAS file output is in lowercase only...
    } else {
      temp_sasname <- gsub("\\.", "_", i)
      file.remove(file.path(R.home(), paste0(stringr::str_to_lower(temp_sasname), ".sas7bdat")))
    }
  }

  # Delete the SAS file
  file.remove(file.path(R.home(), paste0(extract_name, '.sas')))

  # Delete the LOG file (appears to be made in the working dir...)
  file.remove(file.path(getwd(), paste0(extract_name, '.log')))
}

#' Load all sheets from an excel file into R
#'
#' \code{load_excel_sheets} will load all sheets from an excel file.
#'
#' @param filename A character string for the file path where the excel file is located.
#' @param tibble A logical value, if \code{TRUE} will export all sheets as a \code{tibble} object.
#' @param ... Additional parameters passed to \code{read_excel}.
#' @return Datasets for all sheets within excel file.
#'
#' @source Adapted from \href{https://stackoverflow.com/users/180892/jeromy-anglim}{Jeromy Anglim's} stackoverflow contribution:
#' \url{https://stackoverflow.com/questions/12945687/read-all-worksheets-in-an-excel-workbook-into-an-r-list-with-data-frames}
#' @note Requires the \code{readxl} package to operate.
#'
#' @examples
#' \dontrun{
#' loaded_sheets <- load_excel_sheets("myexcelfile.xls")
#' }
#' @export
load_excel_sheets <- function(filename, tibble = FALSE, ...) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X, ...))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}
