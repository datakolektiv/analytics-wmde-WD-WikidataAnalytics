### ---------------------------------------------------------------------------
### --- WMDEdata::hdfs_read_from.R
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE.
### --- Contact: goran.milovanovic_ext@wikimedia.de
### --- June 2021.
### ---------------------------------------------------------------------------
### --- COMMENT:
### --- WMDEData::hdfs_read_from()
### --- read from hdfs (WMF Data Lake)
### --- to the Analytics Clients local filesystem.
### --- The function reads the dataset splits from hdfs
### --- and composes data.table and data.frame class from them.
### ---------------------------------------------------------------------------

#' hdfs_read_from
#'
#' @param kerberosUser charecter, the Kerberos user to impersonate
#' @param localPath character, the path to the directory where the intermediary files should be stored
#' @param localFilenamePrefix character, how should be the local filenames be prefixed for data.table::fread() to pick them up
#' @param hdfsDir character, the hdfs directory where the files are found
#' @param hdfsFilenamePrefix charecter,the prefix used for files in the hdfsDir
#' @param fr_header boolean, whether or not to use "header = T" in data.table::fread()
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' hdfs_read_from(
#'    kerberosUser = "", 
#'    localPath = "", 
#'    localFilenamePrefix = "", 
#'    hdfsDir = "", 
#'    hdfsFilenamePrefix = "", 
#'    fr_header = F)
#' }
hdfs_read_from <- function(kerberosUser,
                           localPath,
                           localFilenamePrefix,
                           hdfsDir,
                           hdfsFilenamePrefix,
                           fr_header = F) {

  message("--- WMDEdata: read from hdfs.")

  # - copy splits from hdfs to localPath
  message("--- WMDEdata: read from hdfs - list splits.")
  system(paste0('sudo -u ',
                kerberosUser,
                ' kerberos-run-command ',
                kerberosUser,
                ' hdfs dfs -ls ',
                paste0(hdfsDir, hdfsFilenamePrefix),
                ' > ',
                localPath, 'temporary_hdfs_filelist.txt'),
         wait = T)

  # - read splits from local filesystem
  message("--- WMDEdata: read from hdfs - copy splits to local.")
  files <- utils::read.table(paste0(localPath, 'temporary_hdfs_filelist.txt'), skip = 1)
  files <- as.character(files$V8)[2:length(as.character(files$V8))]
  file.remove(paste0(localPath, 'temporary_hdfs_filelist.txt'))
  for (i in 1:length(files)) {
    system(paste0('sudo -u ',
                  kerberosUser,
                  ' kerberos-run-command ',
                  kerberosUser,
                  ' hdfs dfs -text ',
                  files[i], ' > ',
                  paste0(localPath, localFilenamePrefix, i, ".csv")),
           wait = T)
  }

  # - compose data.frame/data.table class
  message("--- WMDEdata: read from hdfs - compose data.table class.")
  lF <- list.files(localPath)
  lF <- lF[grepl(localFilenamePrefix, lF)]
  data_set <- lapply(paste0(localPath, lF), function(x) {
    data.table::fread(x,
                      header = fr_header)
    })
  # - put together:
  data_set <- data.table::rbindlist(data_set)

  # - clean up from localPath
  message("--- WMDEdata: read from hdfs - clean up localPath from temp. files.")
  file.remove(paste0(localPath, lF))

  # - output:
  message("--- WMDEdata: read from hdfs - deliver.")
  return(data_set)

}
