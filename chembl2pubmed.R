#' ChEMBL2PubMed
#' 
#' Reads in a list of Excel files that contain a ChEMBL ID and merge in the PMID for the corresponding ChEMBL ID.
#'
#' @param files.in The names of the Excel files (must be in .xlsx format) you want to process.
#' @param files.out The names (including .xlsx) of your processed files.
#' @param haschembl Whether or not the ChEMBL IDs in your Excel files have "CHEMBL" appended to the start.
#' @param idcol.in The name of the column that stores the ChEMBL ID.
#' @param idcol.out The name of the column that you want the PMID stored in.
chembl2pubmed <- function(files.in, files.out = files.in,
                          has.chembl = F,
                          idcol.in = 'Document ChEMBL ID', idcol.out = 'Document PubMed ID') {
  require(openxlsx)
  require(httr)
  require(jsonlite)
  
  invisible(lapply(1:length(files.in), function(fileID) {
    data <- read.xlsx(files.in[fileID], sep.names = ' ')
    
    IDs <- unique(data[, idcol.in])
    if(!has.chembl)
      IDs <- paste0('CHEMBL', IDs)
    
    resp <- GET(paste0('https://www.ebi.ac.uk/chembl/api/data/document/set/', paste0(IDs, collapse = ';'), '?format=json'))
    
    newIDs <- fromJSON(content(resp, 'text', encoding = 'UTF-8'))$documents$pubmed_id
    
    mapping <- data.frame(ID = unique(data[, idcol.in]), tmp = newIDs)
    colnames(mapping)[2] <- idcol.out
    
    write.xlsx(merge(data, mapping, by.x = idcol.in, by.y = 'ID', sort = F), files.out[fileID])
  }))
}