# BioSampleParser converts metadata, obtained from NCBI Biosample in xml format, to a 
# more easily interpretable tabular format, i.e. data frame object or .tsv file. 
# This also allows the user to easily use the metadata for further processinginstall.packages("rentrez").
BioSampleParser = function(query = NULL, filePath = NULL, file.tsv = NULL){
  
  require(xml2)
  require(rentrez)
  
  if (is.null(filePath)) {
    if (is.null(query)){
      warning("Please specify either a NCBI BioProject query or a path to a BioSample .xml file")
      return(NULL)
    }
    # Query NCBI BioProject for identifier
    EntrezResult = entrez_search(db="bioproject", term = query)
    BioProjectID = EntrezResult$ids
    if (length(BioProjectID) == 0){
      warning("NCBI BioProject found zero hits for the specified query")
      return(NULL)
    }
    # Query NCBI BioSample for all related samples belonging to the BioProject ID
    EntrezResult = entrez_link(dbfrom = "bioproject", id = BioProjectID, db = "biosample")
    BioSampleList = EntrezResult$links$bioproject_biosample_all
    if (length(BioSampleList) == 0){
      warning("Unable to find any associated BioSamples for the specified BioProject ID")
      return(NULL)
    }
    # Fetch all BioSample results in .xml format
    meta_xml = entrez_fetch(db="biosample", id = BioSampleList, rettype = "xml")
    # Read queried xml file
    meta = read_xml(meta_xml)
  }
  else {
    # Read xml file from path
    meta = read_xml(filePath)
  }
  # Convert to list
  meta_list = as_list(meta)
  
  # Initialize empty data frame
  nSamples = length(meta_list$BioSampleSet)
  nAttributes = 1 + length(meta_list$BioSampleSet$BioSample$Description) +
    length(meta_list$BioSampleSet$BioSample$Attributes)
  meta_df = data.frame(matrix(NA, nrow = nSamples, ncol = nAttributes))
  
  # Fill data frame with values from .xml file
  index = 1
  for (i in 1:nSamples) {
    # Store BioSample ID
    meta_df[i,index] = meta_list$BioSampleSet[[i]]$Ids[[1]][[1]]
    index = index + 1
    # Store Biosample Description
    for (j in 1:length(meta_list$BioSampleSet$BioSample$Description)) {
      meta_df[i,index] = meta_list$BioSampleSet[[i]]$Description[[j]][[1]][[1]]
      index = index + 1
    }
    # Store Biosample Attributes
    for (j in 1:length(meta_list$BioSampleSet$BioSample$Attributes)) {
      meta_df[i,index] = meta_list$BioSampleSet[[i]]$Attributes[[j]][[1]][[1]]
      index = index + 1
    }
    index = 1
  }
  
  # Add colnames
  index = 1
  colnames(meta_df)[index] = attributes(meta_list$BioSampleSet[[i]]$Ids[[1]])[[1]]
  index = index +1
  for (j in 1:length(meta_list$BioSampleSet$BioSample$Description)) {
    colnames(meta_df)[index] = names(meta_list$BioSampleSet[[i]]$Description)[[j]]
    index = index + 1
  }
  for (j in 1:length(meta_list$BioSampleSet$BioSample$Attributes)) {
    colnames(meta_df)[index] = attributes(meta_list$BioSampleSet[[i]]$Attributes[[j]])[[1]]
    index = index + 1
  }
  
  # Write .csv
  if (!is.null(file.tsv)){
    write.table(meta_df, file = file.tsv, quote = FALSE, sep = "\t", row.names = FALSE)
  }
  return(meta_df)
}
