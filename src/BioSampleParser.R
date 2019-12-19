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
    
    # Add runinfo
    EntrezResult = entrez_link(dbfrom = "bioproject", id = BioProjectID, db = "sra")
    sraList = EntrezResult$links$bioproject_sra_all
    if (length(sraList) == 0){
      warning("Unable to find any associated SRA runs for the specified BioProject ID")
      return(NULL)
    }
    # Fetch all SRA runs in .xml format
    runInfo_xml = entrez_fetch(db="sra", id = sraList, rettype = "xml")
    # Read queried xml file
    runInfo = read_xml(runInfo_xml)
  }
  else {
    # Read xml file from path
    meta = read_xml(filePath)
  }
  
  # Let's parse our xml files (sample info and run info) into data frames
  
  # SAMPLE INFO
  
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
  
  # RUN INFO
  
  # Convert to list
  runInfo_list = as_list(runInfo)
  
  # Initialize empty data frame
  nSamples = length(runInfo_list$EXPERIMENT_PACKAGE_SET)
  nAttributes = 4
  runInfo_df = data.frame(matrix(NA, nrow = nSamples, ncol = nAttributes))
  
  # Fill data frame with values from .xml file
  for (i in 1:nSamples) {
    index = 1
    # Store SRA ID
    runInfo_df[i,index] = runInfo_list$EXPERIMENT_PACKAGE_SET[[i]]$EXPERIMENT[[1]][[1]][[1]]
    index = index + 1
    # Store sample name
    runInfo_df[i,index] = runInfo_list$EXPERIMENT_PACKAGE_SET[[i]]$EXPERIMENT[[2]][[1]][[1]]
    index = index + 1
    # Store Description
    runInfo_df[i,index] = runInfo_list$EXPERIMENT_PACKAGE_SET[[i]]$EXPERIMENT[[4]][[1]][[1]]
    index = index + 1
    # Store Instrument model
    runInfo_df[i,index] = runInfo_list$EXPERIMENT_PACKAGE_SET[[i]]$EXPERIMENT[[5]][[1]][[1]]
  }
  
  # Add colnames
  colnames(runInfo_df)[1] = names(runInfo_list$EXPERIMENT_PACKAGE_SET[[1]]$EXPERIMENT[[1]])
  colnames(runInfo_df)[2] = names(runInfo_list$EXPERIMENT_PACKAGE_SET[[1]]$EXPERIMENT)[[2]]
  colnames(runInfo_df)[3] = names(runInfo_list$EXPERIMENT_PACKAGE_SET[[1]]$EXPERIMENT[[4]])[[1]]
  colnames(runInfo_df)[4] = names(runInfo_list$EXPERIMENT_PACKAGE_SET[[1]]$EXPERIMENT)[[5]]

  
  # Write .csv
  if (!is.null(file.tsv)){
    write.table(meta_df, file = file.tsv, quote = FALSE, sep = "\t", row.names = FALSE)
  }
  return(list(sample_info = meta_df, run_info = runInfo_df))
}
