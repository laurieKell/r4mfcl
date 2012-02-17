`fix.oracle.input` <-
  function (list) 
{
  ## names to lower case and change underscores
  names(list) <- gsub("_",".",tolower(names(list)))
  ## convert all numeric fields to numeric
  for (i in 1:dim(list)[2]){
    list[[i]] <- unfactor(list[[i]])
  }
  ## fix eurocentric longitudes
  for( i in grep("lon$",names(list))) {
    list[[i]] <- ifelse(list[[i]] < 0, 360+list[[i]], list[[i]])
  }
  
  list
}
