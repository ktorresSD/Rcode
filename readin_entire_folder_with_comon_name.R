
#READ IN DATA
file.names <- list.files(path = 'C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/000_GxE_Project/p2_files/')

# read in each file in the directory naming it with the studyname only

for(i in 1:length(file.names)){
  start.stripped.i <- unlist(strsplit(x = file.names[i], split = 'p2_'))[2]
  obj.name.i <- unlist(strsplit(x = start.stripped.i, split = '_.'))[1] # escape character before . so it's not treated as a wildcard
  X.i <- read.csv(file.names[i])
  assign(x = obj.name.i, value = X.i)
  rm(list = c('start.stripped.i', 'obj.name.i', 'X.i'))
  gc()
}

ls()
