library(flowCore)
library(ggplot2)
# Info for all 12 people will be stored in the 'people' variable in dataframe format
# EXAMPLES: 
#     instead of summary(person5), use summary(people[[5]]) (double square brackets are neccesary)
#     people[[5]]$FSC.H
#     kmeans(people[[5]], 3)
# Add your own folder path in folder 
folder = '/home/persimmon/Documents/project/'
people = list()
file = '1'
ext = '.fcs'
for(i in 1:9){
  people = append(people, list(as.data.frame(read.FCS(as.character(paste(c(folder,'00',file,ext), collapse = '')))@exprs)))
  file =gsub(as.character(i), as.character(i+1), file)
             }
people = append(people, list(as.data.frame(read.FCS(as.character(paste(c(folder,'010.fcs'), collapse = '')))@exprs)))
people = append(people, list(as.data.frame(read.FCS(as.character(paste(c(folder,'011.fcs'), collapse = '')))@exprs)))
people = append(people, list(as.data.frame(read.FCS(as.character(paste(c(folder,'012.fcs'), collapse = '')))@exprs)))

# function that creates a matrix to display our kmeans clusters vs true clusters
# Clusters must start with 1 and not 0 so just add one to the true cluster vector first
eval_matrix = function(a, b, n_clusters = 5){
  df = data.frame(a,b)
  matrix = matrix(NA, nrow = n_clusters, ncol = n_clusters)
  for(i in 1:n_clusters){
    for(j in 1:n_clusters){
      matrix[i,j] = length(df[1][df[2] == i & df[1] == j])
    }
  }
  return(matrix)
}
