library(flowCore)
# Info for all 12 people will be stored in the 'people' variable in dataframe format
# EXAMPLES: 
#     instead of summary(person5), use summary(people[[5]]) (double square brackets are neccesary)
#     people[[5]]$FSC.H
#     kmeans(people[[5]], 3)
#     plot(people[[1]]$FSC.H, people[[1]]$SSC.H)
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
