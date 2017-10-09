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

# grouping together all patients into one dataframe
everyone = people[[1]]
for(i in 2:12){
  everyone = rbind(everyone, people[[i]])
}
# Removing 0's rows
everyone = everyone[everyone$Targets != 0, ]

# Testing whether scaling data improves results (It does)
not_scaled_nmi = c()
for(i in 1:20){
  km = kmeans((everyone[,1:6]), 5)
  nmi = external_validation(everyone$Targets[1:10000], km$cluster[1:10000], method = 'nmi')
  not_scaled_nmi = c(not_scaled_nmi, nmi)
}
scaled_nmi = c()
for(i in 1:20){
  km = kmeans(scale(everyone[,1:6]), 5)
  nmi = external_validation(everyone$Targets[1:10000], km$cluster[1:10000], method = 'nmi')
  scaled_nmi = c(scaled_nmi, nmi)
}
# roughly 2% nmi accuracy increase 
# > mean(scaled_nmi);mean(not_scaled_nmi)
# [1] 0.6827666
# [1] 0.6609856
