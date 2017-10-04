library(flowCore)
# Info for all 12 people will be stored in the 'people' variable in dataframe format
# EXAMPLES: 
#     instead of summary(person5), use summary(people[[5]]) (double square brackets are neccesary)
#     people[[5]]$FSC.H
#     kmeans(people[[5]], 3)
#     plot(people[[1]]$FSC.H, people[[1]]$SSC.H)
# Add your own folder path in folder 
folder = '/Users/david/Documents/GitHub/Stat-Clustering-mini-project/FlowCore Data/'
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


# K-means For Loop with F-Test
scores = list()
clusters = list()
for(person in people){
    person.scaled <- scale(person)
    kmscore <- c()
    for (k in 1:30) {
        km <- kmeans(person.scaled, k, iter.max = 20)
        kmscore <- c(kmscore, km$betweenss / km$totss)
	}
    scores <- c(list(kmscore), scores)
    clusters <- c(list(km$cluster), clusters)
}