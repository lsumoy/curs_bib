install.packages("tm")
install.packages("ggplot2")
install.packages("readr")
install.packages("NLP")
install.packages("SnowballC")
install.packages("circlize")
install.packages("wordcloud")
library(ggplot2)
library(readr)
library(NLP)
library(tm)
library(SnowballC)
library(igraph)
library(circlize)
library(wordcloud)

##setwd("~/R studio projects/UB project")
##setwd("I:\LauroLinuxCompartit\posgrau data science 2018\treball")
setwd("/home/labs/lslab/lsumoy/Desktop/COMPARTIT_WINDOWS/Teaching and academics/posgrau data science UB/treball/curs_bib-master")
## 1. Corpus (loading data and creating the corpus)
# Load .txt to dataframe
# table <- read.delim("~/R studio projects/UB project/posts.txt", sep="\n")
table <- read.delim("/home/labs/lslab/lsumoy/Desktop/COMPARTIT_WINDOWS/Teaching and academics/posgrau data science UB/treball/curs_bib-master/posts_small.txt", sep="\n", quote = "", 
                                           row.names = NULL, 
                                           stringsAsFactors = FALSE)
  
# A solucionar un error "Warning message: In scan(file = file, what = what, sep = sep, quote = quote, dec = dec, :EOF within quoted string"
# !! Con read.table da 3765 obs (vs. 7167 read.delim)
# Marcel: Solucionado! Ahora da 10822 sin error.
    
# Load data as corpus
# VectorSource() creates character vectors
mydata <- Corpus(VectorSource(table[,1]))
mydata[[1]]$content
summary(mydata)
Length Class             Mode
1       2      PlainTextDocument list
...
333   2      PlainTextDocument list
[ reached getOption("max.print") -- omitted 10489 rows ]

##inspect(mydata)
# S?lo salen n?meros como contenido!!!
## 2. Preprocessing DATA
# Convert to lower case
mydata <- tm_map(mydata, content_transformer(tolower))
#Error de "tm_map : transformation drops documents". See https://stackoverflow.com/questions/51081415/transformation-drops-documents-error-in-r 
## Marcel: no vi que ya lo habÃ�as reportado y lleguÃ© al mismo artÃ�culo de stackoverflow :).
        
# remove stopwords
mydata <- tm_map(mydata, removeWords, stopwords("english"))
# Connection error 1st time: "In read.dcf(file.path(p, "DESCRIPTION"), c("Package", "Version")) : cannot open compressed file 'C:/Program Files/R/R-3.5.0/library/tm/DESCRIPTION', probable reason 'No such file or directory'"
# Solved deactivating in Tools->General: https://support.rstudio.com/hc/en-us/community/posts/200522573-Can-t-install-packages
          
# Remove punctuations and numbers
mydata <- tm_map(mydata, removePunctuation)
mydata <- tm_map(mydata, removeNumbers)
          
# Stemming (grouping terms with the same root)
mydata <- tm_map(mydata, stemDocument)
            
# Document Term Matrix
dtm <- DocumentTermMatrix(mydata)
inspect(dtm)
class(dtm)  

##if matrix is huge. recommend to remove sparse terms: 
##https://stackoverflow.com/questions/47403591/convert-large-document-term-document-matrix-into-matrix
##dtm2 <- removeSparseTerms(dtm, sparse = 0.999)
##mt_dtm <- as.matrix(dtm2)

mt_dtm <- as.matrix(dtm)
dim(mt_dtm)
head(mt_dtm)
tmp_2<-sort(apply(mt_dtm, 2, function(x) length(x[x > 0])), decreasing = T)
df_dtm_freq<-data.frame("words"=names(tmp_2), "freq"=as.numeric(tmp_2))
ggplot(df_dtm_freq[1:100,], aes(as.factor(words), freq))+geom_bar(stat="identity")+scale_x_discrete(limits=rev(df_dtm_freq[1:100,]$words))+coord_flip()
wordcloud(df_dtm_freq$words[1:199], df_dtm_freq$freq[1:199], random.color = FALSE, colors=colorRampPalette(c("red", "green"))(200))
                
## Marcel:
## We generate the Document Term Matrix filtering with the Dictionary Diseases, defined manually
diseases<-tolower(c("Allergies", "Alzheimer's", "Anxiety", "Panic", "Arthritis", "Breast",
                                                   "Fatigue", "Crohn's", "Cystic", "Fibrosis", "Depression", "Diabetes", "Epilepsy",
                                                   "Fibromyalgia", "GERD", "Reflux", "Headaches", "Heartburn", "Hepatitis","Irritable", "Bowel",
                                                   "Lupus", "Lyme", "Migraines", "Sclerosis", "Parkinson's", "Prostate", "Cancer"))
dtm_dis<-DocumentTermMatrix(mydata, list(dictionary= stemDocument(diseases)))
# We use stemDocument() function to us the root of the words to filter, otherwise they won't coincide with mydata
                    
# We extract the whole matrix of appearence
mt_dtm_dis<-as.matrix(dtm_dis)
                    
# We calculate the number of appearences for every word (counting only one by document)
tmp<-sort(apply(mt_dtm_dis, 2, function(x) length(x[x > 0])), decreasing = T)
df_dtm_dis_freq<-data.frame("words"=names(tmp), "freq"=as.numeric(tmp))
                      
# Representing sorted frequency
ggplot(df_dtm_dis_freq, aes(as.factor(words), freq))+geom_bar(stat="identity")+scale_x_discrete(limits=rev(df_dtm_dis_freq$words))+coord_flip()
wordcloud(df_dtm_dis_freq$words, df_dtm_dis_freq$freq, random.color = FALSE, colors=colorRampPalette(c("red", "green"))(28))
                      
## Create a network from DTM
# We create a weighted edge list
tdm.matrix<-t(as.matrix(dtm_dis))
tdm.matrix[tdm.matrix>=1] <- 1
tdm.matrix <- tdm.matrix %*% t(tdm.matrix)
mat_adja <- as.data.frame(as.table(tdm.matrix))
# We remove self connections and connections without any ocurrence
mat_adja<-mat_adja[mat_adja[,1] != mat_adja[,2],]
mat_adja<-mat_adja[mat_adja[,3] > 0,]
# We create the network graphic from the edge list
g=graph.edgelist(as.matrix(mat_adja[,1:2]), directed = FALSE)
E(g)$weight=as.numeric(mat_adja[,3])
# We exctract the adjacency matrix and we plot it with chordDiagram
adj_graph<-get.adjacency(g, attr='weight', type="lower")
chordDiagram(as.matrix(adj_graph), transparency = 0.5)
# We save a plot using igraph specifying edge size by its weight # This commands are to plot the png, are not suitable for visualizing inside Rstudio, as letters are very big.
png("test_nw.png", res=300, units="px", width=10000, height = 10000)
plot(g, edge.width=E(g)$weight/5, layout=layout_with_kk, edge.color="black", 
        vertex.size=igraph::degree(g, mode="all")/2, rescale=T, vertex.label.cex=4,
       vertex.color="#ffffcc", vertex.frame.cex=10)
dev.off()
#we get dictionaries of drugs and side effects from http://sideeffects.embl.de/
drugs.table <- read.delim("/home/labs/lslab/lsumoy/Desktop/COMPARTIT_WINDOWS/Teaching and academics/posgrau data science UB/treball/curs_bib-master/drug_names.tsv", sep="\t", quote = "", 
                                                                               row.names = NULL, 
                                                                               stringsAsFactors = FALSE)
                                
drugs<-tolower(unique(drugs.table[,2]))
dtm_drg<-DocumentTermMatrix(mydata, list(dictionary= stemDocument(drugs)))
# We extract the whole matrix of appearence
mt_dtm_drg<-as.matrix(dtm_drg)
# We calculate the number of appearences for every word (counting only one by document)
tmp<-sort(apply(mt_dtm_drg, 2, function(x) length(x[x > 0])), decreasing = T)
df_dtm_drg_freq<-data.frame("words"=names(tmp), "freq"=as.numeric(tmp))
# Representing sorted frequency
ggplot(df_dtm_drg_freq, aes(as.factor(words), freq))+geom_bar(stat="identity")+scale_x_discrete(limits=rev(df_dtm_drg_freq$words))+coord_flip()
 wordcloud(df_dtm_drg_freq$words, df_dtm_drg_freq$freq, random.color = FALSE, colors=colorRampPalette(c("red", "green"))(28))
 ## Create a network from DTM
 # We create a weighted edge list
 tdm.matrix<-t(as.matrix(dtm_drg))
 tdm.matrix[tdm.matrix>=1] <- 1
 tdm.matrix <- tdm.matrix %*% t(tdm.matrix)
 mat_adja <- as.data.frame(as.table(tdm.matrix))
 # We remove self connections and connections without any ocurrence
 mat_adja<-mat_adja[mat_adja[,1] != mat_adja[,2],]
 ##To reduce complexity in the chordmap reduce to terms with at least 2 connections
 mat_adja<-mat_adja[mat_adja[,3] > 1,]
 # We create the network graphic from the edge list
 g=graph.edgelist(as.matrix(mat_adja[,1:2]), directed = FALSE)
 E(g)$weight=as.numeric(mat_adja[,3])
 # We exctract the adjacency matrix and we plot it with chordDiagram
 adj_graph<-get.adjacency(g, attr='weight', type="lower")
 chordDiagram(as.matrix(adj_graph), transparency = 0.5)
 # We save a plot using igraph specifying edge size by its weight # This commands are to plot the png, are not suitable for visualizing inside Rstudio, as letters are very big.
 png("test_drg.png", res=300, units="px", width=10000, height = 10000)
 plot(g, edge.width=E(g)$weight/5, layout=layout_with_kk, edge.color="grey", 
      vertex.size=igraph::degree(g, mode="all")/2, rescale=T, vertex.label.cex=1.2,
      vertex.color="#ffffcc", vertex.frame.cex=1)
 dev.off()
 
 
#same for side effects
sidef.table <- read.delim("/home/labs/lslab/lsumoy/Desktop/COMPARTIT_WINDOWS/Teaching and academics/posgrau data science UB/treball/curs_bib-master/meddra_all_se.tsv", sep="\t", quote = "", 
                 row.names = NULL, 
                stringsAsFactors = FALSE)
sidef<-tolower(unique(sidef.table[,6]))
dtm_sef<-DocumentTermMatrix(mydata, list(dictionary= stemDocument(sidef)))
# We extract the whole matrix of appearence
mt_dtm_sef<-as.matrix(dtm_sef)
# We calculate the number of appearences for every word (counting only one by document)
tmp<-sort(apply(mt_dtm_sef, 2, function(x) length(x[x > 0])), decreasing = T)
df_dtm_sef_freq<-data.frame("words"=names(tmp), "freq"=as.numeric(tmp))
                                              
# Representing sorted frequency
ggplot(df_dtm_sef_freq, aes(as.factor(words), freq))+geom_bar(stat="identity")+scale_x_discrete(limits=rev(df_dtm_sef_freq$words))+coord_flip()

wordcloud(df_dtm_sef_freq$words[df_dtm_sef_freq$freq>0], df_dtm_sef_freq$freq, random.color = FALSE, colors=colorRampPalette(c("red", "green"))(200))
wordcloud(df_dtm_sef_freq$words[df_dtm_sef_freq$freq>20], df_dtm_sef_freq$freq[df_dtm_sef_freq$freq>20], random.color = FALSE, colors=colorRampPalette(c("red", "green"))(200))

## Create a network from DTM
# We create a weighted edge list
tdm.matrix<-t(as.matrix(dtm_sef))
tdm.matrix[tdm.matrix>=1] <- 1
tdm.matrix <- tdm.matrix %*% t(tdm.matrix)
mat_adja <- as.data.frame(as.table(tdm.matrix))
summary(mat_adja)
# We remove self connections and connections without any ocurrence
mat_adja<-mat_adja[mat_adja[,1] != mat_adja[,2],]
##To reduce complexity in the chordmap reduce to terms with at least 21 connections
mat_adja<-mat_adja[mat_adja[,3] > 20,]
# We create the network graphic from the edge list
g=graph.edgelist(as.matrix(mat_adja[,1:2]), directed = FALSE)
E(g)$weight=as.numeric(mat_adja[,3])
# We exctract the adjacency matrix and we plot it with chordDiagram
adj_graph<-get.adjacency(g, attr='weight', type="lower")
chordDiagram(as.matrix(adj_graph), transparency = 0.5)
##with full matrix gives an error
Error: cannot allocate vector of size 67.9 Gb
##test with most frequent words

plot(g, edge.width=E(g)$weight/5, layout=layout_with_kk, edge.color="grey", 
     vertex.size=igraph::degree(g, mode="all")/2, rescale=T, vertex.label.cex=1.2,
     vertex.color="#ffffcc", vertex.frame.cex=1)
# We save a plot using igraph specifying edge size by its weight # This commands are to plot the png, are not suitable for visualizing inside Rstudio, as letters are very big.
png("test_sef.png", res=300, units="px", width=10000, height = 10000)
plot(g, edge.width=E(g)$weight/5, layout=layout_with_kk, edge.color="black", 
     vertex.size=igraph::degree(g, mode="all")/2, rescale=T, vertex.label.cex=4,
     vertex.color="#ffffcc", vertex.frame.cex=10)
dev.off()


#explore co-occurrence of disease + drugs + side effects
# by clustering
##join all three document term matrices
dtm_dis
dtm_drg
dtm_sef

findFreqTerms(dtm_dis,100)
findFreqTerms(dtm_drg,100)
findFreqTerms(dtm_sef,100)

install.packages("proxy")
library(proxy)

dissimilarity(dtm_dis,method="cosine")

> InfoSession()
Error in InfoSession() : could not find function "InfoSession"
> sessionInfo()
R version 3.5.0 (2018-04-23)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Debian GNU/Linux 8 (jessie)

Matrix products: default
BLAS: /software/debian-8/general/R-3.5.0-bioc-3.7/lib/R/lib/libRblas.so
LAPACK: /software/debian-8/general/R-3.5.0-bioc-3.7/lib/R/lib/libRlapack.so

locale:
  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C               LC_TIME=C                 
[4] LC_COLLATE=en_US.UTF-8     LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
[7] LC_PAPER=es_ES.UTF-8       LC_NAME=C                  LC_ADDRESS=C              
[10] LC_TELEPHONE=C             LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       

attached base packages:
  [1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
  [1] wordcloud_2.5      RColorBrewer_1.1-2 circlize_0.4.4     igraph_1.2.1       SnowballC_0.5.1   
[6] tm_0.7-4           NLP_0.1-11         readr_1.1.1        ggplot2_2.2.1     

loaded via a namespace (and not attached):
  [1] Rcpp_0.12.17        xml2_1.2.0          magrittr_1.5        hms_0.4.2           munsell_0.5.0      
[6] lattice_0.20-35     colorspace_1.3-2    R6_2.2.2            rlang_0.2.1         plyr_1.8.4         
[11] tools_3.5.0         parallel_3.5.0      grid_3.5.0          gtable_0.2.0        yaml_2.1.19        
[16] lazyeval_0.2.1      tibble_1.4.2        Matrix_1.2-14       GlobalOptions_0.1.0 shape_1.4.4        
[21] slam_0.1-43         labeling_0.3        compiler_3.5.0      pillar_1.2.3        scales_0.5.0       
[26] pkgconfig_2.0.1    
                                                
                                            