######################################
## Tweets - Prisão Lula
## Análises
## Script: Neylson Crepalde
######################################

source('01junta_bases.R')
source('remove_acentos.R')

library(ggplot2)
library(tm)
library(wordcloud)
library(magrittr)
library(stringr)
library(igraph)
library(forcats)

# Quem mais foi citado?
citados = str_extract_all(dados$text, '@\\w+') %>% unlist
pal = brewer.pal(9, 'Blues')[5:9]
wordcloud(enc2native(rm_accent(citados)), min.freq = 5, max.words = 100, random.order = F, colors = pal)

index = grep('@LulapeloBrasil', citados)
wordcloud(enc2native(citados[-index]), min.freq = 5, max.words = 100, random.order = F, colors = pal)

# Quais as hastags mais utilizadas
hashtags = str_extract_all(dados$text, '#\\w+') %>% unlist
wordcloud(enc2native(rm_accent(hashtags)), min.freq = 5, max.words = 100, random.order = F, colors = pal)

hashtags = tolower(hashtags)
hashtags = rm_accent(hashtags)

bd_hash = tibble(hashtags)
banco = bd_hash %>% group_by(hashtags) %>% summarise(n = n()) %>% arrange(desc(n))
banco %>% names
banco = banco[-3,]

ggplot(banco[1:10,], aes(x=fct_reorder(hashtags, n),y=n))+
  geom_col(fill = "#1fa8a9")+coord_flip()+theme_bw(base_size = 14)+
  labs(y='',x='',title='Hashtags mais usadas')

# Que contas mais postaram
wordcloud(enc2native(dados$user_name), min.freq = 300, max.words = 100, random.order = F, colors = pal)

# Análise de conteúdo dos textos
cleaned = dados$text %>% tolower %>% removePunctuation %>%
  removeWords(stopwords('pt')) %>% removeWords('lula') %>% removeWords(stopwords('es'))

corpus <- Corpus(VectorSource(cleaned))
tdm <- TermDocumentMatrix(corpus)
tdm <- removeSparseTerms(tdm, sparse = 0.98)
df <- as.data.frame(as.matrix(tdm))
#dim(df)
df.scale <- scale(df)
d <- dist(df.scale, method = "euclidean")
fit.ward2 <- hclust(d, method = "ward.D2")
plot(fit.ward2)

# Resultado não foi muito bom
#########################################
# Do que falam as postagems com #LulaLivre e #LulaPreso

lula_livre = grep('#lulalivre', dados$text, value = T)
lula_preso = grep('#lulapreso', dados$text, value = T)

#LulaLivre
text = lula_livre %>% tolower %>% removePunctuation %>%
  removeWords(stopwords('pt')) %>% removeWords('lula') %>% removeWords(stopwords('es'))
text = rm_accent(text)
corpus <- Corpus(VectorSource(text))
tdm <- TermDocumentMatrix(corpus)
tdm <- removeSparseTerms(tdm, sparse = 0.96)
df <- as.data.frame(as.matrix(tdm))
dim(df)
df = df[-c(23,26,41),]
rownames(df)
#Monta rede
g <- graph_from_incidence_matrix(as.matrix(df))
p = bipartite_projection(g, which = "FALSE")
V(p)$shape = "none"
deg = degree(p)

plot(p, vertex.label.cex=deg/10, edge.width=(E(p)$weight)/50, 
     edge.color=adjustcolor("grey60", .4),
     vertex.label.color=adjustcolor("#1fa8a9", .9))


#LulaPreso
text = lula_preso %>% tolower %>% removePunctuation %>%
  removeWords(stopwords('pt')) %>% removeWords('lula') %>% removeWords(stopwords('es'))
text = rm_accent(text)
corpus <- Corpus(VectorSource(text))
tdm <- TermDocumentMatrix(corpus)
tdm <- removeSparseTerms(tdm, sparse = 0.98)
df <- as.data.frame(as.matrix(tdm))
dim(df)
df[2,] = df[2,] + df[6,]
df = df[-6,]
rownames(df)
#Monta rede
g <- graph_from_incidence_matrix(as.matrix(df))
p = bipartite_projection(g, which = "FALSE")
V(p)$shape = "none"
deg = degree(p)

plot(p, vertex.label.cex=deg/10, edge.width=(E(p)$weight)/2, 
     edge.color=adjustcolor("grey60", .4),
     vertex.label.color=adjustcolor("#1fa8a9", .8))

##############################################
#MORO
text = dados$text %>% tolower %>% removePunctuation %>%
  removeWords(stopwords('pt')) %>% removeWords('lula') %>% removeWords(stopwords('es'))
moro = grep('moro', text, value = T)
moro = rm_accent(moro)
wordcloud(enc2native(moro), min.freq = 5, max.words = 100, random.order = F, colors = pal)

corpus <- Corpus(VectorSource(moro))
tdm <- TermDocumentMatrix(corpus)
tdm <- removeSparseTerms(tdm, sparse = 0.97)
df <- as.data.frame(as.matrix(tdm))
dim(df)
df[6,] = df[6,] + df[11,]
df = df[-c(11,12,28, 29),]
rownames(df)
#Monta rede
g <- graph_from_incidence_matrix(as.matrix(df))
p = bipartite_projection(g, which = "FALSE")
V(p)$shape = "none"
deg = degree(p)

plot(p, vertex.label.cex=deg/15, edge.width=(E(p)$weight)/500, 
     edge.color=adjustcolor("grey60", .4),
     vertex.label.color=adjustcolor("#1fa8a9", .9))

##############################################
#BOLSONARO
text = dados$text %>% tolower %>% removePunctuation %>%
  removeWords(stopwords('pt')) %>% removeWords('lula') %>% removeWords(stopwords('es'))
bolsonaro = grep('bolsonaro', text, value = T)
bolsonaro = rm_accent(bolsonaro)
wordcloud(enc2native(bolsonaro), min.freq = 5, max.words = 100, random.order = F, colors = pal)

corpus <- Corpus(VectorSource(bolsonaro))
tdm <- TermDocumentMatrix(corpus)
tdm <- removeSparseTerms(tdm, sparse = 0.97)
df <- as.data.frame(as.matrix(tdm))
dim(df)
df = df[-c(14,19,20),]
rownames(df)
#Monta rede
g <- graph_from_incidence_matrix(as.matrix(df))
p = bipartite_projection(g, which = "FALSE")
V(p)$shape = "none"
deg = degree(p)

plot(p, vertex.label.cex=deg/15, edge.width=(E(p)$weight)/350, 
     edge.color=adjustcolor("grey60", .4),
     vertex.label.color=adjustcolor("#1fa8a9", .9))
