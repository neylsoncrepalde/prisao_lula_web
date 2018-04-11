###########################################
## Tweets - Prisão Lula
## Junta bancos de dados
## Script: Neylson Crepale
###########################################

library(readr)
nomes = grep('.csv', dir(), value = T)

bancos = lapply(nomes, read_csv)
length(nomes); length(bancos)

# Juntando
dados = bancos[[1]]
for (i in 2:length(bancos)) {
  dados = rbind(dados, bancos[[i]])
}

# Transformando a data
library(dplyr)
dados$created_at = gsub(' \\+0000 2018', '', dados$created_at)

library(lubridate)
dados$created_at = gsub('Fri Apr 06', '06-04-2018', dados$created_at)
dados$created_at = gsub('Sat Apr 07', '07-04-2018', dados$created_at)
dados$date_time = dados$created_at %>% dmy_hms
dados = dados %>% arrange(date_time)

# Limpando a sujeira
rm(bancos, i)
