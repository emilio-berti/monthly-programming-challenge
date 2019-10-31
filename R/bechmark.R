library(tidyverse)
library(rbenchmark)
library(wesanderson)

source('monkey_themes.R')

scripts <- list.files(path = '01', pattern = '.txt', full.names = TRUE)
participants <- sub('[.]txt', '', scripts)

bench <- list()
for(name in participants){
  i <- which(participants == name)
  bench[[i]] <- sapply(1:50, function(x) benchmark(source(scripts[i]), replications = 50)$elapsed)
}

names(bench) <- participants

res <- bench %>% 
  bind_cols() %>% 
  gather()

res %>% 
  ggplot() +
  geom_violin(aes(reorder(key, - value), value, fill = key)) +
  geom_jitter(aes(reorder(key, - value), value), width = 0.1) +
  xlab('') +
  ylab('Execution time') +
  scale_fill_manual(values = wes_palette('Darjeeling1', 10, type = 'continuous')) +
  coord_flip() +
  monkey_bare
