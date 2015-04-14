# save(mod, df, drop, s, file='test.data.rData')
load(file='test.data.rData')

s <- sample(nrow(df), nrow(df)/50)
classing <- gbm.to.scorecard(mod, gbm.perf(mod), df[s,], precision = NULL)

phat.actual <- predict(mod, df[s,-drop], gbm.perf(mod))
phat.sc <- predict.scorecard(classing, df[s,-drop])

### TRY A RISKVIEW MODEL QUICKLY ###
# library(gbm)
# library(mjollnir)
# df <- read.csv(file = "F:/RV5A1503_6666/data/rv5t_rdev.csv", header=TRUE,
#                na.strings = c('','.'), stringsAsFactors = TRUE)
# dev <- df$Selected == 1
# ok.cols <- grep("^\\w_\\w\\d\\d", colnames(df))
# drop <- which(!get.final.cols(df[dev, ], c(colnames(df)[-ok.cols])))
#
# mono <- get.mono.cols(colnames(df)[-drop])
# s <- sample(which(dev), sum(dev)/10)
#
# # see what else wants to come into RV5A
# mod <- gbm.fit(x = df[s,-drop], y = df[s, 'depvar'],
#                distribution = 'bernoulli', var.monotone = mono,
#                n.trees = 1000, shrinkage = 0.1, nTrain = length(s)/2,
#                keep.data = TRUE, verbose = TRUE, interaction.depth = 1,
#                n.minobsinnode = 100)

###### END GBM BUILD #######

library(gbm)
library(ggplot2)
data(titanic, package='mjollnir')
titanic$Parch <- ordered(titanic$Parch)
titanic$Pclass <- as.numeric(titanic$Pclass)
mono <- c(-1, 0, -1, 1, 1, 1, 0)
mod <- gbm(Survived~., data=titanic, n.trees = 5000, verbose=T, var.monotone=NULL, n.minobsinnode = 50)
n.trees <- gbm.perf(mod)

#### figure out the ordered variables ###

classing <- gbm.to.scorecard(mod, n.trees, newdata = df, precision = NULL)

# which classing vars only have 1 level after rounding
low.cnts <- which(sapply(classing[-1], function(x) length(x$score)) == 1) + 1

classing2 <- structure(classing[-low.cnts], class='scorecard')


phat.actual <- predict(mod, titanic, n.trees)
phat.sc <- predict(classing2, titanic)

ks.table(phat.actual, titanic$Survived)$ks
ks.table(phat.sc, titanic$Survived)$ks


phat <- plogis(predict(classing, titanic))
transformed <- lapply(classing[-1], score.scorecard, titanic)

plt <- data.frame(x=titanic$Embarked, y1=titanic$Survived, y2=transformed$Embarked - classing[[1]])

ggplot(plt, aes(x=x, y=y1)) + geom_smooth() + geom_smooth(aes(y=y2), color='red')

# plot the transformed predictors against the mean bad rates

mm.obj <- mm(Survived~., titanic, score = plogis(phat.sc))


out$score +  classing[[1]]

phat.actual <- predict.(mod, df, gbm.perf(mod))
phat.sc <- predict.scorecard(classing)

classing <- gbm.to.scorecard(mod, gbm.perf(mod), newdata = NULL, precision = NULL)
phat.sc <- predict(classing, df)

lvls <- vlv[vlv <= tree[[2]][p] + 1]


classing1 <- gbm.to.scorecard(mod2, 1:gbm.perf(mod2), titanic, precision = 0.001)
classing2 <- gbm.to.scorecard(mod2, 1:gbm.perf(mod2), titanic, precision = 0.50)
classing3 <- gbm.to.scorecard(mod2, 1:gbm.perf(mod2), titanic, precision = 1.00)

phat1 <- predict.scorecard(classing1, titanic)
phat2 <- predict.scorecard(classing2, titanic)
phat3 <- predict.scorecard(classing3, titanic)

library(mjollnir)
ks.table(phat1, titanic$Survived)$ks
ks.table(phat2, titanic$Survived)$ks
ks.table(phat3, titanic$Survived)$ks

# function to turn a classed list into a plot data.frame
convert <- function(x) {
  data.frame(
    value = c('Null', x$value),
    score = c(x$missing, x$score),
    stringsAsFactors=F)}

plots <- lapply(seq_along(classing), function(i) {
  if (i > 1) {
    plt <- convert(classing[[i]])
    ggplot(plt, aes(x=value, y=score, fill=score)) +
      geom_bar(stat='identity', color='black') +
      coord_flip() +
      scale_x_discrete(limits=rev(plt$value)) +
      scale_fill_gradient2(low='blue', high='red') +
      ggtitle(names(classing)[i])
  }})

plt <- convert(classing[[5]])

# woe plot alongside a counts plot
ggplot(plt, aes(x=value, y=score, fill=score)) +
  geom_bar(stat='identity', color='black') +
  coord_flip() +
  scale_x_discrete(limits=rev(plt$value), expand=c(0.05, 2)) +
  scale_fill_gradient2(low='blue', high='red') +
  ggtitle(names(classing)[5])

ggplot(plt, aes(x=value, y=score, fill=score)) +
  geom_bar(stat='identity', color='black') +
  coord_flip() +
  scale_x_discrete(limits=rev(plt$value), expand=c(0.05, 2)) +
  scale_fill_gradient2(low='blue', high='red') +
  ggtitle(names(classing)[5])


phat <- predict.scorecard(classing, df[s, -drop])
phat.mod <- predict(mod, df[s, -drop], gbm.perf(mod))



adj.weights <- function(x) {
  x$missing <- 0
  x
}

classing2 <- c(classing[1], lapply(classing[-1], adj.weights))

na.rec <- rep(NA, ncol(df[s, -drop]))

tmp <- rbind(df[s, -drop], na.rec)

phat <- predict.scorecard(classing, tmp)

phat2 <- predict.scorecard(classing2, df[s, -drop])


#### CREATE SOME CLUSTERS !!! ####


##################################
### INVERSE DOCUMENT FREQUENCY ###
##################################

fpath <- 'X:/Backup/Heather/RV5T_4752_1503/'
fname <- 'ln_4752_rv5t_dev_val_fcra50_archive_20150303_edina_v50.csv'

cc <- rep("NULL", 1212)
cc[c(134, 138)] <- "character"
cc[4] <- 'factor'

sources <- read.csv(paste0(fpath, fname), header=F, colClasses=cc,
                    na.string=c("","."))

# compute the inverse-document-frequency
get.idf <- function(x) {
  tokens <- strsplit(x[1], split = ",")[[1]]
  counts <- as.numeric(strsplit(x[2], split = ",")[[1]])

  names(counts) <- tokens
  counts
}

# now need to reduce the list to one list of counts
res <- apply(sources[-1], 1, get.idf)

# find unique list of values
vals <- na.omit(unique(unlist(sapply(res, names))))
freqs <- rep(1, length(vals))
names(freqs) <- vals

for (i in seq_along(res)) {
  ids <- na.omit(names(res[[i]]))
  freqs[ids] <- freqs[ids] + ifelse(res[[i]] > 0, 1, 0)
}

idf <- log(length(res)/freqs)

##################################
### TERM FREQUENCY PER RECORD  ###
##################################

term.frequency <- function(x) {
  ids <- na.omit(names(x))
  freqs <- rep(1, length(vals))
  names(freqs) <- vals
  freqs[ids] <- freqs[ids] + x
  log(freqs)
}

tf <- do.call(rbind, lapply(res, term.frequency))

### MULITPLY THEM ###
tf.idf <- t(t(tf) * idf)

### Cluster them ###
kms <- lapply(1:20, function(k) kmeans(tf.idf, k))

### scree plot ###
var.explained <- sapply(kms, function(x) (x$totss - x$tot.withinss) / x$totss)
plot(var.explained) # best is 10 clusters

table(kms[[10]]$cluster)

dev <- df$Selected == 1

aggregate(df$depvar[dev], list(kms[[10]]$cluster), mean)

### MERGE CLUSTER INTO DATASET ###

clusters <- data.frame(cluster=kms[[10]]$cluster, account=sources[,1])

df3 <- merge(df, clusters)

aggregate(depvar~cluster, df3, mean)

### build a GBM model on each cluster! ###

ok.cols <- grep("^\\w_\\w\\d\\d", colnames(df3))
drop <- which(!get.final.cols(df3, c(colnames(df3)[-ok.cols])))

mono <- get.mono.cols(colnames(df3)[-drop])
s <- sample(nrow(df3), nrow(df3)/2)

mods <- list()
for (k in 1:10) {
  f <- s[df3[s, 'cluster'] == k]
  mods[[k]] <-
    gbm.fit(x = df3[f,-drop], y = df3[f, 'depvar'],
            distribution = 'bernoulli', var.monotone = mono,
            n.trees = 1000, shrinkage = 0.1, nTrain = length(f)/2,
            keep.data = TRUE, verbose = TRUE, interaction.depth = 1,
            n.minobsinnode = 100)
}


### turn them into classings ###

bcs <- lapply(mods, function(x) gbm.to.scorecard(x, gbm.perf(x), df3[s,-drop]))

### now predict on all da data


phats <- lapply(bcs, predict.scorecard, df3[, -drop])

combined <- sapply(1:nrow(df3), function(i) phats[[df3$cluster[i]]][i])

ks.table(combined[s], df3[s, 'depvar'])
ks.table(combined[-s], df3[-s, 'depvar'])




mod <- gbm.fit(x = df[s,-drop], y = df[s, 'depvar'],
               distribution = 'bernoulli', var.monotone = mono,
               n.trees = 1000, shrinkage = 0.1, nTrain = length(s)/2,
               keep.data = TRUE, verbose = TRUE, interaction.depth = 1,
               n.minobsinnode = 100)






