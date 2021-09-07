setwd("~/groups/complexsysanalysis/lab 1 - regime shifts/")

library(png)

rbars <- c(879, 811, 698, 409, 365, 216)

df <- data.frame()
for (patch in 1:6) {
    img <- readPNG(paste0("patch", patch, ".png"))
    img <- apply(img, c(1, 2), mean)

    ## image(img)

    boxxs <- cut(1:dim(img)[1], 10)
    boxys <- cut(1:dim(img)[2], 10)

    for (boxx in unique(boxxs)) {
        for (boxy in unique(boxys)) {
            treefrac <- 1 - mean(img[boxxs == boxx, boxys == boxy])
            df <- rbind(df, data.frame(rbar=rbars[patch], patch, treefrac))
        }
    }
}

library(dplyr)

df2 <- df %>% group_by(patch) %>% summarize(rbar=rbar[1], treefrac=mean(treefrac))
df2$treefrac.paper <- c(.65, .54, .32, .19, .14, .04)
cor(df2$treefrac, df2$treefrac.paper)

library(ggplot2)

ggplot(df, aes(rbar, treefrac)) +
    geom_point()

write.csv(df, "treefracs.csv", row.names=F)
