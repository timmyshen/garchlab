library(ggplot2)
library(reshape2)

data <- data.frame(x=c(0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4),
                   y=c(0, 0, 0, 3, 1, 1, 1, 2, 2, 1, 4, 4),
                   group=c(rep(1, 6), rep(2, 4), rep(3, 2)))

counts <- melt(table(data[1:2]))

colnames(counts) <- c(colnames(data)[1:2], "count")
counts <- subset(counts, count != 0)

sizeplot <- qplot(x=x, y=y, size=count, data=counts) + scale_size(range=c(5, 10))

counts.and.groups <- merge(counts, unique(data))

sizeplot.colour <- qplot(x=x, y=y, size=count,
                         colour=factor(group), data=counts.and.groups) + scale_size(range=c(5, 10))