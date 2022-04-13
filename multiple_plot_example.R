DF <- data.frame(X = rnorm(60),                                  
                 Y = rnorm(60),
                 Facets = c("Facet 1", "Facet 2",
                            "Facet 3", "Facet 4"))

library("reshape2")
library("ggpubr")
df <- read.csv("C:\\Users\\ju61191\\Downloads\\model_results(1).csv")
new.df <- melt(df, id.vars = "time")

p1<- ggplot(new.df, aes(x = time, y = value, col = variable)) + geom_line()
p2<- ggplot(new.df, aes(x = time, y = value, col = variable)) + geom_line()
p3<- ggplot(new.df, aes(x = time, y = value, col = variable)) + geom_line()
p4<- ggplot(new.df, aes(x = time, y = value, col = variable)) + geom_line()
final <- ggarrange(p1, p2, p3, p4, ncol = 3, nrow = 2, common.legend = TRUE, legend="right")

final
p1