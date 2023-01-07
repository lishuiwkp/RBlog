---
title: "ERGM导入节点属性的几种方法"
author: "keping wang"
date: "2022-12-15"
output: html_document
---



## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:


```r
library(statnet)
# 生成示例数据
data <- matrix(c(1:9),nrow = 3, ncol = 3)
diag(data) <- 0
net <- as.network(data,directed = T)
summary(net)
```

```
## Network attributes:
##   vertices = 3
##   directed = TRUE
##   hyper = FALSE
##   loops = FALSE
##   multiple = FALSE
##   bipartite = FALSE
##  total edges = 6 
##    missing edges = 0 
##    non-missing edges = 6 
##  density = 1 
## 
## Vertex attributes:
##   vertex.names:
##    character valued attribute
##    3 valid vertex names
## 
## No edge attributes
## 
## Network adjacency matrix:
##   1 2 3
## 1 0 1 1
## 2 1 0 1
## 3 1 1 0
```

```r
# 导入节点属性
X1 <- as.data.frame(c(1,5,10))
X2 <- as.data.frame(c(12,0.1,31))
X3 <- as.data.frame(c(0,1,0))
attr <- as.data.frame(cbind(X1,X2,X3))

# 方法1
# 注：这里的net[i]可以都是net，代码运行他是会覆盖上去的。
net1 <- network(net,vertex.attr = X1,
                    vertex.attrnames = "X1",
                    directed = T)
net2 <- network(net1,vertex.attr = X2,
               vertex.attrnames = "X2",
               directed = T)
net3 <- network(net2,vertex.attr = X3,
               vertex.attrnames = "X3",
               directed = T)
summary(net3)
```

```
## Network attributes:
##   vertices = 3
##   directed = TRUE
##   hyper = FALSE
##   loops = FALSE
##   multiple = FALSE
##   bipartite = FALSE
##  total edges = 6 
##    missing edges = 0 
##    non-missing edges = 6 
##  density = 1 
## 
## Vertex attributes:
##   vertex.names:
##    character valued attribute
##    3 valid vertex names
## 
##  X1:
##    numeric valued attribute
##    attribute summary:
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   3.000   5.000   5.333   7.500  10.000 
## 
##  X2:
##    numeric valued attribute
##    attribute summary:
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.10    6.05   12.00   14.37   21.50   31.00 
## 
##  X3:
##    numeric valued attribute
##    attribute summary:
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.0000  0.0000  0.0000  0.3333  0.5000  1.0000 
## 
## No edge attributes
## 
## Network adjacency matrix:
##   1 2 3
## 1 0 1 1
## 2 1 0 1
## 3 1 1 0
```

```r
# 方法2
net4 <- set.vertex.attribute(net,"X1",attr[,1])
net5 <- set.vertex.attribute(net4,"X2",attr[,2])
net6 <- set.vertex.attribute(net5,"X3",attr[,3])
summary(net6)
```

```
## Network attributes:
##   vertices = 3
##   directed = TRUE
##   hyper = FALSE
##   loops = FALSE
##   multiple = FALSE
##   bipartite = FALSE
##  total edges = 6 
##    missing edges = 0 
##    non-missing edges = 6 
##  density = 1 
## 
## Vertex attributes:
##   vertex.names:
##    character valued attribute
##    3 valid vertex names
## 
##  X1:
##    numeric valued attribute
##    attribute summary:
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   3.000   5.000   5.333   7.500  10.000 
## 
##  X2:
##    numeric valued attribute
##    attribute summary:
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.10    6.05   12.00   14.37   21.50   31.00 
## 
##  X3:
##    numeric valued attribute
##    attribute summary:
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.0000  0.0000  0.0000  0.3333  0.5000  1.0000 
## 
## No edge attributes
## 
## Network adjacency matrix:
##   1 2 3
## 1 0 1 1
## 2 1 0 1
## 3 1 1 0
```

```r
# 方法3
net %v% "X1" <- attr[,1]
net %v% "X2" <- attr[,2]
net %v% "X3" <- attr[,3] 
summary(net)
```

```
## Network attributes:
##   vertices = 3
##   directed = TRUE
##   hyper = FALSE
##   loops = FALSE
##   multiple = FALSE
##   bipartite = FALSE
##  total edges = 6 
##    missing edges = 0 
##    non-missing edges = 6 
##  density = 1 
## 
## Vertex attributes:
##   vertex.names:
##    character valued attribute
##    3 valid vertex names
## 
##  X1:
##    numeric valued attribute
##    attribute summary:
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   3.000   5.000   5.333   7.500  10.000 
## 
##  X2:
##    numeric valued attribute
##    attribute summary:
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.10    6.05   12.00   14.37   21.50   31.00 
## 
##  X3:
##    numeric valued attribute
##    attribute summary:
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.0000  0.0000  0.0000  0.3333  0.5000  1.0000 
## 
## No edge attributes
## 
## Network adjacency matrix:
##   1 2 3
## 1 0 1 1
## 2 1 0 1
## 3 1 1 0
```
