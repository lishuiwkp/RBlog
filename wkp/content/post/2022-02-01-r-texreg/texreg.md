---
title: "利用texreg整理模型结果"
author: "wkp"
date: "2022-02-01"
---



## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:


```r
#######
setwd("E:\\Office Account\\2021\\texreg")

library(readxl)
```

```
## Warning: 程辑包'readxl'是用R版本4.1.3 来建造的
```

```r
library(statnet)
```

```
## Warning: 程辑包'statnet'是用R版本4.1.3 来建造的
```

```
## 载入需要的程辑包：tergm
```

```
## Warning: 程辑包'tergm'是用R版本4.1.3 来建造的
```

```
## 载入需要的程辑包：ergm
```

```
## Warning: 程辑包'ergm'是用R版本4.1.3 来建造的
```

```
## 载入需要的程辑包：network
```

```
## Warning: 程辑包'network'是用R版本4.1.3 来建造的
```

```
## 
## 'network' 1.17.2 (2022-05-20), part of the Statnet Project
## * 'news(package="network")' for changes since last version
## * 'citation("network")' for citation information
## * 'https://statnet.org' for help, support, and other information
```

```
## 
## 'ergm' 4.2.2 (2022-06-01), part of the Statnet Project
## * 'news(package="ergm")' for changes since last version
## * 'citation("ergm")' for citation information
## * 'https://statnet.org' for help, support, and other information
```

```
## 'ergm' 4 is a major update that introduces some backwards-incompatible
## changes. Please type 'news(package="ergm")' for a list of major
## changes.
```

```
## 载入需要的程辑包：networkDynamic
```

```
## Warning: 程辑包'networkDynamic'是用R版本4.1.3 来建造的
```

```
## 
## 'networkDynamic' 0.11.2 (2022-05-04), part of the Statnet Project
## * 'news(package="networkDynamic")' for changes since last version
## * 'citation("networkDynamic")' for citation information
## * 'https://statnet.org' for help, support, and other information
```

```
## Registered S3 method overwritten by 'tergm':
##   method                   from
##   simulate_formula.network ergm
```

```
## 
## 'tergm' 4.1.0 (2022-06-22), part of the Statnet Project
## * 'news(package="tergm")' for changes since last version
## * 'citation("tergm")' for citation information
## * 'https://statnet.org' for help, support, and other information
```

```
## 
## 载入程辑包：'tergm'
```

```
## The following object is masked from 'package:ergm':
## 
##     snctrl
```

```
## 载入需要的程辑包：ergm.count
```

```
## Warning: 程辑包'ergm.count'是用R版本4.1.3 来建造的
```

```
## 
## 'ergm.count' 4.1.1 (2022-05-24), part of the Statnet Project
## * 'news(package="ergm.count")' for changes since last version
## * 'citation("ergm.count")' for citation information
## * 'https://statnet.org' for help, support, and other information
```

```
## 载入需要的程辑包：sna
```

```
## Warning: 程辑包'sna'是用R版本4.1.3 来建造的
```

```
## 载入需要的程辑包：statnet.common
```

```
## Warning: 程辑包'statnet.common'是用R版本4.1.3 来建造的
```

```
## 
## 载入程辑包：'statnet.common'
```

```
## The following object is masked from 'package:ergm':
## 
##     snctrl
```

```
## The following objects are masked from 'package:base':
## 
##     attr, order
```

```
## sna: Tools for Social Network Analysis
## Version 2.7 created on 2022-05-09.
## copyright (c) 2005, Carter T. Butts, University of California-Irvine
##  For citation information, type citation("sna").
##  Type help(package="sna") to get started.
```

```
## 载入需要的程辑包：tsna
```

```
## Warning: 程辑包'tsna'是用R版本4.1.3 来建造的
```

```
## 
## 'statnet' 2019.6 (2019-06-13), part of the Statnet Project
## * 'news(package="statnet")' for changes since last version
## * 'citation("statnet")' for citation information
## * 'https://statnet.org' for help, support, and other information
```

```
## unable to reach CRAN
```

```r
library(btergm)
```

```
## Warning: 程辑包'btergm'是用R版本4.1.3 来建造的
```

```
## Registered S3 methods overwritten by 'btergm':
##   method    from
##   plot.gof  ergm
##   print.gof ergm
```

```
## Package:  btergm
## Version:  1.10.6
## Date:     2022-04-01
## Authors:  Philip Leifeld (University of Essex)
##           Skyler J. Cranmer (The Ohio State University)
##           Bruce A. Desmarais (Pennsylvania State University)
```

```
## 
## 载入程辑包：'btergm'
```

```
## The following object is masked from 'package:ergm':
## 
##     gof
```

```r
library(texreg)
```

```
## Warning: 程辑包'texreg'是用R版本4.1.3 来建造的
```

```
## Version:  1.38.6
## Date:     2022-04-06
## Author:   Philip Leifeld (University of Essex)
## 
## Consider submitting praise using the praise or praise_interactive functions.
## Please cite the JSS article in your publications -- see citation("texreg").
```

```r
load("data.Rdata")
library(texreg) #加载包
screenreg(list(model1,model2,model3,model4),
          stars = c(0.01,0.05,0.1),
          digits = 4,
          #star.symbol = "@",
          single.row = F,
          #custom.model.names = c("M1","M2","M3","M4"),
          custom.coef.names = c("edges","gwesp","homogeneity","tech_proximnity","inst_proximity","geo_proximity"), 
          custom.gof.rows = list("method"=c("MCMC","MCMC","MCMC","MCMC"),
                                 "N" = c("?","?","?","?")),
          #custom.note = c("注:*** p < 0.01; ** p < 0.05; * p < 0.1,括号内为标准误"),
          leading.zero = F,
          #ci.force = c(T,T,T,T),
          group = list("内生属性" = 1:2,"节点属性" = 3,"网络协变量" = 4:6))
```

```
## 
## ===============================================================================
##                      Model 1        Model 2        Model 3        Model 4      
## -------------------------------------------------------------------------------
## 内生属性                                                                           
##                                                                                
##     edges              -3.3389 ***    -3.5034 ***   -18.9766 ***   -17.4879 ***
##                         (.5263)        (.5652)       (5.0123)       (5.2203)   
##     gwesp               1.4337 ***     1.4457 ***                    1.0442 ** 
##                         (.4590)        (.4595)                       (.4475)   
## 节点属性                                                                           
##                                                                                
##     homogeneity                         .0016                         .0002    
##                                        (.0020)                       (.0024)   
## 网络协变量                                                                          
##                                                                                
##     tech_proximnity                                  11.4990 ***     9.5876 ** 
##                                                      (3.9928)       (3.9098)   
##     inst_proximity                                   -2.1879 **     -1.7831    
##                                                      (1.0667)       (1.0975)   
##     geo_proximity                                     9.1770 **      7.7854    
##                                                      (4.0981)       (4.8431)   
## -------------------------------------------------------------------------------
## method               MCMC           MCMC           MCMC           MCMC         
## N                       ?              ?              ?              ?         
## AIC                   141.6632       143.1279       132.3253       127.2766    
## BIC                   148.1573       152.8690       145.3134       146.7587    
## Log Likelihood        -68.8316       -68.5639       -62.1626       -57.6383    
## ===============================================================================
## *** p < 0.01; ** p < 0.05; * p < 0.1
```

```r
htmlreg(list(model1,model2,model3,model4),
          stars = c(0.01,0.05,0.1),
          digits = 4,
          #star.symbol = "@",
          single.row = F,
          #custom.model.names = c("M1","M2","M3","M4"),
          custom.coef.names = c("edges","gwesp","homogeneity","tech_proximnity","inst_proximity","geo_proximity"), 
          custom.gof.rows = list("method"=c("MCMC","MCMC","MCMC","MCMC"),
                                 "N" = c("?","?","?","?")),
          custom.note = c("注:*** p < 0.01; ** p < 0.05; * p < 0.1,括号内为标准误"),
          leading.zero = F,
          #ci.force = c(T,T,T,T),
          group = list("内生属性" = 1:2,"节点属性" = 3,"网络协变量" = 4:6),
          file = "result.html")
```

```
## The table was written to the file 'result.html'.
```

```r
wordreg(list(model1,model2,model3,model4),
        stars = c(0.01,0.05,0.1),
        digits = 4,
        #star.symbol = "@",
        single.row = F,
        #custom.model.names = c("M1","M2","M3","M4"),
        custom.coef.names = c("edges","gwesp","homogeneity","tech_proximnity","inst_proximity","geo_proximity"), 
        custom.gof.rows = list("method"=c("MCMC","MCMC","MCMC","MCMC"),
                               "N" = c("?","?","?","?")),
        #custom.note = c("注:*** p < 0.01; ** p < 0.05; * p < 0.1,括号内为标准误"),
        leading.zero = F,
        #ci.force = c(T,T,T,T),
        group = list("内生属性" = 1:2,"节点属性" = 3,"网络协变量" = 4:6),
        file = "result.doc")
```

```
## processing file: file162462eb53ca.Rmd
```

```
## output file: file162462eb53ca.knit.md~
```

```
## "D:/Program Files/RStudio/bin/quarto/bin/tools/pandoc" +RTS -K512m -RTS file162462eb53ca.knit.md~ --to html4 --from markdown+autolink_bare_uris+tex_math_single_backslash --output pandoc162438994220.doc --lua-filter "D:\Program Files\R\R-4.1.0\library\rmarkdown\rmarkdown\lua\pagebreak.lua" --lua-filter "D:\Program Files\R\R-4.1.0\library\rmarkdown\rmarkdown\lua\latex-div.lua" --embed-resources --standalone --variable bs3=TRUE --section-divs --template "D:\Program Files\R\R-4.1.0\library\rmarkdown\rmd\h\default.html" --no-highlight --variable highlightjs=1 --variable theme=bootstrap --mathjax --variable "mathjax-url=https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML" --include-in-header "C:\Users\wkp\AppData\Local\Temp\RtmpOCt95M\rmarkdown-str16245627790e.html"
```

```
## 
## Output created: result.doc
```
