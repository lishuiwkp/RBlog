---
title: "利用R语言运行ERGM"
author: "wkp"
date: 2021-04-18T21:13:14-05:00
---



## ERG modeling

This is an R Markdown document about how to model ERGM.
You can embed an R code chunk like this:



```r
setwd("E:\\Office Account\\2021\\指数随机图模型2")
# 加载包
library(network)
library(sna)
library(ergm)
library(readxl)
## Step1:导入数据
data1 <- read_xlsx("data1.xlsx")
data1 <- as.matrix(data1)
# 利用network包将原始数据转化为network对象
net1 <- as.network(data1,directed = F)
class(net1)
## [1] "network"
# 简单的描述统计：度数分布
summary(net1 ~ degree(1) + degree(2) + degree(3))
## degree1 degree2 degree3 
##       6       2       1
# 利用sna包的gplot函数进行可视化，默认的可视化方式是fruchtermanreingold
# 默认的可视化方式是fruchtermanreingold,可以调整mode参数进行更改
# 另外：gmode参数指定--单向、无向,具体可以参照sna包gplot函数
gplot(net1,gmode = "graph")
```

<img src="/post/2021-04-21-r-ergm/Rmarkdown_files/figure-html/unnamed-chunk-1-1.png" width="672" />

```r
# Step2:建模
# (1)基准模型
model1 <- ergm(net1 ~ edges,
               control=control.ergm(MCMC.samplesize=100000, 
                                    MCMC.burnin=1000000, 
                                    MCMC.interval=1000, 
                                    seed = 567))
summary(model1)
## Call:
## ergm(formula = net1 ~ edges, control = control.ergm(MCMC.samplesize = 1e+05, 
##     MCMC.burnin = 1e+06, MCMC.interval = 1000, seed = 567))
## 
## Maximum Likelihood Results:
## 
##       Estimate Std. Error MCMC % z value Pr(>|z|)    
## edges   -1.395      0.215      0  -6.492   <1e-04 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
##      Null Deviance: 188.5  on 136  degrees of freedom
##  Residual Deviance: 135.6  on 135  degrees of freedom
##  
## AIC: 137.6  BIC: 140.5  (Smaller is better. MC Std. Err. = 0)

# (2)加入内生结构项
# 注：网络内生项可以根据模型调试加入，也可以通过模体分析再加入
model2 <- ergm(net1 ~ edges + gwesp(0.1,T),
               control=control.ergm(MCMC.samplesize=100, 
                                    MCMC.burnin=100, 
                                    MCMC.interval=100, 
                                    seed = 567))

# (3)加入节点属性
node_attr <- read_xlsx("patent.xlsx")
net2 <- network(net1,vertex.attr = node_attr,
                vertex.attrnames = "patent",
                directed = F)
summary(net2)
## Network attributes:
##   vertices = 17
##   directed = FALSE
##   hyper = FALSE
##   loops = FALSE
##   multiple = FALSE
##   bipartite = FALSE
##  total edges = 27 
##    missing edges = 0 
##    non-missing edges = 27 
##  density = 0.1985294 
## 
## Vertex attributes:
## 
##  patent:
##    numeric valued attribute
##    attribute summary:
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    5.00   15.00   28.00   72.29   88.00  289.00 
##   vertex.names:
##    character valued attribute
##    17 valid vertex names
## 
## No edge attributes
## 
## Network edgelist matrix:
##       [,1] [,2]
##  [1,]    2    1
##  [2,]    3    1
##  [3,]    5    1
##  [4,]    7    1
##  [5,]    8    1
##  [6,]    9    1
##  [7,]    3    2
##  [8,]    5    2
##  [9,]    6    2
## [10,]    8    2
## [11,]    5    3
## [12,]    6    3
## [13,]    8    3
## [14,]    7    4
## [15,]    8    5
## [16,]    9    5
## [17,]   16    5
## [18,]    7    6
## [19,]   12    6
## [20,]   13    6
## [21,]   14    6
## [22,]   13    7
## [23,]   14    7
## [24,]   15   10
## [25,]   17   11
## [26,]   13   12
## [27,]   14   13
model3 <- ergm(net2 ~ edges + gwesp(0.1,T) + nodecov("patent"),
               control=control.ergm(MCMC.samplesize=100, 
                                    MCMC.burnin=100,  
                                    MCMC.interval=100,
                                    seed = 567))
summary(model3)
## Call:
## ergm(formula = net2 ~ edges + gwesp(0.1, T) + nodecov("patent"), 
##     control = control.ergm(MCMC.samplesize = 100, MCMC.burnin = 100, 
##         MCMC.interval = 100, seed = 567))
## 
## Monte Carlo Maximum Likelihood Results:
## 
##                  Estimate Std. Error MCMC % z value Pr(>|z|)    
## edges           -3.320132   0.557965      0  -5.950  < 1e-04 ***
## gwesp.fixed.0.1  0.900361   0.435189      0   2.069  0.03856 *  
## nodecov.patent   0.004902   0.001785      0   2.746  0.00602 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
##      Null Deviance: 188.5  on 136  degrees of freedom
##  Residual Deviance: 116.4  on 133  degrees of freedom
##  
## AIC: 122.4  BIC: 131.1  (Smaller is better. MC Std. Err. = 0.2588)

# (4)加入外生协变量
tech_proximity <- read_xlsx("tech_proximity.xlsx",col_names = T)
tech_proximity <- as.matrix(tech_proximity)
net2 %e% "tech_proximity" <- tech_proximity
summary(net2)
## Network attributes:
##   vertices = 17
##   directed = FALSE
##   hyper = FALSE
##   loops = FALSE
##   multiple = FALSE
##   bipartite = FALSE
##  total edges = 27 
##    missing edges = 0 
##    non-missing edges = 27 
##  density = 0.1985294 
## 
## Vertex attributes:
## 
##  patent:
##    numeric valued attribute
##    attribute summary:
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    5.00   15.00   28.00   72.29   88.00  289.00 
##   vertex.names:
##    character valued attribute
##    17 valid vertex names
## 
## Edge attributes:
## 
##  tech_proximity:
##    numeric valued attribute
##    attribute summary:
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.7101  0.8946  0.9671  0.9301  0.9913  1.0000 
## 
## Network edgelist matrix:
##       [,1] [,2]
##  [1,]    2    1
##  [2,]    3    1
##  [3,]    5    1
##  [4,]    7    1
##  [5,]    8    1
##  [6,]    9    1
##  [7,]    3    2
##  [8,]    5    2
##  [9,]    6    2
## [10,]    8    2
## [11,]    5    3
## [12,]    6    3
## [13,]    8    3
## [14,]    7    4
## [15,]    8    5
## [16,]    9    5
## [17,]   16    5
## [18,]    7    6
## [19,]   12    6
## [20,]   13    6
## [21,]   14    6
## [22,]   13    7
## [23,]   14    7
## [24,]   15   10
## [25,]   17   11
## [26,]   13   12
## [27,]   14   13
model4 <- ergm(net2 ~ edges + gwesp(0.1,T) + 
               nodecov("patent") + edgecov(tech_proximity),
               control=control.ergm(MCMC.samplesize=100, 
                                    MCMC.burnin=100,  
                                    MCMC.interval=100,
                                    seed = 567))
# 模型有效性检验
# (1)拟合优度检验
# 以model4为例
model4_gof <- gof(model4,GOF = ~ degree + espartners+ dspartners,
                  verbose = T, burnin = 10000,
                  interval = 10000)
par(mfrow = c(2,3),mar = c(4,4,4,1),cex.main = .9,cex.lab = .9,cex.axis = .75)
plot(model4_gof,plotlogodds = T)
# (2)MCMC检验
mcmc.diagnostics(model4)
```

<img src="/post/2021-04-21-r-ergm/Rmarkdown_files/figure-html/unnamed-chunk-1-2.png" width="672" />

```
## Sample statistics summary:
## 
## Iterations = 2250:44350
## Thinning interval = 50 
## Number of chains = 1 
## Sample size per chain = 843 
## 
## 1. Empirical mean and standard deviation for each variable,
##    plus standard error of the mean:
## 
##                           Mean       SD Naive SE Time-series SE
## edges                   0.1566    5.141   0.1771         0.4193
## gwesp.fixed.0.1         0.2027    6.680   0.2301         0.5309
## nodecov.patent         -5.2289 1052.424  36.2474        90.1477
## edgecov.tech_proximity  0.1068    4.838   0.1666         0.3970
## 
## 2. Quantiles for each variable:
## 
##                             2.5%      25%      50%     75%    97.5%
## edges                     -9.000   -4.000   0.0000   4.000   10.000
## gwesp.fixed.0.1          -12.519   -4.744   0.2416   4.773   12.847
## nodecov.patent         -2068.450 -714.000 -17.0000 779.000 1993.950
## edgecov.tech_proximity    -8.701   -3.644   0.1417   3.635    9.359
## 
## 
## Are sample statistics significantly different from observed?
##                edges gwesp.fixed.0.1 nodecov.patent edgecov.tech_proximity
## diff.      0.1565836       0.2026806     -5.2289442              0.1067575
## test stat. 0.3734371       0.3818022     -0.0580042              0.2689348
## P-val.     0.7088231       0.7026081      0.9537453              0.7879799
##            Overall (Chi^2)
## diff.                   NA
## test stat.        7.067892
## P-val.            0.140076
## 
## Sample statistics cross-correlations:
##                            edges gwesp.fixed.0.1 nodecov.patent
## edges                  1.0000000       0.9361785      0.8559619
## gwesp.fixed.0.1        0.9361785       1.0000000      0.8507041
## nodecov.patent         0.8559619       0.8507041      1.0000000
## edgecov.tech_proximity 0.9984217       0.9373776      0.8645190
##                        edgecov.tech_proximity
## edges                               0.9984217
## gwesp.fixed.0.1                     0.9373776
## nodecov.patent                      0.8645190
## edgecov.tech_proximity              1.0000000
## 
## Sample statistics auto-correlation:
## Chain 1 
##             edges gwesp.fixed.0.1 nodecov.patent edgecov.tech_proximity
## Lag 0   1.0000000       1.0000000      1.0000000              1.0000000
## Lag 50  0.6674906       0.6477605      0.6939806              0.6708509
## Lag 100 0.4757914       0.4563639      0.5097293              0.4800172
## Lag 150 0.3432779       0.3314066      0.3695527              0.3446701
## Lag 200 0.2663470       0.2359646      0.2846662              0.2653283
## Lag 250 0.2229093       0.2079348      0.2558975              0.2226895
## 
## Sample statistics burn-in diagnostic (Geweke):
## Chain 1 
## 
## Fraction in 1st window = 0.1
## Fraction in 2nd window = 0.5 
## 
##                  edges        gwesp.fixed.0.1         nodecov.patent 
##                -0.5957                -0.2136                -0.7634 
## edgecov.tech_proximity 
##                -0.7333 
## 
## Individual P-values (lower = worse):
##                  edges        gwesp.fixed.0.1         nodecov.patent 
##              0.5513536              0.8308716              0.4452521 
## edgecov.tech_proximity 
##              0.4633997 
## Joint P-value (lower = worse):  0.0935512 .
```

<img src="/post/2021-04-21-r-ergm/Rmarkdown_files/figure-html/unnamed-chunk-1-3.png" width="672" />

```
## 
## MCMC diagnostics shown here are from the last round of simulation, prior to computation of final parameter estimates. Because the final estimates are refinements of those used for this simulation run, these diagnostics may understate model performance. To directly assess the performance of the final model on in-model statistics, please use the GOF command: gof(ergmFitObject, GOF=~model).
## 至此指数随机图模型的三类变量都依次加入模型中
# 模型整理
library(texreg)
screenreg(list(model1,model2,model3,model4),digits = 3)
## 
## ========================================================================
##                         Model 1      Model 2      Model 3      Model 4  
## ------------------------------------------------------------------------
## edges                    -1.396 ***   -2.714 ***   -3.320 ***   -8.284 *
##                          (0.215)      (0.531)      (0.558)      (3.601) 
## gwesp.fixed.0.1                        1.099 *      0.900 *      0.871 *
##                                       (0.435)      (0.435)      (0.444) 
## nodecov.patent                                      0.005 **     0.004 *
##                                                    (0.002)      (0.002) 
## edgecov.tech_proximity                                           5.525  
##                                                                 (3.930) 
## ------------------------------------------------------------------------
## AIC                     137.553      129.078      122.375      122.080  
## BIC                     140.466      134.903      131.112      133.731  
## Log Likelihood          -67.777      -62.539      -58.187      -57.040  
## ========================================================================
## *** p < 0.001; ** p < 0.01; * p < 0.05
```
