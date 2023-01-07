---
title: "network与igraph类互转"
author: "wkp"
date: "2022-05-12"
---



## R Markdown



```r
## network类与igraph类互转
# 作者：wkp
# 设置路径及加载包
setwd("E:\\Office Account\\2021\\network类与igraph类互转")
library(readxl)
```

```
## Warning: 程辑包'readxl'是用R版本4.1.3 来建造的
```

```r
library(network)
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

```r
library(igraph)
```

```
## Warning: 程辑包'igraph'是用R版本4.1.3 来建造的
```

```
## 
## 载入程辑包：'igraph'
```

```
## The following objects are masked from 'package:network':
## 
##     %c%, %s%, add.edges, add.vertices, delete.edges, delete.vertices,
##     get.edge.attribute, get.edges, get.vertex.attribute, is.bipartite,
##     is.directed, list.edge.attributes, list.vertex.attributes,
##     set.edge.attribute, set.vertex.attribute
```

```
## The following objects are masked from 'package:stats':
## 
##     decompose, spectrum
```

```
## The following object is masked from 'package:base':
## 
##     union
```

```r
library(sna)
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
## 
## 载入程辑包：'sna'
```

```
## The following objects are masked from 'package:igraph':
## 
##     betweenness, bonpow, closeness, components, degree, dyad.census,
##     evcent, hierarchy, is.connected, neighborhood, triad.census
```

```r
# 方法1：直接在数据导入时定义类型好
# network类导入
data1 <- read.csv("data.csv")
data1 <- as.matrix(data1)
net1 <- as.network(data1,directed = F)
plot(net1)
```

<img src="/post/2021-05-12-r-network_igraph/network_igraph_files/figure-html/cars-1.png" width="672" />

```r
summary(net1)
```

```
## Network attributes:
##   vertices = 226
##   directed = FALSE
##   hyper = FALSE
##   loops = FALSE
##   multiple = FALSE
##   bipartite = FALSE
##  total edges = 196 
##    missing edges = 0 
##    non-missing edges = 196 
##  density = 0.007708948 
## 
## Vertex attributes:
##   vertex.names:
##    character valued attribute
##    226 valid vertex names
## 
## No edge attributes
## 
## Network edgelist matrix:
##        [,1] [,2]
##   [1,]    2    1
##   [2,]    3    1
##   [3,]    9    1
##   [4,]   15    1
##   [5,]   17    1
##   [6,]   18    1
##   [7,]   19    1
##   [8,]   22    1
##   [9,]   32    1
##  [10,]   33    1
##  [11,]   50    1
##  [12,]   51    1
##  [13,]  102    1
##  [14,]  103    1
##  [15,]  106    1
##  [16,]  107    1
##  [17,]  108    1
##  [18,]  109    1
##  [19,]  110    1
##  [20,]  111    1
##  [21,]  112    1
##  [22,]  113    1
##  [23,]    3    2
##  [24,]    8    2
##  [25,]    9    2
##  [26,]   12    2
##  [27,]   13    2
##  [28,]   18    2
##  [29,]   28    2
##  [30,]   43    2
##  [31,]   66    2
##  [32,]   69    2
##  [33,]   90    2
##  [34,]    4    3
##  [35,]    9    3
##  [36,]   12    3
##  [37,]   18    3
##  [38,]   21    4
##  [39,]   95    4
##  [40,]   11    5
##  [41,]   16    5
##  [42,]   17    5
##  [43,]   70    5
##  [44,]  210    5
##  [45,]  211    5
##  [46,]   29    6
##  [47,]   38    6
##  [48,]   49    6
##  [49,]  194    6
##  [50,]  195    6
##  [51,]  196    6
##  [52,]  197    6
##  [53,]   10    7
##  [54,]  136    7
##  [55,]   13    8
##  [56,]   18    9
##  [57,]   19    9
##  [58,]   27    9
##  [59,]   37    9
##  [60,]  163    9
##  [61,]   71   11
##  [62,]   77   11
##  [63,]   17   12
##  [64,]   24   12
##  [65,]   25   12
##  [66,]   26   12
##  [67,]   31   12
##  [68,]   68   13
##  [69,]   41   14
##  [70,]   42   14
##  [71,]   72   14
##  [72,]   75   14
##  [73,]   76   14
##  [74,]   17   15
##  [75,]   74   16
##  [76,]  212   16
##  [77,]   25   17
##  [78,]   31   17
##  [79,]  193   17
##  [80,]  211   17
##  [81,]   58   18
##  [82,]   51   19
##  [83,]  179   19
##  [84,]   57   20
##  [85,]   65   20
##  [86,]   73   20
##  [87,]  160   20
##  [88,]   36   21
##  [89,]   92   21
##  [90,]   95   21
##  [91,]  127   21
##  [92,]   52   23
##  [93,]   53   23
##  [94,]   54   23
##  [95,]  117   23
##  [96,]   25   24
##  [97,]   26   24
##  [98,]  125   24
##  [99,]   26   25
## [100,]   31   25
## [101,]  164   25
## [102,]   37   27
## [103,]   52   28
## [104,]  205   28
## [105,]   78   30
## [106,]   81   30
## [107,]  222   30
## [108,]   82   31
## [109,]  202   31
## [110,]   35   34
## [111,]  116   34
## [112,]  149   35
## [113,]   62   36
## [114,]  225   36
## [115,]  226   36
## [116,]  191   37
## [117,]  192   37
## [118,]   40   39
## [119,]   85   41
## [120,]   45   44
## [121,]   47   46
## [122,]   91   48
## [123,]  152   48
## [124,]  174   49
## [125,]   53   52
## [126,]  169   53
## [127,]  170   53
## [128,]  171   53
## [129,]   64   55
## [130,]  120   55
## [131,]  121   56
## [132,]  122   56
## [133,]  126   57
## [134,]  160   57
## [135,]  143   59
## [136,]  144   59
## [137,]   61   60
## [138,]  153   60
## [139,]  154   60
## [140,]  153   61
## [141,]  154   61
## [142,]  157   62
## [143,]  180   63
## [144,]  181   63
## [145,]  186   64
## [146,]  208   67
## [147,]  209   67
## [148,]  213   70
## [149,]   80   79
## [150,]   84   83
## [151,]   87   86
## [152,]   89   88
## [153,]   94   93
## [154,]   97   96
## [155,]   99   98
## [156,]  101  100
## [157,]  105  104
## [158,]  115  114
## [159,]  119  118
## [160,]  124  123
## [161,]  129  128
## [162,]  131  130
## [163,]  133  132
## [164,]  135  134
## [165,]  138  137
## [166,]  140  139
## [167,]  142  141
## [168,]  146  145
## [169,]  148  147
## [170,]  151  150
## [171,]  154  153
## [172,]  156  155
## [173,]  159  158
## [174,]  162  161
## [175,]  166  165
## [176,]  168  167
## [177,]  170  169
## [178,]  171  169
## [179,]  171  170
## [180,]  173  172
## [181,]  176  175
## [182,]  178  177
## [183,]  183  182
## [184,]  185  184
## [185,]  188  187
## [186,]  190  189
## [187,]  199  198
## [188,]  201  200
## [189,]  204  203
## [190,]  207  206
## [191,]  215  214
## [192,]  217  216
## [193,]  219  218
## [194,]  221  220
## [195,]  224  223
## [196,]  226  225
```

```r
sna::degree(net1)
```

```
##   [1] 44 24 12  6 12 14  4  4 16  2  6 14  6 10  4  6 16 10  8  8 10  2  8  8 12
##  [26]  6  4  6  2  6 10  2  2  4  4  8  8  2  2  2  4  2  2  2  2  2  2  4  4  2
##  [51]  4  6 10  2  4  4  6  2  4  6  6  4  4  4  2  2  4  2  2  4  2  2  2  2  2
##  [76]  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  4  2  2  2  2  2
## [101]  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2
## [126]  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2
## [151]  2  2  6  6  2  2  2  2  2  4  2  2  2  2  2  2  2  2  6  6  6  2  2  2  2
## [176]  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2
## [201]  2  2  2  2  2  2  2  2  2  2  4  2  2  2  2  2  2  2  2  2  2  2  2  2  4
## [226]  4
```

```r
class(net1)
```

```
## [1] "network"
```

```r
# igraph类导入
data2 <- read.csv("data.csv",header = T)
data2 <- as.matrix(data2)
net2 <- graph.adjacency(data2,mode = "undirected")
net2.1 <- graph_from_adjacency_matrix(data2,mode = "undirected")
plot(net2)
```

<img src="/post/2021-05-12-r-network_igraph/network_igraph_files/figure-html/cars-2.png" width="672" />

```r
summary(net2)
```

```
## IGRAPH 4a62515 UN-- 226 196 -- 
## + attr: name (v/c)
```

```r
summary(net2.1)
```

```
## IGRAPH 4a62ed2 UN-- 226 196 -- 
## + attr: name (v/c)
```

```r
igraph::degree(net2)
```

```
## HITA.C TOKE.C FJIE.C FUJX.C TOYT.C SIEI.C KTKT.C TOBA.C MITQ.C KETR.C TOZK.C 
##     22     12      6      3      6      7      2      2      8      1      3 
## KAWI.C TOSB.C ALLM.C HIGA.C TOYW.C KOBM.C KANT.C RENE.C SCHN.C FUIT.C HITJ.C 
##      7      3      5      2      3      8      5      4      4      5      1 
## HONF.C ISHI.C NIKN.C JFES.C RYOD.C TOEP.C LANI.C HONE.C SUMQ.C HITN.C HTBU.C 
##      4      4      6      3      2      3      1      3      5      1      1 
## HOND.C MATU.C YAWA.C SHDE.C FRAT.C VLEL.C MAAN.C ALSM.C COEN.C CHUB.C ESSO.C 
##      2      2      4      4      1      1      1      2      1      1      1 
## MOBI.C ETHI.C JOHJ.C ROEC.C FRAU.C HIMI.C HITW.C SAOL.C TOLG.C YAMT.C DASE.C 
##      1      1      1      2      2      1      2      3      5      1      2 
## HYNX.C IVSY.C OSKA.C LUCE.C MIUT.C OKUM.C MITO.C ROCW.C SEIK.C SQUA.C TOHB.C 
##      2      3      1      2      3      3      2      2      2      1      1 
## TORY.C DAIZ.C TOSZ.C TOYX.C KOYS.C WESE.C AEGE.C AICI.C HBRA.C TELF.C AMAC.C 
##      2      1      1      2      1      1      1      1      1      1      1 
## BAKO.C BRAX.C HEWP.C BURK.C CANO.C CNVA.C BRIM.C COGE.C CSFC.C THLS.C DAIM.C 
##      1      1      1      1      1      1      1      1      1      1      1 
## SCCH.C DORY.C FICO.C FJTS.C FUJH.C TOSI.C MITY.C GEVA.C FARB.C GRUA.C NOTH.C 
##      1      1      1      1      1      1      2      1      1      1      1 
## HAMM.C SZKA.C HISC.C HISD.C HISU.C HISY.C HISA.C HISZ.C HITO.C HITQ.C HTAG.C 
##      1      1      1      1      1      1      1      1      1      1      1 
## HTCM.C NIEJ.C TOJC.C HITS.C HIVI.C SHOB.C NITV.C HOYA.C HOYB.C HTTE.C GLDS.C 
##      1      1      1      1      1      1      1      1      1      1      1 
## HYMI.C IDEK.C YOKG.C CHKU.C FOXB.C KAGG.C KLOM.C MMLR.C KOAD.C KSCT.C KOMS.C 
##      1      1      1      1      1      1      1      1      1      1      1 
## KAJI.C KOOC.C KIMA.C ETRI.C KUBI.C SBTS.C LERD.C AGIL.C LOCK.C GENK.C AMTT.C 
##      1      1      1      1      1      1      1      1      1      1      1 
## LSIL.C MACM.C WEYE.C MAON.C GILB.C UYCM.C MEID.C OTIS.C MIMO.C MISI.C MRIS.C 
##      1      1      1      1      1      1      1      1      1      3      3 
## MITA.C MITC.C OHBA.C MONU.C CSIR.C NATC.C NIDD.C BRUD.C NIKE.C TOIL.C NITE.C 
##      1      1      1      1      1      2      1      1      1      1      1 
## IREC.C NSMO.C NILE.C OSAG.C TOHG.C YAZA.C PEKE.C DIEH.C PLAC.C QUDI.C SMIK.C 
##      1      1      1      3      3      3      1      1      1      1      1 
## RANK.C AMET.C NIDE.C NPDE.C SHAC.C RORO.C UYBS.C SASK.C YMHA.C SHIH.C SGSA.C 
##      1      1      1      1      1      1      1      1      1      1      1 
## CNSZ.C SHAF.C DNIS.C SKHK.C TAKJ.C SHIA.C BSHB.C INFN.C UNCM.C WELA.C SRER.C 
##      1      1      1      1      1      1      1      1      1      1      1 
## SCHF.C STTS.C IBMC.C TKEL.C TODK.C FUKI.C HAZA.C TORN.C TORA.C NIUG.C OKAY.C 
##      1      1      1      1      1      1      1      1      1      1      1 
## BURS.C DAHM.C TOZS.C TOYM.C UNAY.C SUWB.C UNIW.C IMMR.C USAT.C REGC.C UTON.C 
##      1      2      1      1      1      1      1      1      1      1      1 
## IOWA.C WACR.C YANA.C KZAK.C YASW.C NTTE.C 
##      1      1      1      1      2      2
```

```r
igraph::degree(net2.1)
```

```
## HITA.C TOKE.C FJIE.C FUJX.C TOYT.C SIEI.C KTKT.C TOBA.C MITQ.C KETR.C TOZK.C 
##     22     12      6      3      6      7      2      2      8      1      3 
## KAWI.C TOSB.C ALLM.C HIGA.C TOYW.C KOBM.C KANT.C RENE.C SCHN.C FUIT.C HITJ.C 
##      7      3      5      2      3      8      5      4      4      5      1 
## HONF.C ISHI.C NIKN.C JFES.C RYOD.C TOEP.C LANI.C HONE.C SUMQ.C HITN.C HTBU.C 
##      4      4      6      3      2      3      1      3      5      1      1 
## HOND.C MATU.C YAWA.C SHDE.C FRAT.C VLEL.C MAAN.C ALSM.C COEN.C CHUB.C ESSO.C 
##      2      2      4      4      1      1      1      2      1      1      1 
## MOBI.C ETHI.C JOHJ.C ROEC.C FRAU.C HIMI.C HITW.C SAOL.C TOLG.C YAMT.C DASE.C 
##      1      1      1      2      2      1      2      3      5      1      2 
## HYNX.C IVSY.C OSKA.C LUCE.C MIUT.C OKUM.C MITO.C ROCW.C SEIK.C SQUA.C TOHB.C 
##      2      3      1      2      3      3      2      2      2      1      1 
## TORY.C DAIZ.C TOSZ.C TOYX.C KOYS.C WESE.C AEGE.C AICI.C HBRA.C TELF.C AMAC.C 
##      2      1      1      2      1      1      1      1      1      1      1 
## BAKO.C BRAX.C HEWP.C BURK.C CANO.C CNVA.C BRIM.C COGE.C CSFC.C THLS.C DAIM.C 
##      1      1      1      1      1      1      1      1      1      1      1 
## SCCH.C DORY.C FICO.C FJTS.C FUJH.C TOSI.C MITY.C GEVA.C FARB.C GRUA.C NOTH.C 
##      1      1      1      1      1      1      2      1      1      1      1 
## HAMM.C SZKA.C HISC.C HISD.C HISU.C HISY.C HISA.C HISZ.C HITO.C HITQ.C HTAG.C 
##      1      1      1      1      1      1      1      1      1      1      1 
## HTCM.C NIEJ.C TOJC.C HITS.C HIVI.C SHOB.C NITV.C HOYA.C HOYB.C HTTE.C GLDS.C 
##      1      1      1      1      1      1      1      1      1      1      1 
## HYMI.C IDEK.C YOKG.C CHKU.C FOXB.C KAGG.C KLOM.C MMLR.C KOAD.C KSCT.C KOMS.C 
##      1      1      1      1      1      1      1      1      1      1      1 
## KAJI.C KOOC.C KIMA.C ETRI.C KUBI.C SBTS.C LERD.C AGIL.C LOCK.C GENK.C AMTT.C 
##      1      1      1      1      1      1      1      1      1      1      1 
## LSIL.C MACM.C WEYE.C MAON.C GILB.C UYCM.C MEID.C OTIS.C MIMO.C MISI.C MRIS.C 
##      1      1      1      1      1      1      1      1      1      3      3 
## MITA.C MITC.C OHBA.C MONU.C CSIR.C NATC.C NIDD.C BRUD.C NIKE.C TOIL.C NITE.C 
##      1      1      1      1      1      2      1      1      1      1      1 
## IREC.C NSMO.C NILE.C OSAG.C TOHG.C YAZA.C PEKE.C DIEH.C PLAC.C QUDI.C SMIK.C 
##      1      1      1      3      3      3      1      1      1      1      1 
## RANK.C AMET.C NIDE.C NPDE.C SHAC.C RORO.C UYBS.C SASK.C YMHA.C SHIH.C SGSA.C 
##      1      1      1      1      1      1      1      1      1      1      1 
## CNSZ.C SHAF.C DNIS.C SKHK.C TAKJ.C SHIA.C BSHB.C INFN.C UNCM.C WELA.C SRER.C 
##      1      1      1      1      1      1      1      1      1      1      1 
## SCHF.C STTS.C IBMC.C TKEL.C TODK.C FUKI.C HAZA.C TORN.C TORA.C NIUG.C OKAY.C 
##      1      1      1      1      1      1      1      1      1      1      1 
## BURS.C DAHM.C TOZS.C TOYM.C UNAY.C SUWB.C UNIW.C IMMR.C USAT.C REGC.C UTON.C 
##      1      2      1      1      1      1      1      1      1      1      1 
## IOWA.C WACR.C YANA.C KZAK.C YASW.C NTTE.C 
##      1      1      1      1      2      2
```

```r
class(net2)
```

```
## [1] "igraph"
```

```r
class(net2.1)
```

```
## [1] "igraph"
```

```r
## 方法2：若没有数据，直接是network对象，或直接是igraph对象，怎么转化
# network转igraph
net1.edge.matrix <- as.matrix.network(net1,matrix.type = "adjacency")
net3 <- graph.adjacency(net1.edge.matrix,mode = "undirected")
class(net3)
```

```
## [1] "igraph"
```

```r
# igraph转network
net2.edge.matrix <- as_adjacency_matrix(net2)
```

```
## as(<dsCMatrix>, "dgCMatrix") is deprecated since Matrix 1.5-0; do as(., "generalMatrix") instead
```

```r
class(net2.edge.matrix)
```

```
## [1] "dgCMatrix"
## attr(,"package")
## [1] "Matrix"
```

```r
net4 <- as.network(as.matrix(net2.edge.matrix),directed = F)
summary(net4)
```

```
## Network attributes:
##   vertices = 226
##   directed = FALSE
##   hyper = FALSE
##   loops = FALSE
##   multiple = FALSE
##   bipartite = FALSE
##  total edges = 196 
##    missing edges = 0 
##    non-missing edges = 196 
##  density = 0.007708948 
## 
## Vertex attributes:
##   vertex.names:
##    character valued attribute
##    226 valid vertex names
## 
## No edge attributes
## 
## Network edgelist matrix:
##        [,1] [,2]
##   [1,]    2    1
##   [2,]    3    1
##   [3,]    9    1
##   [4,]   15    1
##   [5,]   17    1
##   [6,]   18    1
##   [7,]   19    1
##   [8,]   22    1
##   [9,]   32    1
##  [10,]   33    1
##  [11,]   50    1
##  [12,]   51    1
##  [13,]  102    1
##  [14,]  103    1
##  [15,]  106    1
##  [16,]  107    1
##  [17,]  108    1
##  [18,]  109    1
##  [19,]  110    1
##  [20,]  111    1
##  [21,]  112    1
##  [22,]  113    1
##  [23,]    3    2
##  [24,]    8    2
##  [25,]    9    2
##  [26,]   12    2
##  [27,]   13    2
##  [28,]   18    2
##  [29,]   28    2
##  [30,]   43    2
##  [31,]   66    2
##  [32,]   69    2
##  [33,]   90    2
##  [34,]    4    3
##  [35,]    9    3
##  [36,]   12    3
##  [37,]   18    3
##  [38,]   21    4
##  [39,]   95    4
##  [40,]   11    5
##  [41,]   16    5
##  [42,]   17    5
##  [43,]   70    5
##  [44,]  210    5
##  [45,]  211    5
##  [46,]   29    6
##  [47,]   38    6
##  [48,]   49    6
##  [49,]  194    6
##  [50,]  195    6
##  [51,]  196    6
##  [52,]  197    6
##  [53,]   10    7
##  [54,]  136    7
##  [55,]   13    8
##  [56,]   18    9
##  [57,]   19    9
##  [58,]   27    9
##  [59,]   37    9
##  [60,]  163    9
##  [61,]   71   11
##  [62,]   77   11
##  [63,]   17   12
##  [64,]   24   12
##  [65,]   25   12
##  [66,]   26   12
##  [67,]   31   12
##  [68,]   68   13
##  [69,]   41   14
##  [70,]   42   14
##  [71,]   72   14
##  [72,]   75   14
##  [73,]   76   14
##  [74,]   17   15
##  [75,]   74   16
##  [76,]  212   16
##  [77,]   25   17
##  [78,]   31   17
##  [79,]  193   17
##  [80,]  211   17
##  [81,]   58   18
##  [82,]   51   19
##  [83,]  179   19
##  [84,]   57   20
##  [85,]   65   20
##  [86,]   73   20
##  [87,]  160   20
##  [88,]   36   21
##  [89,]   92   21
##  [90,]   95   21
##  [91,]  127   21
##  [92,]   52   23
##  [93,]   53   23
##  [94,]   54   23
##  [95,]  117   23
##  [96,]   25   24
##  [97,]   26   24
##  [98,]  125   24
##  [99,]   26   25
## [100,]   31   25
## [101,]  164   25
## [102,]   37   27
## [103,]   52   28
## [104,]  205   28
## [105,]   78   30
## [106,]   81   30
## [107,]  222   30
## [108,]   82   31
## [109,]  202   31
## [110,]   35   34
## [111,]  116   34
## [112,]  149   35
## [113,]   62   36
## [114,]  225   36
## [115,]  226   36
## [116,]  191   37
## [117,]  192   37
## [118,]   40   39
## [119,]   85   41
## [120,]   45   44
## [121,]   47   46
## [122,]   91   48
## [123,]  152   48
## [124,]  174   49
## [125,]   53   52
## [126,]  169   53
## [127,]  170   53
## [128,]  171   53
## [129,]   64   55
## [130,]  120   55
## [131,]  121   56
## [132,]  122   56
## [133,]  126   57
## [134,]  160   57
## [135,]  143   59
## [136,]  144   59
## [137,]   61   60
## [138,]  153   60
## [139,]  154   60
## [140,]  153   61
## [141,]  154   61
## [142,]  157   62
## [143,]  180   63
## [144,]  181   63
## [145,]  186   64
## [146,]  208   67
## [147,]  209   67
## [148,]  213   70
## [149,]   80   79
## [150,]   84   83
## [151,]   87   86
## [152,]   89   88
## [153,]   94   93
## [154,]   97   96
## [155,]   99   98
## [156,]  101  100
## [157,]  105  104
## [158,]  115  114
## [159,]  119  118
## [160,]  124  123
## [161,]  129  128
## [162,]  131  130
## [163,]  133  132
## [164,]  135  134
## [165,]  138  137
## [166,]  140  139
## [167,]  142  141
## [168,]  146  145
## [169,]  148  147
## [170,]  151  150
## [171,]  154  153
## [172,]  156  155
## [173,]  159  158
## [174,]  162  161
## [175,]  166  165
## [176,]  168  167
## [177,]  170  169
## [178,]  171  169
## [179,]  171  170
## [180,]  173  172
## [181,]  176  175
## [182,]  178  177
## [183,]  183  182
## [184,]  185  184
## [185,]  188  187
## [186,]  190  189
## [187,]  199  198
## [188,]  201  200
## [189,]  204  203
## [190,]  207  206
## [191,]  215  214
## [192,]  217  216
## [193,]  219  218
## [194,]  221  220
## [195,]  224  223
## [196,]  226  225
```
