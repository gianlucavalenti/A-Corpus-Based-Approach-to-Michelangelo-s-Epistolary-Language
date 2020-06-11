# Import the libraries

library(ca)
library(mclm)
library(factoextra)
library(FactoMineR)
library(corregp)

interact <- FALSE



##### DATA PREPARATION #####

# Create vectors with long and short names

long_cf_names <- dir(pattern = "[0-9]{4}-[0-9]{4}[.]txt$",
                     recursive = TRUE)

short_cf_names <- gsub("^(\\d{4}-).*.txt$", "\\1",
                     long_cf_names, perl = TRUE)

# Put each document (time interval) in a vector

a <- readLines("1495-1499.txt")
b <- readLines("1505-1509.txt")
c <- readLines("1510-1514.txt")
d <- readLines("1515-1519.txt")
e <- readLines("1520-1524.txt")
f <- readLines("1525-1529.txt")
g <- readLines("1530-1534.txt")
h <- readLines("1535-1539.txt")
i <- readLines("1540-1544.txt")
l <- readLines("1545-1549.txt")
m <- readLines("1550-1554.txt")
n <- readLines("1555-1559.txt")
o <- readLines("1560-1564.txt")

# Put the queries in a variable

CruoG <- '(?mix)[bcdfgpt] (ruo) [^\\b]'
CruoS <- '(?mix)[bcdfgpt] (ro) [^\\b]'

CrieG <- '(?mix)[bcdfgpt] (rie) [^\\b]'
CrieS <- '(?mix)[bcdfgpt] (re) [^0-9] [^0-9]? [^0-9]? \\b'

schiVG <- '(?mix)(schi) [aeou] [^\\b]'
schiVS <- '(?mix)(sti) [aeou] [^\\b]'

lliG <- '(?mix) [aeiou] (lli) \\b'
lliS <- '(?mix) [aeiou] (gli) \\b'

artG <- '\\bil\\b'
artS <- '\\bel\\b'

prIVamoG <- '(amo\\b)'
prIVamoS <- '(àno\\b|emo\\b)'

prVIanoG <- '(ano\\b)'
prVIanoS <- '(ono\\b|eno\\b)'

impfVIvanoG <- '(vano\\b)'
impfVIvanoS <- '(vono\\b|veno\\b)'

futrG <- '[^\\bp](a|e|i|o|u)(rò|rai|rà|remo|rete|ranno)\\b'
futrS <- '(a|e|i|o|u)[r](rò|rai|rà|remo|rete|ranno)\\b'

congG <- '(\\babbia\\b|\\bfaccia\\b|\\bscriva\\b|\\voglia\\b|\\bdebba\\b|\\bpossa\\b|\\babbiano\\b|\\bhabbiano\\b|\\bfacciano\\b|\\bscrivano\\b|\\vogliano\\b|\\bdebbano\\b|\\bpossano\\b)'
congS <- '(\\babbi\\b|\\bfacci\\b|\\bscrivi\\b|\\vogli\\b|\\bdebbi\\b|\\bpossi\\b|\\babbino\\b|\\bhabbino\\b|\\bfaccino\\b|\\bscrivino\\b|\\voglino\\b|\\bdebbino\\b|\\bpossino\\b)'

condG <- '[^\\b](a|e|i|o|u)(rei|ria|resti|rebe|rebbe|remo|remmo|reste|rebbero|rebbono|rebero|rebono)\\b'
condS <- '(a|e|i|o|u)[r](rei|ria|resti|rebe|rebbe|remo|remmo|reste|rebbero|rebbono|rebero|rebono)\\b'

trG <- '(\\bdentro\\b|\\bdietro\\b)'
trS <- '(\\bdrento\\b|\\bdreto\\b)'

senzaG <- '\\bsenza\\b'
senzaS <- '\\bsanza\\b'

ultimG <- '\\bultim'
ultimS <- '\\butim'


# Run the queries, and then manually put the numeric outcome (= the number
# of occurrences of that feature) in a .csv file, called 'feature1.csv'.
# For running the queries, there are two options:

# Ex. 1

length(conc_re(senzaS, a, as_text = TRUE)$match)
length(conc_re(senzaS, b, as_text = TRUE)$match)
length(conc_re(senzaS, c, as_text = TRUE)$match)
length(conc_re(senzaS, d, as_text = TRUE)$match)
length(conc_re(senzaS, e, as_text = TRUE)$match)
length(conc_re(senzaS, f, as_text = TRUE)$match)
length(conc_re(senzaS, g, as_text = TRUE)$match)
length(conc_re(senzaS, h, as_text = TRUE)$match)
length(conc_re(senzaS, i, as_text = TRUE)$match)
length(conc_re(senzaS, l, as_text = TRUE)$match)
length(conc_re(senzaS, m, as_text = TRUE)$match)
length(conc_re(senzaS, n, as_text = TRUE)$match)
length(conc_re(senzaS, o, as_text = TRUE)$match)

# Ex. 2

View(conc_re(CrieG, a, as_text = TRUE))
View(conc_re(CrieG, b, as_text = TRUE))
View(conc_re(CrieG, c, as_text = TRUE))
View(conc_re(CrieG, d, as_text = TRUE))
View(conc_re(CrieG, e, as_text = TRUE))
View(conc_re(CrieG, f, as_text = TRUE))
View(conc_re(CrieG, g, as_text = TRUE))
View(conc_re(CrieG, h, as_text = TRUE))
View(conc_re(CrieG, i, as_text = TRUE))
View(conc_re(CrieG, l, as_text = TRUE))
View(conc_re(CrieG, m, as_text = TRUE))
View(conc_re(CrieG, n, as_text = TRUE))
View(conc_re(CrieG, o, as_text = TRUE))



##### CHI-SQUARED TEST #####

# Put the .csv file in a vector

(features <- read.csv("feature1.csv",
                      sep = ";",
                      row.names = 1))

# Sum all values of golden features (i.e. 14F forms) and silver
# features (i.e. 16F forms), divided into time intervals

SumG <- rowSums(features[c("CruoG", "CrieG", "schiVG", "lliG", "artG", "prIVamoG", "prVIanoG", 
                           "impfVIvanoG", "futrG", "condG", "trG", "senzaG", "ultimG",
                           "congG")], na.rm = TRUE)

SumS <- rowSums(features[c("CruoS", "CrieS", "schiVS", "lliS", "artS", "prIVamoS", "prVIanoS", 
                           "impfVIvanoS", "futrS", "condS", "trS", "senzaS", "ultimS",
                           "congS")], na.rm = TRUE)

# Convert them into two dataframes

dfG <- data.frame(SumG)
dfS <- data.frame(SumS)

# Merge the two dataframes

total <- merge(dfG, dfS, by=0)
total$Row.names <- NULL


# Check the total. The name of the documents [1495-, 1505-, 
# 1510-...] have been automatically replaced by numbers 
# [1, 2, 3...], but this operation does not affect the results.

total


# Run the chi-squared test

(total_chisq <- chisq.test(total))



##### CORRESPONDENCE ANALYSIS #####

features_ca <- ca(features)
features_ca

fviz_screeplot(features_ca, title = "figure 1") +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.line = element_line(
          colour = NULL,
          size = NULL,
          linetype = "solid",
          lineend = NULL,
          color = NULL,
          arrow = NULL,
          inherit.blank = FALSE),
        legend.position="none",
        panel.background=element_rect(fill = "white", colour = "grey50"),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())

summary(features_ca)

fviz_ca_biplot(features_ca, map="rowprincipal", repel=TRUE, 
               shape.col=19, col.row = "darkgrey",
               labelsize=4, pointsize = 1,
               title = "figure 2. Correspondence analysis") +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.line = element_line(
          colour = NULL,
          size = NULL,
          linetype = "solid",
          lineend = NULL,
          color = NULL,
          arrow = NULL,
          inherit.blank = FALSE),
        legend.position="none",
        panel.background=element_rect(fill = "white", colour = "grey50"),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())



### CORREGP ###

(features <- read.csv("feature2.csv",
                      sep = "\t",
                      row.names = 1))

ftable(features, col.vars = "time")

features.crg <- corregp(feature ~ measure * time, data = features, b = 3000)

summary(features.crg, add_ci = TRUE)
screeplot(features.crg, add_ci = TRUE,
          main = "figure 3")

plot(features.crg, x_ell = TRUE, 
     xsub = "measure.time",
     col_top = "black",
     cex_top = 1,
     cex_btm = 1,
     col_ell = "grey",
     lwd_ell = 0.3,
     main = "figure 4. Correspondence regression")
