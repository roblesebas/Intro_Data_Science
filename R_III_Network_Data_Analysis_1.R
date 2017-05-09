# Set working directory

setwd("C:/Users/Sebastian/Google Drive/DOCTORADO/INTERNSHIP/Kentucky University/Activities Internship/D Colloquium/Seccion_II/Data")

# Install libraries 

install.packages(c("pdftools", "XML", "xml2", "plyr", "lubridate", 
                   "readr", "stringr", "igraph", "gpairs", "corrplot", 
                   "gplots", "lattice", "lavaan", "ggplot2", "devtools", 
                   "ggbiplot", "plyr", "scales", "grid", "nFactors", 
                   "GPArotation", "gplots", "RColorBrewer", "semPlot"))

install_github('ggbiplot','vqv')

# Libraries

install_github('ggbiplot','vqv')
library(pdftools) # Reading PDFs
library(XML)
library(xml2)
library(plyr)
library(lubridate)
library(readr)
library(stringr)
library(igraph)
library(gpairs) # Descriptive analysis: Relationships between continuos variables
library(corrplot) # Descriptive analysis: Relationships between continuos variables
library(gplots) # Descriptive analysis: Relationships between continuos variables
library(lattice) # Comparing Groups: Tables and Visualizations
library(lavaan) # Structural Equation Modeling
library(ggplot2)
library(devtools)
library(ggbiplot)
library(scales)
library(grid)
library(nFactors)  # Eploratory factor analysis
library(GPArotation) # Eploratory factor analysis
library(gplots)
library(RColorBrewer)
library(QuantPsyc)
library(semPlot)

# Getting Data ####

# Part I - csv files from our pc

user.raw <- read.csv("users.csv", stringsAsFactors = FALSE)
links.raw <- read.csv("invitations.csv", stringsAsFactors = FALSE)

# Part II - Reading data from webs

cust.df <- read.csv("http://goo.gl/PmPkaG")
seg.df <- read.csv("http://goo.gl/qw303p")
brand.ratings <- read.csv("http://goo.gl/IQl8nc")
sat.df <- read.csv("http://goo.gl/HKnl74")
satData <- read.csv("http://goo.gl/UDv12g")

# Part II - Reading pdfs

library(pdftools)
pdf_file <- pdf_text("http://arxiv.org/pdf/1403.2805.pdf")
pdf_file <- pdf_text("pdffile.pdf")

# Part III 

data_doi <- read.csv("doi.csv", stringsAsFactors = FALSE)

authors <- data.frame(doi = character(), author= character(), stringsAsFactors = FALSE)
ref_ID_DOI_UNKNOWN <- data.frame(doi = character(), stringsAsFactors = FALSE)
for (i in data_doi$doi) {
        ##for (i in ensayo) {
        doi <- i
        url <- paste0("http://api.crossref.org/works/", doi, ".xml")
        xml_data_1 = try(xmlParse(url), silent = TRUE );
        ##xml_data_1 <- xmParse(url)
        if (class(xml_data_1) == "try-error") {
                ref_ID_DOI_UNKNOWNS <- data.frame(doi = i)
                ref_ID_DOI_UNKNOWN = rbind(ref_ID_DOI_UNKNOWN, ref_ID_DOI_UNKNOWNS)
        } else  {
                xml_data_2 <- xmlToList(xml_data_1)
                
                if (as.list(xml_data_2[["query_result"]][["body"]][["query"]][["doi"]][[".attrs"]]) == "journal_article"){
                        if (is.null(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_article"]][["contributors"]]))
                        {next} else {
                                author_ref <- as.list(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_article"]][["contributors"]])
                                
                                author_1_ref <- ldply(author_ref, data.frame)
                                author_2_ref <- author_1_ref[author_1_ref$.attrs == "author", c(2,3) ]
                                
                                authorss <- data.frame(doi= doi, author = paste0(author_2_ref$surname, ", ", author_2_ref$given_name))
                                
                                authors = rbind(authors, authorss)
                        } 
                }else {next}
        }
}

# Part  - Reading bibtex files

bib.file <- read_file("SNManagement.bib")
split <- strsplit(bib.file, "}\n")

author.re <- "Author = [{]([[:alpha:][:punct:][:blank:][:space:]]+)[}]"
title.re <- "Title = [{][{](.*?)[}][}]"
journal.re <- "Journal = [{][{](.*?)[}][}]"
year.re <- "Year = [{][{](.*?)[}][}]"
volume.re <- "Volume = [{][{](.*?)[}][}]"
pages.re <- "Pages = [{][{](.*?)[}][}]" 
month.re <- "Month = [{][{](.*?)[}][}]"
abstract.re <- "Abstract = [{][{](.*?)[}][}]"
references.re <- "nCited-References = [{][{](.*?)[}][}]"
doi.re <- "nDOI = [{][{](.*?)[}][}]"

author <- data.frame(author = str_match_all(split, author.re), stringsAsFactors = FALSE)[2]
title <- data.frame(title = str_match_all(split, title.re), stringsAsFactors = FALSE)[2]
year <- data.frame(year = str_match_all(split, year.re), stringsAsFactors = FALSE)[2]
journal <- data.frame(journal = str_match_all(split, journal.re), stringsAsFactors = FALSE)[2]
volume <- data.frame(volume = str_match_all(split, volume.re), stringsAsFactors = FALSE)[2]
pages <- data.frame(pages = str_match_all(split, pages.re), stringsAsFactors = FALSE)[2]
month <- data.frame(month = str_match_all(split, month.re), stringsAsFactors = FALSE)[2]
abstract <- data.frame(abstract = str_match_all(split, abstract.re), stringsAsFactors = FALSE)[2]
references <- data.frame(references = str_match_all(split, references.re), stringsAsFactors = FALSE)[2]
doi <- data.frame(doi = str_match_all(split, doi.re), stringsAsFactors = FALSE)[2]

bib.df <- data.frame(author = author, title = title, journal = journal, year = year , 
                     volume = volume, pages = pages, month = month, abstract = abstract, 
                     doi = doi, references = references)

# Cleaning Data  ####

# Part I

# Everything in lowercase, Organize Date format, Delete unnecessary columns

user.raw <- read.csv("users.csv", stringsAsFactors = FALSE)
links.raw <- read.csv("invitations.csv", stringsAsFactors = FALSE)

user.cleaned.0 <- data.frame(firstname = tolower(user.raw$Firstname),
                             lastname = tolower(user.raw$Lastname),
                             email = tolower(user.raw$Email),
                             activation.date = mdy(user.raw$Created),
                             stringsAsFactors = FALSE)

links.cleaned.0 <- data.frame(source = tolower(links.raw$Inviter),
                              target = tolower(links.raw$Email),
                              invitation.date = mdy(links.raw$Created),
                              stringsAsFactors = FALSE)

str(user.cleaned.0)
str(links.cleaned.0)

# Create the user.cleaned data frame and add initiator

user.cleaned.1 <- merge(user.cleaned.0, links.cleaned.0[,c(2,3)],
                               by.x = "email", by.y = "target", 
                               all.y = TRUE)

initiator <- data.frame(user.cleaned.0[user.cleaned.0$email == "sam.milton@tos.com",])
initiator$invitation.date <- "2017-04-03"

user.cleaned.2 <- rbind(user.cleaned.1, initiator )

write.csv(user.cleaned.2, "user_cleaned_1.csv", row.names = FALSE)
write.csv(links.cleaned.0, "links_cleaned_1.csv", row.names = FALSE)

# Tidying Data ####

# Adding variables: adoption, activation.delay, in-degree, out-degree

user.tidied.1 <- user.cleaned.2
user.tidied.1$adoption <- ifelse(is.na(user.cleaned.2$activation.date) == TRUE,
                                 0, 1)
user.tidied.1$activation.delay <- user.cleaned.2$activation.date - user.cleaned.2$invitation.date

net <- graph.data.frame(directed = TRUE, links.cleaned.0)

user.tidied.2 <- data.frame(
        email = V(net)$name,
        in.degree = degree(net, mode = "in"),
        out.degree = degree(net, mode = "out"),
        stringsAsFactors = FALSE
)

user.tidied.3 <- merge(user.tidied.1, user.tidied.2, by = "email", all = TRUE)

# Exploratory Analysis ####

# Relationships between continuos variables, chapter 4

# Correlation matrix - Continuos variables

cust.df <- read.csv("http://goo.gl/PmPkaG")
str(cust.df)

gpairs(cust.df[ , c(2:3,5:10)])

library(gplots)
corrplot.mixed(corr = cor(cust.df[ , c(2, 3, 5:12)], use = "complete.obs"),
               upper = "ellipse", tl.pos = "lt",
               col = colorpanel(50, "red", "gray60", "blue4"))

# Comparing Groups: Tables and Visualizations

seg.df <- read.csv("http://goo.gl/qw303p")
str(seg.df)
summary(seg.df)

agg.data <- aggregate(income ~ Segment + ownHome, 
                      data = seg.df, FUN = mean)
View(agg.data)

library(lattice)
histogram(~subscribe | Segment + ownHome, data = seg.df)

# Principal Component Analysis

brand.ratings <- read.csv("http://goo.gl/IQl8nc")
str(brand.ratings)
summary(brand.ratings)

brand.subset <- brand.ratings[,c(1:9)]
brand.pca <- prcomp(brand.subset, center = TRUE, scale = TRUE)
plot(brand.pca, type = "l")
summary(brand.pca)
brand.brand <- brand.ratings[,10]

brand.g <- ggbiplot(brand.pca, obs.scale = 0.5, var.scale = 0.5,
                    groups = brand.brand, ellipse = TRUE,
                    circle = FALSE)
brand.g.g <- brand.g + scale_color_discrete(name = '') +
             theme(legend.direction = 'horizontal', 
                   legend.position = 'top')

print(brand.g.g)

# Exploratory Factor Analysis
brand.sc <- brand.ratings
brand.sc[, 1:9] <- scale(brand.ratings[, 1:9])
summary(brand.sc)

library(nFactors)
factanal(brand.sc[,1:9], factors = 3)

library(GPArotation)
brand.fa.ob <- factanal(brand.sc[, 1:9], factors=3, rotation="oblimin")

library(gplots)
library(RColorBrewer)
heatmap.2(brand.fa.ob$loadings, 
          col=brewer.pal(9, "Greens"), trace="none", key=FALSE, dend="none",
          Colv=FALSE, cexCol = 1.2,
          main="\n\n\n\n\nFactor loadings for brand adjectives")

library(semPlot)
semPaths(brand.fa.ob, what="est", residuals=FALSE,
         cut=0.3, posCol=c("white", "darkgreen"), negCol=c("white", "red"),
         edge.label.cex=0.75, nCharNodes=7)

# Inferential Analysis ####

# Linear models

        sat.df <- read.csv("http://goo.gl/HKnl74")
        str(sat.df)
        
        sat.std <- sat.df[ , -3]  # sat but remove distance
        sat.std[ , 3:7] <- scale(sat.std[ , 3:7])
        sat.std$logdist <- log(sat.df$distance)   # add transformed distance
        
        sat.std$has.child <- factor(sat.std$num.child > 0)
        
        m7 <- lm(overall ~ rides + games + wait + clean + logdist + has.child + 
                          wait:has.child, data = sat.std)
        summary(m7)

# Structural Equation Modeling 

satData <- read.csv("http://goo.gl/UDv12g")
satData$Segment <- factor(satData$Segment)
str(satData)
summary(satData)

satModel <- "SAT =~ iProdSAT + iSalesSAT
             REC =~ iProdREC + iSalesREC
             REC ~  SAT "

sat.fit <- cfa(satModel, data = satData)
semPaths(sat.fit, what = "est",
         residuals = FALSE, intercepts = FALSE, nCharNodes = 9)

# Presenting Results ####





