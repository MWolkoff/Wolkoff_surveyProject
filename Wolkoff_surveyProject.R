# Wolkoff - Bioinformatics Project

# An Analysis of Rodent and General Taxa Population Change from 1977 to 2002

## Survey metadata available from: <http://esapubs.org/archive/ecol/E090/118/Portal_rodent_metadata.htm>.

## Required programs and files

### Rodent Survey Data:

download.file("http://files.figshare.com/2236372/combined.csv",  "C:/Users/Matthew/Desktop/Bioinformatic Programs/R Files/data/portal_data_joined.csv")

rawData <- read.csv("C:/Users/Matthew/Desktop/Bioinformatic Programs/R Files/data/portal_data_joined.csv")

### Install and Load dplyr, ggplot:

install.packages("dplyr")
install.packages("ggplot2")

library(dplyr)
library(ggplot2)

## Filter and organize data

### 1) Remove non-Rodent entries
### 2) Isolate desired columns
### 3) Remove "sp." entries from the "species" column
### 4) Tally species occurences by year

filteredData <- filter(rawData, taxa == "Rodent") %>% 
  select(year, species) %>%
  filter(species != "sp.") %>% 
  group_by(year, species) %>% tally

## Determine most populous species and change column names:

meanData <- aggregate(filteredData[, 3], list(filteredData$species), mean) %>% filter(n >= 100)

colnames(meanData) <- c("species", "population")

## Filter based on most populous species (done manually due to programmer copout)
### Assign target species to value "target":

target <- c("baileyi", "megalotis", "merriami", "ordii", "penicillatus", "spectabilis")

### Create new dataframe by filtering target species:

finalData <- filter(filteredData, species %in% target)

## Plot rodent species population-over-time data and save:

pdf("Rodent_pop.pdf")
ggplot(data = finalData, aes(x = year, y = n, by = species, color = species)) + geom_line(aes(color = species), size=1.2) + xlab("Year") + ylab("Number of Specimens") + ggtitle("Rodent Species Populations from 1977-2002")
dev.off()

## Plot comparison of species means and save:

pdf("Rodent_mean.pdf")
ggplot(data=meanData, aes(x=species, y=population)) + geom_bar(stat="identity", aes(fill=species)) + xlab("Species") + ylab("Population") + ggtitle("Mean Rodent Species Populations from 1977-2002")
dev.off()

## Assess changes in general taxa populations over time:
### Filter taxa data:

taxaData <- select(rawData, year, taxa) %>%
  group_by(year, taxa) %>% tally

### Plot changes in taxa population over time and save:

pdf("Taxa_pop.pdf")
ggplot(data = taxaData, aes(x = year, y = n, by = taxa, color = taxa)) + geom_line(aes(color = taxa), size=1.2) + xlab("Year") + ylab("Taxa Population") + ggtitle("Taxa Populations from 1977-2002")
dev.off()

## Perform ANOVA analysis on rodent species population changes
### Fit the data model for Anova analysis:

fitData <- aov(n ~ species, data = filteredData)

### Summarize fit model data, show results:

summary(fitData)

## Cite R and relevant packages:
### 1) R
### 2) RStudio
### 3) ggplot2
### 4) dplyr

citation()
RStudio.Version()
citation("ggplot2")
citation("dplyr")

print(fitData)

# The End. You have survived my clunky coding - well done noble adventurer!
