#Wikipedia Election Results Scraper


#Packages----

# install.packages("rvest")
# install.packages("xml2")
# install.packages("magrittr")
# install.packages("RPushbullet")
library("rvest")
library("xml2")
library("magrittr")
library("dplyr")
library("stringr")



#Scrape multi-member constituencies from Wikipedia
constituencies.url <- read_html("https://en.wikipedia.org/wiki/List_of_multi-member_constituencies_in_the_United_Kingdom_and_predecessor_Parliaments")

constituencies <- constituencies.url %>%
  html_nodes("table") %>%
  html_table()

constituencies.mm <- c()
for ( a in 1:length(constituencies) ){
  
  temp <- as.data.frame(constituencies[[a]])
  temp[,c("From", "Until", "Prot", "<1832", "1832", "<1707", "1707", "<1801", "1801", "Historic County", "Historic Shire or County")] <- list(NULL)
  
  if ( any(names(temp) == "1922") != TRUE ){
    temp["1922"] <- "-"
  }
  if ( any(names(temp) == "1868") != TRUE ){
    temp["1868"] <- "-"
  }
  constituencies.mm <- rbind(constituencies.mm, temp)
  
}

constituencies.mm[,2] <- as.numeric(constituencies.mm[,2])
constituencies.mm[,3] <- as.numeric(constituencies.mm[,3])
constituencies.mm[,4] <- as.numeric(constituencies.mm[,4])
constituencies.mm[,5] <- as.numeric(constituencies.mm[,5])
constituencies.mm[is.na(constituencies.mm)] <- 0
constituencies.mm$Sum <- rowSums(constituencies.mm[-1])
constituencies.mm <- subset(constituencies.mm, constituencies.mm$Sum > 0)
constituencies.mm <- constituencies.mm[-6]

#Scrape list of Irish counties, they all had 2 members until 1885
constituencies.url <- read_html("https://en.wikipedia.org/wiki/List_of_Irish_counties_by_population")

constituencies <- constituencies.url %>%
  html_nodes("table") %>%
  html_table(fill = TRUE)

constituencies <- constituencies[[1]]
constituencies <- constituencies[2]
constituencies[,1] <- paste(constituencies[,1], "County")
names(constituencies) <- c("Constituency")
constituencies["1868"] <- 2
constituencies["1885"] <- 2
constituencies["1918"] <- 0
constituencies["1922"] <- 0

constituencies.mm <- rbind(constituencies.mm, constituencies)

#Get Years of Elections


#Election Years----

#Scrape years from Wikipedia
years.url <- read_html("https://en.wikipedia.org/wiki/List_of_United_Kingdom_general_elections")

years <- years.url %>%
  html_nodes(".plainrowheaders th:nth-child(1) a:nth-child(1)") %>%
  html_text()


#Manually Rename Double Election Years

years <- as.data.frame(years)
years$year.name <- years$years
years$year.number <- as.numeric(years$years)
years <- years[-1]

years <- subset(years, years$year.number != 1910 & years$year.number != 1974)

new.years <- c(1910, 1910, 1974, 1974)
new.names <- c("January 1910", "December 1910", "February 1974", "October 1974")
new.years <- cbind(new.years, new.names)

new.years <- as.data.frame(new.years)
names(new.years) <- c("year.number", "year.name")
new.years$year.number <- as.numeric(new.years$year.number)

years <- rbind(years, new.years)
years <- years[order(years$year.number, decreasing = FALSE), ]

#Cut off every election after and including 1868. Wikipedia articles are generally not good before this year, and parties are harder to assign.
#Maybe in future, a good source of how to assign a party label to an MP before this date could be found and this can be updated

years <- subset(years, years$year.number >= 1868)

#Results Scraping----

for (a in 1:nrow(years) ){
  
  
  tables <- data.frame(Year = rep(NA,20000),
                       Constituency = rep(NA,20000),
                       Party = rep(NA,20000),
                       Candidate = rep(NA,20000),
                       Votes = rep(NA,20000),
                       Percentage = rep(NA,20000),
                       Unopposed = rep(NA,20000))
  tables.count <- 0
  
  #Use the lists of MPs for each election to get a list of all the constituencies in that year
  mplist.url <- paste("https://en.wikipedia.org/wiki/List_of_MPs_elected_in_the_", gsub(" ", "_", years$year.name[a]), "_United_Kingdom_general_election", sep = "")
  
  mplist.page <- read_html(mplist.url)
  
  #reads the table and gets the text names
  #not every table is formatted the same, so if we pull it this way, we can reliably filter out the ones we don't need
    
  #reads the table and gets the text names
  constituencies.links <- mplist.page %>%
    html_nodes('#elected-mps td:nth-child(1) a:nth-child(1)')  %>%
    html_attrs()
  
  constituencies.links <- sapply(constituencies.links, "[[", 1)
  constituencies.links <- as.data.frame(constituencies.links)
  names(constituencies.links) <- c("links")
  constituencies.links <- subset(constituencies.links, grepl("constituency", constituencies.links$link) == TRUE)
  constituencies.links$names <- gsub("/wiki/", "", constituencies.links$link)
  constituencies.links$names <- gsub("_(UK_Parliament_constituency)", "",constituencies.links$names, fixed = TRUE)
  constituencies.links$names <- gsub("_", " ", constituencies.links$names, fixed = TRUE)
  constituencies.links$names <- gsub("\\([^][]*)", "", constituencies.links$names)
  
  constituencies.links <- unique(constituencies.links)
  
  #Go through all the constituencies for the election
  for (b in 1:nrow(constituencies.links) ){
    
    cat(paste(years$year.name[a], constituencies.links$names[b], "( a =", a, "/ b =", b, ")", "\n"))
    
    #First get the number of seats in constituency
    
    mm.sub <- subset(constituencies.mm, constituencies.mm$Constituency == constituencies.links$names[b])
    if ( nrow(mm.sub) == 0 ){
      
      seats <- 1
      
    } else {
      
      mm.sub <- mm.sub[-1]
      mm.sub <- mm.sub[,names(mm.sub) <= years$year.number[a]]
      seats <- mm.sub[length(mm.sub)]
      
    }
    
    
    #Start to scrape constituency page
    constituency.url <- paste("https://en.wikipedia.org", constituencies.links$links[b], sep = "")
    
    constituency.page <- read_html(constituency.url)
    
    #The election boxes are not easy to scrape so we get the table nodes and search the nodes for what we want
    constituency.nodes <- constituency.page %>%
      html_nodes('table')
    
    #There are two formats used on Wikipedia, this will detect which one is present and scrape it accordingly
    if ( any(constituency.nodes %>% html_text() %>% str_detect(paste("Election\nPolitical result\nCandidate", sep = ""))) == TRUE ){
      
      constituency.nodes <- subset(constituency.nodes,  (constituency.nodes %>% html_text() %>% str_detect(paste("General election ", years$year.name[a], sep = "")) == TRUE)
                                                        |
                                                        (constituency.nodes %>% html_text() %>% str_detect(paste(years$year.name[a], " general election", sep = "")) == TRUE))
      
      constituency.table <- html_table(constituency.nodes[1], fill = TRUE, header = TRUE)
      
      constituency.table <- as.data.frame(constituency.table)
      constituency.table <- subset(constituency.table, str_detect(constituency.table$Election, years$year.name[a]) == TRUE)
      
      current.table <- c()
      
      current.table <- cbind(years$year.name[a], 
                             constituencies.links$name[b], 
                             constituency.table[6], 
                             constituency.table[5], 
                             constituency.table[7], 
                             constituency.table[8])
      
      
    } else {
      
      constituency.nodes <- subset(constituency.nodes,  (constituency.nodes %>% html_text() %>% str_detect(paste(years$year.name[a], ":", sep = "")) == TRUE
                                                        |
                                                        constituency.nodes %>% html_text() %>% str_detect(paste(years$year.name[a], " general election:", sep = "")) == TRUE
                                                        |
                                                        constituency.nodes %>% html_text() %>% str_detect(paste(years$year.name[a], " Westminster election:", sep = "")) == TRUE)
                                                        &
                                                        constituency.nodes %>% html_text() %>% str_detect(regex("By-election", ignore_case = TRUE)) == FALSE
                                                        &
                                                        constituency.nodes %>% html_text() %>% str_detect(regex("Number of members", ignore_case = TRUE)) == FALSE
                                                        &
                                                        constituency.nodes %>% html_text() %>% str_detect(regex("Election\nMember\nParty\nNote\n\n\n", ignore_case = TRUE)) == FALSE)
      
      
      
      #Start to build a dataframe of the data
      constituency.table <- html_table(constituency.nodes[1], fill = TRUE, header = TRUE)
      
      current.table <- c()
      
      current.table <- cbind(years$year.name[a], 
                             constituencies.links$name[b], 
                             constituency.table[[1]][2], 
                             constituency.table[[1]][3], 
                             constituency.table[[1]][4], 
                             constituency.table[[1]][5])
    }
    
    
    current.table <- as.data.frame(current.table)
    names(current.table) <- c("Year",
                              "Constituency",
                              "Party",
                              "Candidate",
                              "Votes",
                              "Percentage")
    
    if ( any(str_detect(current.table$Votes, regex("Unopposed", ignore_case = TRUE))) == TRUE){
      current.table$Unopposed <- "Y"
    } else {
      current.table$Unopposed <- "N"
    }
    
    
    current.table$Party <- gsub("Registered electors", "Electors", current.table$Party)
    current.table$Candidate <- gsub("Registered electors", "Electors", current.table$Candidate)
    
    
    #Check for registered electors
    if ( any(str_detect(current.table$Party, "Electors")) != TRUE ){
      
      elector.table <- data.frame(Year = years$year.name[a],
                                  Constituency = constituencies.links$names[b],
                                  Party = "Electors",
                                  Candidate = "Electors",
                                  Votes = 9999999,
                                  Percentage = 100,
                                  Unopposed = current.table$Unopposed[1])
      
      current.table <- rbind(current.table, elector.table)
      
    }
    
    current.table$Votes[current.table$Unopposed == "Y"] <- current.table$Votes[current.table$Party == "Electors"]
    current.table$Percentage[current.table$Unopposed == "Y"] <- 100
    
    
    # current.table <- subset(current.table, str_detect(current.table$Party, "Registered electors") == FALSE)
    current.table <- subset(current.table, str_detect(current.table$Party, "gain") == FALSE)
    current.table <- subset(current.table, str_detect(current.table$Party, " hold") == FALSE)
    current.table <- subset(current.table, str_detect(current.table$Party, "Turnout") == FALSE)
    current.table <- subset(current.table, str_detect(current.table$Party, "Majority") == FALSE)
    current.table <- subset(current.table, str_detect(current.table$Party, " win") == FALSE)
    current.table <- subset(current.table, str_detect(current.table$Party, "C indicates candidate endorsed by the coalition government.") == FALSE)
    current.table <- subset(current.table, str_detect(current.table$Party, "Quota") == FALSE)
    
    
    
    current.table$Candidate <- gsub("\\[[^][]*]", "", current.table$Candidate)
    current.table$Votes <- gsub("\\([^][]*)", "", current.table$Votes)
    current.table$Votes <- gsub("\\[[^][]*]", "", current.table$Votes)
    current.table$Percentage <- gsub("\\([^][]*)", "", current.table$Percentage)
    
    current.table$Votes <- gsub(",", "", current.table$Votes, fixed = TRUE)
    current.table$Votes <- gsub("*", "", current.table$Votes, fixed = TRUE)
    current.table$Votes <- gsub(".", "", current.table$Votes, fixed = TRUE)
    current.table$Votes <- as.numeric(current.table$Votes)
    current.table$Percentage <- as.numeric(current.table$Percentage)
    
    #Add turnout
    turnout.table <- data.frame(Year = years$year.name[a],
                                Constituency = constituencies.links$names[b],
                                Party = "Turnout",
                                Candidate = "Turnout",
                                Votes = sum(subset(current.table$Votes, current.table$Party != "Electors")),
                                Percentage = NA,
                                Unopposed = current.table$Unopposed[1])
    
    current.table <- rbind(current.table, turnout.table)
    current.table$Votes[current.table$Party == "Turnout"] <- current.table$Votes[current.table$Party == "Turnout"]/seats
    
    
    #Re-calculate percentage
    current.table$Percentage[current.table$Party == "Electors"] <- 100
    current.table$Percentage[current.table$Party == "Turnout"] <- round(current.table$Votes[current.table$Party == "Turnout"]/current.table$Votes[current.table$Party == "Electors"]*100, 2)
    current.table$Percentage[current.table$Party != "Turnout" & current.table$Party != "Electors"] <- round(current.table$Votes[current.table$Party != "Turnout" & current.table$Party != "Electors"]/current.table$Votes[current.table$Party == "Turnout"]*100, 2)
    
    #Get Winner
    
    current.table$Winner <- "N"
    current.table$Winner.rank <- 99
    current.table$Winner.rank[current.table$Party != "Electors" & current.table$Party != "Turnout"] <- rank(-subset(current.table$Votes, current.table$Party != "Electors" & current.table$Party != "Turnout"))
    
    current.table$Winner[current.table$Winner.rank <= seats] <- "Y"
    
    current.table <- current.table[-9]
    
    tables[(tables.count + 1):(nrow(current.table) + tables.count), ] <- current.table
    tables.count <- tables.count + nrow(current.table)
    
  }
  
  tables <- subset(tables, is.na(tables$Year) == FALSE)
  write.csv(tables, paste("Results/", years$year.name[a], ".csv", sep = ""))
  
}
