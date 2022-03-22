library(tidyverse)
seed <- 1234
# Go to the end to see the functions to launch. 
# Set the working directory to the one containg the data generated throw 
# https://dblp.uni-trier.de/xml/ 
setwd("~/Raw")
# tidyverse library required

######################################
######################################
# DATA CLEANING AND PREPARING
######################################
######################################
# scale equal to the number of articles
prepare <- function(scale=1000){
  
  #######
  # cleaning of articles
  #######
  
  # open the data
  article <- read_delim("dblp_article.csv", delim=";", col_names=F)
  article_head <- read_delim("dblp_article_header.csv", delim=";", col_names=F)
  colnames(article) <- article_head[1,]
  # randomly select some articles
  howManyArt <- scale
  set.seed(seed)
  idx_art <- sample(1:nrow(article),howManyArt)
  art_mantained <- article[idx_art,]
  art_mantained$`abstract:string[]` <- "ABCDEFGHILMNOPQRTXYZ abc def ghi lmn"
  colnames(art_mantained)[30] <- "title:string[]"
  sum(art_mantained$`title:string[]` %>% is.na)
  art_mantained <- art_mantained %>% filter(!is.na(`title:string[]`))
  write_csv(art_mantained, "~/data/articles.csv")
  
  #######
  # cleaning of author_authored_by
  # edge btw author and article
  #######
  authored_by <- read_delim("dblp_author_authored_by.csv", delim=";", col_names=TRUE)
  
  article_id_mantained <- art_mantained %>% select('article:ID') 
  idx_auth_by <- which(authored_by$`:START_ID` %in% 
                         as.vector(as.matrix(article_id_mantained)))
  auth_by_mantained <- authored_by[idx_auth_by,]
  
  
  art_ids <- art_mantained$`article:ID`
  
  auth_by_mantained$`corresponding:int` <- NA
  for( id in (art_ids)){
    authors_of_this <- auth_by_mantained %>% filter(`:END_ID`==id) %>%
      select()
  }
  
  auth_by_mantained$`MAIN_AUTH:bool` <- 0
  idss <- NULL
  
  # set if it is the main author (randomly)
  for (i in 1:nrow(auth_by_mantained)){
    print(i/nrow(auth_by_mantained)*100)
    if (auth_by_mantained$`:START_ID`[i] %in% idss) next
    auth_by_mantained$`MAIN_AUTH:bool`[i] <- 1
    idss <- c(idss, auth_by_mantained$`:START_ID`[i])
  }
  
  write_csv(auth_by_mantained, "~/data/art_auth.csv")
  
  #######
  # cleaning of author
  #######
  
  author <- read_delim("dblp_author.csv", delim=";", col_names=TRUE)
  author_id_mantained <- auth_by_mantained %>% select(':END_ID')%>% distinct()
  idx_auth <- which(author$`:ID` %in% 
                      as.vector(as.matrix(author_id_mantained)))
  auth_mantained <- author[idx_auth,]
  write_csv(auth_mantained, "~/data/auth.csv")
  
  #######
  # cleaning of published_in 
  # edge article-volume
  #######
  
  published_in_journal <- read_delim("dblp_journal_published_in.csv", 
                                     delim=";", col_names=T)
  # the articles' ids of interest are in article_id_mantained
  # keep only the edges related to the articles we are keeping
  # we generate the information about the volumes adding a column to 
  # the bridge table article-journal, using the volume column contained in 
  # article
  
  idx_publ_in_journ_mant <- which(published_in_journal$`:START_ID` %in% 
                                    as.vector(as.matrix(article_id_mantained)))
  publ_in_journ_mant <- published_in_journal[idx_publ_in_journ_mant,]
  
  # which edges article-volume and volume-journal
  art_in_vol <- publ_in_journ_mant
  # creating new ID for the volumes: concatenate Journal_id and Volume_number
  # putting "0000" in the middle
  for (i in 1:(nrow(art_in_vol))){
    current_art_id <- art_in_vol$`:START_ID`[i]
    volume_nbr <- art_mantained %>% filter(`article:ID`==current_art_id) %>%
      select(`volume:string`) %>% as.character
    art_in_vol$`:END_ID`[i] <- paste(art_in_vol$`:END_ID`[i],"0000",volume_nbr, sep="")
    print(i)
  }
  write_csv(art_in_vol,
            "~/data/art_in_vol.csv")
  # art_in_vol <- read_csv("~/data/art_in_vol.csv")
  
  #######
  # creating table of volumes
  #######
  
  vol <- art_in_vol %>% select(`:END_ID`) %>% distinct()
  colnames(vol) <- "volume:ID"
  # adding random date
  vol$`date:date` <- sample(seq(as.Date('2015/01/01'), 
                                as.Date('2022/01/01'), by="day"),
                            nrow(vol), 
                            replace=T)
  write_csv(vol, 
            "~/data/volumes.csv")
  
  #######
  # creating edges volume-journal
  #######
  fsplit_journID <- function(x) {
    s <- str_split(x, pattern="0000")
    return(as.numeric(s[[1]][1]))
  }
  
  end_ids <- sapply(vol$`volume:ID`, fsplit_journID)
  vol_in_journ <- tibble(`:START_ID` = vol$`volume:ID`,
                         `:END_ID` = end_ids)
  
  write_csv(vol_in_journ, 
            "~/data/vol_in_journ.csv")
  
  
  
  #######
  # cleaning of journal
  #######
  
  journal <- read_delim("dblp_journal.csv", 
                        delim=";", col_names=T)
  
  # the journals' ids of interest are in end_ids
  
  journ_mant <- journal %>% filter(`:ID` %in% end_ids)
  write_csv(journ_mant, 
            "~/data/journals.csv")
  
  
  
  #######
  # creating edges article-proceedings
  #######
  
  inproc <- read_delim("dblp_inproceedings.csv", 
                       delim=";", col_names=F)
  colnames(inproc) <- read_delim("dblp_inproceedings_header.csv", delim=";", col_names=F)
  
  # sum(proc$`inproceedings:ID` %in% article_id_mantained)
  
  #######
  # creating proceedings
  #######
  # take randomly 
  # art_not_in_journ <- which( !(art_mantained$`article:ID` %in% art_in_vol$`:START_ID`) )
  # randomly select some articles that were preented in a proceedings (60% of all of them)
  set.seed(seed)
  artId_in_proc <- sample(art_mantained$`article:ID`, 
                          floor(0.2*nrow(art_mantained)) )
  proc <- read_delim("dblp_proceedings.csv", 
                     delim=";", col_names=F)
  colnames(proc) <- read_delim("dblp_proceedings_header.csv", delim=";", col_names=F)
  # randomly select the proceedings from which these articles have been taken from
  set.seed(seed)
  some_proc <- proc[ sample(1:nrow(proc),
                            floor(0.1*length(artId_in_proc)) ),
  ]
  
  art_in_proc <- tibble(`:START_ID` = artId_in_proc,
                        `:END_ID` = sample(some_proc$`proceedings:ID`, 
                                           length(artId_in_proc), replace=T))
  write_csv(art_in_proc, 
            "~/data/art_in_proc.csv")
  proc <- proc %>% filter(`proceedings:ID` %in% 
                            art_in_proc$`:END_ID`) %>%
    select(-`mdate:date`)
  write_csv(proc, 
            "~/data/proceedings.csv")
  
  
  
  #######
  # create the conferences
  #######
  library(stringdist)
  conf <- tibble(`conference:ID` = NA, `name:string[]`=proc$`title:string`)
  
  id <- 0
  already_set <- NULL
  
  for(i in 1:(nrow(conf)-1)){
    if (i %in% already_set) next 
    id <- id+1
    conf$`conference:ID`[i] <- id
    title_i <- (conf$`name:string[]`[i])
    idx_to_set <- NULL
    for(j in (i+1):nrow(conf)){
      if (j %in% already_set) next 
      # if the name of the conference is similar, assume it is the same
      if( stringdist(title_i, conf$`name:string[]`[j], method = "lv") < 40 ){
        idx_to_set <- c(idx_to_set, j)
      }
    }
    # all the conferences that have name similar to the one of the i-th are setted
    # with the same id and same name
    conf$`name:string[]`[idx_to_set] <- title_i
    conf$`conference:ID`[idx_to_set] <- id
    already_set <- c(i, already_set, idx_to_set)
    print(i)
  }
  nn <- nrow(conf)
  conf$`conference:ID`[nn] <- nn
  conf <- conf %>% distinct
  # c <- unite(conf[,3:14], tot, na.rm=TRUE)
  # conf <- conf %>% select(`:ID`, `name:string[]`) %>%
  #   add_column(`info:string[]` = c$tot)
  
  # keep only a few of them (250)
  # random_idx <- sample(1:nrow(conf), 250)
  # conf <- conf[random_idx,]
  # colnames(conf) <- 
  write_delim(conf,
              "~/data/conferences.csv",
              delim=",")
  
  
  ########
  # edges proceedings-conferences
  ########
  
  # randomly assign one conference to each proceedings
  
  ids_conf <- sample(conf$`conference:ID`, nrow(proc), replace=T )
  proc_in_conf <- tibble(`:START_ID`=proc$`proceedings:ID`,
                         `:END_ID`= ids_conf)
  write_csv(proc_in_conf, 
            "~/data/proc_in_conf.csv")
  
  
  
  #######
  # creating citations
  #######
  
  # on average, 10 citations per article
  nOfCitation_eachArt <- rpois(nrow(art_mantained), lambda = 10)
  # for each article, create list of nOfCitation_eachArt[i] ids of articles that quoted it
  ids <- art_mantained$`article:ID`
  # sapply(nOfCitation_eachArt, function(x) sample(ids, x))
  id_is_quoted <- NULL
  id_quotes <- NULL
  for( i in 1:nrow(art_mantained) ){
    art_that_quote <- sample(ids, nOfCitation_eachArt[i])
    id_is_quoted <- c( id_is_quoted, rep(ids[i], nOfCitation_eachArt[i]))
    id_quotes <- c(id_quotes, art_that_quote)
    print(i/1000)
  }
  
  art_quote_art <- tibble(`:START_ID`=id_quotes,
                          `:END_ID`= id_is_quoted)
  write_csv(art_quote_art, 
            "~/data/art_quote_art.csv")
  
  
  #######
  # generating assigned_rewiers
  #######
  
  # one set of rev for each article
  ass_rev <- tibble(`rewier:ID` = 1:nrow(art_mantained))
  write_csv(ass_rev, 
            "~/data/assigned_rewiers.csv")
  
  #######
  # edges btw author and assigned_rewiers
  #######
  
  # each set of rev is composed by 3 auth
  auth_rev <- tibble(`:START_ID`=sample(auth_mantained$`:ID`,
                                        nrow(ass_rev)*3, replace=T),
                     `:END_ID`=rep(ass_rev$`rewier:ID`, 3))
  write_csv(auth_rev, 
            "~/data/auth_is_reviewer.csv")
  
  #######
  # edges btw assigned_rewiers and article
  #######
  # each article has one assigned_reviewer
  assRev_art <- tibble(`:START_ID`=ass_rev$`rewier:ID`,
                       `:END_ID`=article_id_mantained$`article:ID`)
  write_csv(assRev_art, 
            "~/data/assignedRewiers_rev_art.csv")
  
  #######
  # keywords
  #######
  keywords <- read_csv("Keywords.csv") 
  wordsMust <- c("data management", "indexing", "data modeling", "big data", 
                 "data processing", "data storage", "data querying")
  words <- NULL
  for (k in keywords$`author_keywords;;`){
    k <- gsub(';', '', k)
    res <- str_split(k, pattern=" 00 ")[[1]]
    drop <- NULL
    l <- 0
    for(j in 1:length(res)){
      if (res[j]=="") {
        drop <- c(drop,j)
        l <- 1
      }
    }
    if(l==1) res <- res[-drop]
    words <- c(words, res)
  }
  # take only about 400 keywords
  wordsTot <- c(words[1:400], wordsMust)
  keywords <- tibble(`keyword:ID`=1:length(wordsTot),
                     `text:string`=wordsTot)
  write_csv(keywords, 
            "~/data/keywords.csv")
  
  
  #######
  # edges article-keywords
  #######
  
  # to each article assign a random number of keywords (from 3 to 6)
  set.seed(seed)
  how_many <- sample(3:7, nrow(art_mantained), replace=T)
  # for each article, repeat its id how_many[i] times
  IDs <- NULL
  for(i in 1:length(how_many)){
    IDs <- c(IDs, rep(art_mantained$`article:ID`[i],how_many[i] ))
    print(i/50)
  }
  art_keyword <- tibble(`:START_ID`=IDs,
                        `:END_ID`=sample(keywords$`keyword:ID`,sum(how_many), replace=T ))
  write_csv(art_keyword, 
            "~/data/art_keyword.csv")
  
  
  
  #######
  # creating journal_editors and conference_chair
  #######
  proc <- read_delim("dblp_proceedings.csv", 
                     delim=";", col_names=F)
  colnames(proc) <- read_delim("dblp_proceedings_header.csv", delim=";", col_names=F)
  
  # saving the from proceedings
  editor <- tibble(`editor:ID`=proc$`editor:string[]`)
  editor <- editor %>% distinct
  # some of them will be journal_editors, some conference_chair
  set.seed(seed)
  some_editors <- sample(editor$`editor:ID`,150)
  # create 150 journal_editors and conference_chair
  journal_editors <- tibble(`journal_editors:ID`=1:75,
                            `name:string[]`=some_editors[1:75])
  conference_chair<- tibble(`conference_chair:ID`=76:150,
                            `name:string[]`=some_editors[76:150])
  write_csv(journal_editors, 
            "~/data/journal_editors.csv")
  write_csv(conference_chair, 
            "~/data/conference_chair.csv")
  
  #######
  # edges journal_editors - journal
  #######
  jourEdit_journ <- tibble(`:START_ID`=sample(journal_editors$`journal_editors:ID`,
                                              nrow(journ_mant), replace=T),
                           `:END_ID`=journ_mant$`:ID`)
  write_csv(jourEdit_journ, 
            "~/data/jourEdit_journ.csv")
  
  #######
  # creating edges conference_chair - conference
  #######
  
  confChair_conf <- tibble(`:START_ID`=sample(conference_chair$`conference_chair:ID`,
                                              nrow(conf), replace=T),
                           `:END_ID`=conf$`conference:ID`)
  write_csv(confChair_conf, 
            "~/data/confChair_conf.csv")
  
  
  #######
  # edges assigned_reviewers - journal_editors 
  #######
  
  
  tot <- merge(art_mantained,art_in_vol, 
               by.x="article:ID", by.y=":START_ID") %>% as_tibble %>%
    select(c(`article:ID`, `:END_ID`))
  tot1 <- merge(tot, vol_in_journ,
                by.x=":END_ID", by.y=":START_ID") %>% as_tibble %>%
    select(c(`article:ID`, `:END_ID.y`))
  colnames(tot1)[2] <- "journal"
  article_journEditor <- merge(tot1, jourEdit_journ,
                               by.x="journal", by.y=":END_ID") %>% as_tibble %>%
    select(c(`article:ID`, `:START_ID`))
  colnames(article_journEditor)[2] <- "journal_editor"
  
  # article_journEditor is the association btw the article and its editor
  # assRev_art is the association btw the reviewers and the article
  # associate the article to the editor that
  
  journEd_reviewers <- merge(article_journEditor, assRev_art,
                             by.x="article:ID", by.y=":END_ID")%>% as_tibble %>%
    select(c(journal_editor, `:START_ID`))
  colnames(journEd_reviewers)<- c(":START_ID", ":END_ID")
  
  write_csv(journEd_reviewers,
            "~/data/journEd_reviewers.csv")
  
  
  #######
  # edges assigned_reviewers - conference_chair 
  #######
  
  tot <- merge(art_mantained,art_in_proc, 
               by.x="article:ID", by.y=":START_ID") %>% as_tibble %>%
    select(c(`article:ID`, `:END_ID`))
  tot1 <- merge(tot, proc_in_conf,
                by.x=":END_ID", by.y=":START_ID") %>% as_tibble %>%
    select(c(`article:ID`, `:END_ID.y`))
  colnames(tot1)[2] <- "conf"
  article_cofChair <- merge(tot1, confChair_conf,
                            by.x="conf", by.y=":END_ID") %>% as_tibble %>%
    select(c(`article:ID`, `:START_ID`))
  colnames(article_cofChair)[2] <- "chair_conf"
  
  # article_cofChair is the association btw the article and its editor
  # assRev_art is the association btw the reviewers and the article
  # associate the article to the editor that
  
  cofChair_reviewers <- merge(article_cofChair, assRev_art,
                              by.x="article:ID", by.y=":END_ID")%>% as_tibble %>%
    select(c(chair_conf, `:START_ID`))
  colnames(cofChair_reviewers) <- c(":START_ID", ":END_ID")
  
  write_csv(cofChair_reviewers,
            "~/data/cofChair_reviewers.csv")
  conferences <- read_csv("~/data/conferences.csv")
  
  l <- NULL
  conferences <- conferences %>% 
    mutate(`name:string[]`=sapply(`name:string[]`, FUN=gsub, pattern='[[:digit:]]+', replacement="")) %>%
    mutate(`name:string[]`=sapply(`name:string[]`, FUN=gsub, pattern='nd', replacement="")) %>%
    mutate(`name:string[]`=sapply(`name:string[]`, FUN=gsub, pattern='rd', replacement="")) %>%  
    mutate(`name:string[]`=sapply(`name:string[]`, FUN=gsub, pattern='th', replacement="")) %>%  
    mutate(`name:string[]`=sapply(`name:string[]`, FUN=gsub, pattern='-', replacement=""))
  
  write_csv(conferences, 
            "~/data/conferences.csv")
}

######################################
######################################
# SCHEMA CHANGES
######################################
######################################
generateSchemaChanges <- function()
{
  authors <- read_csv("~/data/auth.csv")
  auth_is_reviewer <- read_csv("~/data/auth_is_reviewer.csv")
  
  ### ORG: ID, UniOrCompany, NameOrg
  ### Relation btw author and org
  ### Relation Author-ReviewerGroup-> add random review and decision(0/1)
  ######
  # Organization
  ######
  nOrg <- 500
  nUni <- 300
  nCom <- nOrg-nUni
  
  IDs <- 1:nOrg
  UniOrCompany <- c(rep("University",nUni), rep("Company", nCom))
  Names <- readxl::read_xlsx("~/data/List of 300 universities.xlsx",
                             col_names=F         )
  Names <- rbind(Names, 
                 readxl::read_xlsx("~/data/list of 200 companies.xlsx",
                                   col_names=F     ))
  organiz <- tibble("organization:ID"=IDs,
                    "name:string[]"=Names$...1,
                    "type:string"=UniOrCompany)
  write_csv(organiz, 
            "~/data/organizations.csv")
  
  ######
  # edges author-organization
  ######
  
  nAuth <- nrow(authors)
  set.seed(seed)
  whichOrg <- sample(organiz$`organization:ID`, nAuth,  replace=T)
  
  auth_organ <- tibble(":START_ID"=authors$`:ID`,
                       ":END_ID"=whichOrg)
  write_csv(auth_organ, 
            "~/data/author_organization.csv")
  
  ######
  # add review and decision(0/1) to auth_is_reviewer
  ######
  n <- nrow(auth_is_reviewer)
  # 75% of yes
  auth_is_reviewer$`decision:bool` <- sample(c(0,1,1,1), n, replace=T)
  auth_is_reviewer$`text:strin[]` <- "Forza juve next year we'll win the champions"
  write_csv(auth_is_reviewer, 
            "~/data/auth_is_reviewerVersion2.csv")
  
  
}



modifyToHaveResults <- function(artToAdd=2000, IDauthorToIncrease1,
                                IDauthorToIncrease2, IDJourToBelongTo,
                                IDNewArticles, 
                                IDNewArtsForQuery2,
                                authorIDForQuery2){
  add <- artToAdd
  ######
  # add more articles inside the DB community (adding to some articles the "wordsMust" keywords)
  # add more citations inside the DB community
  # i.e. btw articles that talk about "wordsMust" keywords 
  #####
  articles <-  read_csv("~/data/articles.csv")
  art_quote_art <- read_csv("~/data/art_quote_art.csv")
  keywords <- read_csv("~/data/keywords.csv")
  art_keyword <-read_csv("~/data/art_keyword.csv")
  wordsMust <- c("data management", "indexing", "data modeling", "big data", 
                 "data processing", "data storage", "data querying")
  IDMust <- NULL
  for(w in wordsMust){
    IDMust <- c(IDMust,keywords$`keyword:ID`[which(keywords$`text:string`==w)])
  }
  
  # take 500 articles to add to the DB community. Maybe they will be duplicated, but at least
  # this will add something
  someArtToAdd <- sample(articles$`article:ID`, 500)
  
  art_keyword <- art_keyword %>% add_row(`:START_ID`=someArtToAdd,
                                         `:END_ID`=sample(IDMust, 500, replace = T))
  
  ### which art are in DB community
  IDsOfArtInDBComm <- art_keyword$`:START_ID`[art_keyword$`:END_ID` %in% IDMust]
  artInDBComm <- articles %>% filter(`article:ID` %in% IDsOfArtInDBComm)
  
  ### add quotations btw these articles
  # add 2000 quotations btw these articles
  # OC, can't create self-quotations
  artQuotating<- sample(IDsOfArtInDBComm, add, replace=T)
  # remove these ones from the quoted.
  # doing like this create also unbalanced btw number of citations btw arts
  # that is a nice thing to have
  artQuoted <- IDsOfArtInDBComm[ which(IDsOfArtInDBComm %in% artQuotating) ] %>% 
    unique %>% sample(add, replace=T)
  
  art_quote_art <- art_quote_art %>% add_row(`:START_ID`=artQuoted,
                                             `:END_ID`=artQuotating)
  
  write_csv(art_quote_art, 
            "~/data/art_quote_art.csv")
  
  write_csv(art_keyword, 
            "~/data/art_keyword.csv")
  ###########
  # AFTER HAVING SEEN THE DATA
  ###########
  
  ########
  # adding 3 articles to 2 Authors
  # from a journal that belongs to the DB community
  # and that has at least one keyword of the DB community
  ########
  
  articles <-  read_csv("~/data/articles.csv")
  art_in_vol <- read_csv("~/data/art_in_vol.csv")
  volumes <- read_csv("~/data/volumes.csv")
  vol_in_journ <- read_csv("~/data/vol_in_journ.csv")
  journals <- read_csv("~/data/journals.csv")
  keywords <- read_csv("~/data/keywords.csv")
  art_keyword <-read_csv("~/data/art_keyword.csv")
  
  
  wordsMust <- c("data management", "indexing", "data modeling", "big data", 
                 "data processing", "data storage", "data querying")
  IDMust <- NULL
  for(w in wordsMust){
    IDMust <- c(IDMust,keywords$`keyword:ID`[which(keywords$`text:string`==w)])
  }
  
  
  # adding three relations art_auth to the ones we are interested in
  art_auth <- read_csv("~/data/art_auth.csv")
  
  set.seed(seed)
  art_auth <- art_auth %>% add_row(`:START_ID`=IDNewArticles,
                                   `:END_ID`=rep(c(IDauthorToIncrease1,
                                                   IDauthorToIncrease2), 3),
                                   `corresponding:int`=NA,
                                   `MAIN_AUTH:bool`=0)
  art_keyword <- art_keyword %>% add_row(`:START_ID`=IDNewArticles,
                                         `:END_ID`=IDMust[1])
  articles <- articles %>% add_row(`article:ID`=IDNewArticles,
                                   `pages:string`="202",
                                   `title:string[]`=c("Grpah DB new era 1",
                                                      "Grpah DB new era 2",
                                                      "Grpah DB new era 3",
                                                      "Grpah DB new era 4",
                                                      "Grpah DB new era 5",
                                                      "Grpah DB new era 6"),
                                   `url:string`="www.graphsdb.es",
                                   `year:int`=2016,
                                   `abstract:string[]`="asbtract aaaa"
  )
  # add these 6 articles to the journal of interest
  # first find a volume of it
  volID <- vol_in_journ %>% filter(`:END_ID`==IDJourToBelongTo) %>% select(`:START_ID` ) %>%as.character()
  art_in_vol <- art_in_vol %>% add_row(`:START_ID`=IDNewArticles,
                                       `:END_ID` =volID)
  
  write_csv(articles, 
            "~/data/articles.csv")
  write_csv(art_in_vol, 
            "~/data/art_in_vol.csv")
  write_csv(art_keyword, 
            "~/data/art_keyword.csv")
  write_csv(art_auth, 
            "~/data/art_auth.csv")
  
  # adding citations btw these articles
  art_quote_art <- read_csv("~/data/art_quote_art.csv")
  # adding one to each
  art_quote_art1 <- art_quote_art %>% add_row(`:START_ID`=IDNewArticles[-6],
                                              `:END_ID`=IDNewArticles[-1])
  
  # adding two to 111111 and 222222
  art_quote_art1 <- art_quote_art1 %>% add_row(`:START_ID`=rep(c(IDNewArticles[1],
                                                                 IDNewArticles[2]),
                                                               2),
                                               `:END_ID`=IDNewArticles[3:6])
  
  write_csv(art_quote_art1, 
            "~/data/art_quote_art.csv")
  
  
  #######
  # having results in query 2
  #######
  
  # add to 4 different volumes published in conf an article written by author 9016863
  proc <- read_csv("~/data/proceedings.csv")
  conferences <- read_csv("~/data/conferences.csv")
  art_in_proc <- read_csv("~/data/art_in_proc.csv")
  proc_in_conf <- read_csv("~/data/proc_in_conf.csv")
  articles <-  read_csv("~/data/articles.csv")
  art_auth <- read_csv("~/data/art_auth.csv")
  
  artIDs <- IDNewArtsForQuery2
  articles <- articles %>% add_row(`article:ID`=artIDs,
                                   `pages:string`="202",
                                   `title:string[]`=c("History of DB",
                                                      "History of statistics",
                                                      "History of maths",
                                                      "History of science"),
                                   `url:string`="www.history.es",
                                   `year:int`=2017,
                                   `abstract:string[]`="summary of history")
  
  proc_in_conf$`:END_ID` %>% table # only conf 11 have at least 4 volumes
  confID <- 11
  procID <- proc_in_conf %>% filter(`:END_ID` ==confID) %>% 
    select(`:START_ID`) %>% as.matrix %>% as.vector
  art_in_proc <- art_in_proc %>% add_row(`:START_ID`=artIDs,
                                         `:END_ID`=procID)
  art_auth <- art_auth %>% add_row(`:START_ID`=artIDs,
                                   `:END_ID`=authorID,
                                   `MAIN_AUTH:bool`=1)
  write_csv(articles, 
            "~/data/articles.csv")
  write_csv(art_auth, 
            "~/data/art_auth.csv")
  write_csv(art_in_proc, 
            "~/data/art_in_proc.csv")
}


######################################
######################################
# MAIN
######################################
######################################
prepare(2000)
generateSchemaChanges
modifyToHaveResults(IDauthorToIncrease1=9124315,
                    IDauthorToIncrease2=9057816,
                    IDJourToBelongTo=11998906,
                    IDNewArticles=c(111111,222222,333333,444444,555555,666666),
                    IDNewArtsForQuery2=c(777777,888888,999999,101010),
                    authorIDForQuery2=9016863)

