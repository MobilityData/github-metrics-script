library(httr)
library(jsonlite)
library(httpuv)
library(ghql)
library(shiny)
library(tidyverse)
library(rvest)
library(DT)

server <- function(input, output) {
  
#The dates below should be changed to select the desired timerange for metric tracking. Dates are inclusive. 
  
  FilterStartDate<-"2019-05-01"
  FilterEndDate<-"2019-05-31"
  
#Change "token" below, but I think that the \n needs to be added to the end. 
  
  cat("GITHUB_GRAPHQL_TOKEN=token\n",
      file = file.path(normalizePath("~/"), ".Renviron"), append = TRUE)
  
  token <- Sys.getenv("GITHUB_GRAPHQL_TOKEN")
  
  cli <- GraphqlClient$new(
    url = "https://api.github.com/graphql",
    headers = add_headers(Authorization = paste0("Bearer ", token))
  )
  
  cli$load_schema()
  
  qry <- Query$new()
  
  test<-cli$exec(qry$queries$myquery)
  
  # Gets the number of pull requests that were sucessfully merged
  qry$query('GetPRnumdata', '{
            repository(owner:"google", name:"transit") {
            pullRequests(last:99 states:MERGED) {
            nodes{
            mergedAt
            }
            }
            }}')

  PRrep<-fromJSON(cli$exec(qry$queries$GetPRnumdata))
  PRdates<-as_data_frame(as.Date(PRrep$data$repository$pullRequests$nodes$mergedAt))
  
  PRsmerged<-PRdates %>%
    filter(FilterStartDate < value) %>%
    filter(value < FilterEndDate) %>%
    count
  
  output$value1 <- renderPrint(PRsmerged$n[[1]])
  
  #Get number of issues opened
  qry$query('GetIssueNum', '{
            repository(owner: "google", name: "transit") {
            issues(last: 99) {
            nodes {
            createdAt
            }
            }
            }
            }')

  IssueRep<-fromJSON(cli$exec(qry$queries$GetIssueNum))
  IssueDates<-as_data_frame(as.Date(IssueRep$data$repository$issues$nodes$createdAt))
  
  IssuesCreated<-IssueDates %>%
    filter(FilterStartDate < value) %>%
    filter(value < FilterEndDate) %>%
    count
  
  output$value2 <- renderPrint(IssuesCreated$n[[1]])
  
  #Get number of comments on issues
  
  pastestr <-paste0( ' {\n    repository(owner: \"google\", name: \"transit\") {\nissues(last:70) {\nnodes {\ntimeline(last:80, since:\"' , FilterStartDate,'T00:00:00Z\") {\nnodes {\n... on IssueComment {\nauthor {\nlogin\n}\ncreatedAt\n}\n}\n}\n}}}}' )
 
  qry$query("GetIssueComments1", pastestr)
  
  IssueCommentsRep<-fromJSON(cli$exec(qry$queries$GetIssueComments1))
  t1<-IssueCommentsRep$data$repository$issues$nodes$timeline$nodes
  IssueCommentUsers<-data.frame()
  
  for (i in 1:length(t1)) {
    t2<-data.frame(t1[[i]]$author$login, t1[[i]]$createdAt)
    IssueCommentUsers<-rbind(IssueCommentUsers, t2)
  }
  IssueCommentUsers<-na.omit(IssueCommentUsers) 
  colnames(IssueCommentUsers) <- c("user","date")
  IssueCommentUsers$date<-as.Date(IssueCommentUsers$date)
  
  dfIssueUserCount<-IssueCommentUsers %>% 
    filter(FilterStartDate < date) %>%
    filter(date < FilterEndDate) %>%
    group_by(user) %>%
    summarize(count=n()) %>%
    arrange(desc(count))
  
  output$value4<-renderPrint(sum(dfIssueUserCount$count))
  
  dfIssueUsers<-IssueCommentUsers %>% 
    filter(FilterStartDate < date) %>%
    filter(date < FilterEndDate)
  
  #Get total number of comments on PRs updated after the given date.
  pastestr <-paste0( '{\n    repository(owner: \"google\", name: \"transit\") {\npullRequests(last: 20) {\nnodes {\ntimeline(since: \"' , FilterStartDate, 'T00:00:00Z\", first: 60) {\nnodes {\n... on PullRequestReviewThread {\ncomments(last:50) {\nnodes{\ncreatedAt\n}\n}\n}\n... on CommitCommentThread {\ncomments(last:50) {\nnodes{\ncreatedAt\n}\n}\n}\n... on IssueComment {\ncreatedAt\n}\n}\n}\n}\n}}}' )
  
  qry$query("GetCommentCount2", pastestr)
  
  CommentList<-data.frame()
  CommentCountResp <- fromJSON(cli$exec(qry$queries$GetCommentCount2))
  t1<-CommentCountResp$data$repository$pullRequests$nodes$timeline$nodes
  for (i in 1:length(t1)) {
    t2<-data.frame(na.omit(t1[[i]]$createdAt))
    CommentList<-rbind(CommentList, t2)
  }
  
  colnames(CommentList)<-c("date")
  CommentList$date<-as.Date(CommentList$date)
  
  TotalComments<-CommentList %>%
    filter(FilterStartDate < date) %>%
    filter(FilterEndDate > date) %>%
    count
  
  output$value3<-renderPrint(TotalComments$n[1])
  }

ui <- fluidPage(
  
  titlePanel("Monthly MobilityData Metrics"),
  
  mainPanel(
    h3("Number of PRs Merged"), 
    h3(verbatimTextOutput("value1")),
    h3("Number of Issues Opened"),
    h3(verbatimTextOutput("value2")),
    h3("Number of PR Comments"),
    h3(verbatimTextOutput("value3")),  
    h3("Number of Issue Comments"),
    h3(verbatimTextOutput("value4"))
  )
)

shinyApp(ui = ui, server = server)

