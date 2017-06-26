crawl<-function(startyear,endyear){
  #install.packages("RCurl")
  #install.packages("XML")
  library("RCurl")
  library("XML")
  library(stringr)
  Title = NULL
  val = NULL
  year = NULL
  result = NULL
  URLs = NULL
  
  Book = NULL
  Author_Details = NULL
  Email_Address = NULL
  Keywords = NULL
  Abstracts = NULL
  Affiliations = NULL
  Datee = NULL
  Paragraph = NULL
  
  input = "http://genomemedicine.biomedcentral.com/articles?tab=keyword&searchType=journalSearch&sort=PubDate&page="
  
  temp = "http://genomemedicine.biomedcentral.com/articles?tab=keyword&searchType=journalSearch&sort=PubDate&page=1"
  html <- getURL(temp, followlocation=TRUE)
  doc = htmlParse(html, asText=TRUE)
  spanVal = xpathSApply(doc, "//*[@class='Control_name']", xmlValue, "span")
  TotalPages = word(spanVal[3],-1)
  
  #for(i in 1:TotalPages){
  for(i in 1:2){ 
   new_input = paste(input,i,sep="")
    html <- getURL(new_input, followlocation=TRUE)
    doc = htmlParse(html, asText=TRUE)
    
    publ = xpathSApply(doc, "//*[@class='ResultsList_published']", xmlValue)
    URLs = xpathSApply(doc, "//*[@class='fulltexttitle']", xmlGetAttr, "href")
    year = word(publ,-1)
    val=c(year,URLs)
    
    if(startyear %in% year | endyear %in% year){
      for(j in 1:length(year)){
        if(val[j] == startyear | val[j] == endyear){
          new_URL=paste("http://genomemedicine.biomedcentral.com",val[length(publ)+j],sep="")
    
          new_html <- getURL(new_URL, followlocation=TRUE)
          new_doc = htmlParse(new_html, asText=TRUE)
          new_Title = xpathSApply(new_doc, "//*[@name='dc.title']", xmlGetAttr, "content")
          authors = xpathSApply(new_doc, "//*[@class='AuthorName']", xmlValue)
          email = xpathSApply(new_doc, "//*[@class='EmailAuthor']", xmlGetAttr, "href")
          abstract1 <-xpathSApply(new_doc, "//*[@class='Abstract']", xmlValue)
          affiliation1 <- xpathSApply(new_doc, "//*[@class='AffiliationText']", xmlValue)
          new_date = xpathSApply(new_doc, "//*[@name='dc.date']", xmlGetAttr, "content")
          para <- xpathSApply(new_doc, "//*[@class='Para']", xmlValue)
    
         
          Book_Title = new_Title
          Author_Det = paste(authors,collapse = ", ")
          Email_Addr = paste(email,collapse = ", ")
          Abstract = toString(abstract1)
          Affliation <- paste(affiliation1,collapse = ", ")
          datemid <- new_date
          paramid <- toString(para)
           
          Book = append(Book,Book_Title)
          Author_Details = append(Author_Details,Author_Det)
          Email_Address = append(Email_Address,Email_Addr)
          Abstracts = append(Abstracts,Abstract)
          Affiliations = append(Affiliations,Affliation)
          Datee = append(Datee,datemid)
          Paragraph <- append(Paragraph,paramid)
       
        }
      } 
    }
  }
  if(is.null(Book)){
    print(paste("There are no books published between the years specified ", startyear, " and ", endyear))
  } 
  else {

    result1 = data.frame(Book,Author_Details,Email_Address,Abstracts,Affiliations,Datee,Paragraph)
    write.table(result1, "/Users/arif/Desktop/out.txt", sep="\t") 
  }
}