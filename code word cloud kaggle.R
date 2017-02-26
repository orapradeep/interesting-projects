
u = "http://www.kaggle.com/forums/t/5153/pandora-media-inc-is-looking-for-a-senior-scientist-growth-hacking"

library(XML)
doc = htmlParse(u)
doc

tbls = getNodeSet(doc, "//table[@class = 'post ']")

length(tbls)

table = tbls[[1]]
names(table)


names(row1)


xmlAttrs(row1[[3]])


postNodes = getNodeSet(doc, "//td[@class = 'postbox']")[[1]]

txt = xmlValue(postNodes)

uls = getNodeSet(postNodes, 
                  ".//p[strong]/following-sibling::ul")

items = xpathSApply(uls[[1]], ".//li", xmlValue)

items = lapply(uls, function(node)
                     xpathSApply(node, ".//li", xmlValue))

getSibling(getSibling(uls[[1]], TRUE), TRUE)

tmp = getNodeSet(uls[[1]], 
                 "./preceding-sibling::p[strong]/strong")
xmlValue(tmp[[1]])

titles = xpathSApply(doc, 
             "//p[strong and following-sibling::ul]/strong",
             xmlValue)

paras = getNodeSet(postNodes[[1]], ".//p[not(strong)]")

itemWords = lapply(items, strsplit, "[[:space:][:punct:]]+")

words = lapply(paras, function(p)
                         strsplit(xmlValue(p), 
                                  "[[:space:][:punct:]]+"))

post = list(freeForm = unlist(words),
            requirements = itemWords[[1]],
            optionalSkills = itemWords[[2]])

load("StopWords.rda")

removeStopWords = 
function(x, stopWords = StopWords) 
{
   if(is.character(x))
      setdiff(x, stopWords)
   else if(is.list(x))
      lapply(x, removeStopWords, stopWords)
   else
      x
}

post = lapply(post, removeStopWords)

readKagglePost = 
function(u, stopWords = StopWords, doc = htmlParse(u))
{
  
}

readKagglePost = 
function(u, stopWords = StopWords)
{
   if(!is(u, "HTMLInternalDocument"))
      u = htmlParse(u)
}

library(RCurl)

post = getNodeSet(doc, "//td[@class = 'postbox']")

readKagglePost = 
function(u, stopWords = StopWords, doc = htmlParse(u))
{
    post = getNodeSet(doc, "//td[@class = 'postbox']")

    if(length(postNodes) == 0)
       stop("cannot find <td class='postbox'> element in HTML")
    else if(length(postNodes) > 1)
       stop("found more than one <td class='postbox'> 
           elements in HTML")

     txt = xmlValue(post[[1]])
     strsplit(txt, "[[:space:][:punct:]]+")[[1]]
}

baseURL = "https://www.kaggle.com/forums/f/145/data-science-jobs"
sprintf("%s?page=%d", baseURL, 2:40)

readAllKagglePosts =
function(numPages, baseURL = 
         "https://www.kaggle.com/forums/f/145/data-science-jobs")
{
  pageURLs = c(baseURL,
               sprintf("%s?page=%d", baseURL, 2:numPages))

  postLinks = unlist(lapply(pageURLs, getPostLinks))
  lapply(postLinks, readKagglePost)
}

baseURL = "https://www.kaggle.com/forums/f/145/data-science-jobs"
txt = getForm(baseURL, page = "2")
doc = htmlParse(txt, asText = TRUE)

getNodeSet(doc, "//a[starts-with(@href, '/forums/t')]/@href")

getNodeSet(doc, "//h3/a[starts-with(@href, '/forums/t')]/@href")

getNodeSet(doc, "//h3/a/@href[starts-with(., '/forums/t')]")

getPostLinks =
function(link)
{
   txt = getURLContent(link, followlocation = TRUE)
   doc = htmlParse(txt, asText = TRUE)
   getNodeSet(doc, "//h3/a[starts-with(@href, '/forums/t')]
                           /@href")
}

cyurl = "http://www.cybercoders.com/data-scientist-job-158482"
cyurl = "http://www.cybercoders.com/data-scientist-job-181068"
doc = cydoc = cy = htmlParse(cyurl)

node = getNodeSet(cydoc, "//div[@class = 'job-info-main']")[[1]]

info = xpathSApply(cydoc, "//div[@class='job-info-main'][1]/div", 
                    xmlValue)

lis = getNodeSet(cydoc, "//div[@class = 'skills-section']//
                           li[@class = 'skill-item']//
                           span[@class = 'skill-name']")

xmlValue(getNodeSet(doc, 
                    "//div[@class = 'job-details']//
                       div[@class='posted']/
                       span/following-sibling::text()")[[1]],
          trim = TRUE) 

cy.readPost = 
function(u, stopWords = StopWords, doc = htmlParse(u))
{
  ans = list(words = cy.getFreeFormWords(doc, stopWords),
             datePosted = cy.getDatePosted(doc),
             skills = cy.getSkillList(doc))
  o = cy.getLocationSalary(doc)
  ans[names(o)] = o

  ans
}

cy.getSkillList =
function(doc)
{
  lis = getNodeSet(doc, "//div[@class = 'skills-section']//
                         li[@class = 'skill-item']//
                         span[@class = 'skill-name']")

  sapply(lis, xmlValue)
}


cy.getDatePosted = 
function(doc)
  xmlValue(getNodeSet(doc, "//div[@class = 'job-details']//
                            div[@class='posted']/
                            span/following-sibling::text()")[[1]], 
          trim = TRUE)

cy.getLocationSalary =
function(doc)
{
  ans = xpathSApply(doc, "//div[@class = 'job-info-main'][1]/div", xmlValue)
  names(ans) = c("location", "salary")
  ans
}

a = getNodeSet(cydoc, 
            "//*[starts-with(., 'Are you a Data Scientist')]")[[1]]

a = getNodeSet(cydoc, 
        "//*[normalize-space(., 'Are you a Data Scientist')]")[[1]]

names(xmlParent(a))


sapply(xmlParent(a)["div"], xmlAttrs)


getNodeSet(doc, "//div[@class='job-details']/
                   div[@data-section]")

details = getNodeSet(doc, "//div[@class='job-details']")[[1]]
xpathSApply(details, "./h4", getSibling)

cy.getFreeFormWords = 
function(doc, stopWords = StopWords)
{
  words = xpathApply(doc, "//div[@class='job-details']/
                             div[@data-section]",
                      function(x) 
                         asWords(xmlValue(x)))
}

u = "http://www.cybercoders.com/data-scientist-job-140783"
ans1 = cy.readPost(u)
names(ans1)

ans1$salary


u = "http://www.cybercoders.com/search/?searchterms=Data+Science\
&searchlocation=&newsearch=true&sorttype="
p = getFormParams(u)


txt = getForm("http://www.cybercoders.com/search/",
              searchterms = '"Data Scientist"', 
              searchlocation = "",  newsearch = "true",
              sorttype = "")

doc = htmlParse(txt, asText = TRUE)

links = getNodeSet(doc, "//div[@class = 'job-title']/a/@href")

getRelativeURL("/data-scientist-job-127835",
               "http://www.cybercoders.com/search/")


links = getRelativeURL(as.character(links),
                       "http://www.cybercoders.com/search/")

posts = lapply(links, cy.readPost)

cy.getPostLinks =
function(doc, baseURL = "http://www.cybercoders.com/search/")
{
    if(is.character(doc))
       doc = htmlParse(doc)

   links = getNodeSet(doc, "//div[@class = 'job-title']/a/@href")
   getRelativeURL(as.character(links), baseURL)
}

cy.readPagePosts =
function(doc, links = cy.getPostLinks(doc, baseURL),
          baseURL = "http://www.cybercoders.com/search/")
{
    if(is.character(doc))
       doc = htmlParse(doc)
    lapply(links, cy.readPost)
}

posts = cy.readPagePosts(doc)

sapply(posts, `[[`, "salary")

summary(sapply(posts, function(x) length(unlist(x$words))))

which(sapply(posts, function(x) length(unlist(x$words))) == 0)


posts[[16]]


nodes = getNodeSet(doc, 
                   "//*[starts-with(normalize-space(.), 
                        'Are you a Data Scientist with a PhD')]")

cy.getFreeFormWords = 
function(doc, stopWords = StopWords)
{
  nodes = getNodeSet(doc, "//div[@class='job-details']/
                                div[@data-section]")
  if(length(nodes) == 0) 
     nodes = getNodeSet(doc, "//div[@class='job-details']//p")

  if(length(nodes) == 0) 
     warning("did not find any nodes for the free form text in ",
              docName(doc))

  words = lapply(nodes,
                  function(x)
                      strsplit(xmlValue(x), 
                                 "[[:space:][:punct:]]+"))

  removeStopWords(words, stopWords)
}

doc = htmlParse(txt, asText = TRUE)

getNodeSet(doc, "//a[@rel='next']/@href")[[1]]

cy.getNextPageLink =
function(doc, baseURL = docName(doc))
{
  if(is.na(baseURL))
     baseURL = "http://www.cybercoders.com/search/"
  link = getNodeSet(doc, "//a[@rel='next']/@href")
  if(length(link) == 0)
     return(character())

  getRelativeURL(link[[1]], baseURL)
}

tmp = cy.getNextPageLink(doc,
                         "http://www.cybercoders.com/search/")

cyberCoders =
function(query)
{
   txt = getForm("http://www.cybercoders.com/search/",
                  searchterms = query,  searchlocation = "",
                  newsearch = "true",  sorttype = "")
   doc = htmlParse(txt)

   posts = list()
   while(TRUE) {
       posts = c(posts, cy.readPagePosts(doc))
       nextPage = cy.getNextPageLink(doc)
       if(length(nextPage) == 0)
          break

       nextPage = getURLContent(nextPage)
       doc = htmlParse(nextPage, asText = TRUE)
   }
   invisible(posts)
}

dataSciPosts = cyberCoders("Data Scientist")

tt = sort(table(unlist(lapply(cy.dataSciPosts, `[[`, "skills"))),
           decreasing = TRUE)
tt[tt >= 2]

searchJobs =
  # Given a search query, get the pages listing the jobs.
  # we loop over these pages and harvest the 
  # individual jobs in each.
function(firstPage, getNextPage_f, getJobDescriptionLinks_f,
         getJobDescription_f = getJobDescription,
         max = NA, curl = getCurlHandle(followlocation = TRUE))
{
  curPage = firstPage
  jobs = list()
    
  pageNum = 1L
  while(is.na(max) || length(jobs) < max) {

      doc = htmlParse(curPage, asText = TRUE)

      links = getJobDescriptionLinks_f(doc, curl = curl)
      posts = structure(lapply(links, 
                               function(l) 
                                 try(getJobDescription_f(
                                       getURLContent(l, 
                                                     curl = curl),
                                     stem = FALSE,
                                     curl = curl))), 
                        names = links)
      jobs = c(jobs, posts)

      curPage = getNextPage_f(doc, curl = curl)
      if(length(curPage) == 0)
        break
      pageNum = pageNum + 1L
  }

  invisible(jobs[!sapply(jobs, inherits, "try-error")])
}

u = "http://www.kaggle.com/forums/f/145/data-science-jobs"
getURLContent(u)

getForm("http://www.cybercoders.com/search/",
        searchterms = queryString,  searchlocation = "",
        newsearch = "true",  sorttype = "")

searchCyberCoders = 
function(query, ...) 
{
   txt = getForm("http://www.cybercoders.com/search/",
                 searchterms = query,  searchlocation = "",
                 newsearch = "true",  sorttype = "")

   searchJobs(txt, cy.getNextPageLink, 
                   cy.getPostLinks, 
                   cy.readPost, ...)
}

baseURL = "http://www.careerbuilder.com/jobseeker/jobs/jobresults.aspx"

cb.getJobLinks =
function(doc)
  getNodeSet(doc, "//a[@class = 'jt prefTitle']/@href")

cb.getNextPage =
function(doc)
{
   nxt = getNodeSet(doc, "//a[. = 'Next Page']/@href")
   if(length(nxt) == 0)
       return(character())

   nxt[[1]]
}

searchCareerBuilders =
function(query, ..., baseURL = 
  'http://www.careerbuilder.com/jobseeker/jobs/jobresults.aspx')
{
   
   txt = getForm(baseURL, IPath= "QH", qb = "1", 
                   s_rawwords=query, 
                   s_freeloc="", s_jobtypes="ALL",
                   sc_cmp2= "js_findjob_home", 
                   FindJobHomeButton="hptest_ignore2",
                 .opts = list(followlocation = TRUE))

   searchJobs(getNextPage_f = cb.getNextPage, 
              getJobDescriptionLinks_f = cb.getJobLinks, 
              txt = txt, ...)   
}

monsterSearchURL =
function(q)
{
  sprintf("http://jobsearch.monster.com/search/%s_5?",
            gsub(" ", "-", q))
}

monsterSearch =
function(q, firstPage = monsterSearchURL(q), ...)
{
  txt = getURLContent(firstPage, followlocation = TRUE)
  searchJobs(txt, monsterNextPage, monsterJobLinks, ...)
}

nxt = getNodeSet(doc, "//a[@title='Next']/@href")

nodes = getNodeSet(doc, "//a[starts-with(@class, 'slJobTitle')]
                            /@href")

monsterNextPage =
function(doc)
{
   nxt = getNodeSet(doc, "//a[@title='Next']/@href")
   if(length(nxt))
      nxt[[1]]
   else
     character()
}

monsterJobLinks = 
function(doc)
   unlist(getNodeSet(doc, "//a[starts-with(@class, 'slJobTitle')]
                            /@href"))

source("monster.R")
monster.DataScientist = monsterSearch("Data Scientist")
sort(table(tolower(unlist(monster.DataScientist)))[tolower(terms)])

source("general.R")
source("careerbuilder1.R")
careerbuilder.DataScientist = searchCareerBuilders('"Data Scientist"')
sort(table(tolower(unlist(careerbuilder.DataScientist)))[tolower(terms)])

kaggleWords = lapply(kaggleJobPosts, asWords, stem = FALSE)
allWords = c(unlist(kaggleWords), 
             unlist(lapply(cy.dataSciPosts, function(x) x$words)),
             unlist(monster.DataScientist), 
             unlist(careerbuilder.DataScientist))

tmp = sort(table(tolower(allWords))[tolower(terms)])
dotchart(tmp[ tmp > 9], cex = .7)

cyber.dsSkills = table(unlist(lapply(cyber.DataScientist, 
                                        `[[`, "skills")))

cy.processSalaries =
function(posts)
{
  tmp = strsplit(gsub(".* \\$([0-9]+)k - \\$([0-9]+)k", 
                      "\\1,\\2", sapply(posts, `[[`, "salary")),
                 ",")
  vals = lapply(1:2, function(i) 
                         1000*as.integer(sapply(tmp, `[`, i)))
  ans = as.data.frame(vals)
  names(ans) = c("low", "high")
  ans
}

dsPosts = c(cyber.DataScientist, cyber.DataScience)
sl = cy.processSalaries(dsPosts)

which(is.na(sl$low))


sapply(dsPosts[is.na(sl$low)], `[[`, "salary")

summary(rowMeans(sl))


unique(unlist(sapply(dice.dsPosts, `[[`, "baseSalary")))

SalaryRegularExpressions = 
  c("^([0-9,]+)(\\.00)?( DOE)?$" = "\\1;\\1",
    "^([0-9,]+)[Kk]$" = "\\1;\\1",
    "^\\$?([0-9,]+)[Kk]\\+$" = "\\1;NA",
    "^to \\$?([0-9,]+)[Kk]$" = "NA;\\1",              
    "\\$?([0-9,]+)[Kk]?( - |-)\\$?([0-9,]+)[Kk]?" = "\\1;\\3",
    "\\$?([0-9,]+)[Kk]?(\\+DOE)? ?\\+ ?\
(Equity|Bonus|Stock|Stock Opts).*$" = "\\1;\\1",
    "^\\$([0-9,]+)([Kk]|\\.00|\\+)?$" = "\\1;\\1",
    "\\$?([0-9,]+)[Kk]? to \\$?([0-9,]+)[Kk]?.*" = "\\1;\\3",
    "\\$?([0-9,]+)[Kk]?\\+?( DOE)?$" = "\\1;\\1",
    "\\$?([0-9,]+)[Kk]?/ann.*" = "\\1;\\1",
    "\\$?([0-9,]+)[Kk]?/ann.*" = "\\1;\\1",
    "\\$?([0-9,]+)[Kk]? all in" = "NA;\\1")

getSalaryRange =
function(values, asDataFrame = TRUE, 
          rx = SalaryRegularExpressions)
{

   done = rep(FALSE, length(values))
   ans = rep(NA, length(values))
   
   for(i in seq(along = rx)) {
      w = grepl(names(rx)[i], values[!done])
      tmp = gsub(names(rx[i]), rx[i], values[!done][w])
      ans[!done][w] = tmp
      done = !is.na(ans)
   }
   
   ans = structure(gsub(",", "", ans), names = values)

   if(asDataFrame) 
      convertLowHighToDataframe(ans)
   else
      ans
}

convertLowHighToDataframe =
function(ans)
{
    ans = as.data.frame(do.call(rbind, strsplit(ans, ";")), 
                        stringsAsFactors = FALSE)
    names(ans) = c("low", "high")
    ans[] = lapply(ans, fixNum)
    ans
}

dice.salaries = getSalaryRange(unlist(sapply(dice.dsPosts, 
                                            `[[`, "baseSalary")))
summary(rowMeans(dice.salaries))


w = apply(dice.salaries, 1, function(x) sum(is.na(x))) == 1
avgSalary = rowMeans(dice.salaries)
avgSalary[w] = apply(dice.salaries[w,], 1, 
                       function(x) x[!is.na(x)])
summary(avgSalary)

