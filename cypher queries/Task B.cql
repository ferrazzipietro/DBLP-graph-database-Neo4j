// 1. Find the top 3 most cited papers of each conference.
MATCH (y:Article)<-[:cited_by]-(x:Article)-[:published_in]->(z:Proceeding)-[:belongs_to]->(w:Conference)
WITH w.name AS ConferenceTitle, z.year AS Year, x.article_id AS ArticleTitle, count(*) AS Citations
ORDER BY ConferenceTitle, Year, Citations DESC
WITH ConferenceTitle, Year, collect(distinct Citations)[0..3] AS Top3CitationsAmount, collect({ArticleTitle:ArticleTitle, Citations:Citations}) AS ArticlesCited
UNWIND ArticlesCited AS TopCitedArticles
WITH ConferenceTitle, Year, Top3CitationsAmount, TopCitedArticles
WHERE TopCitedArticles.Citations IN Top3CitationsAmount
RETURN ConferenceTitle, Year, TopCitedArticles.ArticleTitle AS TopCitedArticle, TopCitedArticles.Citations AS Citations

// 2. For each conference find its community: i.e., those authors that have published papers on that conference in, at least, 4 different editions.
MATCH (a:Author)<-[:written_by]-(b:Article)-[:published_in]->(c:Proceeding)-[:belongs_to]->(d:Conference)
WITH DISTINCT d.name AS ConferenceTitle, c.year AS Edition, a.author_id AS AuthorId, a.name AS AuthorName
WITH ConferenceTitle, AuthorId, AuthorName, count(*) AS Publications
WHERE Publications >= 4
WITH ConferenceTitle, collect({AuthorId:AuthorId, AuthorName:AuthorName, Publications:Publications}) AS AuthorsPublications
UNWIND [t1 IN AuthorsPublications WHERE t1.Publications >= 1] AS TopAuthors
RETURN ConferenceTitle, TopAuthors.AuthorId AS AuthorId, TopAuthors.AuthorName AS AuthorName, TopAuthors.Publications AS Publications

// 3. Find the impact factors of the journals in your graph
MATCH (article:Article)-[:published_in]->(volume:Volume)-[:belongs_to]->(journal:Journal)
WITH journal AS Journal, toInteger(date(volume.name).year) AS Year
MATCH (article:Article)-[:published_in]->(volume:Volume)-[:belongs_to]->(Journal)
WHERE toInteger(date(volume.name).year) = Year-1 OR toInteger(date(volume.name).year) = Year-2
WITH Journal, Year, COUNT(article) AS Articles_2years
MATCH (volume_proceeding)<-[:published_in]-(citing_article:Article)<-[:cited_by]-(article:Article)-[:published_in]->(volume:Volume)-[:belongs_to]->(Journal)
WHERE (toInteger(date(volume_proceeding.name).year) = Year OR volume_proceeding.year = Year) AND (toInteger(date(volume.name).year) = Year-1 OR toInteger(date(volume.name).year) = Year-2)
WITH Journal, Year, Articles_2years, count(*) AS Citations_2years
WHERE Articles_2years > 0
RETURN Journal, Year, Articles_2years, Citations_2years, toFloat(Citations_2years) / Articles_2years  AS ImpactFactor


//In any given year, the two-year journal impact factor is the ratio between the number of citations received 
//in that year for publications in that journal that were published in the two preceding years and the total 
//number of "citable items" published in that journal during the two preceding years
//-----------------------------------------------------------------------------------------------------------------------------------------------------


// 4. Find the h-indexes of the authors in your graph

// The h-index is calculated by counting the number of publications for which an author has been cited by other authors at least 
// that same number of times.  For instance, an h-index of 17 means that the scientist has published at least 17 papers that have 
// been cited at least 17 times each.  If the scientist's 18th most cited publication was cited only 10 times, the h-index would 
// remain at 17.  If the scientist's 18th most cited publication was cited 18 or more times, the h-index would rise to 18.



MATCH (auth:Author)<-[w:written_by]-(:Article)
WITH auth, count(w) AS numberOfArt
MATCH (auth)<-[:written_by]-(art:Article)
OPTIONAL MATCH (art)<-[cited_by]-(cityingArt:Article)
WITH auth, numberOfArt, art, count(cityingArt) as numberOfCitations
ORDER BY auth, numberOfCitations DESC
WITH auth, numberOfArt, collect(numberOfCitations) AS citations
RETURN auth, reduce(hfact=0, i in (range(0,numberOfArt-1)) | CASE WHEN i<citations[i] THEN hfact+1 ELSE hfact END ) AS Hfactor
ORDER BY Hfactor DESC


// OLD VERSION

// Get the amount of articles of each author
MATCH (a1:Author)<-[:written_by]-(p1:Article)
WITH a1.author_id as AuthorId, a1.name as AuthorName, count(*) AS TotalArticles
// Get the articles written by each author
MATCH (a1:Author {author_id:AuthorId })<-[:written_by]-(p1:Article)
WITH a1.author_id as AuthorId, a1.name as AuthorName, p1.article_id as Article, TotalArticles
// Get the number of citations of each article
MATCH (a1:Author {author_id:AuthorId})<-[:written_by]-(p1:Article {article_id:Article})
OPTIONAL MATCH (a1)<-[:written_by]-(p1)-[:cited_by]->(p2:Article)
WITH  a1.author_id as AuthorId, a1.name as AuthorName, p1.article_id as Article, (CASE WHEN exists(p2.article_id) THEN 1 ELSE 0 END) AS CitationPoint
WITH AuthorId, AuthorName, Article, sum(CitationPoint) AS ArticlesCitations
// Get the total number of citations of each author
WITH AuthorId, AuthorName, sum(ArticlesCitations) AS AuthorCitations
// Join with the amount of articles of each author
MATCH (a1:Author {author_id:AuthorId})<-[:written_by]-(p1:Article)
WITH a1.author_id as AuthorId, a1.name as AuthorName, AuthorCitations, count(*) AS TotalArticles
// Join with the articles written by each author
MATCH (a1:Author {author_id:AuthorId })<-[:written_by]-(p1:Article)
WITH a1.author_id as AuthorId, a1.name as AuthorName, p1.article_id as Article, AuthorCitations, TotalArticles
// Join with the number of citations of each article
MATCH (a1:Author {author_id:AuthorId})<-[:written_by]-(p1:Article {article_id:Article})
OPTIONAL MATCH (a1)<-[:written_by]-(p1)-[:cited_by]->(p2:Article)
WITH  a1.author_id as AuthorId, a1.name as AuthorName, p1.article_id as Article, (CASE WHEN exists(p2.article_id) THEN 1 ELSE 0 END) AS CitationPoint, AuthorCitations, TotalArticles
WITH AuthorId, AuthorName, Article, sum(CitationPoint) AS ArticlesCitations, AuthorCitations, TotalArticles
ORDER BY AuthorId, AuthorName, ArticlesCitations DESC
// Get the h-index for each author
WITH AuthorId, AuthorName, AuthorCitations, TotalArticles, collect(ArticlesCitations) AS CitationsByArticle
RETURN AuthorId, AuthorName, AuthorCitations, TotalArticles, reduce(hindex = 0, n IN CitationsByArticle | CASE WHEN n > hindex THEN hindex+1 ELSE hindex END) AS Hindex


// Delete all nodes and edges
MATCH (n) 
DETACH DELETE n

