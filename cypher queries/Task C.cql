
// Louvain Communiy detection: detect the communities of authors defined by the conferences
// Louvain Communiy detection: detect the communities of authors defined by the conferences

// link authors and conferences
MATCH (auth:Author)<-[:written_by]-(:Article)-[:published_in]-> (:Proceeding) -[:belongs_to]->(conf:Conference)
WITH conf, auth
MERGE (auth)-[p:partecipates_in]->(conf)
RETURN auth, p, conf

// link authors that partecipate at the same conference
MATCH (auth1:Author)<-[:written_by]-(:Article)-[:published_in]-> 
    (:Proceeding) -[:belongs_to]->(conf:Conference)<-[:belongs_to]-(:Proceeding)<-[:published_in]-(:Article)-[:written_by]->(auth2:Author)
WITH auth1, auth2
MERGE (auth1)-[p:wasInSameConfThan]->(auth2)
RETURN auth1, auth2, (p) 

// delete graph projection
CALL gds.graph.drop('authorsConferenceGraph')
// see al the projected graphs
CALL gds.graph.list()

// create projected graph Author -- conference
CALL gds.graph.create.cypher(
    'authorsConferenceGraph',
    'MATCH (n) WHERE n:Author OR n:Conference
    RETURN id(n) AS id, labels(n) AS labels',
    'MATCH (auth:Author)<-[:written_by]-(:Article)-[:published_in]-> (:Proceeding) -[:belongs_to]->(conf:Conference)
    WITH conf, auth
    MERGE (auth)-[p:partecipates_in]->(conf)
    RETURN  id(auth) AS source, id(conf) AS target'
)
YIELD graphName AS graph

// create projected graph Author -[sameConference]- Author
CALL gds.graph.create.cypher(
    'sameConferenceAuthorsGraph',
    'MATCH (n) WHERE n:Author 
    RETURN id(n) AS id, labels(n) AS labels',
    'MATCH (auth1:Author)<-[:written_by]-(:Article)-[:published_in]-> 
    (:Proceeding) -[:belongs_to]->(conf:Conference)<-[:belongs_to]-(:Proceeding)
    <-[:published_in]-(:Article)-[:written_by]->(auth2:Author)
    WHERE id(auth1) < id(auth2)
    WITH auth1, auth2
    MERGE (auth1)-[p:wasInSameConfThan]->(auth2)
    RETURN id(auth1) AS source, id(auth2) AS target'
)

// create projected graph Author -[assignedAsReviewerBySameEditor]- Author
CALL gds.graph.create.cypher(
    'sameEditorAssignesAuthorsGraph',
    'MATCH (n:Author)
    RETURN id(n) AS id, labels(n) AS labels',
    'MATCH (auth1:Author)-[:reviewer_in]->(:ReviewGroup)<-[:assignes]-(Conf_Jour)-[:assignes]->(:ReviewGroup)<-[:reviewer_in]-(auth2:Author)
    WHERE id(auth1) < id(auth2)
    WITH auth1, auth2
    MERGE (auth1)-[:sameEditor]->(auth2)
    RETURN id(auth1) AS source, id(auth2) AS target'
)



// Luovain application

// how many community indentified
CALL gds.louvain.stream('sameConferenceAuthorsGraph')
YIELD nodeId, communityId
WITH DISTINCT communityId
RETURN count(communityId )

// see most cronwed communities
CALL gds.louvain.stream(
            'sameConferenceAuthorsGraph'
        )
        YIELD nodeId, communityId
        RETURN communityId, COUNT(DISTINCT nodeId) AS members
        ORDER BY members DESC
        LIMIT 5




// NODE SIMILARITY 
// articles similarity based on the topics they talk about
// I want to find all the articles similar to the one "Parallel Machine Scheduling with Time Dependent Processing Times"

// first, all the articles has to be check for similarity
CALL gds.graph.create.cypher(
    'articleTopicsGraph',
    'MATCH (n) 
    WHERE n:Article OR n:Keyword 
    RETURN id(n) AS id, labels(n) AS labels',
    'MATCH (art:Article)-[:talks_about]->(k:Keyword)
    RETURN id(art) AS source, id(k) AS target'
)

// the model can be applied to the all data and then we'll take what we need
CALL gds.nodeSimilarity.stream('articleTopicsGraph')
YIELD node1, node2, similarity
WHERE gds.util.asNode(node1).title="Energy Infrastructure Defense Systems."
RETURN gds.util.asNode(node2).title AS Article2, similarity
ORDER BY  similarity DESC







// TASK D

// page rank creating graph
CALL gds.graph.create.cypher(
  'DBCommunityGraph',
  'MATCH (art:Article)-[:talks_about]->(:Keyword)-[:feature_of]->(com:Community{name:"database community"}) 
    MATCH (art)-[:published_in]->()-[:belongs_to]->()-[:is_part_of]->(com) 
    RETURN DISTINCT id(art) AS id, labels(art) AS labels',
  'MATCH (art1:Article)-[:talks_about]->(:Keyword)-[:feature_of]->(com:Community{name:"database community"}) 
  MATCH (art1)-[:published_in]->()-[:belongs_to]->()-[:is_part_of]->(com) 
  MATCH (art2:Article)-[:talks_about]->(:Keyword)-[:feature_of]->(com:Community{name:"database community"}) 
  MATCH (art2)-[:published_in]->()-[:belongs_to]->()-[:is_part_of]->(com)  
  WHERE art1.article_id<art2.article_id 
  MATCH (art1)-[:cited_by]-(art2) 
  RETURN id(art1) AS source, id(art2) AS target'
)

// applying page rank
CALL gds.pageRank.stream('DBCommunityGraph')
YIELD nodeId, score
RETURN gds.util.asNode(nodeId).title AS name, score
ORDER BY score DESC, name ASC

// write new info 
CALL gds.pageRank.write('DBCommunityGraph',{writeProperty:"pageRankInDBComm"})