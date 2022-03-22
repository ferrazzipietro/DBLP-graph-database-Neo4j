// DATA TO TEST QUERIES

// --Create conferences--
CREATE (:Conference {title:'Conference_a'}), (:Conference {title:'Conference_b'}), (:Conference {title:'Conference_c'}), (:Conference {title:'Conference_d'})

// --Create preceedings--
CREATE (:Proceeding {proceeding_id:'proceeding_a1', year:'2021'}), (:Proceeding {proceeding_id:'proceeding_a2', year:'2020'}), (:Proceeding {proceeding_id:'proceeding_a3', year:'2019'}), (:Proceeding {proceeding_id:'proceeding_a4', year:'2018'})
CREATE (:Proceeding {proceeding_id:'proceeding_b1', year:'2021'}), (:Proceeding {proceeding_id:'proceeding_b2', year:'2020'}), (:Proceeding {proceeding_id:'proceeding_b3', year:'2019'}), (:Proceeding {proceeding_id:'proceeding_b4', year:'2018'})
CREATE (:Proceeding {proceeding_id:'proceeding_c1', year:'2021'}), (:Proceeding {proceeding_id:'proceeding_c2', year:'2020'}), (:Proceeding {proceeding_id:'proceeding_c3', year:'2019'}), (:Proceeding {proceeding_id:'proceeding_c4', year:'2018'})
CREATE (:Proceeding {proceeding_id:'proceeding_d1', year:'2021'}), (:Proceeding {proceeding_id:'proceeding_d2', year:'2020'}), (:Proceeding {proceeding_id:'proceeding_d3', year:'2019'}), (:Proceeding {proceeding_id:'proceeding_d4', year:'2018'});
// --Link proceedings to conferences--

// Conference a
MATCH (n:Conference {title:'Conference_a'})
MATCH (a:Proceeding {proceeding_id:'proceeding_a1', year:'2021'})
CREATE (a)-[:belongs_to]->(n)
WITH n
MATCH (b:Proceeding {proceeding_id:'proceeding_a2', year:'2020'})
CREATE (b)-[:belongs_to]->(n)
WITH n
MATCH (c:Proceeding {proceeding_id:'proceeding_a3', year:'2019'})
CREATE (c)-[:belongs_to]->(n)
WITH n
MATCH (d:Proceeding {proceeding_id:'proceeding_a4', year:'2018'})
CREATE (d)-[:belongs_to]->(n);

// Conference b
MATCH (n:Conference {title:'Conference_b'})
MATCH (a:Proceeding {proceeding_id:'proceeding_b1', year:'2021'})
CREATE (a)-[:belongs_to]->(n)
WITH n
MATCH (b:Proceeding {proceeding_id:'proceeding_b2', year:'2020'})
CREATE (b)-[:belongs_to]->(n)
WITH n
MATCH (c:Proceeding {proceeding_id:'proceeding_b3', year:'2019'})
CREATE (c)-[:belongs_to]->(n)
WITH n
MATCH (d:Proceeding {proceeding_id:'proceeding_b4', year:'2018'})
CREATE (d)-[:belongs_to]->(n);

// Conference c
MATCH (n:Conference {title:'Conference_c'})
MATCH (a:Proceeding {proceeding_id:'proceeding_c1', year:'2021'})
CREATE (a)-[:belongs_to]->(n)
WITH n
MATCH (b:Proceeding {proceeding_id:'proceeding_c2', year:'2020'})
CREATE (b)-[:belongs_to]->(n)
WITH n
MATCH (c:Proceeding {proceeding_id:'proceeding_c3', year:'2019'})
CREATE (c)-[:belongs_to]->(n)
WITH n
MATCH (d:Proceeding {proceeding_id:'proceeding_c4', year:'2018'})
CREATE (d)-[:belongs_to]->(n);

// Conference d
MATCH (n:Conference {title:'Conference_d'})
MATCH (a:Proceeding {proceeding_id:'proceeding_d1', year:'2021'})
CREATE (a)-[:belongs_to]->(n)
WITH n
MATCH (b:Proceeding {proceeding_id:'proceeding_d2', year:'2020'})
CREATE (b)-[:belongs_to]->(n)
WITH n
MATCH (c:Proceeding {proceeding_id:'proceeding_d3', year:'2019'})
CREATE (c)-[:belongs_to]->(n)
WITH n
MATCH (d:Proceeding {proceeding_id:'proceeding_d4', year:'2018'})
CREATE (d)-[:belongs_to]->(n);

// -- Create articles and link them to proceedings--
// Conference a
MATCH (n1:Proceeding {proceeding_id:'proceeding_a1', year:'2021'}),
(n2:Proceeding {proceeding_id:'proceeding_a2', year:'2020'}),
(n3:Proceeding {proceeding_id:'proceeding_a3', year:'2019'}),
(n4:Proceeding {proceeding_id:'proceeding_a4', year:'2018'})
WITH n1,n2,n3,n4
CREATE (:Article {article_id: 'article_a11'})-[:published_in]->(n1)
CREATE (:Article {article_id: 'article_a12'})-[:published_in]->(n1)
CREATE (:Article {article_id: 'article_a13'})-[:published_in]->(n1)
CREATE (:Article {article_id: 'article_a14'})-[:published_in]->(n1)
CREATE (:Article {article_id: 'article_a21'})-[:published_in]->(n2)
CREATE (:Article {article_id: 'article_a22'})-[:published_in]->(n2)
CREATE (:Article {article_id: 'article_a23'})-[:published_in]->(n2)
CREATE (:Article {article_id: 'article_a24'})-[:published_in]->(n2)
CREATE (:Article {article_id: 'article_a31'})-[:published_in]->(n3)
CREATE (:Article {article_id: 'article_a32'})-[:published_in]->(n3)
CREATE (:Article {article_id: 'article_a33'})-[:published_in]->(n3)
CREATE (:Article {article_id: 'article_a34'})-[:published_in]->(n3)
CREATE (:Article {article_id: 'article_a41'})-[:published_in]->(n4)
CREATE (:Article {article_id: 'article_a42'})-[:published_in]->(n4)
CREATE (:Article {article_id: 'article_a43'})-[:published_in]->(n4)
CREATE (:Article {article_id: 'article_a44'})-[:published_in]->(n4);

// Conference b
MATCH (n1:Proceeding {proceeding_id:'proceeding_b1', year:'2021'}),
(n2:Proceeding {proceeding_id:'proceeding_b2', year:'2020'}),
(n3:Proceeding {proceeding_id:'proceeding_b3', year:'2019'}),
(n4:Proceeding {proceeding_id:'proceeding_b4', year:'2018'})
WITH n1,n2,n3,n4
CREATE (:Article {article_id: 'article_b11'})-[:published_in]->(n1)
CREATE (:Article {article_id: 'article_b12'})-[:published_in]->(n1)
CREATE (:Article {article_id: 'article_b13'})-[:published_in]->(n1)
CREATE (:Article {article_id: 'article_b14'})-[:published_in]->(n1)
CREATE (:Article {article_id: 'article_b21'})-[:published_in]->(n2)
CREATE (:Article {article_id: 'article_b22'})-[:published_in]->(n2)
CREATE (:Article {article_id: 'article_b23'})-[:published_in]->(n2)
CREATE (:Article {article_id: 'article_b24'})-[:published_in]->(n2)
CREATE (:Article {article_id: 'article_b31'})-[:published_in]->(n3)
CREATE (:Article {article_id: 'article_b32'})-[:published_in]->(n3)
CREATE (:Article {article_id: 'article_b33'})-[:published_in]->(n3)
CREATE (:Article {article_id: 'article_b34'})-[:published_in]->(n3)
CREATE (:Article {article_id: 'article_b41'})-[:published_in]->(n4)
CREATE (:Article {article_id: 'article_b42'})-[:published_in]->(n4)
CREATE (:Article {article_id: 'article_b43'})-[:published_in]->(n4)
CREATE (:Article {article_id: 'article_b44'})-[:published_in]->(n4);

// Conference c
MATCH (n1:Proceeding {proceeding_id:'proceeding_c1', year:'2021'}),
(n2:Proceeding {proceeding_id:'proceeding_c2', year:'2020'}),
(n3:Proceeding {proceeding_id:'proceeding_c3', year:'2019'}),
(n4:Proceeding {proceeding_id:'proceeding_c4', year:'2018'})
WITH n1,n2,n3,n4
CREATE (:Article {article_id: 'article_c11'})-[:published_in]->(n1)
CREATE (:Article {article_id: 'article_c12'})-[:published_in]->(n1)
CREATE (:Article {article_id: 'article_c13'})-[:published_in]->(n1)
CREATE (:Article {article_id: 'article_c14'})-[:published_in]->(n1)
CREATE (:Article {article_id: 'article_c21'})-[:published_in]->(n2)
CREATE (:Article {article_id: 'article_c22'})-[:published_in]->(n2)
CREATE (:Article {article_id: 'article_c23'})-[:published_in]->(n2)
CREATE (:Article {article_id: 'article_c24'})-[:published_in]->(n2)
CREATE (:Article {article_id: 'article_c31'})-[:published_in]->(n3)
CREATE (:Article {article_id: 'article_c32'})-[:published_in]->(n3)
CREATE (:Article {article_id: 'article_c33'})-[:published_in]->(n3)
CREATE (:Article {article_id: 'article_c34'})-[:published_in]->(n3)
CREATE (:Article {article_id: 'article_c41'})-[:published_in]->(n4)
CREATE (:Article {article_id: 'article_c42'})-[:published_in]->(n4)
CREATE (:Article {article_id: 'article_c43'})-[:published_in]->(n4)
CREATE (:Article {article_id: 'article_c44'})-[:published_in]->(n4);

// Conference d
MATCH (n1:Proceeding {proceeding_id:'proceeding_d1', year:'2021'}),
(n2:Proceeding {proceeding_id:'proceeding_d2', year:'2020'}),
(n3:Proceeding {proceeding_id:'proceeding_d3', year:'2019'}),
(n4:Proceeding {proceeding_id:'proceeding_d4', year:'2018'})
WITH n1,n2,n3,n4
CREATE (:Article {article_id: 'article_d11'})-[:published_in]->(n1)
CREATE (:Article {article_id: 'article_d12'})-[:published_in]->(n1)
CREATE (:Article {article_id: 'article_d13'})-[:published_in]->(n1)
CREATE (:Article {article_id: 'article_d14'})-[:published_in]->(n1)
CREATE (:Article {article_id: 'article_d21'})-[:published_in]->(n2)
CREATE (:Article {article_id: 'article_d22'})-[:published_in]->(n2)
CREATE (:Article {article_id: 'article_d23'})-[:published_in]->(n2)
CREATE (:Article {article_id: 'article_d24'})-[:published_in]->(n2)
CREATE (:Article {article_id: 'article_d31'})-[:published_in]->(n3)
CREATE (:Article {article_id: 'article_d32'})-[:published_in]->(n3)
CREATE (:Article {article_id: 'article_d33'})-[:published_in]->(n3)
CREATE (:Article {article_id: 'article_d34'})-[:published_in]->(n3)
CREATE (:Article {article_id: 'article_d41'})-[:published_in]->(n4)
CREATE (:Article {article_id: 'article_d42'})-[:published_in]->(n4)
CREATE (:Article {article_id: 'article_d43'})-[:published_in]->(n4)
CREATE (:Article {article_id: 'article_d44'})-[:published_in]->(n4);


// -- Create authors and link them to articles--
// Conference a
MATCH (n1:Article {article_id:'article_a11'}),
(n2:Article {article_id:'article_a12'}),
(n3:Article {article_id:'article_a13'}),
(n4:Article {article_id:'article_a14'}),
(n5:Article {article_id:'article_a21'}),
(n6:Article {article_id:'article_a22'}),
(n7:Article {article_id:'article_a23'}),
(n8:Article {article_id:'article_a24'}),
(n9:Article {article_id:'article_a31'}),
(n10:Article {article_id:'article_a32'}),
(n11:Article {article_id:'article_a33'}),
(n12:Article {article_id:'article_a34'}),
(n13:Article {article_id:'article_a41'}),
(n14:Article {article_id:'article_a42'}),
(n15:Article {article_id:'article_a43'}),
(n16:Article {article_id:'article_a44'})
WITH n1,n2,n3,n4,n5,n6,n7,n8,n9,n10,n11,n12,n13,n14,n15,n16
CREATE (:Author {author_id:'author_a1', name: 'Jose'})<-[:written_by]-(n1)
CREATE (:Author {author_id:'author_a2', name: 'Pedro'})<-[:written_by]-(n2)
CREATE (:Author {author_id:'author_a3', name: 'Luis'})<-[:written_by]-(n3)
CREATE (:Author {author_id:'author_a4', name: 'Gonzalo'})<-[:written_by]-(n4)
CREATE (:Author {author_id:'author_a5', name: 'Maria'})<-[:written_by]-(n5)
CREATE (:Author {author_id:'author_a6', name: 'Lucia'})<-[:written_by]-(n6)
CREATE (:Author {author_id:'author_a7', name: 'Fernanda'})<-[:written_by]-(n7)
CREATE (:Author {author_id:'author_a8', name: 'Ines'})<-[:written_by]-(n8)
CREATE (:Author {author_id:'author_a9', name: 'Maritza'})<-[:written_by]-(n9)
CREATE (:Author {author_id:'author_a10', name: 'Angela'})<-[:written_by]-(n10)
CREATE (:Author {author_id:'author_a11', name: 'Amparo'})<-[:written_by]-(n11)
CREATE (:Author {author_id:'author_a12', name: 'Jesus'})<-[:written_by]-(n12)
CREATE (:Author {author_id:'author_a13', name: 'Nicole'})<-[:written_by]-(n13)
CREATE (:Author {author_id:'author_a14', name: 'Tejaswini'})<-[:written_by]-(n14)
CREATE (:Author {author_id:'author_a15', name: 'Adam'})<-[:written_by]-(n15)
CREATE (:Author {author_id:'author_a16', name: 'Niccolo'})<-[:written_by]-(n16);

// Conference b
MATCH (n1:Article {article_id:'article_b11'}),
(n2:Article {article_id:'article_b12'}),
(n3:Article {article_id:'article_b13'}),
(n4:Article {article_id:'article_b14'}),
(n5:Article {article_id:'article_b21'}),
(n6:Article {article_id:'article_b22'}),
(n7:Article {article_id:'article_b23'}),
(n8:Article {article_id:'article_b24'}),
(n9:Article {article_id:'article_b31'}),
(n10:Article {article_id:'article_b32'}),
(n11:Article {article_id:'article_b33'}),
(n12:Article {article_id:'article_b34'}),
(n13:Article {article_id:'article_b41'}),
(n14:Article {article_id:'article_b42'}),
(n15:Article {article_id:'article_b43'}),
(n16:Article {article_id:'article_b44'})
WITH n1,n2,n3,n4,n5,n6,n7,n8,n9,n10,n11,n12,n13,n14,n15,n16
CREATE (:Author {author_id:'author_b1', name: 'Sergio'})<-[:written_by]-(n1)
CREATE (:Author {author_id:'author_b2', name: 'Milagros'})<-[:written_by]-(n2)
CREATE (:Author {author_id:'author_b3', name: 'Ximena'})<-[:written_by]-(n3)
CREATE (:Author {author_id:'author_b4', name: 'Guliana'})<-[:written_by]-(n4)
CREATE (:Author {author_id:'author_b5', name: 'Valentina'})<-[:written_by]-(n5)
CREATE (:Author {author_id:'author_b6', name: 'Claudia'})<-[:written_by]-(n6)
CREATE (:Author {author_id:'author_b7', name: 'Giovana'})<-[:written_by]-(n7)
CREATE (:Author {author_id:'author_b8', name: 'Cannavaro'})<-[:written_by]-(n8)
CREATE (:Author {author_id:'author_b9', name: 'Van Basten'})<-[:written_by]-(n9)
CREATE (:Author {author_id:'author_b10', name: 'Ronaldo'})<-[:written_by]-(n10)
CREATE (:Author {author_id:'author_b11', name: 'Nedved'})<-[:written_by]-(n11)
CREATE (:Author {author_id:'author_b12', name: 'Guerrero'})<-[:written_by]-(n12)
CREATE (:Author {author_id:'author_b13', name: 'Pizarro'})<-[:written_by]-(n13)
CREATE (:Author {author_id:'author_b14', name: 'Lapadula'})<-[:written_by]-(n14)
CREATE (:Author {author_id:'author_b15', name: 'Davide'})<-[:written_by]-(n15)
CREATE (:Author {author_id:'author_b16', name: 'Marita'})<-[:written_by]-(n16);

// Conference c
MATCH (n1:Article {article_id:'article_c11'}),
(n2:Article {article_id:'article_c12'}),
(n3:Article {article_id:'article_c13'}),
(n4:Article {article_id:'article_c14'}),
(n5:Article {article_id:'article_c21'}),
(n6:Article {article_id:'article_c22'}),
(n7:Article {article_id:'article_c23'}),
(n8:Article {article_id:'article_c24'}),
(n9:Article {article_id:'article_c31'}),
(n10:Article {article_id:'article_c32'}),
(n11:Article {article_id:'article_c33'}),
(n12:Article {article_id:'article_c34'}),
(n13:Article {article_id:'article_c41'}),
(n14:Article {article_id:'article_c42'}),
(n15:Article {article_id:'article_c43'}),
(n16:Article {article_id:'article_c44'})
WITH n1,n2,n3,n4,n5,n6,n7,n8,n9,n10,n11,n12,n13,n14,n15,n16
CREATE (:Author {author_id:'author_c1', name: 'Tapia'})<-[:written_by]-(n1)
CREATE (:Author {author_id:'author_c2', name: 'Gallese'})<-[:written_by]-(n2)
CREATE (:Author {author_id:'author_c3', name: 'Advincula'})<-[:written_by]-(n3)
CREATE (:Author {author_id:'author_c4', name: 'Ramos'})<-[:written_by]-(n4)
CREATE (:Author {author_id:'author_c5', name: 'Trauco'})<-[:written_by]-(n5)
CREATE (:Author {author_id:'author_c6', name: 'Corzo'})<-[:written_by]-(n6)
CREATE (:Author {author_id:'author_c7', name: 'Santa Maria'})<-[:written_by]-(n7)
CREATE (:Author {author_id:'author_c8', name: 'Aquino'})<-[:written_by]-(n8)
CREATE (:Author {author_id:'author_c9', name: 'Lopez'})<-[:written_by]-(n9)
CREATE (:Author {author_id:'author_c10', name: 'Cueva'})<-[:written_by]-(n10)
CREATE (:Author {author_id:'author_c11', name: 'Paolo'})<-[:written_by]-(n11)
CREATE (:Author {author_id:'author_c12', name: 'Cristian'})<-[:written_by]-(n12)
CREATE (:Author {author_id:'author_c13', name: 'Melanie'})<-[:written_by]-(n13)
CREATE (:Author {author_id:'author_c14', name: 'Andres'})<-[:written_by]-(n14)
CREATE (:Author {author_id:'author_c15', name: 'Miguel'})<-[:written_by]-(n15)
CREATE (:Author {author_id:'author_c16', name: 'Pepe'})<-[:written_by]-(n16);

// Conference d
MATCH (n1:Article {article_id:'article_d11'}),
(n2:Article {article_id:'article_d12'}),
(n3:Article {article_id:'article_d13'}),
(n4:Article {article_id:'article_d14'}),
(n5:Article {article_id:'article_d21'}),
(n6:Article {article_id:'article_d22'}),
(n7:Article {article_id:'article_d23'}),
(n8:Article {article_id:'article_d24'}),
(n9:Article {article_id:'article_d31'}),
(n10:Article {article_id:'article_d32'}),
(n11:Article {article_id:'article_d33'}),
(n12:Article {article_id:'article_d34'}),
(n13:Article {article_id:'article_d41'}),
(n14:Article {article_id:'article_d42'}),
(n15:Article {article_id:'article_d43'}),
(n16:Article {article_id:'article_d44'})
WITH n1,n2,n3,n4,n5,n6,n7,n8,n9,n10,n11,n12,n13,n14,n15,n16
CREATE (:Author {author_id:'author_d1', name: 'Manu'})<-[:written_by]-(n1)
CREATE (:Author {author_id:'author_d2', name: 'Draxler'})<-[:written_by]-(n2)
CREATE (:Author {author_id:'author_d3', name: 'Messi'})<-[:written_by]-(n3)
CREATE (:Author {author_id:'author_d4', name: 'Mbappe'})<-[:written_by]-(n4)
CREATE (:Author {author_id:'author_d5', name: 'Kimpembe'})<-[:written_by]-(n5)
CREATE (:Author {author_id:'author_d6', name: 'Navas'})<-[:written_by]-(n6)
CREATE (:Author {author_id:'author_d7', name: 'Donarumma'})<-[:written_by]-(n7)
CREATE (:Author {author_id:'author_d8', name: 'Gueye'})<-[:written_by]-(n8)
CREATE (:Author {author_id:'author_d9', name: 'Divi'})<-[:written_by]-(n9)
CREATE (:Author {author_id:'author_d10', name: 'Ferrazi'})<-[:written_by]-(n10)
CREATE (:Author {author_id:'author_d11', name: 'Postigo'})<-[:written_by]-(n11)
CREATE (:Author {author_id:'author_d12', name: 'Dhupad'})<-[:written_by]-(n12)
CREATE (:Author {author_id:'author_d13', name: 'Kovacs'})<-[:written_by]-(n13)
CREATE (:Author {author_id:'author_d14', name: 'Miroslav'})<-[:written_by]-(n14)
CREATE (:Author {author_id:'author_d15', name: 'Klose'})<-[:written_by]-(n15)
CREATE (:Author {author_id:'author_d16', name: 'Liliana'})<-[:written_by]-(n16);

// -- Create citations --
MATCH (n:Article {article_id:'article_a22'}), (m:Article {article_id:'article_a41'})
CREATE (n)<-[:cited_by]-(m);
MATCH (n:Article {article_id:'article_a23'}), (m:Article {article_id:'article_a42'})
CREATE (n)<-[:cited_by]-(m);
MATCH (n:Article {article_id:'article_a24'}), (m:Article {article_id:'article_a42'})
CREATE (n)<-[:cited_by]-(m)
MATCH (n:Article {article_id:'article_a31'}), (m:Article {article_id:'article_a44'})
CREATE (n)<-[:cited_by]-(m)

// -- Make some authors publish in more than 1 edition of conferences --
MATCH (n:Author {name:'Divi'})
DETACH DELETE n;
MATCH (n:Author {name:'Messi'}), (m:Article {article_id:'article_d31'})
CREATE (n)<-[:written_by]-(m);
MATCH (n:Author {name:'Niccolo'})
DETACH DELETE n;
MATCH (n:Author {name:'Nicole'}), (m:Article {article_id:'article_a44'})
CREATE (n)<-[:written_by]-(m)

// -- Add some articles
MATCH (n:Author {name:'Nicole'}), (m:Proceeding {proceeding_id:'proceeding_a4'})
CREATE (n)<-[:written_by]-(:Article {article_id:'article_a45'})-[:published_in]->(m)


// --Remove some articles--
MATCH (n:Article {article_id:'article_a21'})
DETACH DELETE n;
MATCH (n:Article {article_id:'articlea_43'})
DETACH DELETE n;