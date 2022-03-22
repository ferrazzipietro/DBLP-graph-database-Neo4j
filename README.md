# DBLP-graph-database-Neo4j
Neo4j project based on DBLP graph database.
Based on an UPC assignment.  


**DATA**
There are two ways: 
- generating following the "DOWNLOAD AND CONVERT DBLP DATABASE" procedure or
- using files in _data_, equivalent to the first way (scale factor=6000)

**DOWNLOAD AND CONVERT DBLP DATABASE**
The graph has to be loaded from https://dblp.uni-trier.de/xml/ in .xlm format. dblp.xml.gz and dblp.dtd are needed. These two files are to be saved in ~/.../data.
The XML file can then be converted in CSV following the instructions at https://github.com/ThomHurks/dblp-to-csv, generating a CSV file that is Neo4j compatible:
  1) obtain a local copy of XMLToCSV.py and save it in ~/.../data
  2) from command line launch the following command: 
          python XMLToCSV.py --annotate --neo4j data/dblp.xml data/dblp.dtd dblp.csv --relations author:authored_by journal:published_in
These data should be saved in the _Raw_ directory

**GENERATE MISSING DATA**
The csv do not contain all the necessary data. The preprocessing can be done throw the functions provided in _Cleaning and Preparing_.

**Task A**
Create the connection to the db and upload the generated nodes and edges in neo4j. The schema is represented in _graph schemas/modelA1_

**Task A.3** 
Update the data to new schema. The schema is represented in _graph schemas/modelA3_.

**Task B**
Cypher queries:
- Find the top 3 most cited papers of each conference.
- For each conference find its community: i.e., those authors that have published papers on that conference in, at least, 4 different editions.
- Find the impact factors of the journals in your graph (see https://en.wikipedia. org/wiki/Impact_factor, for the definition of the impact factor).
- Find the h-indexes of the authors in your graph (see https://en.wikipedia.org/ wiki/H-index, for a definition of the h-index metric).

**Task C**
Need of Data Science library https://neo4j.com/product/graph-data-science/
Application of:
  - Louvain community detection btw authors based on the partecipation at the same conference and 
  - similarity to a certain article (given its title) based on the topics it talks about

**Task D**

- Research database community, defined through the following keywords: data management, indexing, data modeling, big data, data processing, data storage and data querying
- Find the conferences and journals related to the database community. If 90% of the papers published in a conference/journal contain one of the keywords of the database community we consider that conference/journal as related to that community.
- Identify the top 50 papers of these conferences/journals based on the highest page rank provided by the papers of the same community (papers in the conferences/journals of the database community). 
- Identify "gurus", i.e., authors that are authors of, at least, two papers among the top-100 identified.

