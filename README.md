
It's a web-GIS  plafrotm based on PostGIS and R for forestry analysis.
The platform allows to select the damaged area by drawing polygons; several vegetation indices (VIs) are automatically calculated using remote sensing data (Sentinel-2A) 
and tested to identify the more suitable ones for quantifying forest damage.
This webplatform has been development for italian forestry tecnichian, so the main interface and some name inside the code are in italian. 
The following paper describes the system and the assessment of tseverity of the damage: 
Piragnolo, M.; Pirotti, F.; Zanrosso, C.; Lingua, E.; Grigolato, S. Responding to Large-Scale Forest Damage in an Alpine Environment with Remote Sensing, Machine Learning, and Web-GIS. 
Remote Sens. 2021, 13, 1541. https://doi.org/10.3390/rs13081541

[Webb_appl](https://github.com/markgis8/Infortrac/assets/4560446/231b96c3-d932-4924-b131-62962953ce71)

[Schema_web_based_platform](https://github.com/markgis8/Infortrac/assets/4560446/9fd7e664-b947-4cc8-8f7a-e71aa208ccb3)


**************
The Sentinel-2A images are stored in Postgres/PostGIS database
To connect to the database set these variables on dbcon.R file 

 dbname = "mydb"
 host = "localhost"
 user = "user"
 pass = "password"
 
set the table for the spatial query on indici.R 

  mytable="table"
  
****************
  
