  
 CREATE DATABASE CUNYDATA607;
 
  \c cunydata607
 
 DROP TABLE IF EXISTS Movies
CREATE TABLE Movies (Name char(20) PRIMARY KEY NOT NULL, Rating char(40), Director char(40));
INSERT INTO Movies VALUES('Avatar2','PG-13','James Cameron'),('KnivesOut2','PG-13','Rian Johnson'),('The Batman','PG-13','Matt Reeves'),('The Black Phone','R','Scott Derrickson'),('The Menu','R','Mark Mylod'),('Top Gun: Maverick','PG-13','Joseph Kosinski'),('Violent Night','R','Tommy Wirkola');
					  
 DROP TABLE IF EXISTS Week2_Movies
CREATE TABLE Week2_Movies (FirstName char(20) NOT NULL, LastName char(20) NOT NULL, Rating numeric, Movie char(20) REFERENCES Movies (Name));
 
 --READABILITY VERSION
 INSERT INTO Week2_Movies
 VALUES ('Kurt','Craig',4,'Avatar2'),
 		('Donna','Craig',3,'Avatar2'),
		('Justin','Flemming',4.5,'Avatar2'),
		('Justin','Rhinehart',5,'Avatar2'),
		('Madden','Neilson',3.5,'Avatar2'),
		
		('Kurt','Craig',4.5,'KnivesOut2'),
 		('Donna','Craig',3.5,'KnivesOut2'),
		('Justin','Flemming',4,'KnivesOut2'),
		('Justin','Rhinehart',2,'KnivesOut2'),
		('Madden','Neilson',4.5,'KnivesOut2'),
		
		('Kurt','Craig',3,'The Menu'),
 		('Donna','Craig',3.5,'The Menu'),
		('Justin','Flemming',4,'The Menu'),
		('Justin','Rhinehart',5,'The Menu'),
		('Madden','Neilson',3.5,'The Menu'),
		
		('Kurt','Craig',NULL,'Violent Night'),
 		('Donna','Craig',NULL,'Violent Night'),
		('Justin','Flemming',3,'Violent Night'),
		('Justin','Rhinehart',3,'Violent Night'),
		('Madden','Neilson',3,'Violent Night'),
				
		('Kurt','Craig',4,'The Batman'),
 		('Donna','Craig',4,'The Batman'),
		('Justin','Flemming',3,'The Batman'),
		('Justin','Rhinehart',4,'The Batman'),
		('Madden','Neilson',4,'The Batman'),
				
		('Kurt','Craig',NULL,'The Black Phone'),
 		('Donna','Craig',NULL,'The Black Phone'),
		('Justin','Flemming',NULL,'The Black Phone'),
		('Justin','Rhinehart',3,'The Black Phone'),
		('Madden','Neilson',NULL,'The Black Phone'),
				
		('Kurt','Craig',4,'Top Gun: Maverick'),
 		('Donna','Craig',4,'Top Gun: Maverick'),
		('Justin','Flemming'3.5,'Top Gun: Maverick'),
		('Justin','Rhinehart',NULL,'Top Gun: Maverick'),
		('Madden','Neilson',3,'Top Gun: Maverick')
		;
/* What to copy/paste into PSQL

 INSERT INTO Week2_Movies VALUES ('Kurt','Craig',4,'Avatar2'),('Donna','Craig',3,'Avatar2'),('Justin','Flemming',4.5,'Avatar2'),('Justin','Rhinehart',5,'Avatar2'),('Madden','Neilson',3.5,'Avatar2'),('Kurt','Craig',4.5,'KnivesOut2'),('Donna','Craig',3.5,'KnivesOut2'),('Justin','Flemming',4,'KnivesOut2'),('Justin','Rhinehart',2,'KnivesOut2'),('Madden','Neilson',4.5,'KnivesOut2'),('Kurt','Craig',3,'The Menu'),('Donna','Craig',3.5,'The Menu'),('Justin','Flemming',4,'The Menu'),('Justin','Rhinehart',5,'The Menu'),('Madden','Neilson',3.5,'The Menu'),('Kurt','Craig',NULL,'Violent Night'),('Donna','Craig',NULL,'Violent Night'),('Justin','Flemming',3,'Violent Night'),('Justin','Rhinehart',3,'Violent Night'),('Madden','Neilson',3,'Violent Night'),('Kurt','Craig',4,'The Batman'),('Donna','Craig',4,'The Batman'),('Justin','Flemming',3,'The Batman'),('Justin','Rhinehart',4,'The Batman'),('Madden','Neilson',4,'The Batman'),('Kurt','Craig',NULL,'The Black Phone'),('Donna','Craig',NULL,'The Black Phone'),('Justin','Flemming',NULL,'The Black Phone'),('Justin','Rhinehart',3,'The Black Phone'),('Madden','Neilson',NULL,'The Black Phone'),('Kurt','Craig',4,'Top Gun: Maverick'),('Donna','Craig',4,'Top Gun: Maverick'),('Justin','Flemming',3.5,'Top Gun: Maverick'),('Justin','Rhinehart',NULL,'Top Gun: Maverick'),('Madden','Neilson',3,'Top Gun: Maverick');

*/