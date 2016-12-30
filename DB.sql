/*
 * ComicShop Database Schema and Demo Data
 * by Thai Flowers
 * for Introduction to Databases, Summer-A 2016 Florida State University
 *     taught by Dr. David Gaitros
 * Emulating www.MailMeMyComics.com, the digital department of CosmicCatComics comic shop
 */
DROP TABLE IF EXISTS New_Users;
DROP TABLE IF EXISTS Web_Transactions;
DROP TABLE IF EXISTS Pull_List;
DROP TABLE IF EXISTS Users;
DROP TABLE IF EXISTS Shipping_Class_Rates;
DROP TABLE IF EXISTS Shipping_Class;
DROP TABLE IF EXISTS Physical_Pull_List;
DROP TABLE IF EXISTS Physical_Users;
DROP TABLE IF EXISTS Subscribable_Series;
DROP VIEW IF EXISTS Web_Subscribable;
DROP TABLE IF EXISTS Series;
DROP TABLE IF EXISTS Publisher;
DROP TABLE IF EXISTS Release_Schedule;
DROP TABLE IF EXISTS Address;
DROP TABLE IF EXISTS Rating;

CREATE TABLE Release_Schedule (
	id		INTEGER NOT NULL auto_increment,
	name		varchar(128) NOT NULL,
	January		INTEGER NOT NULL DEFAULT 0,
	February	INTEGER NOT NULL DEFAULT 0,
	March		INTEGER NOT NULL DEFAULT 0,
	April		INTEGER NOT NULL DEFAULT 0,
	May		INTEGER NOT NULL DEFAULT 0,
	June		INTEGER NOT NULL DEFAULT 0,
	July		INTEGER NOT NULL DEFAULT 0,
	August		INTEGER NOT NULL DEFAULT 0,
	September	INTEGER NOT NULL DEFAULT 0,
	October		INTEGER NOT NULL DEFAULT 0,
	November	INTEGER NOT NULL DEFAULT 0,
	December	INTEGER NOT NULL DEFAULT 0,
	PRIMARY KEY (id)
);

INSERT INTO Release_Schedule values
	(1, 'Monthly', 1,1,1,1,1,1,1,1,1,1,1,1), /* for shipping 2nd wed */
	(2, 'Twice-Monthly', 2,2,2,2,2,2,2,2,2,2,2,2), /* for shipping 1st and 3rd wed */
	(3, 'Weekly', 4,4,4,4,4,4,4,4,4,4,4,4), /* for shipping every wed */
	(4, 'Bi-Monthly-Jan', 1,0,1,0,1,0,1,0,1,0,1,0),
	(5, 'Bi-Monthly-Feb', 0,1,0,1,0,1,0,1,0,1,0,1),
	(6, 'Quarterly-Jan', 1,0,0,1,0,0,1,0,0,1,0,0),
	(7, 'Quarterly-Feb', 0,1,0,0,1,0,0,1,0,0,1,0),
	(8, 'Quarterly-Mar', 0,0,1,0,0,1,0,0,1,0,0,1);

CREATE TABLE Address (
	id	INTEGER NOT NULL auto_increment,
	name	varchar(128), /* name of address, for those using multiple */
	address1	varchar(256) DEFAULT '',
	address2	varchar(256) DEFAULT '',
	address3	varchar(256) DEFAULT '',
	city	varchar(128) DEFAULT '',
	state	char(2) DEFAULT '',
	zip	char(5) DEFAULT '',
	CONSTRAINT zip_sanity_ck CHECK (zip LIKE '_____' OR zip LIKE '_____-____'),
	/* http://www.sqldbpros.com/2011/11/free-zip-code-city-county-state-csv/ 
	   Use above for generating a table of valid zip+state combinations */
	PRIMARY KEY (id)

);

/* Use id of this as FK in Users, then use number ordered to index into Shipping_Class_Rates to get the cost (overall or per-comic TBD). Use smallest num >= sum(current_shippment)

Also, this has imperfect resolution on ultimate shipping costs at prepayment time.  Suggest the most appropriate shipping class+rate for prepayment, store the presumptive rate, and warn about possible need to add additional funds later and refund each shippment if a lower rate is used.
*/
CREATE TABLE Shipping_Class (
	id	INTEGER NOT NULL,
	name	VARCHAR(128),
	flat	BOOLEAN NOT NULL DEFAULT FALSE,
	PRIMARY KEY (id)
);

CREATE TABLE Shipping_Class_Rates (
	id	INTEGER NOT NULL,
	num	INTEGER NOT NULL,
	rate	DOUBLE NOT NULL,
	PRIMARY KEY (id,num),
	FOREIGN KEY (id) REFERENCES Shipping_Class(id)
	ON DELETE CASCADE
	ON UPDATE CASCADE
);

INSERT INTO Shipping_Class VALUES
	(1, 'Standard', False),
	(2, 'Expediated', False);

INSERT INTO Shipping_Class_Rates VALUES
	(1, 10, 3.99),
	(1, 20, 4.99),
	(2, 10, 5.99),
	(2, 20, 6.99);

CREATE TABLE Publisher (
	id	INTEGER NOT NULL auto_increment,
	name	VARCHAR(128) NOT NULL,
	owner	INTEGER,
	phone	CHAR(10) NOT NULL,
	address 	INTEGER,
	CONSTRAINT owner_sanity_ck CHECK (id <> owner),
	PRIMARY KEY (id),
	FOREIGN KEY (address) REFERENCES Address(id)
	ON UPDATE CASCADE,
	FOREIGN KEY (owner) REFERENCES Publisher(id)
	ON UPDATE CASCADE
	ON DELETE CASCADE
);

INSERT INTO Address (id,name) VALUES
(1,'DC Comics'),(2,'Marvel'),(3,'Image'),(4,'IDW'),
(5,'IDW'),(6,'Dark Horse Comics'),(7,'Radio Comix'),(8,'Sin Factory');

INSERT INTO Publisher (id, name, owner, phone, address) VALUES
	(1, 'DC Comics', NULL, '9999999999', 1),
	(2, 'Marvel', NULL, '8888888888', 2),
	(3, 'Image', NULL, '7777777777', 3),
	(4, 'IDW', NULL, '6666666666', 4),
	(5, 'Dark Horse Comics', NULL, '5555555555', 5),
	(6, 'Avatar Press', NULL, '4444444444', 6),
	(7, 'Radio Comix', NULL, '3333333333', 7),
	(8, 'Sin Factory', 7, '3333333333', 8);

CREATE TABLE Rating ( rating varchar(3) PRIMARY KEY NOT NULL );
INSERT INTO  Rating VALUES ('3+'), ('E'), ('T'), ('M'), ('18+'), ('N/A'), ('UN');

CREATE TABLE Series (
	id	INTEGER NOT NULL auto_increment,
	name	VARCHAR(128),
	volume	SMALLINT NOT NULL DEFAULT 1,
	publisher	INTEGER NOT NULL,
	rating	VARCHAR(3) NOT NULL DEFAULT 'E',
	limited_series	BOOLEAN NOT NULL DEFAULT FALSE,
	start_date	DATE,
	end_date	DATE,
	PRIMARY KEY (id),
	FOREIGN KEY (publisher) REFERENCES Publisher(id)
	ON UPDATE CASCADE
	ON DELETE CASCADE,
	FOREIGN KEY (rating) REFERENCES Rating(rating),
	CONSTRAINT uc_sanity UNIQUE (name, volume)
	/* CONSTRAINT rating_values CHECK (rating IN ('3+','E','T','M','18+', 'N/A', 'UN')) */
);

INSERT INTO Series (id, name, volume, publisher, rating) VALUES
	(1, 'Detective Comics', 2, 1, 'E'),
	(2, 'Batman', 2, 1, 'E'),
	(3, 'Action Comics', 2, 1, 'E'),
	(4, 'Superman', 4, 1, 'E'),
	(5, 'Wonder Woman', 5, 1, 'E'),
	(6, 'Sensation Comics', 2, 1, 'E'),
	(7, 'Sensational She-Hulk', 1, 2, 'E'),
	(8, 'Autumlands: Tooth and Claw', DEFAULT, 3, 'M'),
	(9, 'Teen Titans', 5, 1, 'T'),
	(10, 'Bat-Mite', DEFAULT, 1, 'E'),
	(11, 'Gotham Academy', DEFAULT, 1, 'E'),
	(12, 'International Iron Man', DEFAULT, 2, 'T'),
	(13, 'Superior Spider-Man', DEFAULT, 2, 'T'),
	(14, 'Batman: Rebirth', DEFAULT, 1, 'T'),
	(15, 'Scooby Apocalypse', DEFAULT, 1, 'T'),
	(16, 'Empowered', DEFAULT, 5, 'T'),
	(17, 'Jungle Fantasy', DEFAULT, 6, '18+'),
	(18, 'Genus', DEFAULT, 8, '18+'),
	(19, 'Genus Male', DEFAULT, 8, '18+'),
	(20, 'Prez', 1, 1, 'E'),
	(21, 'Prez', 2, 1, 'E');

UPDATE Series
SET limited_series=True
WHERE name='Prez' OR 'Batman: Rebirth';

CREATE TABLE Subscribable_Series (
	id	INTEGER NOT NULL,
	cap	SMALLINT DEFAULT 3,
	web	BOOLEAN NOT NULL DEFAULT FALSE,
	shop	BOOLEAN NOT NULL DEFAULT TRUE,
	ongoing_rate	DOUBLE NOT NULL,
	six_issue_rate	DOUBLE,
	/* a limited series may have a release_schedule, but a one-shot will have a one-month release period thus Monthly is valid */
	release_schedule	INTEGER NOT NULL DEFAULT 1,
	CONSTRAINT ongoing_ck CHECK (ongoing_rate > 0),
	CONSTRAINT six_issue_ck CHECK (six_issue_rate=NULL OR six_issue_rate > 0),
	CONSTRAINT sub_sanity_ck CHECK (shop=True OR Web=True),
	PRIMARY KEY (id),
	FOREIGN KEY (id) REFERENCES Series(id)
	ON DELETE CASCADE
	ON UPDATE CASCADE,
	FOREIGN KEY (release_schedule) REFERENCES Release_Schedule(id)
);

INSERT INTO Subscribable_Series (id, web, shop, ongoing_rate, six_issue_rate) VALUES
	(1, True, DEFAULT, 2.99,2.85),
	(2, True, DEFAULT, 2.99,2.85),
	(3, True, DEFAULT, 2.99,2.85),
	(4, True, DEFAULT, 2.99,2.85),
	(5, True, DEFAULT, 2.99,2.85),
	(6, True, DEFAULT, 2.99,2.85),
	(7, True, DEFAULT, 2.99,2.85),
	(8, True, DEFAULT, 3.99,3.85),
	(9, True, DEFAULT, 2.99,2.85),
	(10, True, DEFAULT, 2.99,2.85),
	(11, True, DEFAULT, 2.99,2.85),
	(12, True, DEFAULT, 2.99,2.85),
	(13, True, DEFAULT, 2.99,2.85),
	(14, True, DEFAULT, 3.99,3.85),
	(15, True, DEFAULT, 2.99,2.85),
	(16, False, DEFAULT, 1.99,1.99),
	(17, False, DEFAULT, 3.99,3.99),
	(18, False, DEFAULT, 4.99,4.99),
	(19, False, DEFAULT, 4.99,4.99),
	(20, True, False, 99.99, 99.99),
	(21, True, True, 2.99,2.85);

UPDATE Subscribable_Series SET release_schedule=5 WHERE id=8;

DROP VIEW IF EXISTS Web_Subscribable;
CREATE VIEW Web_Subscribable as
SELECT Series.id AS id,name,volume,cap,ongoing_rate,six_issue_rate,publisher
FROM Series,Subscribable_Series
WHERE web=true AND Series.id=Subscribable_Series.id;

/* Users from the Physical store, added by administrator(s) here for consistency */
CREATE TABLE Physical_Users (
	id	INTEGER NOT NULL auto_increment,
	first_name	VARCHAR(128) NOT NULL,
	last_name	VARCHAR(128) NOT NULL,
	phone	CHAR(10) NOT NULL,
	email	CHAR(128),
	address	INTEGER,
	FOREIGN KEY (address) REFERENCES Address(id),
	PRIMARY KEY (id)
);

CREATE TABLE Physical_Pull_List (
	user_id	INTEGER NOT NULL,
	series_id	INTEGER NOT NULL,
	FOREIGN KEY (user_id) REFERENCES Physical_Users(id)
	ON DELETE CASCADE
	ON UPDATE CASCADE,
	FOREIGN KEY (series_id) REFERENCES Subscribable_Series(id),
	CONSTRAINT shop_sanity_ck CHECK (user_id IN (SELECT id
	                                              FROM Subscribable_Series
	                                              WHERE shop=True)),
	PRIMARY KEY (user_id,series_id) 
);

CREATE TABLE New_Users (
	id	INTEGER NOT NULL auto_increment PRIMARY KEY,
	user_name	VARCHAR(50) NOT NULL UNIQUE,
	password	CHAR(40) NOT NULL,
	email	VARCHAR(128),	
	first_name	VARCHAR(128),
	last_name	VARCHAR(128),
	address	INTEGER REFERENCES Address(id),
	billing_address	INTEGER REFERENCES Address(id)
);

/* add trigger to set default shipping class on deletion of reference tuple */
CREATE TABLE Users (
	id	INTEGER NOT NULL auto_increment,
	user_name	VARCHAR(50) NOT NULL UNIQUE,
	admin	BOOLEAN NOT NULL DEFAULT False,
	password	CHAR(40) NOT NULL,
	failed_logins	SMALLINT NOT NULL DEFAULT 0,
	first_name	VARCHAR(128) NOT NULL, 
	last_name	VARCHAR(128) NOT NULL, 
	address	INTEGER NOT NULL,
	billing_address	INTEGER, /* NULL means same as address */
	phone	VARCHAR(10),
	email	VARCHAR(128) NOT NULL UNIQUE,
	verified_email	BOOLEAN NOT NULL DEFAULT FALSE,
	balance	DOUBLE NOT NULL DEFAULT 0, /* sum of prepaid funds deduct per fulfilled issue and early cancelation refunds */ 
	shipping_schedule	INTEGER NOT NULL DEFAULT 2,
	shipping_class	INTEGER NOT NULL DEFAULT 1,
	last_shipment_date	DATE DEFAULT NULL,
	FOREIGN KEY (shipping_schedule) REFERENCES Release_Schedule(id)
	ON UPDATE CASCADE,
	FOREIGN KEY (shipping_class) REFERENCES Shipping_Class(id)
	ON UPDATE CASCADE,
	FOREIGN KEY (address) REFERENCES Address(id)
	ON UPDATE CASCADE,
	FOREIGN KEY (billing_address) REFERENCES Address(id)
	ON UPDATE CASCADE,
	PRIMARY KEY (id)
);

INSERT INTO Address VALUES
	(9,'User Ron Addr','5600 Pineland Ave','','','Port Orange','FL','37127'),
	(10,'User AAA Addr','3105 Evergreen Terrace','Apt C','','Lakeland','FL','33801');

INSERT INTO Users (id,user_name,admin,first_name,last_name,password,phone,email,verified_email,address) VALUES
	(1,'Ron',True, 'Place', 'Holder', 'password', '0123456789', 'Ron@CCComics.com',True,9),
	(2,'AAA',False,'Example', 'Data', 'AAApass',  '0123456789', 'AAA@CCComics.com',False,10);

CREATE TABLE Pull_List (
	user_id	INTEGER NOT NULL,
	series_id	INTEGER NOT NULL,
	issues_left	INTEGER, /* NULL means ongoing */
	rate_at_sub	DOUBLE NOT NULL, /* fixed if issues_left is not null, otherwise updates via trigger */
	date_added	DATE NOT NULL, /* if cleared after date added to cart, then reload rate_at_sub, also can be used to figure out hot series and similar trends */
	cart	BOOLEAN NOT NULL DEFAULT True, /* Add subs to cart before pre-payment only then make official, cleanist form of cart creation I can think of */
	FOREIGN KEY (user_id) REFERENCES Users(id),
	/* ON UPDATE CASCADE*/ 
	/* ON DELETE CASCADE*/  /* ??? If I delete a user are these entries deleted first? If so can I use a trigger to handle scheduling refunds? */
	FOREIGN KEY (series_id) REFERENCES Subscribable_Series(id) /* view FK's verboten */
	ON UPDATE CASCADE
	ON DELETE CASCADE,
	CONSTRAINT web_sanity_ck CHECK (user_id IN (SELECT id
	                                            FROM Subscribable_Series
	                                            WHERE web=True)),
	PRIMARY KEY (user_id,series_id)
);

delimiter //
CREATE TRIGGER update_onging_rate AFTER UPDATE ON Subscribable_Series
FOR EACH ROW
BEGIN
	UPDATE Pull_List
	SET    rate_at_sub=NEW.ongoing_rate
	WHERE  series_id=New.id
	  AND  issues_left is NULL;
END;//
delimiter ;

/*
delimiter //
CREATE TRIGGER manage_user_balance AFTER UPDATE ON Pull_List
FOR EACH ROW
BEGIN
*/
	/* assuming tuples are added to Pull_List with cart=True first */
/*
	IF (NEW.cart=False AND NEW.issues_left is NULL) THEN
		SET @sub_total (NEW.issues_left * rate_at_sub);
		UPDATE Users balance=balance+@sub_total WHERE id=NEW.user_id;
	END IF;
END;//
delimiter ;
*/

/* add trigger if timestamp=NULL, timestampt=current_time */
/* add constraint for type, and triggers for appropriate types */
CREATE TABLE Web_Transactions (
	user_id	INTEGER NOT NULL,
	timestamp	TIMESTAMP NOT NULL DEFAULT NOW(),
	type	varchar(128) NOT NULL,
	sum	DOUBLE NOT NULL,
	FOREIGN KEY (user_id) REFERENCES Users(id),
	PRIMARY KEY (user_id,timestamp,type)
);

delimiter //
CREATE TRIGGER unsub_refund_event BEFORE DELETE ON Pull_List
FOR EACH ROW
BEGIN
	IF OLD.cart=False THEN
		SET @refund:=(SELECT IFNULL(issues_left,0) * rate_at_sub
			      FROM   Pull_list
			      WHERE  user_id=OLD.user_id
				AND  series_id=OLD.series_id);
		IF @refund>0 THEN
			INSERT INTO Web_Transactions VALUES
			(OLD.user_id, NOW(), "Unsub Refund", -1*@refund);
			UPDATE Users SET balance=balance-@refund WHERE id=OLD.user_id;
		END IF;
	END IF;
END;//
	                                              WHERE shop=True)),
delimiter ;
