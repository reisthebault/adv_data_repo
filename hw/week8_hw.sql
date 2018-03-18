CREATE TABLE "booking" (
	"﻿JID" VARCHAR NOT NULL, 
	"BOOKING_NUMBER" VARCHAR NOT NULL, 
	"OFFENDERNAME" VARCHAR NOT NULL, 
	"DOB" DATE, 
	"GENDER" VARCHAR NOT NULL, 
	"RACE" VARCHAR, 
	"ZIP" VARCHAR, 
	"START_DATE" TIMESTAMP WITHOUT TIME ZONE, 
	"END_DATE" TIMESTAMP WITHOUT TIME ZONE, 
	time_jailed DECIMAL, 
	"ARRESTAGENCY" VARCHAR, 
	"RELEASE_TYPE" VARCHAR, 
	"OCCUPATION" VARCHAR, 
	"CHARGES" VARCHAR
);

COPY booking FROM '/Users/reis/Documents/Berkeley J-School/4th Semester/Adv Data/week8/bookingFresno_1718.csv' CSV HEADER;



/* finding jail demographics */
SELECT "RACE", count(*)
FROM booking GROUP BY "RACE";

CREATE TABLE "booking_demo" (
	"RACE" VARCHAR,
	"COUNT" INTEGER
);

COPY booking_demo FROM '/Users/reis/Documents/Berkeley J-School/4th Semester/Adv Data/week8/booking_demo.csv' CSV HEADER;

ALTER TABLE booking_demo ADD COLUMN pct numeric;

SELECT *, (("COUNT"::numeric/33852)) AS rate
  FROM booking_demo;

UPDATE booking_demo SET pct = (("COUNT"::numeric/33852));

SELECT * FROM booking_demo ORDER BY pct DESC;


/* finding most common types of releases */
SELECT "RELEASE_TYPE", count(*)
FROM booking GROUP BY "RELEASE_TYPE";

CREATE TABLE "booking_release" (
	"RELEASE_TYPE" VARCHAR,
	"COUNT" INTEGER
);

copy booking_release FROM '/Users/reis/Documents/Berkeley J-School/4th Semester/Adv Data/week8/booking_release.csv' CSV HEADER;

ALTER TABLE booking_release ADD COLUMN pct numeric;

SELECT *, (("COUNT"::numeric/33852)) AS rate
  FROM booking_release;

UPDATE booking_release SET pct = (("COUNT"::numeric/33852));

SELECT * FROM booking_release ORDER BY pct DESC;


/* finding most common release types white v. black ... this isn't working wtf?!?!?! */

SELECT WHITE and BLACK FROM booking.RACE 
GROUP BY "RELEASE_TYPE";

SELECT "RELEASE_TYPE", "RACE", count(*) FROM booking
	WHERE "RACE" IN ('WHITE','BLACK')
	GROUP BY "RELEASE_TYPE", "RACE";

CREATE TABLE "booking_release_demo" (
	"RELEASE_TYPE" VARCHAR,
	"RACE" VARCHAR,
	"COUNT" INTEGER
);

copy booking_release_demo FROM '/Users/reis/Documents/Berkeley J-School/4th Semester/Adv Data/week8/booking_release_demo.csv' CSV HEADER;

ALTER TABLE booking_release_demo ADD COLUMN pct_of_race numeric;

/*this isn't working */

SELECT *, ( "RACE" LIKE '%WHITE%' ("COUNT"::numeric/8107)) AS rate
  FROM booking_release_demo;


