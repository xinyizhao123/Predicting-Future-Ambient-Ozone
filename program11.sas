proc transpose data=no out=not;
by date hour;
run;

data not2;
set not;
rename _NAME_=siteID COL1=NO;
label _NAME_="siteID";
if _NAME_="_13_247_1" then _NAME_="13-247-1";
if _NAME_="_13_89_2" then _NAME_="13-89-2";
if _NAME_="_18_89_22" then _NAME_="18-89-22";
if _NAME_="_24_5_3001" then _NAME_="24-5-3001";
if _NAME_="_48_113_69" then _NAME_="48-113-69";
if _NAME_="_48_121_34" then _NAME_="48-121-34";
if _NAME_="_6_37_1002" then _NAME_="6-37-1002";
if _NAME_="_6_37_2" then _NAME_="6-37-2";
if _NAME_="_6_37_6012" then _NAME_="6-37-6012";
if _NAME_="_6_71_1004" then _NAME_="6-71-1004";
/*char_date = put(date, BEST12.);*/
run;

/*
data not2_2;
set not2;
format _date julian7.;
_date=put(trim(char_date),julian7.);
jdate = put(substr(strip(char_date),1,7),julian7.);
run;
*/

proc sort data=not2;
by siteID date hour;
run;

proc sql;
create table not3 as
 select unique date, siteID, avg(NO) as NO_s, count(hour) as nobs
 from not2
 group by siteID, date
 order by siteID, date;
quit;

proc freq data=not3;
tables nobs;
run;

data not4;
set not3;
if nobs ge 23;
drop nobs;
run;

proc contents data=not4;
run;




proc transpose data=no2 out=no2t;
by date hour;
run;

data no2t2;
set no2t;
rename _NAME_=siteID COL1=NO2;
label _NAME_="siteID";
if _NAME_="_13_247_1" then _NAME_="13-247-1";
if _NAME_="_13_89_2" then _NAME_="13-89-2";
if _NAME_="_18_89_22" then _NAME_="18-89-22";
if _NAME_="_24_5_3001" then _NAME_="24-5-3001";
if _NAME_="_48_113_69" then _NAME_="48-113-69";
if _NAME_="_48_121_34" then _NAME_="48-121-34";
if _NAME_="_6_37_1002" then _NAME_="6-37-1002";
if _NAME_="_6_37_2" then _NAME_="6-37-2";
if _NAME_="_6_37_6012" then _NAME_="6-37-6012";
if _NAME_="_6_71_1004" then _NAME_="6-71-1004";
/*char_date = put(date, BEST12.);*/
run;

/*
data no2t2_2;
set no2t2;
format _date julian7.;
_date=put(trim(char_date),julian7.);
jdate = put(substr(strip(char_date),1,7),julian7.);
run;
*/

proc sort data=no2t2;
by siteID date hour;
run;

proc sql;
create table no2t3 as
 select unique date, siteID, avg(NO2) as NO2_s, count(hour) as nobs
 from no2t2
 group by siteID, date
 order by siteID, date;
quit;

proc freq data=no2t3;
tables nobs;
run;

proc print data=no2t3;
where nobs < 23;
run;

data no2t4;
set no2t3;
if nobs ge 23;
drop nobs;
run;

proc contents data=no2t4;
run;


data nox;
merge not4 no2t4;
by siteID date;
NOx_s = NO_s + NO2_s;
drop NO_s NO2_s;
label siteID="siteID";
run;


proc transpose data=vocs out=vocst;
by date hour;
run;

data vocst2;
set vocst;
rename _NAME_=siteID COL1=VOCS;
label _NAME_="siteID";
if _NAME_="_13_247_1" then _NAME_="13-247-1";
if _NAME_="_13_89_2" then _NAME_="13-89-2";
if _NAME_="_18_89_22" then _NAME_="18-89-22";
if _NAME_="_24_5_3001" then _NAME_="24-5-3001";
if _NAME_="_48_113_69" then _NAME_="48-113-69";
if _NAME_="_48_121_34" then _NAME_="48-121-34";
if _NAME_="_6_37_1002" then _NAME_="6-37-1002";
if _NAME_="_6_37_2" then _NAME_="6-37-2";
if _NAME_="_6_37_6012" then _NAME_="6-37-6012";
if _NAME_="_6_71_1004" then _NAME_="6-71-1004";
/*char_date = put(date, BEST12.);*/
run;

/*
data vocst2_2;
set vocst2;
format _date julian7.;
_date=put(trim(char_date),julian7.);
jdate = put(substr(strip(char_date),1,7),julian7.);
run;
*/

proc sort data=vocst2;
by siteID date hour;
run;

proc sql;
create table vocst3 as
 select unique date, siteID, avg(VOCS) as VOCS_s, count(hour) as nobs
 from vocst2
 group by siteID, date
 order by siteID, date;
quit;

proc freq data=vocst3;
tables nobs;
run;

data vocst4;
set vocst3;
if nobs ge 23;
drop nobs;
run;

proc contents data=vocst4;
run;


data simu;
merge nox vocst4;
by siteID date;
run;
