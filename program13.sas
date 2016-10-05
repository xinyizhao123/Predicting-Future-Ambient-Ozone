*****gswhist;

data gswhist1;
set gswhist;
drop var1 lon lat;
run;

proc sort data=gswhist1;
by row col;
run;

proc transpose data=gswhist1 out=gswhistt;
by row col;
run;

data gswhistt_edit;
set gswhistt;
siteid="            ";
if row=313 then siteid="13-247-1";
if row=311 then siteid="13-89-2";
if row=280 then siteid="18-89-22";
if row=359 then siteid="24-5-3001";
if row=215 then siteid="48-113-69";
if row=212 then siteid="48-121-34";
if row=52 then siteid="6-37-1002";
if row=54 then siteid="6-37-2";
if row=51 then siteid="6-37-6012";
if row=57 then siteid="6-71-1004";
rename _NAME_=date COL1=gsw;
label _NAME_="date" siteid="siteID";
run;

proc freq data=gswhistt_edit;
tables row*col*siteid/list;
run;

data gswhistt_final;
set gswhistt_edit;
tgsw=gsw*24;
drop row col gsw;
run;


*****gswfortyfive;

data gswfortyfive1;
set gswfortyfive;
drop var1 lon lat;
run;

proc sort data=gswfortyfive1;
by row col;
run;

proc transpose data=gswfortyfive1 out=gswfortyfivet;
by row col;
run;

data gswfortyfivet_edit;
set gswfortyfivet;
siteid="            ";
if row=313 then siteid="13-247-1";
if row=311 then siteid="13-89-2";
if row=280 then siteid="18-89-22";
if row=359 then siteid="24-5-3001";
if row=215 then siteid="48-113-69";
if row=212 then siteid="48-121-34";
if row=52 then siteid="6-37-1002";
if row=54 then siteid="6-37-2";
if row=51 then siteid="6-37-6012";
if row=57 then siteid="6-71-1004";
rename _NAME_=date COL1=gsw;
label _NAME_="date" siteid="siteID";
run;

proc freq data=gswfortyfivet_edit;
tables row*col*siteid/list;
run;

data gswfortyfivet_final;
set gswfortyfivet_edit;
tgswf=gsw*24;
drop row col gsw;
run;


*****gsweightyfive;

data gsweightyfive1;
set gsweightyfive;
drop var1 lon lat;
run;

proc sort data=gsweightyfive1;
by row col;
run;

proc transpose data=gsweightyfive1 out=gsweightyfivet;
by row col;
run;

data gsweightyfivet_edit;
set gsweightyfivet;
siteid="            ";
if row=313 then siteid="13-247-1";
if row=311 then siteid="13-89-2";
if row=280 then siteid="18-89-22";
if row=359 then siteid="24-5-3001";
if row=215 then siteid="48-113-69";
if row=212 then siteid="48-121-34";
if row=52 then siteid="6-37-1002";
if row=54 then siteid="6-37-2";
if row=51 then siteid="6-37-6012";
if row=57 then siteid="6-71-1004";
rename _NAME_=date COL1=gsw;
label _NAME_="date" siteid="siteID";
run;

proc freq data=gsweightyfivet_edit;
tables row*col*siteid/list;
run;

data gsweightyfivet_final;
set gsweightyfivet_edit;
tgswe=gsw*24;
drop row col gsw;
run;




*****rgrndhist;

data rgrndhist1;
set rgrndhist;
drop var1 lon lat;
run;

proc sort data=rgrndhist1;
by row col;
run;

proc transpose data=rgrndhist1 out=rgrndhistt;
by row col;
run;

data rgrndhistt_edit;
set rgrndhistt;
siteid="            ";
if row=313 then siteid="13-247-1";
if row=311 then siteid="13-89-2";
if row=280 then siteid="18-89-22";
if row=359 then siteid="24-5-3001";
if row=215 then siteid="48-113-69";
if row=212 then siteid="48-121-34";
if row=52 then siteid="6-37-1002";
if row=54 then siteid="6-37-2";
if row=51 then siteid="6-37-6012";
if row=57 then siteid="6-71-1004";
rename _NAME_=date COL1=rgrnd;
label _NAME_="date" siteid="siteID";
run;

proc freq data=rgrndhistt_edit;
tables row*col*siteid/list;
run;

data rgrndhistt_final;
set rgrndhistt_edit;
trgrnd=rgrnd*24;
drop row col rgrnd;
run;


*****rgrndfortyfive;

data rgrndfortyfive1;
set rgrndfortyfive;
drop var1 lon lat;
run;

proc sort data=rgrndfortyfive1;
by row col;
run;

proc transpose data=rgrndfortyfive1 out=rgrndfortyfivet;
by row col;
run;

data rgrndfortyfivet_edit;
set rgrndfortyfivet;
siteid="            ";
if row=313 then siteid="13-247-1";
if row=311 then siteid="13-89-2";
if row=280 then siteid="18-89-22";
if row=359 then siteid="24-5-3001";
if row=215 then siteid="48-113-69";
if row=212 then siteid="48-121-34";
if row=52 then siteid="6-37-1002";
if row=54 then siteid="6-37-2";
if row=51 then siteid="6-37-6012";
if row=57 then siteid="6-71-1004";
rename _NAME_=date COL1=rgrnd;
label _NAME_="date" siteid="siteID";
run;

proc freq data=rgrndfortyfivet_edit;
tables row*col*siteid/list;
run;

data rgrndfortyfivet_final;
set rgrndfortyfivet_edit;
trgrndf=rgrnd*24;
drop row col rgrnd;
run;


*****rgrndeightyfive;

data rgrndeightyfive1;
set rgrndeightyfive;
drop var1 lon lat;
run;

proc sort data=rgrndeightyfive1;
by row col;
run;

proc transpose data=rgrndeightyfive1 out=rgrndeightyfivet;
by row col;
run;

data rgrndeightyfivet_edit;
set rgrndeightyfivet;
siteid="            ";
if row=313 then siteid="13-247-1";
if row=311 then siteid="13-89-2";
if row=280 then siteid="18-89-22";
if row=359 then siteid="24-5-3001";
if row=215 then siteid="48-113-69";
if row=212 then siteid="48-121-34";
if row=52 then siteid="6-37-1002";
if row=54 then siteid="6-37-2";
if row=51 then siteid="6-37-6012";
if row=57 then siteid="6-71-1004";
rename _NAME_=date COL1=rgrnd;
label _NAME_="date" siteid="siteID";
run;

proc freq data=rgrndeightyfivet_edit;
tables row*col*siteid/list;
run;

data rgrndeightyfivet_final;
set rgrndeightyfivet_edit;
trgrnde=rgrnd*24;
drop row col rgrnd;
run;



/*ºF =(K - 273.15)* 1.8000 + 32.00 */

data tmaxhist1;
set tmaxhist;
drop var1 lon lat;
run;

proc sort data=tmaxhist1;
by row col;
run;

proc transpose data=tmaxhist1 out=tmaxhistt;
by row col;
run;

data tmaxhistt_edit;
set tmaxhistt;
siteid="            ";
if row=313 then siteid="13-247-1";
if row=311 then siteid="13-89-2";
if row=280 then siteid="18-89-22";
if row=359 then siteid="24-5-3001";
if row=215 then siteid="48-113-69";
if row=212 then siteid="48-121-34";
if row=52 then siteid="6-37-1002";
if row=54 then siteid="6-37-2";
if row=51 then siteid="6-37-6012";
if row=57 then siteid="6-71-1004";
rename _NAME_=date COL1=ktmax;
label _NAME_="date" siteid="siteID";
run;

proc freq data=tmaxhistt_edit;
tables row*col*siteid/list;
run;

data tmaxhistt_final;
set tmaxhistt_edit;
tmax=(ktmax-273.15)*1.8+32;
drop row col ktmax;
run;


data tmaxfortyfive1;
set tmaxfortyfive;
drop var1 lon lat;
run;

proc sort data=tmaxfortyfive1;
by row col;
run;

proc transpose data=tmaxfortyfive1 out=tmaxfortyfivet;
by row col;
run;

data tmaxfortyfivet_edit;
set tmaxfortyfivet;
siteid="            ";
if row=313 then siteid="13-247-1";
if row=311 then siteid="13-89-2";
if row=280 then siteid="18-89-22";
if row=359 then siteid="24-5-3001";
if row=215 then siteid="48-113-69";
if row=212 then siteid="48-121-34";
if row=52 then siteid="6-37-1002";
if row=54 then siteid="6-37-2";
if row=51 then siteid="6-37-6012";
if row=57 then siteid="6-71-1004";
rename _NAME_=date COL1=ktmax;
label _NAME_="date" siteid="siteID";
run;

proc freq data=tmaxfortyfivet_edit;
tables row*col*siteid/list;
run;

data tmaxfortyfivet_final;
set tmaxfortyfivet_edit;
tmax=(ktmax-273.15)*1.8+32;
drop row col ktmax;
run;


data tmaxfortyfive1;
set tmaxfortyfive;
drop var1 lon lat;
run;

proc sort data=tmaxfortyfive1;
by row col;
run;

proc transpose data=tmaxfortyfive1 out=tmaxfortyfivet;
by row col;
run;

data tmaxfortyfivet_edit;
set tmaxfortyfivet;
siteid="            ";
if row=313 then siteid="13-247-1";
if row=311 then siteid="13-89-2";
if row=280 then siteid="18-89-22";
if row=359 then siteid="24-5-3001";
if row=215 then siteid="48-113-69";
if row=212 then siteid="48-121-34";
if row=52 then siteid="6-37-1002";
if row=54 then siteid="6-37-2";
if row=51 then siteid="6-37-6012";
if row=57 then siteid="6-71-1004";
rename _NAME_=date COL1=ktmax;
label _NAME_="date" siteid="siteID";
run;

proc freq data=tmaxfortyfivet_edit;
tables row*col*siteid/list;
run;

data tmaxfortyfivet_final;
set tmaxfortyfivet_edit;
tmaxf=(ktmax-273.15)*1.8+32;
drop row col ktmax;
run;


data tmaxeightyfive1;
set tmaxeightyfive;
drop var1 lon lat;
run;

proc sort data=tmaxeightyfive1;
by row col;
run;

proc transpose data=tmaxeightyfive1 out=tmaxeightyfivet;
by row col;
run;

data tmaxeightyfivet_edit;
set tmaxeightyfivet;
siteid="            ";
if row=313 then siteid="13-247-1";
if row=311 then siteid="13-89-2";
if row=280 then siteid="18-89-22";
if row=359 then siteid="24-5-3001";
if row=215 then siteid="48-113-69";
if row=212 then siteid="48-121-34";
if row=52 then siteid="6-37-1002";
if row=54 then siteid="6-37-2";
if row=51 then siteid="6-37-6012";
if row=57 then siteid="6-71-1004";
rename _NAME_=date COL1=ktmax;
label _NAME_="date" siteid="siteID";
run;

proc freq data=tmaxeightyfivet_edit;
tables row*col*siteid/list;
run;

data tmaxeightyfivet_final;
set tmaxeightyfivet_edit;
tmaxe=(ktmax-273.15)*1.8+32;
drop row col ktmax;
run;




data preciphist1;
set preciphist;
drop var1 lon lat;
run;

proc sort data=preciphist1;
by row col;
run;

proc transpose data=preciphist1 out=preciphistt;
by row col;
run;

data preciphistt_edit;
set preciphistt;
siteid="            ";
if row=313 then siteid="13-247-1";
if row=311 then siteid="13-89-2";
if row=280 then siteid="18-89-22";
if row=359 then siteid="24-5-3001";
if row=215 then siteid="48-113-69";
if row=212 then siteid="48-121-34";
if row=52 then siteid="6-37-1002";
if row=54 then siteid="6-37-2";
if row=51 then siteid="6-37-6012";
if row=57 then siteid="6-71-1004";
rename _NAME_=date COL1=precip;
label _NAME_="date" siteid="siteID";
run;

proc freq data=preciphistt_edit;
tables row*col*siteid/list;
run;

data preciphistt_final;
set preciphistt_edit;
drop row col;
run;



data precipfortyfive1;
set precipfortyfive;
drop var1 lon lat;
run;

proc sort data=precipfortyfive1;
by row col;
run;

proc transpose data=precipfortyfive1 out=precipfortyfivet;
by row col;
run;

data precipfortyfivet_edit;
set precipfortyfivet;
siteid="            ";
if row=313 then siteid="13-247-1";
if row=311 then siteid="13-89-2";
if row=280 then siteid="18-89-22";
if row=359 then siteid="24-5-3001";
if row=215 then siteid="48-113-69";
if row=212 then siteid="48-121-34";
if row=52 then siteid="6-37-1002";
if row=54 then siteid="6-37-2";
if row=51 then siteid="6-37-6012";
if row=57 then siteid="6-71-1004";
rename _NAME_=date COL1=precipf;
label _NAME_="date" siteid="siteID";
run;

proc freq data=precipfortyfivet_edit;
tables row*col*siteid/list;
run;

data precipfortyfivet_final;
set precipfortyfivet_edit;
drop row col;
run;



data precipeightyfive1;
set precipeightyfive;
drop var1 lon lat;
run;

proc sort data=precipeightyfive1;
by row col;
run;

proc transpose data=precipeightyfive1 out=precipeightyfivet;
by row col;
run;

data precipeightyfivet_edit;
set precipeightyfivet;
siteid="            ";
if row=313 then siteid="13-247-1";
if row=311 then siteid="13-89-2";
if row=280 then siteid="18-89-22";
if row=359 then siteid="24-5-3001";
if row=215 then siteid="48-113-69";
if row=212 then siteid="48-121-34";
if row=52 then siteid="6-37-1002";
if row=54 then siteid="6-37-2";
if row=51 then siteid="6-37-6012";
if row=57 then siteid="6-71-1004";
rename _NAME_=date COL1=precipe;
label _NAME_="date" siteid="siteID";
run;

proc freq data=precipeightyfivet_edit;
tables row*col*siteid/list;
run;

data precipeightyfivet_final;
set precipeightyfivet_edit;
drop row col;
run;
