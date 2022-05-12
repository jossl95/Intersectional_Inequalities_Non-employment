* Encoding: UTF-8.
* Jop Roeleven Bachelor Thesis

**********************************************Working Directory*************************************************

* Set working directory .
CD 'U:\Scriptie\Data' .

* Importeren datasets .
GET FILE = 'avars_201810_EN_1.0p.sav'
    /KEEP nomem_encr geslacht herkomstgroep leeftijd aantalki oplcat .
DATASET NAME avars1810 .

GET FILE = 'avars_201910_EN_1.0p.sav'
    /KEEP nomem_encr belbezig .
DATASET NAME avars1910 .

GET FILE = 'cs18k_EN_1.0p.sav'
    /KEEP nomem_encr cs18k294 cs18k295 cs18k296 cs18k297 cs18k298 
        cs18k327 cs18k338 cs18k349 cs18k360 cs18k371 .
DATASET NAME integration .

**********************************************Make Variables for analysis*************************************************.
DATASET ACTIVATE avars1810 .
SORT CASES BY nomem_encr (A) .

*************Variabele Geslacht*************.
*Dummy female .
COMPUTE female = geslacht - 1 .
VALUE LABELS female 0 "Male" 1"Female".
VARIABLE LABELS female "sex" .
FREQUENCIES female .

*************Variabele Etniciteit*************

FREQUENCIES herkomstgroep .

*dummies migratieachtergronden.
RECODE herkomstgroep (101 102 201 202 = 0) (0 = 1) INTO dutch .
RECODE herkomstgroep (0 201 202 = 0) (101 102 = 1) INTO west .
RECODE herkomstgroep (0 102 101 = 0) (201 202 = 1) INTO nonwest .
FREQUENCIES dutch west nonwest .
DESCRIPTIVES dutch west nonwest .

*************Variabele controle*************.
COMPUTE age = leeftijd .

*RECODE aantalki (0 = 1) (LO THRU HI = 0) INTO nochild .
*RECODE aantalki (1 = 1) (LO THRU HI = 0) INTO onechild .
*RECODE aantalki (2 = 1) (LO THRU HI = 0) INTO twochild .
*RECODE aantalki (3 4 5 6 = 1) (LO THRU HI = 0) INTO threechild .
*FREQUENCIES nochild onechild twochild threechild .

FREQUENCIES aantalki .
RECODE aantalki (0 = 0) (1 2 3 4 5 6 = 1) into child .
FREQUENCIES child .

FREQUENCIES oplcat .
RECODE oplcat (1 = 8) (2 = 12) (3 = 13.5) (4 = 16) (5 = 17) (6 = 18) INTO educ .
FREQUENCIES educ .

VARIABLE LABELS
    educ 'education in years'
    age 'age' 
    child 'having a child' .

COUNT miss18 = female TO educ (missing) .
RECODE miss18 (0 = 0) (ELSE = 1) .
FREQUENCIES miss18 .

SELECT IF miss18 = 0 .
EXECUTE .

*************Variabele Werkloos*************.
DATASET ACTIVATE avars1910 .
SORT CASES BY nomem_encr (A) .

*dummy unemployed .
FREQUENCIES belbezig .
RECODE belbezig (1 2 3 = 0) (4 5 8 11 = 1) (6 7 9 10 12 13 14 = SYSMIS) INTO unemp .
VARIABLE LABELS unemp "unemployed" .
VALUE LABELS unemp 0 "employed" 1 "unemployed" .
FREQUENCIES unemp .
DESCRIPTIVES unemp .

COUNT nomiss19 = unemp (missing) .
SELECT IF nomiss19 = 0 .
EXECUTE .

*********************DATASET Social Integration / Leisure*********************.
DATASET ACTIVATE integration .
SORT CASES BY nomem_encr (A) .

*************Variabele aandeel werkenden*************.

*Number of working contacts .
FREQUENCIES cs18k327 cs18k338 cs18k349 cs18k360 cs18k371 .
RECODE cs18k327 (1 2 = 1) (3 = 0) INTO cont1 .
RECODE cs18k338 (1 2 = 1) (3 = 0) INTO cont2 .
RECODE cs18k349 (1 2 = 1) (3 = 0) INTO cont3 .
RECODE cs18k360 (1 2 = 1) (3 = 0) INTO cont4 .
RECODE cs18k371 (1 2 = 1) (3 = 0) INTO cont5 .
COUNT numwork = cont1 cont2 cont3 cont4 cont5 (1).
FREQUENCIES numwork .
DESCRIPTIVES numwork .

*Number of contacts filled in .
COUNT numcon =  cs18k294 cs18k295 cs18k296 cs18k297 cs18k298 (1) .

FREQUENCIES numcon numwork .

**********************************************Merge Datasets*************************************************.

MATCH FILES FILE = avars1810 / FILE = avars1910 / FILE = integration
    /BY nomem_encr
    /KEEP nomem_encr female dutch west nonwest unemp 
               numcon numwork age child educ.
EXECUTE .
DATASET NAME merged .
DATASET ACTIVATE merged .
DISPLAY DICTIONARY .

COUNT miss = female to educ (MISSING) .
FREQUENCIES miss .
SELECT IF miss = 0 .
*Alleen respondenten bewaren met complete waarden op alle variabelen: 5008 over .

COMPUTE minor = age < 25 .
COMPUTE old = age > 65 .
COUNT miss = female TO educ (MISSING) .
EXECUTE .
FREQUENCIES miss minor old .

SELECT IF miss = 0  AND minor = 0 AND old = 0 .
EXECUTE .

FREQUENCIES female dutch west nonwest .

COMPUTE f_d = female * dutch .
COMPUTE f_w = female * west .
COMPUTE f_nw = female * nonwest .
EXECUTE .

FREQUENCIES unemp.
DESCRIPTIVES unemp numwork dutch west nonwest female educ child numcon .

DISPLAY DICTIONARY .

SAVE OUTFILE = 'merged2.sav' .

**********************************************Analyses*************************************************.
DATASET CLOSE ALL .
GET FILE = 'merged2.sav' .
DATASET NAME merged .
DATASET ACTIVATE merged.



DESCRIPTIVES unemp numwork dutch west nonwest female educ child numcon .

*T-tests belangrijkste variabelen

T-TEST
  /TESTVAL=1.7402
  /MISSING=ANALYSIS
  /VARIABLES=numwork
  /CRITERIA=CI(.95).

T-TEST
  /TESTVAL = .1608
  /MISSING = ANALYSIS
  /VARIABLES = unemp
  /CRITERIA = CI(.95) .

T-TEST
  /TESTVAL = .0897
  /MISSING = ANALYSIS
  /VARIABLE = west 
  /CRITERIA = CI(.95) .

T-TEST
  /TESTVAL = .0965
  /MISSING = ANALYSIS
  /VARIABLE = nonwest
  /CRITERIA = CI(.95)
  
T-TEST
  /TESTVAL = .5101
  /MISSING = ANALYSIS
  /VARIABLE = female 
  /CRITERIA = CI(.95)

*********************************************END**************************************************

*************************************************crosstabs***************************************************************************

COMPUTE intersex = 0 .
FREQUENCIES intersex .
IF (dutch = 1) AND (female = 0) intersex = 0 .
IF (dutch = 1) AND (female = 1) intersex = 1 .
IF (west = 1) AND (female = 1) intersex = 3 .
IF (west = 1) AND (female = 0) intersex = 2 .
IF (nonwest = 1) AND (female = 1) intersex = 5 .
IF (nonwest = 1) AND (female = 0) intersex = 4 .
FREQUENCIES intersex .

VALUE LABELS intersex 0 'NL man' 1 'NL vrouw' 2 'west man' 3 'west vrouw' 4 'nwest man' 5 'nwest vrouw' .

CROSSTABS
 /TABLES = unemp BY intersex


