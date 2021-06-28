*************************************************************************************************************
* Final Code - 2nd August 2019					
* Developed by Chaplain Katumbi - simulating nutritional 2015-16 MDHS Dataset to assess impact of			*	
* mis-recording cluster identification details on point and interval estimates								*	
*************************************************************************************************************


//0. Analyse Original dataset - descriptives
cd "C:\Users\Lenovo\Documents\Chaplain\Documents\Personal\Rowland\MW_2015-16_DHS_12112017_1140_113119\mwpr7hdt"
//cd "C:\Users\user\Documents\Personal\Rowland\MW_2015-16_DHS_12112017_1140_113119\mwpr7hdt"


use MWPR7HFL, clear
br

//explore variables
tab hv001 //cluster id
tab hc27 //sex
sum hc1, d //age
sum hc2, d //weight
sum hc3, d //height
sum hc15, d //standing or lying down


//0. Analysis of original dataset- nutritional indicators and storage of results
//set more off 	
//program define p_0, rclass
//syntax [, c(real 1)]
//set tracedepth 1
//set trace on
//set matsize 800

	// Random Sampling code
//cd "C:\Users\Lenovo\Documents\Chaplain\Documents\Personal\Rowland\MW_2015-16_DHS_12112017_1140_113119\mwpr7hdt"
//cd "C:\Users\user\Documents\Personal\Rowland\MW_2015-16_DHS_12112017_1140_113119\mwpr7hdt"

//use MWPR7HFL, clear
//br


// calcuate nutrition indices

//simple sample weighting using DHS for clusters only, no strata; must multiply by 1000000
gen weight=hv005/1000000
svyset [pweight=weight], psu(hv021)


//hw15 is lying (1) and standing (2) height measure; clean DHS code
gen newm=hc15 if hc15!=9
//hw3 is height, clean DHS code and converts to cm
gen newh=hc3/10 if hc3!=9999
//hw3 is height in kg to one decimal w/o the decimal; convert to kg w/decimal; clean DHS code and
gen neww=hc2/10 if hc2!=999
//run zscore06. age in months (hw1) and gender (b4) need no cleaning in this dataset
zscore06, a(hc1) s(hc27) h(newh) w(neww) measure(newm) male(1) female(2)

//remove biologically implausible scores
replace haz06=. if haz06<-6 | haz06>6
replace waz06=. if waz06<-6 | waz06>5
replace whz06=. if whz06<-5 | whz06>5
replace bmiz06=. if bmiz06<-5 | bmiz06>5

//example using svy: mean of height-for-age
svy: mean haz06 if !missing(hc0)
svy, over(hv024): mean haz06
estat effects 
loneway haz06 hv021 if hv024==1 & !missing(hc0)
loneway haz06 hv021 if hv024==2 & !missing(hc0)
loneway haz06 hv021 if hv024==3 & !missing(hc0)



//set more off 	
//program define p_5, rclass
//syntax [, c(real 1)]
//set tracedepth 1
//set trace on
//set matsize 800

	// Random Sampling code
cd "C:\Users\Lenovo\Documents\Chaplain\Documents\Personal\Rowland\MW_2015-16_DHS_12112017_1140_113119\mwpr7hdt"
//cd "C:\Users\user\Documents\Personal\Rowland\MW_2015-16_DHS_12112017_1140_113119\mwpr7hdt"

use MWPR7HFL, clear
br

//explore variables
tab hv001 //cluster id
tab hc27 //sex
sum hc1, d //age
sum hc2, d //weight
sum hc3, d //height
sum hc15, d //standing or lying down

	//introduce misclassification
	gen x=runiform()
	sort x
	gen clusterID2=runiformint(1,850) in 1/1220 //misclassification in selected set of elements
	list clusterID2 hv021 hv023 hv024 hv025 in 1/70

	//checking original clusterid and generated id are the same and re-assigning new one gen check1=.
	gen check1=.
	replace check1=1 if hv021==clusterID2
	replace check1=0 if hv021!=clusterID2
	tab check1

	gen district3=.
	gen region3=.
	gen residence3=.

	replace district3=1 if clusterID2==42|clusterID2==69|clusterID2==78|clusterID2==86| clusterID2==97| ///
						 clusterID2==105 | clusterID2==140 | clusterID2==172 | clusterID2==332| ///
						 clusterID2==393 | clusterID2==458 | clusterID2==461 | clusterID2==480| ///
						 clusterID2==626 | clusterID2==716 | clusterID2==760 | clusterID2==779| ///
						 clusterID2==813 | clusterID2==817 | clusterID2==850 
						 
						 
	replace district3=2 if clusterID2==149 | clusterID2==162 | clusterID2==177 | clusterID2==673 | clusterID2==759

						 
	replace district3 =3 if clusterID2==120|clusterID2==128 | clusterID2==178|clusterID2==204 | clusterID2==243 | ///
						 clusterID2==248 | clusterID2==343 | clusterID2==361 | clusterID2==388 | ///
						 clusterID2==459 | clusterID2==473 | clusterID2==558 | clusterID2==585 | ///
						 clusterID2==631 | clusterID2==648 | clusterID2==672 | clusterID2==685 | ///
						 clusterID2==776 | clusterID2==830 | clusterID2==8


	replace district3 =4 if clusterID2==48|clusterID2==95 | clusterID2==250|clusterID2==280 | ///
						   clusterID2==309 | clusterID2==507 | clusterID2==603 | clusterID2==649

				   

	replace district3=5 if clusterID2==17 | clusterID2==40 | clusterID2==144| ///
							clusterID2==158| clusterID2==159 | clusterID2==179| ///
							clusterID2==184| clusterID2==185| clusterID2==205| ///
							clusterID2==342 | clusterID2==364| clusterID2==397| ///
							clusterID2==424 | clusterID2==460 | clusterID2==494| ///
							clusterID2==531| clusterID2==547 | clusterID2==555| ///
							clusterID2==571| clusterID2==599| clusterID2==641| ///
							clusterID2==846

	replace district3=6 if clusterID2==59 | clusterID2==226 | clusterID2==227 |clusterID2==492 | clusterID2==809



	replace district3=7 if clusterID2==27 | clusterID2==29 | clusterID2==101 | clusterID2==118 | clusterID2==154| ///
						  clusterID2==167 | clusterID2==237 | clusterID2==242 | clusterID2==373 | clusterID2==378| ///
						  clusterID2==389 | clusterID2==417 | clusterID2==437 | clusterID2==497 | clusterID2==523| ///
						  clusterID2==527 |clusterID2==573 | clusterID2==595 | clusterID2==600 | clusterID2==780

	replace district3=8 if clusterID2==79 | clusterID2==114 |clusterID2==165 | clusterID2==188 | clusterID2==405 | clusterID2==794

	replace district3=9 if clusterID2==6 |clusterID2==18 | clusterID2==51 | clusterID2==80 | clusterID2==92 | clusterID2==126 | ///
						clusterID2==201 |clusterID2==264 |clusterID2==270 |clusterID2==272 | clusterID2==289 | clusterID2==382| ///
						clusterID2==420 | clusterID2==440 | clusterID2==462 | clusterID2==464 |clusterID2==499 |clusterID2==563| ///
						clusterID2==591 | clusterID2==674 | clusterID2==752 | clusterID2==781 | clusterID2==803 | clusterID2==838


	replace district3=10 if clusterID2==37| clusterID2==39 | clusterID2==319 | clusterID2==387 | clusterID2==445 | clusterID2==508| ///
						clusterID2==543 | clusterID2==554 | clusterID2==615 | clusterID2==676 | clusterID2==746
						
	replace district3=11 if clusterID2==4| clusterID2==168|clusterID2==230 | clusterID2==268|clusterID2==339| clusterID2==372| ///
						clusterID2==395 | clusterID2==451|clusterID2==506 | clusterID2==522| clusterID2==546| clusterID2==548| ///
						clusterID2==569 | clusterID2==588|clusterID2==629 | clusterID2==650|clusterID2==671 | clusterID2==711

						
	replace district3=12 if clusterID2==15 | clusterID2==486 | clusterID2==696 | clusterID2==111 | clusterID2==486| ///
					clusterID2==696
					
	replace district3=13 if clusterID2==2 | clusterID2==32 | clusterID2==49 | clusterID2==100 | clusterID2==102 | clusterID2==131| ///
					clusterID2==173 | clusterID2==183 | clusterID2==223 | clusterID2==228 | clusterID2==244 | clusterID2==283| ///
					clusterID2==285 | clusterID2==297 | clusterID2==315 |clusterID2==324 | clusterID2==538 | clusterID2==540| ///
					clusterID2==565 | clusterID2==616 | clusterID2==656 | clusterID2==688 | clusterID2==734 | clusterID2==772| ///
					clusterID2==822 | clusterID2==841

	replace district3=14 if clusterID2==89 | clusterID2==246 | clusterID2==252 | clusterID2==318 | clusterID2==394 | ///
					clusterID2==741 | clusterID2==791


	replace district3=15 if clusterID2==54 | clusterID2==56 | clusterID2==72 | clusterID2==123 | clusterID2==142| ///
				clusterID2==219 | clusterID2==225 | clusterID2==311 |  clusterID2==374 | clusterID2==409| ///
				clusterID2==448 | clusterID2==455 | clusterID2==475 | clusterID2==482 | clusterID2==488| ///
				clusterID2==490 | clusterID2==519 | clusterID2==607 | clusterID2==726 | clusterID2==728 | ///
				clusterID2==787 | clusterID2==819
				
	replace district3=16 if clusterID2==53 | clusterID2==312 | clusterID2==634 | clusterID2==655| ///
			clusterID2==698 | clusterID2==827

	replace district3=17 if clusterID2==28 | clusterID2==66 | clusterID2==85 | clusterID2==125| ///
			clusterID2==136 | clusterID2==150 | clusterID2==278 | clusterID2==287 | clusterID2==331| ///
			clusterID2==334 | clusterID2==353 | clusterID2==416 | clusterID2==436 | clusterID2==481| ///
			clusterID2==514 | clusterID2==586 | clusterID2==597 | clusterID2==665 | clusterID2==703| ///
			clusterID2==713 | clusterID2==747 | clusterID2==775 | clusterID2==806

	replace district3=18 if clusterID2==1 | clusterID2==224 | clusterID2==576| clusterID2==808

	replace district3=19 if clusterID2==13 | clusterID2==33 | clusterID2==34 | clusterID2==132| ///
			clusterID2==143 | clusterID2==153 | clusterID2==189| clusterID2==229|clusterID2==238| ///
			clusterID2==275 |clusterID2==366 |clusterID2==418 |clusterID2==469|clusterID2==498| ///
			clusterID2==510 |clusterID2==542 |clusterID2==552 | clusterID2==570 | clusterID2==592| ///
			clusterID2==618 | clusterID2==621 | clusterID2==639| clusterID2==644 | clusterID2==670| ///
			clusterID2==693 | clusterID2==725 | clusterID2==810 | clusterID2==836
			
	replace district3=20 if clusterID2==135 | clusterID2==247| clusterID2==292| clusterID2==399|clusterID2==536

	replace district3=21 if clusterID2==134 | clusterID2==170 | clusterID2==209 | clusterID2==258 | clusterID2==320| ///
			clusterID2==325| clusterID2==348 | clusterID2==358 | clusterID2==383 | clusterID2==429 | clusterID2==441| ///
			clusterID2==501 | clusterID2==602| clusterID2==611 | clusterID2==625 | clusterID2==666 | clusterID2==709| ///
			clusterID2==729 | clusterID2==770| clusterID2==778 | clusterID2==825 | clusterID2==826 | clusterID2==834
			
	replace district3=22 if clusterID2==110 | clusterID2==192 | clusterID2==578 | clusterID2==619 | clusterID2==738| ///
			clusterID2==812
			
	replace district3=23 if clusterID2==94 | clusterID2==147 | clusterID2==166 | clusterID2==231 | clusterID2==267| ///
			clusterID2==294 | clusterID2==329 | clusterID2==369| clusterID2==371|clusterID2==484 | clusterID2==512| ///
			clusterID2==517 | clusterID2==556 | clusterID2==562 | clusterID2==574| clusterID2==575| clusterID2==604| ///
			clusterID2==633 | clusterID2==667 | clusterID2==768 | clusterID2==769 | clusterID2==771 | clusterID2==807
			
	replace district3 =24 if clusterID2==104 | clusterID2==109 | clusterID2==279 | clusterID2==384 | clusterID2==431| ///
			clusterID2==530 | clusterID2==560 | clusterID2==630 | clusterID2==640 | clusterID2==659 | clusterID2==735| ///
			clusterID2==777 | clusterID2==815 | clusterID2==831


	replace district3=25 if clusterID2==5 | clusterID2==57 | clusterID2==117 | clusterID2==175 | clusterID2==263| ///
			clusterID2==271 | clusterID2==293 | clusterID2==341 | clusterID2==386 | clusterID2==435 | clusterID2==474| ///
			clusterID2==493 | clusterID2==516 | clusterID2==534 | clusterID2==549 | clusterID2==553 | clusterID2==572| ///
			clusterID2==668 | clusterID2==733 | clusterID2==736 | clusterID2==762 | clusterID2==764 | clusterID2==788| ///
			clusterID2==823 | clusterID2==835 | clusterID2==849

	 replace district3=26 if clusterID2== 38 | clusterID2==313 | clusterID2==433 | clusterID2==557 | clusterID2==642
	 
	 replace district3=27 if clusterID2==7 | clusterID2==63 | clusterID2==67 | clusterID2==137 | clusterID2==141| ///
				clusterID2==180 | clusterID2==182 |clusterID2==186 | clusterID2==208 |clusterID2==235 | clusterID2==245| ///
				clusterID2==249| clusterID2==254 | clusterID2==290 | clusterID2==291 | clusterID2==321 | clusterID2==375| ///
				clusterID2==412 | clusterID2==472 | clusterID2==491 |clusterID2==511 | clusterID2==590 | clusterID2==610| ///
				clusterID2==689 | clusterID2==697 |clusterID2==753 | clusterID2==761 | clusterID2==829 | clusterID2==845

	replace district3=28 if clusterID2==403 | clusterID2==479 | clusterID2==605 | clusterID2== 758 | clusterID2==828

	 replace district3=29 if clusterID2==14| clusterID2==55| clusterID2==71| clusterID2==81|clusterID2==130| ///
				clusterID2==164 | clusterID2==198 |clusterID2==206 |clusterID2==207|clusterID2==240|clusterID2==307| ///
				clusterID2==377 | clusterID2==402 | clusterID2==428| clusterID2==432| clusterID2==442| ///
				clusterID2==449 | clusterID2==465 | clusterID2==487| clusterID2==504| clusterID2==513| ///
				clusterID2==579 | clusterID2==601 |clusterID2==614 | clusterID2==658|clusterID2==721| ///
				clusterID2==731 | clusterID2==832

	 replace district3=30 if clusterID2==335 | clusterID2==411 | clusterID2==518 | clusterID2==811

	 replace district3=31 if clusterID2==23 | clusterID2==44 | clusterID2==50 | clusterID2==70| ///
				clusterID2==84| clusterID2==87 |clusterID2==152| clusterID2==197| clusterID2==214| ///
				clusterID2==284 | clusterID2==302 | clusterID2==304 | clusterID2==344| clusterID2==350| ///
				clusterID2==406 | clusterID2==413 | clusterID2==419 | clusterID2==463| clusterID2==467| ///
				clusterID2==470 | clusterID2==594 | clusterID2==627 | clusterID2==647| clusterID2==652| ///
				clusterID2==692 | clusterID2==793 | clusterID2==801 | clusterID2==840 | clusterID2==847


	 replace district3=32 if clusterID2==77 | clusterID2==234 | clusterID2==450 | clusterID2==495| ///
				clusterID2==500 | clusterID2==622

	 replace district3=33 if clusterID2==3| clusterID2==36 | clusterID2==73| clusterID2==115| ///
				clusterID2==129 | clusterID2==146| clusterID2==191| clusterID2==241| clusterID2==337| ///
				clusterID2==356 | clusterID2==359| clusterID2==408 | clusterID2==410 | clusterID2==414| ///
				clusterID2==422 | clusterID2==434| clusterID2==566 | clusterID2==581 | clusterID2==643| ///
				clusterID2==657 | clusterID2==678| clusterID2==690 | clusterID2==744 |clusterID2==749| ///
				clusterID2==766 | clusterID2==818 | clusterID2==824

	 replace district3=34 if clusterID2==99 | clusterID2==535 | clusterID2==577| clusterID2==617 | clusterID2==664

	 replace district3=35 if clusterID2==64 | clusterID2==68 | clusterID2==91 | clusterID2==119 | clusterID2==122| ///
				clusterID2==212 | clusterID2==213 | clusterID2==218| clusterID2==260| clusterID2==266| ///
				clusterID2==277 | clusterID2==317 | clusterID2==349 | clusterID2==351 | clusterID2==357| ///
				clusterID2==392 | clusterID2==453 | clusterID2==528 | clusterID2==596 | clusterID2==606| ///
				clusterID2==637 | clusterID2==662 | clusterID2==756 | clusterID2==814| clusterID2==842| clusterID2==843
				
	replace district3=36 if clusterID2==82| clusterID2==187 | clusterID2==308 | clusterID2==404 | clusterID2==447| ///
				clusterID2==539 | clusterID2==789| clusterID2==790| clusterID2==844

	replace district3=37 if clusterID2==26 | clusterID2==45 | clusterID2==65 | clusterID2==93| ///
				clusterID2==112| clusterID2==121 | clusterID2==161 | clusterID2==169 | clusterID2==193| ///
				clusterID2==220 | clusterID2==232 | clusterID2==257 | clusterID2==265 | clusterID2==322| ///
				clusterID2==330 | clusterID2==345 | clusterID2==362 | clusterID2==390 | clusterID2==407| ///
				clusterID2==515 | clusterID2==541 | clusterID2==593 | clusterID2==623 | clusterID2==680| ///
				clusterID2==750 | clusterID2==754 | clusterID2==805

	replace district3=38 if clusterID2==427 | clusterID2==719

	replace district3=39 if clusterID2==635 | clusterID2==714 | clusterID2==821 | clusterID2==88 | ///
			clusterID2==103 | clusterID2==145 | clusterID2==199 | clusterID2==233| ///
			clusterID2==233 | clusterID2==273 | clusterID2==327 | clusterID2==352| ///
			clusterID2==368 | clusterID2==379 | clusterID2==567 | clusterID2==609| clusterID2==620

	replace district3=40 if clusterID2==75 | clusterID2==116| clusterID2==157| clusterID2==181| ///
			clusterID2==190| clusterID2==211 | clusterID2==354| clusterID2==398 |clusterID2==524| ///
			clusterID2==529 | clusterID2==580 | clusterID2==587 | clusterID2==612 | clusterID2==661| ///
			clusterID2==683 | clusterID2==705 | clusterID2==718 | clusterID2==784| clusterID2==792

	replace district3=41 if clusterID2==24 | clusterID2==52| clusterID2==74| clusterID2==76| ///
			clusterID2==138 | clusterID2==239 | clusterID2==261 | clusterID2==300| clusterID2==423| ///
			clusterID2==457 | clusterID2==526 | clusterID2==702 | clusterID2==720 |clusterID2==737| ///
			clusterID2==745 | clusterID2==799 | clusterID2==802 | clusterID2==820 | clusterID2==848

					
	replace district3=42 if clusterID2==47| clusterID2==215| clusterID2==365| clusterID2==521| ///
			clusterID2==653| clusterID2==700| clusterID2==763

	replace district3=43 if clusterID2==11 | clusterID2==46| clusterID2==148| ///
			clusterID2==156| clusterID2==202 | clusterID2==306| clusterID2==323| ///
			clusterID2==326 | clusterID2==347 | clusterID2==376 | clusterID2==380| ///
			clusterID2==396 | clusterID2==425 |clusterID2==443 | clusterID2==444| ///
			clusterID2==452 |clusterID2==503 | clusterID2==520 | clusterID2==598| ///
			clusterID2==608 | clusterID2==645 | clusterID2==654 | clusterID2==660| ///
			clusterID2==694 | clusterID2==704 | clusterID2==706 | clusterID2==707| ///
			clusterID2==804 | clusterID2==833 | clusterID2==837

	replace district3=44 if clusterID2==60 | clusterID2==385| clusterID2==613| clusterID2==646

	replace district3=45 if clusterID2==21| clusterID2==22| clusterID2==25| clusterID2==90| ///
				clusterID2==96 | clusterID2==139| clusterID2==174| clusterID2==194 | clusterID2==210| ///
				clusterID2==276| clusterID2==305| clusterID2==336| clusterID2==363 |clusterID2==381| ///
				clusterID2==391 | clusterID2==477| clusterID2==533 | clusterID2==559 | clusterID2==564| ///
				clusterID2==628 | clusterID2==684 | clusterID2==710 | clusterID2==712 | clusterID2==717| ///
				clusterID2==723 | clusterID2==765| clusterID2==800| clusterID2==816 | clusterID2==839

				
	replace district3=46 if clusterID2==30 | clusterID2==632| clusterID2==686| clusterID2==796

	replace district3=47 if clusterID2==9 | clusterID2==19 | clusterID2==106 | clusterID2==107| ///
				clusterID2==151 | clusterID2==163 | clusterID2==171| clusterID2==203| clusterID2==221| ///
				clusterID2==236 | clusterID2==253 | clusterID2==269 | clusterID2==286 | clusterID2==310| ///
				clusterID2==328 | clusterID2==446 | clusterID2==466| clusterID2==468 | clusterID2==478| ///
				clusterID2==545 | clusterID2==551 | clusterID2==679 | clusterID2==687 |clusterID2==748| ///
				clusterID2==773 | clusterID2==783 | clusterID2==795

	replace district3=48 if clusterID2==401| clusterID2==483| clusterID2==502

	replace district3=49 if clusterID2==12| clusterID2==35| clusterID2==43| clusterID2==98| ///
				clusterID2==108| clusterID2==113| clusterID2==160| clusterID2==255| clusterID2==274| ///
				clusterID2==298| clusterID2==316 | clusterID2==340 | clusterID2==400| clusterID2==415| ///
				clusterID2==485|clusterID2==505| clusterID2==532| clusterID2==568| clusterID2==682| ///
				clusterID2==695|clusterID2==701|clusterID2==708| clusterID2==740| clusterID2==742| ///
				clusterID2==751|clusterID2==785| clusterID2==798

	replace district3=50 if clusterID2==41| clusterID2==127| clusterID2==296| clusterID2==360

	replace district3=51 if clusterID2==58| clusterID2==124| clusterID2==196| clusterID2==200| ///
				clusterID2==301| clusterID2==370| clusterID2==421| clusterID2==430| clusterID2==471| ///
				clusterID2==476| clusterID2==525| clusterID2==550| clusterID2==583| clusterID2==584| ///
				clusterID2==624| clusterID2==691| clusterID2==715|clusterID2==722|clusterID2==730| ///
				clusterID2==739| clusterID2==786

	replace district3=52 if clusterID2==16| clusterID2==133| clusterID2==217| clusterID2==638| ///
				clusterID2==681| clusterID2==755
				
	replace district3=53 if clusterID2==31| clusterID2==61| clusterID2==176| clusterID2==259| ///
				clusterID2==299| clusterID2==303| clusterID2==314| clusterID2==338| clusterID2==426| ///
				clusterID2==438|clusterID2==439|clusterID2==454|clusterID2==489|clusterID2==509| ///
				clusterID2==582| clusterID2==589| clusterID2==636| clusterID2==651| clusterID2==663| ///
				clusterID2==669| clusterID2==675|clusterID2==727|clusterID2==743| clusterID2==797

	replace district3=54 if clusterID2==155| clusterID2==262| clusterID2==496| clusterID2==544| ///
				clusterID2==724| clusterID2==732

	replace district3=55 if clusterID2==10| clusterID2==20| clusterID2==62| clusterID2==83| ///
				clusterID2==216| clusterID2==222 | clusterID2==251| clusterID2==256| clusterID2==281| ///
				clusterID2==282| clusterID2==288 |clusterID2==295 | clusterID2==333| clusterID2==346| ///
				clusterID2==367| clusterID2==456 | clusterID2==537| clusterID2==561|clusterID2==677| ///
				clusterID2==699| clusterID2==767 |clusterID2==774 | clusterID2==782

	replace district3=56 if clusterID2==195| clusterID2==355| clusterID2==757

	tab district3
	list clusterID2 hv021 hv023 district3 in 1/70


	//region definition
	replace region3=1 if district3==1| district3==2| district3==3|district3==4|district3==5| district3==6| ///
				district3==7| district3==8| district3==9| district3==10|district3==11|district3==12
				
	replace region3=2 if district3==13| district3==14|district3==15|district3==16|district3==17|district3==18| ///
				district3==19|district3==20|district3==21|district3==22| district3==23|district3==24|district3==25| ///
				district3==26| district3==27| district3==28| district3==29| district3==30
				
	replace region3=3 if district3==31| district3==32|district3==33|district3==34|district3==35|district3==36| ///
				district3==37|district3==38|district3==39|district3==40| district3==41|district3==42|district3==43| ///
				district3==44| district3==45| district3==46| district3==47| district3==48|district3==49|district3==50| ///
				district3==51|district3==52|district3==53|district3==54|district3==55| district3==56
				
	list clusterID2 hv021 hv023 district3 hv025 region3 in 1/70
				
	//area of residence
	replace residence3=1 if district3==2|district3==4|district3==6|district3==8|district3==10|district3==12|district3==14| ///
				district3==16|district3==18|district3==20|district3==22|district3==24|district3==26|district3==28| ///
				district3==30| district3==32| district3==34|district3==36|district3==38|district3==40|district3==42| ///
				district3==44| district3==46| district3==48|district3==50|district3==52|district3==54|district3==56
				
	replace residence3=2 if district3==1|district3==3|district3==5|district3==7|district3==9|district3==11|district3==13| ///
				district3==15|district3==17|district3==19|district3==21|district3==23|district3==25|district3==27| ///
				district3==29| district3==31| district3==33|district3==35|district3==37|district3==39|district3==41| ///
				district3==43| district3==45| district3==47|district3==49|district3==51|district3==53|district3==55			



	list hv021 clusterID2 hv023 district3 hv024 region3 hv025 residence3 in 1/70

	//replace the records that were not misclassified...
	replace clusterID2=hv021 if missing(clusterID2)
	list hv021 clusterID2 hv023 district3 hv024 region3 hv025 residence3 in 1/70
	replace district3=hv023 if (clusterID2==hv021 & missing(district3))
	list hv021 clusterID2 hv023 district3 hv024 region3 hv025 residence3 in 1/70
	replace region3=hv024 if (clusterID2==hv021 & missing(region3))
	list hv021 clusterID2 hv023 district3 hv024 region3 hv025 residence3 in 1/70
	replace residence3=hv025 if (clusterID2==hv021 & missing(residence3))
	list hv021 clusterID2 hv023 district3 hv024 region3 hv025 residence3 in 1/70
	
	
	//sample weights by cluster
gen v5=.
replace v5= 18371	if hv021==1
replace v5= 1274691	if hv021==2
replace v5= 1933111	if hv021==3
replace v5= 48145	if hv021==4
replace v5= 915341	if hv021==5
replace v5= 2005000	if hv021==6
replace v5= 1050938	if hv021==7
replace v5= 717797	if hv021==8
replace v5= 874265	if hv021==9
replace v5= 289617	if hv021==10
replace v5= 1327589	if hv021==11
replace v5= 922960	if hv021==12
replace v5= 1572140	if hv021==13
replace v5= 1085926	if hv021==14
replace v5= 17409	if hv021==15
replace v5= 204210	if hv021==16
replace v5=565023 if hv021==17
replace v5=1227678 if hv021==18
replace v5=813728 if hv021==19
replace v5=457690 if hv021==20
replace v5=1193037 if hv021==21
replace v5=954656 if hv021==22
replace v5=692154 if hv021==23
replace v5=316639 if hv021==24
replace v5=2018986 if hv021==25
replace v5=686584 if hv021==26
replace v5=486210 if hv021==27
replace v5=626269 if hv021==28
replace v5=436350 if hv021==29
replace v5=221976 if hv021==30
replace v5=776161 if hv021==31
replace v5=1182997 if hv021==32
replace v5=1049975 if hv021==33
replace v5=1491752 if hv021==34
replace v5=811989 if hv021==35
replace v5=1167772 if hv021==36
replace v5=786928 if hv021==37
replace v5=233712 if hv021==38
replace v5=815231 if hv021==39
replace v5=333877 if hv021==40
replace v5=220573 if hv021==41
replace v5=519019 if hv021==42
replace v5=833431 if hv021==43
replace v5=2749261 if hv021==44
replace v5=632623 if hv021==45
replace v5=1362054 if hv021==46
replace v5=145666 if hv021==47
replace v5=409768 if hv021==48
replace v5=1281626 if hv021==49
replace v5=1672151 if hv021==50
replace v5=2078842 if hv021==51
replace v5=245295 if hv021==52
replace v5=238561 if hv021==53
replace v5=754380 if hv021==54
replace v5=1145208 if hv021==55
replace v5=789708 if hv021==56
replace v5=869656 if hv021==57
replace v5=610651 if hv021==58
replace v5=143490 if hv021==59
replace v5=242293 if hv021==60
replace v5=790909 if hv021==61
replace v5=282472 if hv021==62
replace v5=1084150 if hv021==63
replace v5=1530563 if hv021==64
replace v5=668080 if hv021==65
replace v5=727190 if hv021==66
replace v5=856376 if hv021==67
replace v5=1519325 if hv021==68
replace v5=419202 if hv021==69
replace v5=1709335 if hv021==70
replace v5=1091500 if hv021==71
replace v5=462463 if hv021==72
replace v5=999239 if hv021==73
replace v5=244567 if hv021==74
replace v5=2461919 if hv021==75
replace v5=180983 if hv021==76
replace v5=454073 if hv021==77
replace v5=411936 if hv021==78
replace v5=141679 if hv021==79
replace v5=2378474 if hv021==80
replace v5=1002846 if hv021==81
replace v5=800101 if hv021==82
replace v5=268258 if hv021==83
replace v5=1762039 if hv021==84
replace v5=830671 if hv021==85
replace v5=348705 if hv021==86
replace v5=1789498 if hv021==87
replace v5=1456494 if hv021==88
replace v5=619943 if hv021==89
replace v5=989296 if hv021==90
replace v5=1774598 if hv021==91
replace v5=1601835 if hv021==92
replace v5=692642 if hv021==93
replace v5=3414646 if hv021==94
replace v5=371795 if hv021==95
replace v5=1042005 if hv021==96
replace v5=387392 if hv021==97
replace v5=899785 if hv021==98
replace v5=318682 if hv021==99
replace v5=1334087 if hv021==100
replace v5=453540 if hv021==101
replace v5=1524256 if hv021==102
replace v5=1634610 if hv021==103
replace v5=3100624 if hv021==104
replace v5=302795 if hv021==105
replace v5=956884 if hv021==106
replace v5=798317 if hv021==107
replace v5=1033979 if hv021==108
replace v5=3020632 if hv021==109
replace v5=301719 if hv021==110
replace v5=22831 if hv021==111
replace v5=659659 if hv021==112
replace v5=1132821 if hv021==113
replace v5=167479 if hv021==114
replace v5=1150466 if hv021==115
replace v5=2338478 if hv021==116
replace v5=879169 if hv021==117
replace v5=670310 if hv021==118
replace v5=1772377 if hv021==119
replace v5=661752 if hv021==120
replace v5=656619 if hv021==121
replace v5=1925434 if hv021==122
replace v5=721578 if hv021==123
replace v5=455465 if hv021==124
replace v5=893495 if hv021==125
replace v5=1587563 if hv021==126
replace v5=222686 if hv021==127
replace v5=691797 if hv021==128
replace v5=1129600 if hv021==129
replace v5=1079148 if hv021==130
replace v5=1076904 if hv021==131
replace v5=1393554 if hv021==132
replace v5=274829 if hv021==133
replace v5=854285 if hv021==134
replace v5=297080 if hv021==135
replace v5=669974 if hv021==136
replace v5=1159542 if hv021==137
replace v5=375038 if hv021==138
replace v5=1100710 if hv021==139
replace v5=357034 if hv021==140
replace v5=1663490 if hv021==141
replace v5=654625 if hv021==142
replace v5=1228053 if hv021==143
replace v5=372332 if hv021==144
replace v5=1174015 if hv021==145
replace v5=1156626 if hv021==146
replace v5=3154107 if hv021==147
replace v5=1158840 if hv021==148
replace v5=142340 if hv021==149
replace v5=611478 if hv021==150
replace v5=850014 if hv021==151
replace v5=1791219 if hv021==152
replace v5=1469504 if hv021==153
replace v5=404750 if hv021==154
replace v5=250332 if hv021==155
replace v5=1124652 if hv021==156
replace v5=2364321 if hv021==157
replace v5=514528 if hv021==158
replace v5=483685 if hv021==159
replace v5=950927 if hv021==160
replace v5=689131 if hv021==161
replace v5=184436 if hv021==162
replace v5=766810 if hv021==163
replace v5=1104302 if hv021==164
replace v5=270031 if hv021==165
replace v5=3166694 if hv021==166
replace v5=384312 if hv021==167
replace v5=22162 if hv021==168
replace v5=726100 if hv021==169
replace v5=844698 if hv021==170
replace v5=911970 if hv021==171
replace v5=423583 if hv021==172
replace v5=1009986 if hv021==173
replace v5=1119579 if hv021==174
replace v5=854155 if hv021==175
replace v5=755206 if hv021==176
replace v5=208489 if hv021==177
replace v5=716010 if hv021==178
replace v5=523268 if hv021==179
replace v5=1141496 if hv021==180
replace v5=2216545 if hv021==181
replace v5=1351585 if hv021==182
replace v5=1343672 if hv021==183
replace v5=503939 if hv021==184
replace v5=597127 if hv021==185
replace v5=901204 if hv021==186
replace v5=816819 if hv021==187
replace v5=156609 if hv021==188
replace v5=1592259 if hv021==189
replace v5=2392569 if hv021==190
replace v5=925272 if hv021==191
replace v5=320131 if hv021==192
replace v5=685792 if hv021==193
replace v5=1689201 if hv021==194
replace v5=65366 if hv021==195
replace v5=582190 if hv021==196
replace v5=1590036 if hv021==197
replace v5=1127856 if hv021==198
replace v5=1393326 if hv021==199
replace v5=1086293 if hv021==200
replace v5=1461447 if hv021==201
replace v5=2184804 if hv021==202
replace v5=762510 if hv021==203
replace v5=619712 if hv021==204
replace v5=436758 if hv021==205
replace v5=1078124 if hv021==206
replace v5=1049347 if hv021==207
replace v5=869279 if hv021==208
replace v5=954077 if hv021==209
replace v5=1447818 if hv021==210
replace v5=1948131 if hv021==211
replace v5=1422179 if hv021==212
replace v5=1503479 if hv021==213
replace v5=1700829 if hv021==214
replace v5=141671 if hv021==215
replace v5=360955 if hv021==216
replace v5=180329 if hv021==217
replace v5=1412321 if hv021==218
replace v5=430180 if hv021==219
replace v5=695606 if hv021==220
replace v5=850906 if hv021==221
replace v5=338717 if hv021==222
replace v5=1652173 if hv021==223
replace v5=116878 if hv021==224
replace v5=715192 if hv021==225
replace v5=128629 if hv021==226
replace v5=130562 if hv021==227
replace v5=1186541 if hv021==228
replace v5=1314390 if hv021==229
replace v5=21907 if hv021==230
replace v5=3307163 if hv021==231
replace v5=646478 if hv021==232
replace v5=1881377 if hv021==233
replace v5=414669 if hv021==234
replace v5=1507684 if hv021==235
replace v5=901088 if hv021==236
replace v5=397129 if hv021==237
replace v5=1035074 if hv021==238
replace v5=302099 if hv021==239
replace v5=1231722 if hv021==240
replace v5=1190552 if hv021==241
replace v5=386594 if hv021==242
replace v5=622774 if hv021==243
replace v5=1625620 if hv021==244
replace v5=1138876 if hv021==245
replace v5=229717 if hv021==246
replace v5=289974 if hv021==247
replace v5=656947 if hv021==248
replace v5=1599081 if hv021==249
replace v5=277252 if hv021==250
replace v5=339350 if hv021==251
replace v5=523017 if hv021==252
replace v5=980176 if hv021==253
replace v5=1064294 if hv021==254
replace v5=959605 if hv021==255
replace v5=278959 if hv021==256
replace v5=677650 if hv021==257
replace v5=989921 if hv021==258
replace v5=677703 if hv021==259
replace v5=1193265 if hv021==260
replace v5=255387 if hv021==261
replace v5=262213 if hv021==262
replace v5=1144773 if hv021==263
replace v5=1709227 if hv021==264
replace v5=690985 if hv021==265
replace v5=1465994 if hv021==266
replace v5=3859828 if hv021==267
replace v5=32861 if hv021==268
replace v5=714437 if hv021==269
replace v5=1360266 if hv021==270
replace v5=936975 if hv021==271
replace v5=1411033 if hv021==272
replace v5=1201522 if hv021==273
replace v5=758926 if hv021==274
replace v5=1123515 if hv021==275
replace v5=1222565 if hv021==276
replace v5=1767194 if hv021==277
replace v5=437654 if hv021==278
replace v5=3202867 if hv021==279
replace v5=347158 if hv021==280
replace v5=558738 if hv021==281
replace v5=297476 if hv021==282
replace v5=1326094 if hv021==283
replace v5=1767627 if hv021==284
replace v5=1242937 if hv021==285
replace v5=783877 if hv021==286
replace v5=673882 if hv021==287
replace v5=734341 if hv021==288
replace v5=1779893 if hv021==289
replace v5=1820402 if hv021==290
replace v5=1213065 if hv021==291
replace v5=311427 if hv021==292
replace v5=998086 if hv021==293
replace v5=3150325 if hv021==294
replace v5=287880 if hv021==295
replace v5=221670 if hv021==296
replace v5=1170202 if hv021==297
replace v5=872916 if hv021==298
replace v5=767634 if hv021==299
replace v5=249655 if hv021==300
replace v5=462887 if hv021==301
replace v5=1728399 if hv021==302
replace v5=756732 if hv021==303
replace v5=1758372 if hv021==304
replace v5=704841 if hv021==305
replace v5=1445286 if hv021==306
replace v5=1059466 if hv021==307
replace v5=601803 if hv021==308
replace v5=307562 if hv021==309
replace v5=900204 if hv021==310
replace v5=708621 if hv021==311
replace v5=256127 if hv021==312
replace v5=150298 if hv021==313
replace v5=791689 if hv021==314
replace v5=1056878 if hv021==315
replace v5=844425 if hv021==316
replace v5=1899338 if hv021==317
replace v5=410503 if hv021==318
replace v5=1367237 if hv021==319
replace v5=1120284 if hv021==320
replace v5=1441039 if hv021==321
replace v5=676293 if hv021==322
replace v5=1037539 if hv021==323
replace v5=1214544 if hv021==324
replace v5=642780 if hv021==325
replace v5=1123244 if hv021==326
replace v5=1274764 if hv021==327
replace v5=518307 if hv021==328
replace v5=3229652 if hv021==329
replace v5=817703 if hv021==330
replace v5=797400 if hv021==331
replace v5=378509 if hv021==332
replace v5=294550 if hv021==333
replace v5=556902 if hv021==334
replace v5=263394 if hv021==335
replace v5=1098692 if hv021==336
replace v5=1137167 if hv021==337
replace v5=397315 if hv021==338
replace v5=30314 if hv021==339
replace v5=946715 if hv021==340
replace v5=744683 if hv021==341
replace v5=360813 if hv021==342
replace v5=607458 if hv021==343
replace v5=1861174 if hv021==344
replace v5=700047 if hv021==345
replace v5=284384 if hv021==346
replace v5=1463877 if hv021==347
replace v5=960113 if hv021==348
replace v5=1846657 if hv021==349
replace v5=1646244 if hv021==350
replace v5=1480090 if hv021==351
replace v5=1675945 if hv021==352
replace v5=658088 if hv021==353
replace v5=2204631 if hv021==354
replace v5=39946 if hv021==355
replace v5=1082554 if hv021==356
replace v5=1305574 if hv021==357
replace v5=908751 if hv021==358
replace v5=1295740 if hv021==359
replace v5=225183 if hv021==360
replace v5=627181 if hv021==361
replace v5=714238 if hv021==362
replace v5=1102792 if hv021==363
replace v5=484026 if hv021==364
replace v5=145491 if hv021==365
replace v5=1318786 if hv021==366
replace v5=308022 if hv021==367
replace v5=1346039 if hv021==368
replace v5=4000999 if hv021==369
replace v5=945723 if hv021==370
replace v5=2973097 if hv021==371
replace v5=31333 if hv021==372
replace v5=423308 if hv021==373
replace v5=735260 if hv021==374
replace v5=1263457 if hv021==375
replace v5=993801 if hv021==376
replace v5=1020280 if hv021==377
replace v5=317073 if hv021==378
replace v5=1065974 if hv021==379
replace v5=1530668 if hv021==380
replace v5=1215505 if hv021==381
replace v5=851893 if hv021==382
replace v5=951239 if hv021==383
replace v5=3010751 if hv021==384
replace v5=234390 if hv021==385
replace v5=1068965 if hv021==386
replace v5=1111580 if hv021==387
replace v5=719723 if hv021==388
replace v5=428233 if hv021==389
replace v5=690875 if hv021==390
replace v5=1449070 if hv021==391
replace v5=1484999 if hv021==392
replace v5=402230 if hv021==393
replace v5=439082 if hv021==394
replace v5=22162 if hv021==395
replace v5=923307 if hv021==396
replace v5=414559 if hv021==397
replace v5=2323971 if hv021==398
replace v5=406124 if hv021==399
replace v5=1038416 if hv021==400
replace v5=134689 if hv021==401
replace v5=1120735 if hv021==402
replace v5=288045 if hv021==403
replace v5=1340774 if hv021==404
replace v5=205987 if hv021==405
replace v5=1532484 if hv021==406
replace v5=833055 if hv021==407
replace v5=993021 if hv021==408
replace v5=521250 if hv021==409
replace v5=1074313 if hv021==410
replace v5=265985 if hv021==411
replace v5=1254300 if hv021==412
replace v5=1922651 if hv021==413
replace v5=1134308 if hv021==414
replace v5=1065368 if hv021==415
replace v5=635617 if hv021==416
replace v5=305823 if hv021==417
replace v5=1510305 if hv021==418
replace v5=1877909 if hv021==419
replace v5=1379169 if hv021==420
replace v5=361266 if hv021==421
replace v5=1185243 if hv021==422
replace v5=251664 if hv021==423
replace v5=493955 if hv021==424
replace v5=1228700 if hv021==425
replace v5=1108986 if hv021==426
replace v5=97769 if hv021==427
replace v5=940276 if hv021==428
replace v5=1269149 if hv021==429
replace v5=631514 if hv021==430
replace v5=2438591 if hv021==431
replace v5=1053008 if hv021==432
replace v5=206243 if hv021==433
replace v5=1221757 if hv021==434
replace v5=713414 if hv021==435
replace v5=524117 if hv021==436
replace v5=478340 if hv021==437
replace v5=835364 if hv021==438
replace v5=900471 if hv021==439
replace v5=1621801 if hv021==440
replace v5=916052 if hv021==441
replace v5=1198586 if hv021==442
replace v5=1347677 if hv021==443
replace v5=1227637 if hv021==444
replace v5=676908 if hv021==445
replace v5=648177 if hv021==446
replace v5=522801 if hv021==447
replace v5=680454 if hv021==448
replace v5=1106847 if hv021==449
replace v5=418982 if hv021==450
replace v5=37956 if hv021==451
replace v5=1282902 if hv021==452
replace v5=1314024 if hv021==453
replace v5=799201 if hv021==454
replace v5=912800 if hv021==455
replace v5=292816 if hv021==456
replace v5=490204 if hv021==457
replace v5=466322 if hv021==458
replace v5=701679 if hv021==459
replace v5=555999 if hv021==460
replace v5=332432 if hv021==461
replace v5=1304092 if hv021==462
replace v5=1966347 if hv021==463
replace v5=1187026 if hv021==464
replace v5=1060298 if hv021==465
replace v5=771962 if hv021==466
replace v5=2143753 if hv021==467
replace v5=520984 if hv021==468
replace v5=1817069 if hv021==469
replace v5=1067126 if hv021==470
replace v5=656895 if hv021==471
replace v5=1319864 if hv021==472
replace v5=697248 if hv021==473
replace v5=927340 if hv021==474
replace v5=712267 if hv021==475
replace v5=518092 if hv021==476
replace v5=1234632 if hv021==477
replace v5=822070 if hv021==478
replace v5=176337 if hv021==479
replace v5=414377 if hv021==480
replace v5=100647 if hv021==481
replace v5=406649 if hv021==482
replace v5=135432 if hv021==483
replace v5=3676972 if hv021==484
replace v5=941687 if hv021==485
replace v5=20263 if hv021==486
replace v5=1116192 if hv021==487
replace v5=688452 if hv021==488
replace v5=645000 if hv021==489
replace v5=682307 if hv021==490
replace v5=1945063 if hv021==491
replace v5=256705 if hv021==492
replace v5=1010397 if hv021==493
replace v5=391170 if hv021==494
replace v5=478816 if hv021==495
replace v5=333939 if hv021==496
replace v5=559782 if hv021==497
replace v5=1239904 if hv021==498
replace v5=2389242 if hv021==499
replace v5=487069 if hv021==500
replace v5=1056605 if hv021==501
replace v5=79971 if hv021==502
replace v5=1400950 if hv021==503
replace v5=1056111 if hv021==504
replace v5=825978 if hv021==505
replace v5=21143 if hv021==506
replace v5=409360 if hv021==507
replace v5=913690 if hv021==508
replace v5=806546 if hv021==509
replace v5=1196838 if hv021==510
replace v5=1176325 if hv021==511
replace v5=3624825 if hv021==512
replace v5=1084582 if hv021==513
replace v5=437878 if hv021==514
replace v5=672852 if hv021==515
replace v5=935265 if hv021==516
replace v5=3094665 if hv021==517
replace v5=195675 if hv021==518
replace v5=1145214 if hv021==519
replace v5=1157730 if hv021==520
replace v5=221986 if hv021==521
replace v5=38975 if hv021==522
replace v5=403772 if hv021==523
replace v5=2217450 if hv021==524
replace v5=470512 if hv021==525
replace v5=290567 if hv021==526
replace v5=361328 if hv021==527
replace v5=1304810 if hv021==528
replace v5=2366540 if hv021==529
replace v5=3774896 if hv021==530
replace v5=462170 if hv021==531
replace v5=688449 if hv021==532
replace v5=1168997 if hv021==533
replace v5=946920 if hv021==534
replace v5=307839 if hv021==535
replace v5=344530 if hv021==536
replace v5=179296 if hv021==537
replace v5=1183893 if hv021==538
replace v5=686074 if hv021==539
replace v5=936185 if hv021==540
replace v5=625284 if hv021==541
replace v5=1336168 if hv021==542
replace v5=1472070 if hv021==543
replace v5=255825 if hv021==544
replace v5=730038 if hv021==545
replace v5=32606 if hv021==546
replace v5=379740 if hv021==547
replace v5=32097 if hv021==548
replace v5=1031370 if hv021==549
replace v5=588760 if hv021==550
replace v5=757157 if hv021==551
replace v5=1407422 if hv021==552
replace v5=879716 if hv021==553
replace v5=1023356 if hv021==554
replace v5=472737 if hv021==555
replace v5=3172588 if hv021==556
replace v5=245774 if hv021==557
replace v5=725896 if hv021==558
replace v5=1484800 if hv021==559
replace v5=3560485 if hv021==560
replace v5=489879 if hv021==561
replace v5=6433046 if hv021==562
replace v5=1458886 if hv021==563
replace v5=1218211 if hv021==564
replace v5=1201197 if hv021==565
replace v5=942284 if hv021==566
replace v5=1405728 if hv021==567
replace v5=1018206 if hv021==568
replace v5=36682 if hv021==569
replace v5=1326256 if hv021==570
replace v5=360653 if hv021==571
replace v5=381216 if hv021==572
replace v5=386431 if hv021==573
replace v5=3054906 if hv021==574
replace v5=3767280 if hv021==575
replace v5=112597 if hv021==576
replace v5=351083 if hv021==577
replace v5=283720 if hv021==578
replace v5=1070550 if hv021==579
replace v5=2381964 if hv021==580
replace v5=581882 if hv021==581
replace v5=750365 if hv021==582
replace v5=587044 if hv021==583
replace v5=515498 if hv021==584
replace v5=658738 if hv021==585
replace v5=544116 if hv021==586
replace v5=2526270 if hv021==587
replace v5=21907 if hv021==588
replace v5=945339 if hv021==589
replace v5=1264033 if hv021==590
replace v5=1437111 if hv021==591
replace v5=1147819 if hv021==592
replace v5=767857 if hv021==593
replace v5=1788713 if hv021==594
replace v5=469125 if hv021==595
replace v5=1351253 if hv021==596
replace v5=772156 if hv021==597
replace v5=1223889 if hv021==598
replace v5=588371 if hv021==599
replace v5=294257 if hv021==600
replace v5=1080042 if hv021==601
replace v5=1031936 if hv021==602
replace v5=805016 if hv021==603
replace v5=3236226 if hv021==604
replace v5=199036 if hv021==605
replace v5=1504365 if hv021==606
replace v5=730445 if hv021==607
replace v5=1241656 if hv021==608
replace v5=1503246 if hv021==609
replace v5=1358052 if hv021==610
replace v5=817824 if hv021==611
replace v5=1614384 if hv021==612
replace v5=279147 if hv021==613
replace v5=1045706 if hv021==614
replace v5=862827 if hv021==615
replace v5=1373504 if hv021==616
replace v5=405002 if hv021==617
replace v5=1305234 if hv021==618
replace v5=288577 if hv021==619
replace v5=1294974 if hv021==620
replace v5=1297215 if hv021==621
replace v5=477802 if hv021==622
replace v5=681648 if hv021==623
replace v5=747447 if hv021==624
replace v5=876280 if hv021==625
replace v5=419865 if hv021==626
replace v5=1453524 if hv021==627
replace v5=1636458 if hv021==628
replace v5=33116 if hv021==629
replace v5=3637607 if hv021==630
replace v5=608766 if hv021==631
replace v5=273818 if hv021==632
replace v5=3346408 if hv021==633
replace v5=258946 if hv021==634
replace v5=1163672 if hv021==635
replace v5=786407 if hv021==636
replace v5=2183972 if hv021==637
replace v5=314135 if hv021==638
replace v5=1153880 if hv021==639
replace v5=3600753 if hv021==640
replace v5=606339 if hv021==641
replace v5=202143 if hv021==642
replace v5=1111081 if hv021==643
replace v5=1240143 if hv021==644
replace v5=1313441 if hv021==645
replace v5=355287 if hv021==646
replace v5=1735981 if hv021==647
replace v5=650481 if hv021==648
replace v5=356170 if hv021==649
replace v5=21653 if hv021==650
replace v5=894877 if hv021==651
replace v5=1802485 if hv021==652
replace v5=138114 if hv021==653
replace v5=1539399 if hv021==654
replace v5=275501 if hv021==655
replace v5=1133810 if hv021==656
replace v5=1041643 if hv021==657
replace v5=2226343 if hv021==658
replace v5=2847411 if hv021==659
replace v5=1934575 if hv021==660
replace v5=2367461 if hv021==661
replace v5=1746318 if hv021==662
replace v5=788524 if hv021==663
replace v5=275599 if hv021==664
replace v5=552794 if hv021==665
replace v5=921665 if hv021==666
replace v5=3776693 if hv021==667
replace v5=785726 if hv021==668
replace v5=809097 if hv021==669
replace v5=1298991 if hv021==670
replace v5=31588 if hv021==671
replace v5=689788 if hv021==672
replace v5=304125 if hv021==673
replace v5=1220877 if hv021==674
replace v5=952671 if hv021==675
replace v5=916925 if hv021==676
replace v5=286721 if hv021==677
replace v5=978386 if hv021==678
replace v5=745329 if hv021==679
replace v5=685647 if hv021==680
replace v5=184034 if hv021==681
replace v5=1524296 if hv021==682
replace v5=2301383 if hv021==683
replace v5=1451004 if hv021==684
replace v5=707860 if hv021==685
replace v5=217986 if hv021==686
replace v5=787064 if hv021==687
replace v5=1191386 if hv021==688
replace v5=1131598 if hv021==689
replace v5=1294274 if hv021==690
replace v5=576905 if hv021==691
replace v5=1736956 if hv021==692
replace v5=890254 if hv021==693
replace v5=1506398 if hv021==694
replace v5=1019867 if hv021==695
replace v5=13128 if hv021==696
replace v5=1390436 if hv021==697
replace v5=250472 if hv021==698
replace v5=390351 if hv021==699
replace v5=158110 if hv021==700
replace v5=918849 if hv021==701
replace v5=252875 if hv021==702
replace v5=696766 if hv021==703
replace v5=1179133 if hv021==704
replace v5=2307111 if hv021==705
replace v5=1150848 if hv021==706
replace v5=1269030 if hv021==707
replace v5=876726 if hv021==708
replace v5=926472 if hv021==709
replace v5=1366266 if hv021==710
replace v5=21653 if hv021==711
replace v5=1690086 if hv021==712
replace v5=482598 if hv021==713
replace v5=1223416 if hv021==714
replace v5=426910 if hv021==715
replace v5=326477 if hv021==716
replace v5=1390447 if hv021==717
replace v5=2155398 if hv021==718
replace v5=48326 if hv021==719
replace v5=246354 if hv021==720
replace v5=1025247 if hv021==721
replace v5=596356 if hv021==722
replace v5=1476094 if hv021==723
replace v5=238592 if hv021==724
replace v5=1334217 if hv021==725
replace v5=715291 if hv021==726
replace v5=828344 if hv021==727
replace v5=689829 if hv021==728
replace v5=915265 if hv021==729
replace v5=602460 if hv021==730
replace v5=1073148 if hv021==731
replace v5=238259 if hv021==732
replace v5=1211716 if hv021==733
replace v5=1215207 if hv021==734
replace v5=3341071 if hv021==735
replace v5=806275 if hv021==736
replace v5=217472 if hv021==737
replace v5=359280 if hv021==738
replace v5=567513 if hv021==739
replace v5=1191504 if hv021==740
replace v5=550757 if hv021==741
replace v5=887474 if hv021==742
replace v5=854880 if hv021==743
replace v5=1117493 if hv021==744
replace v5=262101 if hv021==745
replace v5=1175146 if hv021==746
replace v5=671884 if hv021==747
replace v5=786224 if hv021==748
replace v5=959485 if hv021==749
replace v5=672852 if hv021==750
replace v5=989266 if hv021==751
replace v5=1366541 if hv021==752
replace v5=1472718 if hv021==753
replace v5=690537 if hv021==754
replace v5=192210 if hv021==755
replace v5=1659530 if hv021==756
replace v5=67321 if hv021==757
replace v5=239127 if hv021==758
replace v5=187613 if hv021==759
replace v5=440558 if hv021==760
replace v5=1356561 if hv021==761
replace v5=946135 if hv021==762
replace v5=183914 if hv021==763
replace v5=978458 if hv021==764
replace v5=1139034 if hv021==765
replace v5=910217 if hv021==766
replace v5=436857 if hv021==767
replace v5=3204813 if hv021==768
replace v5=3682977 if hv021==769
replace v5=819781 if hv021==770
replace v5=3395748 if hv021==771
replace v5=1486956 if hv021==772
replace v5=959503 if hv021==773
replace v5=376725 if hv021==774
replace v5=671876 if hv021==775
replace v5=1019376 if hv021==776
replace v5=3098902 if hv021==777
replace v5=1269794 if hv021==778
replace v5=405955 if hv021==779
replace v5=397003 if hv021==780
replace v5=2145375 if hv021==781
replace v5=278567 if hv021==782
replace v5=799381 if hv021==783
replace v5=2560687 if hv021==784
replace v5=825697 if hv021==785
replace v5=497030 if hv021==786
replace v5=725218 if hv021==787
replace v5=838639 if hv021==788
replace v5=555634 if hv021==789
replace v5=723551 if hv021==790
replace v5=377213 if hv021==791
replace v5=2694818 if hv021==792
replace v5=1837578 if hv021==793
replace v5=147216 if hv021==794
replace v5=859369 if hv021==795
replace v5=246685 if hv021==796
replace v5=843058 if hv021==797
replace v5=1027872 if hv021==798
replace v5=243302 if hv021==799
replace v5=2616253 if hv021==800
replace v5=1674067 if hv021==801
replace v5=295172 if hv021==802
replace v5=2213678 if hv021==803
replace v5=1298985 if hv021==804
replace v5=678703 if hv021==805
replace v5=1291275 if hv021==806
replace v5=3942053 if hv021==807
replace v5=196763 if hv021==808
replace v5=103122 if hv021==809
replace v5=648488 if hv021==810
replace v5=264239 if hv021==811
replace v5=311215 if hv021==812
replace v5=373026 if hv021==813
replace v5=1482240 if hv021==814
replace v5=3432339 if hv021==815
replace v5=988916 if hv021==816
replace v5=333139 if hv021==817
replace v5=1161015 if hv021==818
replace v5=680489 if hv021==819
replace v5=251713 if hv021==820
replace v5=1747066 if hv021==821
replace v5=795742 if hv021==822
replace v5=831181 if hv021==823
replace v5=1191462 if hv021==824
replace v5=846478 if hv021==825
replace v5=846358 if hv021==826
replace v5=246601 if hv021==827
replace v5=302475 if hv021==828
replace v5=1554700 if hv021==829
replace v5=670077 if hv021==830
replace v5=3357552 if hv021==831
replace v5=1062962 if hv021==832
replace v5=1528686 if hv021==833
replace v5=1035214 if hv021==834
replace v5=896895 if hv021==835
replace v5=695662 if hv021==836
replace v5=1325465 if hv021==837
replace v5=1123639 if hv021==838
replace v5=1088360 if hv021==839
replace v5=1744686 if hv021==840
replace v5=1228050 if hv021==841
replace v5=1760771 if hv021==842
replace v5=1256489 if hv021==843
replace v5=1326684 if hv021==844
replace v5=978121 if hv021== 845
replace v5=381618 if hv021== 846
replace v5=1695564 if hv021==847
replace v5=256020 if hv021==848
replace v5=837739 if hv021==849
replace v5=365859 if hv021==850
	
// calcuate nutrition indices
//simple sample weighting using DHS for clusters only, no strata; must multiply by 1000000
gen weight=v5/1000000
svyset [pweight=weight], psu(clusterID2)

//hw15 is lying (1) and standing (2) height measure; clean DHS code
gen newm=hc15 if hc15!=9
//hw3 is height, clean DHS code and converts to cm
gen newh=hc3/10 if hc3!=9999
//hw3 is height in kg to one decimal w/o the decimal; convert to kg w/decimal; clean DHS code and
gen neww=hc2/10 if hc2!=999
//run zscore06. age in months (hw1) and gender (b4) need no cleaning in this dataset
zscore06, a(hc1) s(hc27) h(newh) w(neww) measure(newm) male(1) female(2)

//remove biologically implausible scores
replace haz06=. if haz06<-6 | haz06>6
replace waz06=. if waz06<-6 | waz06>5
replace whz06=. if whz06<-5 | whz06>5
replace bmiz06=. if bmiz06<-5 | bmiz06>5

//example using svy: mean of height-for-age
svy: mean haz06
svy, over(region3): mean haz06
estat effects 
loneway haz06 clusterID2 if hv024==1
loneway haz06 clusterID2 if hv024==2
loneway haz06 clusterID2 if hv024==3

//Analyse merged dataset

//cd "C:\Users\Lenovo\Documents\Chaplain\Documents\Personal\Rowland\MW_2015-16_DHS_12112017_1140_113119\mwpr7hdt"
cd "C:\Users\user\Documents\Personal\Rowland\MW_2015-16_DHS_12112017_1140_113119\mwpr7hdt"
use merged_dataset_27may19, clear
br

tabstat haz rhaz1 rhaz2 rhaz3 if level==0| level==15|level==30, stat(mean sd semean)by(level)
tabstat waz rwaz1 rwaz2 rwaz3 if level==0| level==15|level==30, stat(mean sd semean)by(level)
tabstat whz rwhz1 rwhz2 rwhz3 if level==0| level==15|level==30, stat(mean sd semean)by(level)





rename haz malawi_haz
rename rhaz1 north_haz
rename rhaz2 central_haz
rename rhaz3 south_haz
tabstat malawi_haz north_haz central_haz south_haz if level==0| level==15|level==30, stat(mean sd semean)by(level)


tabstat stunted_rural stunted_urban wasted_rural wasted_urban underweight_rural underweight_urban  if level==0| level==15|level==30, stat(mean sd semean)by(level)


//Regional Results
bysort level: sum rhaz1
bysort level: sum rhaz2
bysort level: sum rhaz3

bysort level: sum rwaz1
bysort level: sum rwaz2
bysort level: sum rwaz3

bysort level: sum rwhz1
bysort level: sum rwhz2
bysort level: sum rwhz3

//location...
bysort level: sum lhaz1
bysort level: sum lhaz2

bysort level: sum lwaz1
bysort level: sum lwaz2

bysort level: sum lwhz1
bysort level: sum lwhz2

//median results - stunted, wasted, underweight
bysort level: sum stunted_north
bysort level: sum stunted_central
bysort level: sum stunted_south

bysort level: sum wasted_north
bysort level: sum wasted_central
bysort level: sum wasted_south

bysort level: sum underweight_north
bysort level: sum underweight_central
bysort level: sum underweight_south

bysort level: sum stunted_rural
bysort level: sum stunted_urban

bysort level: sum wasted_rural
bysort level: sum wasted_urban

bysort level: sum underweight_rural
bysort level: sum underweight_urban

//deft
bysort level: sum r1_deft
bysort level: sum r2_deft
bysort level: sum r3_deft

tabstat r1_deft r2_deft r3_deft, stat(mean) by(level)
tabstat r1waz_deft r2waz_deft r3waz_deft, stat(mean) by(level)
tabstat r1whz_deft r2whz_deft r3whz_deft, stat(mean) by(level)

tabstat l1_deft l2_deft, stat(mean) by(level)
tabstat l1waz_deft l2waz_deft, stat(mean) by(level)
tabstat l1whz_deft l2whz_deft, stat(mean) by(level)

//ICC
tabstat rhohaz1 rhohaz2 rhohaz3, stat(mean) by(level)
tabstat rhowaz1 rhowaz2 rhowaz3, stat(mean) by(level)
tabstat rhowhz1 rhowhz2 rhowhz3, stat(mean) by(level)

tabstat lrhohaz1 lrhohaz2, stat(mean) by(level)
tabstat lrhowaz1 lrhowaz2, stat(mean) by(level)
tabstat lrhowhz1 lrhowhz2, stat(mean) by(level)

//connected scatter plots with error bars
//HAZ-regional
gen HAZ_North=.
replace HAZ_North=north_haz
twoway (scatter HAZ_North level, connect(l) sort)||qfitci north_haz level, name(haznorth) title("HAZ-North")
twoway (scatter level HAZ_North, connect(l) sort)||qfitci level rhaz1, name(haznorth1) title("HAZ-North")



gen HAZ_Centre=.
replace HAZ_Centre=rhaz2
twoway (scatter HAZ_Centre level, connect(l) sort)||qfitci rhaz2 level, name(hazcentre) title("HAZ-Centre")

gen HAZ_South=.
replace HAZ_South=rhaz3
twoway (scatter HAZ_South level, connect(l) sort)||qfitci rhaz3 level, name(hazsouth) title("HAZ-South")

graph combine haznorth hazcentre hazsouth, title("Height-for-Age Z-Scores (HAZ) by level of misclassification")
 
//HAZ- location
gen HAZ_Urban=.
replace HAZ_Urban=lhaz1
twoway (scatter HAZ_Urban level, connect(l) sort)||qfitci lhaz1 level, name(hazurban) title("HAZ-Urban")

gen HAZ_Rural=.
replace HAZ_Rural=lhaz2
twoway (scatter HAZ_Rural level, connect(l) sort)||qfitci lhaz2 level, name(hazrural) title("HAZ-Rural")

graph combine hazurban hazrural, title("Height-for-Age Z-Scores (HAZ) by level of misclassification")



//WAZ-regional
gen WAZ_North=.
replace WAZ_North=rwaz1
twoway (scatter WAZ_North level, connect(l) sort)||qfitci rwaz1 level, name(waznorth) title("WAZ-North")

gen WAZ_Centre=.
replace WAZ_Centre=rwaz2
twoway (scatter WAZ_Centre level, connect(l) sort)||qfitci rwaz2 level, name(wazcentre) title("WAZ-Centre")

gen WAZ_South=.
replace WAZ_South=rwaz3
twoway (scatter WAZ_South level, connect(l) sort)||qfitci rwaz3 level, name(wazsouth) title("WAZ-South")

graph combine waznorth wazcentre wazsouth, title("Weight-for-Age Z-Scores (WAZ) by level of misclassification")
 
//HAZ- location
gen WAZ_Urban=.
replace WAZ_Urban=lwaz1
twoway (scatter WAZ_Urban level, connect(l) sort)||qfitci lwaz1 level, name(wazurban) title("WAZ-Urban")

gen WAZ_Rural=.
replace WAZ_Rural=lwaz2
twoway (scatter WAZ_Rural level, connect(l) sort)||qfitci lwaz2 level, name(wazrural) title("WAZ-Rural")

graph combine wazurban wazrural, title("Weight-for-Age Z-Scores (WAZ) by level of misclassification")
 
//WHZ-regional
gen WHZ_North=.
replace WHZ_North=rwhz1
twoway (scatter WHZ_North level, connect(l) sort)||qfitci rwhz1 level, name(whznorth) title("WHZ-North")

gen WHZ_Centre=.
replace WHZ_Centre=rwhz2
twoway (scatter WHZ_Centre level, connect(l) sort)||qfitci rwhz2 level, name(whzcentre) title("WHZ-Centre")

gen WHZ_South=.
replace WHZ_South=rwhz3
twoway (scatter WHZ_South level, connect(l) sort)||qfitci rwhz3 level, name(whzsouth) title("WHZ-South")

graph combine whznorth whzcentre whzsouth, title("Weight-for-Height Z-Scores (WHZ) by level of misclassification")
 
//HAZ- location
gen WHZ_Urban=.
replace WHZ_Urban=lwhz1
twoway (scatter WHZ_Urban level, connect(l) sort)||qfitci lwhz1 level, name(whzurban) title("WHZ-Urban")

gen WHZ_Rural=.
replace WHZ_Rural=lwhz2
twoway (scatter WHZ_Rural level, connect(l) sort)||qfitci lwhz2 level, name(whzrural) title("WHZ-Rural")

graph combine whzurban whzrural, title("Weight-for-Height Z-Scores (WHZ) by level of misclassification")
 

 //stunted-regional
 gen stunting_north=.
 replace stunting_north=stunted_north * 100
twoway (scatter stunting_nort level, connect(l) sort)||qfitci stunting_nort level, name(stuntingnorth) title("Stunting-North")

 gen stunting_central=.
 replace stunting_central=stunted_central * 100
twoway (scatter stunting_central level, connect(l) sort)||qfitci stunting_central level, name(stuntingcentre) title("Stunting-Centre")

 gen stunting_south=.
 replace stunting_south=stunted_south * 100
twoway (scatter stunting_south level, connect(l) sort)||qfitci stunting_south level, name(stuntingsouth) title("Stunting-South")

graph combine stuntingnorth stuntingcentre stuntingsouth, title("Stunting in Children by level of misclassification")
 
//stunted- location
gen stunting_rural=.
replace stunting_rural=stunted_rural*100
twoway (scatter stunting_rural level, connect(l) sort)||qfitci stunting_rural level, name(stuntingrural2) title("Stunting-Rural")

gen stunting_urban=.
replace stunting_urban=stunted_urban*100
twoway (scatter stunting_urban level, connect(l) sort)||qfitci stunting_urban level, name(stuntingurban2) title("Stunting-Urban")

graph combine stuntingrural2 stuntingurban2, title("Stunting in Children by level of misclassification")


 //wasted-regional
gen wasting_north=.
replace wasting_north=wasted_north*100
twoway (scatter wasting_north level, connect(l) sort)||qfitci wasting_north level, name(wastingnorth) title("Wasting-North")

gen wasting_central=.
replace wasting_central=wasted_central*100
twoway (scatter wasting_central level, connect(l) sort)||qfitci wasting_central level, name(wastingcentre) title("Wasting-Centre")

gen wasting_south=.
replace wasting_south=wasted_south*100
twoway (scatter wasting_south level, connect(l) sort)||qfitci wasting_south level, name(wastingsouth) title("Wasting-South")

graph combine wastingnorth wastingcentre wastingsouth, title("Wasting in Children by level of misclassification")
 
//stunted- location
gen wasting_rural=.
replace wasting_rural=wasted_rural*100
twoway (scatter wasting_rural level, connect(l) sort)||qfitci wasting_rural level, name(wastingrural) title("Wasting-Rural")

gen wasting_urban=.
replace wasting_urban=wasted_urban*100
twoway (scatter wasting_urban level, connect(l) sort)||qfitci wasting_urban level, name(wastingurban) title("Wasting-Urban")

graph combine wastingrural wastingurban, title("Wasting in Children by level of misclassification")


 //underweight-regional
gen underwt_north=.
replace underwt=underweight_north*100
twoway (scatter underwt_north level, connect(l) sort)||qfitci underwt_north level, name(underweightnorth) title("Underweight-North")

gen underwt_central=.
replace underwt_central=underweight_central*100
twoway (scatter underwt_central level, connect(l) sort)||qfitci underwt_central level, name(underweightcentre) title("Underweight-Centre")

gen underwt_south=.
replace underwt_south=underweight_south*100
twoway (scatter underwt_south level, connect(l) sort)||qfitci underwt_south level, name(underweightsouth) title("Underweight-South")

graph combine underweightnorth underweightcentre underweightsouth, title("Underweight Children by level of misclassification")
 
//underweight- location
gen underwt_rural=.
replace underwt_rural=underweight_rural*100
twoway (scatter underwt_rural level, connect(l) sort)||qfitci underwt_rural level, name(underweightrural) title("Underweight-Rural")

gen underwt_urban=.
replace underwt_urban=underweight_urban*100
twoway (scatter underwt_urban level, connect(l) sort)||qfitci underwt_urban level, name(underweighturban) title("Underweight-Urban")

graph combine underweightrural underweighturban, title("Underweight Children by level of misclassification")

 
//effect size
drop if level==5 | level==10|level==15
esize twosample rhaz1, by(level) cohensd hedgesg glassdelta
esize twosample rhaz2, by(level) cohensd hedgesg glassdelta
esize twosample rhaz3, by(level) cohensd hedgesg glassdelta
esize twosample lhaz1, by(level) cohensd hedgesg glassdelta
esize twosample lhaz2, by(level) cohensd hedgesg glassdelta
esize twosample rwaz1, by(level) cohensd hedgesg glassdelta
esize twosample rwaz2, by(level) cohensd hedgesg glassdelta
esize twosample rwaz3, by(level) cohensd hedgesg glassdelta
esize twosample lwaz1, by(level) cohensd hedgesg glassdelta
esize twosample lwaz2, by(level) cohensd hedgesg glassdelta
esize twosample rwhz1, by(level) cohensd hedgesg glassdelta
esize twosample rwhz2, by(level) cohensd hedgesg glassdelta
esize twosample rwhz3, by(level) cohensd hedgesg glassdelta
esize twosample lwhz1, by(level) cohensd hedgesg glassdelta
esize twosample lwhz2, by(level) cohensd hedgesg glassdelta
esize twosample stunted_north, by(level) cohensd hedgesg glassdelta
esize twosample stunted_central, by(level) cohensd hedgesg glassdelta
esize twosample stunted_south, by(level) cohensd hedgesg glassdelta
esize twosample stunted_rural, by(level) cohensd hedgesg glassdelta
esize twosample stunted_urban, by(level) cohensd hedgesg glassdelta
esize twosample wasted_north, by(level) cohensd hedgesg glassdelta
esize twosample wasted_central, by(level) cohensd hedgesg glassdelta
esize twosample wasted_south, by(level) cohensd hedgesg glassdelta
esize twosample wasted_rural, by(level) cohensd hedgesg glassdelta
esize twosample wasted_south, by(level) cohensd hedgesg glassdelta
esize twosample underweight_north, by(level) cohensd hedgesg glassdelta
esize twosample underweight_central, by(level) cohensd hedgesg glassdelta
esize twosample underweight_south, by(level) cohensd hedgesg glassdelta
esize twosample underweight_rural, by(level) cohensd hedgesg glassdelta
esize twosample underweight_urban, by(level) cohensd hedgesg glassdelta

//effect size for level zero and level 5
cd "C:\Users\Lenovo\Documents\Chaplain\Documents\Personal\Rowland\MW_2015-16_DHS_12112017_1140_113119\mwpr7hdt"
use merged_dataset, clear
drop if level==1 | level==10|level==15 | level==20 | level==25 | level==30
esize twosample rhaz1, by(level) cohensd hedgesg glassdelta
esize twosample rhaz2, by(level) cohensd hedgesg glassdelta
esize twosample rhaz3, by(level) cohensd hedgesg glassdelta
esize twosample lhaz1, by(level) cohensd hedgesg glassdelta
esize twosample lhaz2, by(level) cohensd hedgesg glassdelta
esize twosample rwaz1, by(level) cohensd hedgesg glassdelta
esize twosample rwaz2, by(level) cohensd hedgesg glassdelta
esize twosample rwaz3, by(level) cohensd hedgesg glassdelta
esize twosample lwaz1, by(level) cohensd hedgesg glassdelta
esize twosample lwaz2, by(level) cohensd hedgesg glassdelta
esize twosample rwhz1, by(level) cohensd hedgesg glassdelta
esize twosample rwhz2, by(level) cohensd hedgesg glassdelta
esize twosample rwhz3, by(level) cohensd hedgesg glassdelta
esize twosample lwhz1, by(level) cohensd hedgesg glassdelta
esize twosample lwhz2, by(level) cohensd hedgesg glassdelta
esize twosample stunted_north, by(level) cohensd hedgesg glassdelta
esize twosample stunted_central, by(level) cohensd hedgesg glassdelta
esize twosample stunted_south, by(level) cohensd hedgesg glassdelta
esize twosample stunted_rural, by(level) cohensd hedgesg glassdelta
esize twosample stunted_urban, by(level) cohensd hedgesg glassdelta
esize twosample wasted_north, by(level) cohensd hedgesg glassdelta
esize twosample wasted_central, by(level) cohensd hedgesg glassdelta
esize twosample wasted_south, by(level) cohensd hedgesg glassdelta
esize twosample wasted_rural, by(level) cohensd hedgesg glassdelta
esize twosample wasted_south, by(level) cohensd hedgesg glassdelta
esize twosample underweight_north, by(level) cohensd hedgesg glassdelta
esize twosample underweight_central, by(level) cohensd hedgesg glassdelta
esize twosample underweight_south, by(level) cohensd hedgesg glassdelta
esize twosample underweight_rural, by(level) cohensd hedgesg glassdelta
esize twosample underweight_urban, by(level) cohensd hedgesg glassdelta

//effect size for level 0 and level 15
cd "C:\Users\Lenovo\Documents\Chaplain\Documents\Personal\Rowland\MW_2015-16_DHS_12112017_1140_113119\mwpr7hdt"
use merged_dataset, clear
drop if level==1 | level==5| level==10| level==20 | level==25 | level==30
esize twosample rhaz1, by(level) cohensd hedgesg glassdelta
esize twosample rhaz2, by(level) cohensd hedgesg glassdelta
esize twosample rhaz3, by(level) cohensd hedgesg glassdelta
esize twosample lhaz1, by(level) cohensd hedgesg glassdelta
esize twosample lhaz2, by(level) cohensd hedgesg glassdelta
esize twosample rwaz1, by(level) cohensd hedgesg glassdelta
esize twosample rwaz2, by(level) cohensd hedgesg glassdelta
esize twosample rwaz3, by(level) cohensd hedgesg glassdelta
esize twosample lwaz1, by(level) cohensd hedgesg glassdelta
esize twosample lwaz2, by(level) cohensd hedgesg glassdelta
esize twosample rwhz1, by(level) cohensd hedgesg glassdelta
esize twosample rwhz2, by(level) cohensd hedgesg glassdelta
esize twosample rwhz3, by(level) cohensd hedgesg glassdelta
esize twosample lwhz1, by(level) cohensd hedgesg glassdelta
esize twosample lwhz2, by(level) cohensd hedgesg glassdelta
esize twosample stunted_north, by(level) cohensd hedgesg glassdelta
esize twosample stunted_central, by(level) cohensd hedgesg glassdelta
esize twosample stunted_south, by(level) cohensd hedgesg glassdelta
esize twosample stunted_rural, by(level) cohensd hedgesg glassdelta
esize twosample stunted_urban, by(level) cohensd hedgesg glassdelta
esize twosample wasted_north, by(level) cohensd hedgesg glassdelta
esize twosample wasted_central, by(level) cohensd hedgesg glassdelta
esize twosample wasted_south, by(level) cohensd hedgesg glassdelta
esize twosample wasted_rural, by(level) cohensd hedgesg glassdelta
esize twosample wasted_south, by(level) cohensd hedgesg glassdelta
esize twosample underweight_north, by(level) cohensd hedgesg glassdelta
esize twosample underweight_central, by(level) cohensd hedgesg glassdelta
esize twosample underweight_south, by(level) cohensd hedgesg glassdelta
esize twosample underweight_rural, by(level) cohensd hedgesg glassdelta
esize twosample underweight_urban, by(level) cohensd hedgesg glassdelta


//effect size for level 0 and level 30
cd "C:\Users\Lenovo\Documents\Chaplain\Documents\Personal\Rowland\MW_2015-16_DHS_12112017_1140_113119\mwpr7hdt"
use merged_dataset, clear
drop if level==1 | level==5|level==10 | level==15 | level==20 | level==25
esize twosample rhaz1, by(level) cohensd hedgesg glassdelta
esize twosample rhaz2, by(level) cohensd hedgesg glassdelta
esize twosample rhaz3, by(level) cohensd hedgesg glassdelta
esize twosample lhaz1, by(level) cohensd hedgesg glassdelta
esize twosample lhaz2, by(level) cohensd hedgesg glassdelta
esize twosample rwaz1, by(level) cohensd hedgesg glassdelta
esize twosample rwaz2, by(level) cohensd hedgesg glassdelta
esize twosample rwaz3, by(level) cohensd hedgesg glassdelta
esize twosample lwaz1, by(level) cohensd hedgesg glassdelta
esize twosample lwaz2, by(level) cohensd hedgesg glassdelta
esize twosample rwhz1, by(level) cohensd hedgesg glassdelta
esize twosample rwhz2, by(level) cohensd hedgesg glassdelta
esize twosample rwhz3, by(level) cohensd hedgesg glassdelta
esize twosample lwhz1, by(level) cohensd hedgesg glassdelta
esize twosample lwhz2, by(level) cohensd hedgesg glassdelta
esize twosample stunted_north, by(level) cohensd hedgesg glassdelta
esize twosample stunted_central, by(level) cohensd hedgesg glassdelta
esize twosample stunted_south, by(level) cohensd hedgesg glassdelta
esize twosample stunted_rural, by(level) cohensd hedgesg glassdelta
esize twosample stunted_urban, by(level) cohensd hedgesg glassdelta
esize twosample wasted_north, by(level) cohensd hedgesg glassdelta
esize twosample wasted_central, by(level) cohensd hedgesg glassdelta
esize twosample wasted_south, by(level) cohensd hedgesg glassdelta
esize twosample wasted_rural, by(level) cohensd hedgesg glassdelta
esize twosample wasted_south, by(level) cohensd hedgesg glassdelta
esize twosample underweight_north, by(level) cohensd hedgesg glassdelta
esize twosample underweight_central, by(level) cohensd hedgesg glassdelta
esize twosample underweight_south, by(level) cohensd hedgesg glassdelta
esize twosample underweight_rural, by(level) cohensd hedgesg glassdelta
esize twosample underweight_urban, by(level) cohensd hedgesg glassdelta


//effect size for level 0 and level 15
cd "C:\Users\Lenovo\Documents\Chaplain\Documents\Personal\Rowland\MW_2015-16_DHS_12112017_1140_113119\mwpr7hdt"
use merged_dataset, clear
drop if level==1 | level==5|level==10
esize twosample rhaz1, by(level) cohensd hedgesg glassdelta
esize twosample rhaz2, by(level) cohensd hedgesg glassdelta
esize twosample rhaz3, by(level) cohensd hedgesg glassdelta
esize twosample lhaz1, by(level) cohensd hedgesg glassdelta
esize twosample lhaz2, by(level) cohensd hedgesg glassdelta
esize twosample rwaz1, by(level) cohensd hedgesg glassdelta
esize twosample rwaz2, by(level) cohensd hedgesg glassdelta
esize twosample rwaz3, by(level) cohensd hedgesg glassdelta
esize twosample lwaz1, by(level) cohensd hedgesg glassdelta
esize twosample lwaz2, by(level) cohensd hedgesg glassdelta
esize twosample rwhz1, by(level) cohensd hedgesg glassdelta
esize twosample rwhz2, by(level) cohensd hedgesg glassdelta
esize twosample rwhz3, by(level) cohensd hedgesg glassdelta
esize twosample lwhz1, by(level) cohensd hedgesg glassdelta
esize twosample lwhz2, by(level) cohensd hedgesg glassdelta
esize twosample stunted_north, by(level) cohensd hedgesg glassdelta
esize twosample stunted_central, by(level) cohensd hedgesg glassdelta
esize twosample stunted_south, by(level) cohensd hedgesg glassdelta
esize twosample stunted_rural, by(level) cohensd hedgesg glassdelta
esize twosample stunted_urban, by(level) cohensd hedgesg glassdelta
esize twosample wasted_north, by(level) cohensd hedgesg glassdelta
esize twosample wasted_central, by(level) cohensd hedgesg glassdelta
esize twosample wasted_south, by(level) cohensd hedgesg glassdelta
esize twosample wasted_rural, by(level) cohensd hedgesg glassdelta
esize twosample wasted_south, by(level) cohensd hedgesg glassdelta
esize twosample underweight_north, by(level) cohensd hedgesg glassdelta
esize twosample underweight_central, by(level) cohensd hedgesg glassdelta
esize twosample underweight_south, by(level) cohensd hedgesg glassdelta
esize twosample underweight_rural, by(level) cohensd hedgesg glassdelta
esize twosample underweight_urban, by(level) cohensd hedgesg glassdelta






//icc
twoway (scatter rhohaz1 level, connect(l) sort)||qfitci rhohaz1 level, name(underweighturban) title("ICC")
graph twoway (bar rhohaz1 level)

twoway (scatter rhohaz1 level)
 
//1. reshape data to long



//graphs with error bars
//Now, let’s use the collapse command to make the mean and standard deviation by race and ses.

    collapse (mean) meanrhohaz1= rhohaz1 (sd) sdwrite=rhohaz1 (count) n=rhohaz1, by(level)

//Now, let’s make the upper and lower values of the confidence interval.

    generate hirhohaz1 = meanrhohaz1 + invttail(n-1,0.025)*(sdwrite / sqrt(n))
    generate lowrhohaz1 = meanrhohaz1 - invttail(n-1,0.025)*(sdwrite / sqrt(n))

graph twoway (bar meanrhohaz1 level) (rcap hirhohaz1 lowrhohaz1 level) ///, by(level)
twoway (bar meanrhohaz1 level) (rcap hirhohaz1 lowrhohaz1 level)



//scatter plot
twoway (scatter newh hc1, connect(l) sort)||qfitci newh hc1, name(ha)
graph save ha.gph

twoway (scatter neww hc1, connect(l) sort)||qfitci neww hc1, name(wa)
graph save wa.gph

twoway (scatter neww newh, connect(l) sort)||qfitci neww newh, name(wh)
graph save wh.gph

twoway (scatter neww newh hc1, connect(l) sort)||qfitci neww hc1, name(wha)
graph save wha.gph

graph combine ha wa wh wha, title("Relationship of Height and Weight to Age")

//graphs for indices
//1. haz
hist haz06 if hv024==1, title (HAZ - Northern Region) name(haz_north1)  
graph save haz_north1.gph

hist haz06 if hv024==2, title (HAZ - Central Region) name(haz_centre1)  
graph save haz_centre1.gph

hist haz06 if hv024==3, title (HAZ - Southern Region) name(haz_south1)  
graph save haz_south1.gph

hist haz06, title (HAZ - Malawi) name(haz_malawi1)  
graph save haz_malawi1.gph

graph combine haz_malawi1 haz_north1 haz_centre1 haz_south1, title("Histograms for Height-for-Age Z-scores")

//2. waz
hist waz06 if hv024==1, title (WAZ - Northern Region) name(waz_north1)  
graph save waz_north1.gph

hist waz06 if hv024==2, title (WAZ - Central Region) name(waz_centre1)  
graph save waz_centre1.gph

hist waz06 if hv024==3, title (WAZ - Southern Region) name(waz_south11)  
graph save waz_south11.gph

hist waz06, title (WAZ - Malawi) name(waz_malawi1)  
graph save waz_malawi1.gph

graph combine waz_malawi1 waz_north waz_centre1 waz_south11, title("Histograms for Weight-for-Age Z-scores")


//3. whz
hist whz06 if hv024==1, title (WHZ - Northern Region) name(whz_north1)  
graph save whz_north1.gph

hist whz06 if hv024==2, title (WHZ - Central Region) name(whz_centre1)  
graph save whz_centre1.gph

hist whz06 if hv024==3, title (WHZ - Southern Region) name(whz_south1)  
graph save whz_south1.gph

hist whz06, title (WHZ - Malawi) name(whz_malawi1)  
graph save whz_malawi1.gph

graph combine whz_malawi whz_north whz_centre whz_south, title("Histograms for Weight-for-Height Z-scores")


//4. spaghetti plots - by district

 lgraph haz06 hv023, errortype(sd) separate(0.01) //by district - std deviation
 lgraph haz06 hv023, errortype(se) separate(0.01) //by district- standard errors

 lgraph haz06 hv024, errortype(se) separate(0.01) //by region - se

 
 //lgraph haz06 hv024, fit(bar) fop(barw(.4)) sep(.1) nomarker
 //legend(on order(3 "Domestic" 4 "Foreign"))
 
spagplot haz06 hv023, id(hv024)
spagplot waz06 hc1, id(hv023)
spagplot whz06 hc1, id(hv023)

bysort level: sum 


//scatter plot
twoway (scatter haz level, connect(l) sort)||qfitci haz level
twoway (scatter waz level, connect(l) sort)||qfitci waz level
twoway (scatter whz level, connect(l) sort)||qfitci whz level
//twoway (scatter rhohaz level, connect(l) sort)||qfitci rhohaz level
//twoway (scatter rhowaz level, connect(l) sort)||qfitci rhowaz level
//twoway (scatter rhowhz level, connect(l) sort)||qfitci rhowhz level



//graphs for indices by age
//egen mhaz = mean(haz), by(level)
//twoway (scatter mhaz level, connect(l) sort)

//egen mwaz = mean(waz), by(level)
//twoway (scatter waz level, connect(l) sort)

//egen mwhz = mean(whz06), by(hc1)
//twoway (scatter mwhz hc1, connect(l) sort)


//graphs with error bars
//collapse (mean) meanhaz= haz06 (sd) sdhaz=haz06 (count) n=haz06, by(hc1)
//serrbar meanhaz sdhaz hc1, scale (1.96)

//collapse (mean) meanwaz= waz06 (sd) sdwaz=waz06 (count) n=waz06, by(hc1)
//serrbar meanwaz sdwaz hc1, scale (1.96)

//collapse (mean) meanwhz= whz06 (sd) sdwhz=whz06 (count) n=whz06, by(hc1)
//serrbar meanwhz sdwhz hc1, scale (1.96)

//spaghetti plots - by district
//spagplot haz rhohaz, id(level)
//spagplot neww hc1, id(hv023)
//spagplot neww newh, id(hv023)
//spagplot haz06 hc1, id(hv023)
//spagplot waz06 hc1, id(hv023)
//spagplot whz06 hc1, id(hv023)

//ciplots - jan 2021
//HAZ
gen north_haz=.
replace north_haz=rhaz1
replace north_haz=-1.40 if level==0
bysort level: sum north_haz
ciplot north_haz, by(level)
//ciplot rhaz1, by(level)

gen central_haz=.
replace central_haz=rhaz2
replace central_haz=-1.41 if level==0
bysort level: sum central_haz
ciplot central_haz, by(level)
ciplot rhaz2, by(level)

gen south_haz=.
replace south_haz=rhaz3
replace south_haz=-1.36955 if level==0
bysort level: sum south_haz
ciplot south_haz, by(level)
//ciplot rhaz3, by(level)

//WAZ
gen north_waz=.
replace north_waz=rwaz1
replace north_waz=-0.645 if level==0
bysort level: sum north_waz
ciplot north_waz, by(level)
//ciplot rwaz1, by(level)

gen central_waz=.
replace central_waz=rwaz2
replace central_waz=-0.662 if level==0
bysort level: sum central_waz
ciplot central_waz, by(level)
//ciplot rwaz2, by(level)

gen south_waz=.
replace south_waz=rwaz3
replace south_waz=-0.7353635  if level==0
bysort level: sum south_waz
bysort level: sum rwaz3
ciplot south_waz, by(level)
//ciplot rwaz3, by(level)

//Z-scores by area of residence
bysort level: sum lhaz1
//ciplot lhaz1, by(level)
gen rural_haz=.
replace rural_haz=lhaz1
replace rural_haz=-1.027813 if level==0
ciplot rural_haz, by(level)

bysort level: sum lhaz2
ciplot lhaz2, by(level)
gen urban_haz=.
replace urban_haz=lhaz2
replace urban_haz=-1.441781 if level==0
ciplot urban_haz, by(level)

bysort level: sum lwaz1
//ciplot lwaz1, by(level)
gen rural_waz=.
replace rural_waz=lwaz1
replace rural_waz=-.4498221 if level==0
ciplot rural_waz, by(level)

bysort level: sum lwaz2
ciplot lwaz2, by(level)
gen urban_waz=.
replace urban_waz=lwaz2
replace urban_waz=-.7295832 if level==0
ciplot urban_waz, by(level)

bysort level: sum lwhz1
//ciplot lwhz1, by(level)
gen rural_whz=.
replace rural_whz=lwhz1
replace rural_whz=.2049189 if level==0
ciplot rural_whz, by(level)

bysort level: sum lwhz2
//ciplot lwhz2, by(level)
gen urban_whz=.
replace urban_whz=lwhz2
replace urban_whz=.1316984 if level==0
ciplot urban_whz, by(level)

//prevalence rates 
ciplot stunting_north2, by(level)
ciplot stunting_centre2, by(level)
ciplot stunting_south2, by(level)
ciplot stunting_rural2, by(level)
ciplot stunting_urban2, by(level)

gen wasting_north2=.
replace wasting_north2=100*wasted_north
bysort level: sum wasting_north2
ciplot wasting_north2, by(level)
replace wasting_north2=2.05568 if level==0
ciplot wasting_north2, by(level)

gen wasting_south2=.
replace wasting_south2=100*wasted_south
bysort level: sum wasting_south2
ciplot wasting_south2, by(level)
replace wasting_south2= 3.50296 if level==0
ciplot wasting_south2, by(level)

