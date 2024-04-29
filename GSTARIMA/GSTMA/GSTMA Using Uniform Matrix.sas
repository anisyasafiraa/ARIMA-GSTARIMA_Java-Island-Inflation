proc model data=WORK.Tr;
	parms teta110 teta210 teta310 teta410 teta510 teta610
 		  teta111 teta211 teta311 teta411 teta511 teta611; 
	/*identifikasi parameter*/

	Banten = -teta110*zlag1(resid.Banten) - teta111*(0.167*zlag1(resid.DIY)+0.167*zlag1(resid.DKI_Jakarta)+0.167*zlag1(resid.Jawa_Barat)+0.167*zlag1(resid.Jawa_Tengah)+0.167*zlag1(resid.Jawa_Timur));
	DIY = -teta210*zlag1(resid.DIY) - teta211*(0.167*zlag1(resid.Banten)+0.167*zlag1(resid.DKI_Jakarta)+0.167*zlag1(resid.Jawa_Barat)+0.167*zlag1(resid.Jawa_Tengah)+0.167*zlag1(resid.Jawa_Timur));
	DKI_Jakarta = -teta310*zlag1(resid.DKI_Jakarta) - teta311*(0.167*zlag1(resid.Banten)+0.167*zlag1(resid.DIY)+0.167*zlag1(resid.Jawa_Barat)+0.167*zlag1(resid.Jawa_Tengah)+0.167*zlag1(resid.Jawa_Timur));
	Jawa_Barat = -teta410*zlag1(resid.Jawa_Barat) - teta411*(0.167*zlag1(resid.Banten)+0.167*zlag1(resid.DIY)+0.167*zlag1(resid.DKI_Jakarta)+0.167*zlag1(resid.Jawa_Tengah)+0.167*zlag1(resid.Jawa_Timur));
	Jawa_Tengah = -teta510*zlag1(resid.Jawa_Tengah) - teta511*(0.167*zlag1(resid.Banten)+0.167*zlag1(resid.DIY)+0.167*zlag1(resid.DKI_Jakarta)+0.167*zlag1(resid.Jawa_Barat)+0.167*zlag1(resid.Jawa_Timur));
	Jawa_Timur = -teta610*zlag1(resid.Jawa_Timur) - teta611*(0.167*zlag1(resid.Banten)+0.167*zlag1(resid.DIY)+0.167*zlag1(resid.DKI_Jakarta)+0.167*zlag1(resid.Jawa_Barat)+0.167*zlag1(resid.Jawa_Tengah));
	
FIT Banten DIY DKI_Jakarta Jawa_Barat Jawa_Tengah Jawa_Timur/ OLS
OUT = GSTMA_paramSeragam OUTRESID OUTEST=Koefisien;
proc Print Data=GSTMA_paramSeragam;
run;

proc IML;
USE GSTMA_paramSeragam;
READ ALL VAR {Banten DIY DKI_Jakarta Jawa_Barat Jawa_Tengah Jawa_Timur} INTO e_duga;
	T=nrow(e_duga);
	eT1_1 = e_duga[T,1];
	eT2_1 = e_duga[T,2];
	eT3_1 = e_duga[T,3];
	eT4_1 = e_duga[T,4];
	eT5_1 = e_duga[T,5];
	eT6_1 = e_duga[T,6];
	
CREATE EROR1 VAR {eT1_1 eT2_1 eT3_1 eT4_1 eT5_1 eT6_1};
APPEND;
proc print data=eror1;
run;

data predSeragam;
set eror1;
set Koefisien;

	estBanten1 = -teta110*eT1_1 - teta111*(0.167*eT2_1+0.167*eT3_1+0.167*eT4_1+0.167*eT5_1+0.167*eT6_1);
	estDIY1 = -teta210*eT2_1 - teta211*(0.167*eT1_1+0.167*eT3_1+0.167*eT4_1+0.167*eT5_1+0.167*eT6_1);
	estDKI_Jakarta1 = -teta310*eT3_1 - teta311*(0.167*eT1_1+0.167*eT2_1+0.167*eT4_1+0.167*eT5_1+0.167*eT6_1);
	estJawa_Barat1 = -teta410*eT4_1 - teta411*(0.167*eT1_1+0.167*eT2_1+0.167*eT3_1+0.167*eT5_1+0.167*eT6_1);
	estJawa_Tengah1 = -teta510*eT5_1 - teta511*(0.167*eT1_1+0.167*eT2_1+0.167*eT3_1+0.167*eT4_1+0.167*eT6_1);
	estJawa_Timur1 = -teta610*eT6_1 - teta611*(0.167*eT1_1+0.167*eT2_1+0.167*eT3_1+0.167*eT4_1+0.167*eT5_1);

output;
proc print data=predSeragam;
run;