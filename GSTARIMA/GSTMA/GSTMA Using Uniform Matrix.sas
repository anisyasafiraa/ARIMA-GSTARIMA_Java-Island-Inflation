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
