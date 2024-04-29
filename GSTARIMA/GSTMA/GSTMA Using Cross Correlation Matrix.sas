proc model data=WORK.tr;
	parms teta110 teta210 teta310 teta410 teta510 teta610
 		  teta111 teta211 teta311 teta411 teta511 teta611; 
	/*identifikasi parameter*/

	Banten = -teta110*zlag1(resid.Banten) - teta111*(0.198*zlag1(resid.DIY)+0.206*zlag1(resid.DKI_Jakarta)+0.231*zlag1(resid.Jawa_Barat)+0.185*zlag1(resid.Jawa_Tengah)+0.180*zlag1(resid.Jawa_Timur));
	DIY = -teta210*zlag1(resid.DIY) - teta211*(0.219*zlag1(resid.Banten)+0.197*zlag1(resid.DKI_Jakarta)+0.211*zlag1(resid.Jawa_Barat)+0.178*zlag1(resid.Jawa_Tengah)+0.195*zlag1(resid.Jawa_Timur));
	DKI_Jakarta = -teta310*zlag1(resid.DKI_Jakarta) - teta311*(0.211*zlag1(resid.Banten)+0.195*zlag1(resid.DIY)+0.224*zlag1(resid.Jawa_Barat)+0.182*zlag1(resid.Jawa_Tengah)+0.188*zlag1(resid.Jawa_Timur));
	Jawa_Barat = -teta410*zlag1(resid.Jawa_Barat) - teta411*(0.224*zlag1(resid.Banten)+0.200*zlag1(resid.DIY)+0.191*zlag1(resid.DKI_Jakarta)+0.194*zlag1(resid.Jawa_Tengah)+0.191*zlag1(resid.Jawa_Timur));
	Jawa_Tengah = -teta510*zlag1(resid.Jawa_Tengah) - teta511*(0.237*zlag1(resid.Banten)+0.195*zlag1(resid.DIY)+0.170*zlag1(resid.DKI_Jakarta)+0.221*zlag1(resid.Jawa_Barat)+0.176*zlag1(resid.Jawa_Timur));
	Jawa_Timur = -teta610*zlag1(resid.Jawa_Timur) - teta611*(0.224*zlag1(resid.Banten)+0.203*zlag1(resid.DIY)+0.179*zlag1(resid.DKI_Jakarta)+0.217*zlag1(resid.Jawa_Barat)+0.177*zlag1(resid.Jawa_Tengah));
	
FIT Banten DIY DKI_Jakarta Jawa_Barat Jawa_Tengah Jawa_Timur/ OLS
OUT = GSTMA_paramKor OUTRESID OUTEST=Koefisien;
proc Print Data=GSTMA_paramKor;
run;