proc model data=WORK.tr;
	parms teta110 teta210 teta310 teta410 teta510 teta610
 		  teta111 teta211 teta311 teta411 teta511 teta611; 
	/*identifikasi parameter*/

	Banten = -teta110*zlag1(resid.Banten) - teta111*(0.085*zlag1(resid.DIY)+0.513*zlag1(resid.DKI_Jakarta)+0.250*zlag1(resid.Jawa_Barat)+0.093*zlag1(resid.Jawa_Tengah)+0.059*zlag1(resid.Jawa_Timur));
	DIY = -teta210*zlag1(resid.DIY) - teta211*(0.080*zlag1(resid.Banten)+0.092*zlag1(resid.DKI_Jakarta)+0.122*zlag1(resid.Jawa_Barat)+0.538*zlag1(resid.Jawa_Tengah)+0.168*zlag1(resid.Jawa_Timur));
	DKI_Jakarta = -teta310*zlag1(resid.DKI_Jakarta) - teta311*(0.443*zlag1(resid.Banten)+0.084*zlag1(resid.DIY)+0.321*zlag1(resid.Jawa_Barat)+0.095*zlag1(resid.Jawa_Tengah)+0.056*zlag1(resid.Jawa_Timur));
	Jawa_Barat = -teta410*zlag1(resid.Jawa_Barat) - teta411*(0.256*zlag1(resid.Banten)+0.132*zlag1(resid.DIY)+0.381*zlag1(resid.DKI_Jakarta)+0.152*zlag1(resid.Jawa_Tengah)+0.078*zlag1(resid.Jawa_Timur));
	Jawa_Tengah = -teta510*zlag1(resid.Jawa_Tengah) - teta511*(0.086*zlag1(resid.Banten)+0.529*zlag1(resid.DIY)+0.102*zlag1(resid.DKI_Jakarta)+0.137*zlag1(resid.Jawa_Barat)+0.145*zlag1(resid.Jawa_Timur));
	Jawa_Timur = -teta610*zlag1(resid.Jawa_Timur) - teta611*(0.109*zlag1(resid.Banten)+0.334*zlag1(resid.DIY)+0.121*zlag1(resid.DKI_Jakarta)+0.142*zlag1(resid.Jawa_Barat)+0.293*zlag1(resid.Jawa_Tengah));
	
FIT Banten DIY DKI_Jakarta Jawa_Barat Jawa_Tengah Jawa_Timur/ OLS
OUT = GSTMA_paramInvers OUTRESID OUTEST=Koefisien;
proc Print Data=GSTMA_paramInvers;
run;