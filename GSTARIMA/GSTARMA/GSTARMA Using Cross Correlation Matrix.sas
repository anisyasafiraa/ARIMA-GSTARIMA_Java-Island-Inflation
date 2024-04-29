proc model data=WORK.Tr;
	parms psi110 psi210 psi310 psi410 psi510 psi610
		  psi111 psi211 psi311 psi411 psi511 psi611
		  teta110 teta210 teta310 teta410 teta510 teta610
 		  teta111 teta211 teta311 teta411 teta511 teta611; 
	/*identifikasi parameter*/

	Banten = psi110*zlag1(Banten) + psi111*(0.198*zlag1(DIY)+0.206*zlag1(DKI_Jakarta)+0.231*zlag1(Jawa_Barat)+0.185*zlag1(Jawa_Tengah)+0.180*zlag1(Jawa_Timur)) - teta110*zlag1(resid.Banten) - teta111*(0.198*zlag1(resid.DIY)+0.206*zlag1(resid.DKI_Jakarta)+0.231*zlag1(resid.Jawa_Barat)+0.185*zlag1(resid.Jawa_Tengah)+0.180*zlag1(resid.Jawa_Timur));
	
	DIY = psi210*zlag1(DIY) + psi211*(0.080*zlag1(Banten)+0.092*zlag1(DKI_Jakarta)+0.122*zlag1(Jawa_Barat)+0.538*zlag1(Jawa_Tengah)+0.168*zlag1(Jawa_Timur))- teta210*zlag1(resid.DIY) - teta211*(0.219*zlag1(resid.Banten)+0.197*zlag1(resid.DKI_Jakarta)+0.211*zlag1(resid.Jawa_Barat)+0.178*zlag1(resid.Jawa_Tengah)+0.195*zlag1(resid.Jawa_Timur));
	
	DKI_Jakarta = psi310*zlag1(DKI_Jakarta) + psi311*(0.443*zlag1(Banten)+0.084*zlag1(DIY)+0.321*zlag1(Jawa_Barat)+0.095*zlag1(Jawa_Tengah)+0.056*zlag1(Jawa_Timur)) - teta310*zlag1(resid.DKI_Jakarta) - teta311*(0.211*zlag1(resid.Banten)+0.195*zlag1(resid.DIY)+0.224*zlag1(resid.Jawa_Barat)+0.182*zlag1(resid.Jawa_Tengah)+0.188*zlag1(resid.Jawa_Timur));
	
	Jawa_Barat = psi410*zlag1(Jawa_Barat) + psi411*(0.256*zlag1(Banten)+0.132*zlag1(DIY)+0.381*zlag1(DKI_Jakarta)+0.152*zlag1(Jawa_Tengah)+0.078*zlag1(Jawa_Timur)) - teta410*zlag1(resid.Jawa_Barat) - teta411*(0.224*zlag1(resid.Banten)+0.200*zlag1(resid.DIY)+0.191*zlag1(resid.DKI_Jakarta)+0.194*zlag1(resid.Jawa_Tengah)+0.191*zlag1(resid.Jawa_Timur));
	
	Jawa_Tengah = psi510*zlag1(Jawa_Tengah) + psi511*(0.086*zlag1(Banten)+0.529*zlag1(DIY)+0.102*zlag1(DKI_Jakarta)+0.137*zlag1(Jawa_Barat)+0.145*zlag1(Jawa_Timur)) - teta510*zlag1(resid.Jawa_Tengah) - teta511*(0.237*zlag1(resid.Banten)+0.195*zlag1(resid.DIY)+0.170*zlag1(resid.DKI_Jakarta)+0.221*zlag1(resid.Jawa_Barat)+0.176*zlag1(resid.Jawa_Timur));
	
	Jawa_Timur = psi610*zlag1(Jawa_Timur) + psi611*(0.109*zlag1(Banten)+0.334*zlag1(DIY)+0.121*zlag1(DKI_Jakarta)+0.142*zlag1(Jawa_Barat)+0.293*zlag1(Jawa_Tengah)) - teta610*zlag1(resid.Jawa_Timur) - teta611*(0.224*zlag1(resid.Banten)+0.203*zlag1(resid.DIY)+0.179*zlag1(resid.DKI_Jakarta)+0.217*zlag1(resid.Jawa_Barat)+0.177*zlag1(resid.Jawa_Tengah));
	
FIT Banten DIY DKI_Jakarta Jawa_Barat Jawa_Tengah Jawa_Timur/ OLS
OUT = GSTMA_paramKorTr OUTRESID OUTEST=Koefisien;
proc print Data=GSTMA_paramKorTr;
run;