proc model data=WORK.Tr;
	parms psi110 psi210 psi310 psi410 psi510 psi610
		  psi111 psi211 psi311 psi411 psi511 psi611; 
	/*identifikasi parameter*/

	Banten = psi110*zlag1(Banten) + psi111*(0.167*zlag1(DIY)+0.167*zlag1(DKI_Jakarta)+0.167*zlag1(Jawa_Barat)+0.167*zlag1(Jawa_Tengah)+0.167*zlag1(Jawa_Timur));
	
	DIY = psi210*zlag1(DIY) + psi211*(0.167*zlag1(Banten)+0.167*zlag1(DKI_Jakarta)+0.167*zlag1(Jawa_Barat)+0.167*zlag1(Jawa_Tengah)+0.167*zlag1(Jawa_Timur));
	
	DKI_Jakarta = psi310*zlag1(DKI_Jakarta) + psi311*(0.167*zlag1(Banten)+0.167*zlag1(DIY)+0.167*zlag1(Jawa_Barat)+0.167*zlag1(Jawa_Tengah)+0.167*zlag1(Jawa_Timur));
	
	Jawa_Barat = psi410*zlag1(Jawa_Barat) + psi411*(0.167*zlag1(Banten)+0.167*zlag1(DIY)+0.167*zlag1(DKI_Jakarta)+0.167*zlag1(Jawa_Tengah)+0.167*zlag1(Jawa_Timur));
	
	Jawa_Tengah = psi510*zlag1(Jawa_Tengah) + psi511*(0.167*zlag1(Banten)+0.167*zlag1(DIY)+0.167*zlag1(DKI_Jakarta)+0.167*zlag1(Jawa_Barat)+0.167*zlag1(Jawa_Timur));
	
	Jawa_Timur = psi610*zlag1(Jawa_Timur) + psi611*(0.167*zlag1(Banten)+0.167*zlag1(DIY)+0.167*zlag1(DKI_Jakarta)+0.167*zlag1(Jawa_Barat)+0.167*zlag1(Jawa_Tengah));
	
FIT Banten DIY DKI_Jakarta Jawa_Barat Jawa_Tengah Jawa_Timur/ OLS
OUT = GSTAR_paramSeragam OUTRESID OUTEST=Koefisien;
proc print Data=GSTAR_paramSeragam;
run;