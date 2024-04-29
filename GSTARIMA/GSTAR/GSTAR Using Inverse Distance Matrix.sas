proc model data=WORK.tr;
	parms psi110 psi210 psi310 psi410 psi510 psi610
		  psi111 psi211 psi311 psi411 psi511 psi611; 
	/*identifikasi parameter*/

	Banten = psi110*zlag1(Banten) + psi111*(0.085*zlag1(DIY)+0.513*zlag1(DKI_Jakarta)+0.250*zlag1(Jawa_Barat)+0.093*zlag1(Jawa_Tengah)+0.059*zlag1(Jawa_Timur));
	
	DIY = psi210*zlag1(DIY) + psi211*(0.080*zlag1(Banten)+0.092*zlag1(DKI_Jakarta)+0.122*zlag1(Jawa_Barat)+0.538*zlag1(Jawa_Tengah)+0.168*zlag1(Jawa_Timur));
	
	DKI_Jakarta = psi310*zlag1(DKI_Jakarta) + psi311*(0.443*zlag1(Banten)+0.084*zlag1(DIY)+0.321*zlag1(Jawa_Barat)+0.095*zlag1(Jawa_Tengah)+0.056*zlag1(Jawa_Timur));
	
	Jawa_Barat = psi410*zlag1(Jawa_Barat) + psi411*(0.256*zlag1(Banten)+0.132*zlag1(DIY)+0.381*zlag1(DKI_Jakarta)+0.152*zlag1(Jawa_Tengah)+0.078*zlag1(Jawa_Timur));
	
	Jawa_Tengah = psi510*zlag1(Jawa_Tengah) + psi511*(0.086*zlag1(Banten)+0.529*zlag1(DIY)+0.102*zlag1(DKI_Jakarta)+0.137*zlag1(Jawa_Barat)+0.145*zlag1(Jawa_Timur));
	
	Jawa_Timur = psi610*zlag1(Jawa_Timur) + psi611*(0.109*zlag1(Banten)+0.334*zlag1(DIY)+0.121*zlag1(DKI_Jakarta)+0.142*zlag1(Jawa_Barat)+0.293*zlag1(Jawa_Tengah));
	
FIT Banten DIY DKI_Jakarta Jawa_Barat Jawa_Tengah Jawa_Timur/ OLS
OUT = GSTAR_paramInvers OUTRESID OUTEST=Koefisien;
proc print Data=GSTAR_paramInvers;
run;