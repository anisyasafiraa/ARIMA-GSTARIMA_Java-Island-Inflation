proc varmax data=WORK.Tr;
	model Banten DIY DKI_Jakarta Jawa_Barat Jawa_Tengah Jawa_Timur= / 
		minic=(p=3 q=3) print=(corry pcorr) lagmax=6 method=ls;
	output lead=12 back=0 alpha=0.05;
run;