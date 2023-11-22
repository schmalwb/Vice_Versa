set more off
clear all


cd"YOUR PATH"

local csv_files = "replication"
foreach x of local csv_files {
	import delimited document_probs_`x', clear
	egen nt = max(topic)
	sort document
	reshape wide gamma nt, i(document) j(topic)
	qui{ 
		sum nt1
		scalar m = r(max)
	foreach j of numlist 1/`=m' {
		ren gamma`j' topic`j'
		drop nt`j'
	}
	export delimited document_probs_transformed_`x'.csv, replace
	}
	disp ""
	disp ""
	disp "		> Number of topics = `=m' <"
	disp ""
	disp "		> Export of `x' DONE <"
	disp "-------------------------------------------------------"
}



*******************************************************************************
