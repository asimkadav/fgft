Code splitting tool for Fine-grained Fault Tolerance
=====================================================

Fine-Grained Fault Tolerance (FGFT) isolates driver code at the granularity of a single entry point. It executes driver code as a transaction, allowing roll back if the driver fails. 

Following is the directory wise listing of this code tree:

/cil:

This piece of code takes a driver and splits it to produce two kernel modules. First is the regular kernel module, that contains callouts to the SFI module that runs code with SFI checks. Since this can be done on a per-entry basis and decided at runtime, FGFT generates three copies of each function. First is the normal function w/o SFI in regular module. Second, is a callout version that just contains stubs for call-in and out of SFI. Third, is the SFI version that does the actual job and contains SFI checks.









 



