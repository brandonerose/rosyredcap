# rosyredcap 2.4.4
* added fixes to codebook for checkboxes

# rosyredcap 2.4.3
* add merge_non_repeating_DB() and fix bug by adding all.x=T

# rosyredcap 2.4.2
* fixed choices df missing dropdowns

# rosyredcap 2.4.1
* added add id_col to DF function

# rosyredcap 2.4.0
* added get file function

# rosyredcap 2.3.0
* added a clean_DB function that uses the REDCap metadata to convert all functions for better plotting

# rosyredcap 2.2.0
* added deidentify_DB and 
* internally preparing for other DB processing such as merging based on events and non-repeating etc.
* added select_redcap_records to external

# rosyredcap 2.1.2
* fixed bugs with multi events and repeating instance projects
* projects with multiple arm remain to be tested
* remove html tags from metadata (interferes with excel saves)

# rosyredcap 2.1.1
* patch for repeat instrument bug

# rosyredcap 2.1.0
* several internal code changes for readability and effeciency
* working on allowing for multi-arm and multi event projects
* initial tests working on basic projects, expect patches and further testing

# rosyredcap 2.0.0
* big changes in structure to allow for multiple projects. Refer to new readme!
* added file repository functions, may integrate at some point
* added checkbox support to codebook and choices output
* tokens are now more secure in users environment with option to have it never directly in the project or console
* fixing/testing upload function
* integrated missing codes from redcap project info, could be overwritten by user if needed

# rosyredcap 1.0.9
* added missing code support needs to be tested

# rosyredcap 1.0.8
* fixed for non repeating instrument projects
* added codebook output
* fixed dropdown issues

# rosyredcap 1.0.7
* allowing for multiple non-repeating instruments in DB object

# rosyredcap 1.0.6
* added choices to DB object
* added batch_size to internal upload function

# rosyredcap 1.0.5
* adjusting for multiple projects, saves .PID.rdata file to R_objects

# rosyredcap 1.0.4
* added repeat redcap instance to meta data for direct matching clean labels
* added show dir metadata

# rosyredcap 1.0.3

# rosyredcap 1.0.2
* big internal function update to allow for uploads. Won't bump to minor version until it's external and tested more.

# rosyredcap 1.0.1
* minor patch to fix small error in internal code

# rosyredcap 1.0.0
* first release

# rosyredcap 0.0.0.9000
* Added a `NEWS.md` file to track changes to the package.
