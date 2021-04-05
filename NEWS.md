
* Changed internals to apply 3 tries to each attempt to access a URL
* Fixed CDFW FTP URL
* Fixing 20mm bottom depths (formally in feet, now correcting to meters like all other studies are converted).
* Updating with newest 20mm data.
* Added station location for FMWT 520 (=STN 520)

# zooper 1.1.1

* Corrected timezone error that was shifting the date of some STN, FMWT, and 20mm samples by 1 day relative to the datetime.
* Updated FMWT station location coordinates to the newest version from the FMWT database.
* Corrected inconsistent capitalization of the STN Mont and Honk stations, resulting in correct coordinates for all those samples.
* Removed EMP Meso Sample corresponding to a mostly empty line at the end of the csv.

# zooper 1.1.0

* Updated citation info
* Added 2018 20mm data
* Added tests to ensure column names have not changed.
* Added 2019 EMP, FMWT, and STN data

# zooper 1.0.0

* Added SMSCG data
* Changed EMP source to the Environmental Data Initiative instead of the FTP site.
* Updated to newest versions of FMWT and STN data
* Added more flexibility to `zoop_downloader` so the package will not break when dates are changed in the source dataset filenames.
* Changed Summer Townet acronym from TNS to STN.
* Updated to `dplyr` 1.0, had to remove `dtplyr` usage due to incompatibility with `dplyr` 1.0 "_at" functions and new "across" function. Everything will be slower until `dtplyr` is updated.

# zooper 0.3.0

* Moved to github actions for continuous integration testing. 
* Added EMP EZ station latitude and longitude locations for 2004-present.
* Fixed inconsistent Microcystis values in FRP data.
* Fixed typo in Crosswalk table.

# zooper 0.2.1

* Updated to newest FMWT & STN data. (04/01/2020)

# zooper 0.2.0

* Added an option to the `Zoopsynther` function to correct for changes in taxonomic resolution over time by enforcing taxonomic consistency over time.
* Made all argument names start with a capital letter for consistency.
* Changed all "Larvae" to "Larva" and "Pupae" to "Pupa" in crosswalk and elsewhere for consistency with other singular life stage names.

# zooper 0.1.0

* First stable release.
* Added testthat tests and continuous integration with Travis CI.
* Started incorporating zooper version into shiny app. 

# zooper 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
