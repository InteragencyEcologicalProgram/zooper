# zooper (development version)

* Fixing malfunctions in the taxonomic consistency over time approach. Uses of that function prior to this fix may not have appropriately resolved for changes in taxonomic consistency over time.

# zooper 2.5.1

* Added DOP and STN/FMWT 2022 data 

# zooper 2.5.0

* Added some missing environmental variables to DOP and removed bottom samples for environmental variables except conductivity. 
* Added TowType variable to identify whether each tow was a surface, bottom, oblique, or vertical pump sample. 
* Removed crosswalk Inf and -Inf dates and used 2500 and 1800, respectively, instead. For some reason, Inf and -Inf dates were no longer being treated correctly. 
* Added DOP to `Zoopdownloader` test
* Added new co-authors
* Converted undersampled and crosswalk files to csv so it is easier to track changes on git
* Fixed previously incorrect times in EMP data. Previously, times in the EMP data were automatically converted to AM even if they were recorded as PM
* Changed FRP data coordinates to use the sample-specific coordinates instead of the general station location coordinates
* Added DOP data
* Changed data source for FMWT/STN to EDI
* Changed default download method of `Zoopdownloader` to "auto"
* Updating to newest EMP data. 
* Added newest FMWT/STN data, including new station STN 722
* Add option to specify download method in `Zoopdownloader` function.
* Updated to newest SMSCG data

# zooper 2.4.1

* Updated to newest FMWT macro column names.

# zooper 2.4.0

* Changed all *Acanthocyclops vernalis* in crosswalk to *Acanthocyclops*.
* Added YBFMP data. YBFMP data cannot be used in the `Zoopsynther` function due to issues with taxonomic and life stage resolution. 
* Updated to newest FMWT/SMSCG macro data. 

# zooper 2.3.1

* Updated EMP EZ station locations

# zooper 2.3.0

* Used EMP Macro QAQC flags for amphipod data to set amphipod CPUE to NA whenever issues were detected (vegetation in net)
* Added EMP Amphipod data
* Fixed error where Undersampled taxa were not being flagged, identified by Rosemary Hartman
* Updated EDI download URLs to use pasta links
* Added 2020 SMSCG data
* Added updated 2020 EMP data with corrected depth values and corrected timezones
* Added 2020 EMP data (NOTE there is an outstanding issues with the EMP bottom depth values, two of which are 0 which should be incorrect).

# zooper 2.2.0

* Updated to a corrected version of the 20mm dataset.

# zooper 2.1.0

* Added tests to ensure no volume values are ever NA.
* Changed the name of the 20mm dataset to "20mm" everywhere for consistency. Previously, the output of `Zoopdownloader` would have the name as "twentymm", as would the built-in datasets, and the stations dataset.
* Added 2020 20mm data

# zooper 2.0.0

* Added some simple progress messages to `Zoopdownloader`.
* Re-implemented use of the dtplyr package to greatly speed up computation, now that support for `dplyr::across` has been implemented. dtplyr >1.1.0 must be used for this to work.
* Fixed annoying warning message from `Zoopdownloader` function that 2 Datetimes failed to parse. Resulting dataset unchanged
* Redirected data downloads (for those datasets not on EDI) toward the new CDFW filelib website (https://filelib.wildlife.ca.gov/Public) instead of the old FTP (ftp://ftp.wildlife.ca.gov) by creating a new function to pull file lists from HTMLs (and a new dependency on rvest), replacing the old function to do the same with FTPs
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
