# revolution

If the Vignette is build, the installation process takes a significant time due to the package downloading the latest rki data.

If you are a linux user, you need to build the vignette by hand (not by build_vignettes=TRUE) due to download problems of the RKI data during the building process of the vignette. Please refere to the documentation of update_rki_data().

The same applies if you want to manually load an RKI data instead of using the automated download the same applies.

Manual loading can be done via the manual_load_rki_data().

