The code in this repository produces a locality profile Word document for the Pharmaceutical Needs Assessment (PNA) for any given PNA locality or list of PNA localities.
The code is currently written for the localities in Bristol, North Somerset, and South Gloucestershire, but can easily be adapted to run for any locality in the South West.

The following files are needed:
- A lookup of postcode to LSOA 2021 to PNA locality to Local Authority. Requires the following columns: postcode, lsoa_2021_code, pna_locality, local_authority. Note that the postcodes should have no spaces.
- Original, unedited file from the SW Hub with a list of pharmacies, their services and hours
- Original, unedited file from SW Hub with dispensing practices

STEPS:
1. Clone GitHub repo
2. Place files listed above in the "Data" folder
3. Open script "data_prep.R"
4. If new pharmacy data has been received, edit line 27 "pharm_data_month" with the month and year of data, and line 37 with the number of pharmacies in the South West for that month.
5. Replace file names at lines 78 (pc_lsoa_loc_lookup), 88 (pharmacies), 100 (disp_prac)
6. Optional: Edit colour palette for charts at line 40 and ggplot chart theme at line 43 (charts are also formatted directly in the Quarto script "quarto_profile.qmd").
7. Optional: Edit Word reference document to match your template (ref_doc.xlsx)
8. Optional: If you wish to run with more recent population data, overwrite the ONS extracts "mype_lsoa" and "mype_la_sw_england" in the data folder, and update years at lines 30 and 31.
9. Open render_code.R script, edit code chunk 20 to be the list of your PNA localities (names must be identical to names in lookup)
10. Run render_code :)

NOTE: This process will only work if PNA localities align to 2021 LSOAs.

NOTE 28/05/2025: Significant changes to dispensing data table and dispensing data download process. If repo was cloned, ensure latest changes are pulled through.
