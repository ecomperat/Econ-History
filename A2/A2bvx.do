
clear
cd "C:\Users\etien\OneDrive\Documents\Github\Econ-History\A2"

// Step 1: Load the data and rename columns with indices
import excel "C:\Users\etien\OneDrive\Documents\GitHub\Econ-History\A2\bvx.xlsx", sheet("TABLE 6 - BVX crisis list") cellrange(A2) clear firstrow

// Step 1: List and examine variable labels to identify columns with the same label
describe

// Step 2: Use a loop to merge variables based on their labels
// Define a list of unique labels for variables to merge
local labels "Country" "BVX starting year" "Bank equity return" "Panic banking crisis" "Bank equity crisis"

foreach lbl in `labels' {
    // Generate a new variable to store merged values
    gen `lbl'_merged = .
    
    // Loop over each variable with the matching label and merge non-missing values
    foreach var of varlist * {
        if "`: variable label `var''" == "`lbl'" {
            replace `lbl'_merged = `var' if missing(`lbl'_merged) & !missing(`var')
        }
    }
}

// Step 3: Drop original columns after merging to avoid duplicates
drop Country BVX_starting_year Bank_equity_return Panic_banking_crisis Bank_equity_crisis


