*** Assignment: Crisis prediction and crisis cost***
cd "C:\Users\etien\OneDrive\Documents\Github\Econ-History\A2"


***************************************************
*** PART A ***
***************************************************

clear
use JSTdatasetR6, clear

*i. Logit model estimation

// Step 1: Ensure the dataset is set up for panel data
xtset ifs year  // Replace with your dataset's panel identifiers

gen tcred = tloans + thh + tbus

// Step 2: Generate five lags of the debt-to-GDP ratio
foreach i of numlist 1/5 {
    gen tcred_L`i' = L`i'.tcred
}

// Step 3: Logit model with country fixed effects for JST crisis prediction
xtlogit crisisJST tcred_L1 tcred_L2 tcred_L3 tcred_L4 tcred_L5, fe

// Test if the five lags jointly contribute to crisis prediction (significance test for β)
testparm tcred_L1 tcred_L2 tcred_L3 tcred_L4 tcred_L5

**NEED TO FIND BVX DATES
clear
// Import BVX dates from: https://www.quantfinhistory.org/data
import excel "C:\Users\etien\OneDrive\Documents\GitHub\Econ-History\A2\bvx.xlsx", sheet("TABLE 6 - BVX crisis list") cellrange(A2) firstrow

// Define the variables you want to rename in the desired order
local vars_c Country G L R W AB
local vars_bvx BVXstartingyear H M S x AC
local vars_br Bankequityreturn I N T Y AD
local vars_p Panicbankingcrisis J O U Z AE
local vars_bc Bankequitycrisis K P V AA AF

// Initialize an index counter
local i = 1

// Loop through each variable and rename it with the indexed format
foreach var of local vars_c {
    rename `var' country_`i'
    local i = `i' + 1
}
	
foreach var of local vars_bvx {
    rename `var' BVXstartingyear_`i'
    local i = `i' + 1
}

foreach var of local vars_br {
    rename `var' Bankequityreturn_`i'
    local i = `i' + 1
}


foreach var of local vars_p {
    rename `var' Panicbankingcrisis_`i'
    local i = `i' + 1
}

foreach var of local vars_bc {
    rename `var' Bankequitycrisis_`i'
    local i = `i' + 1
}





// Step 4: Repeat the analysis for the BVX crisis variable
*xtlogit BVX_crisis tcred_L1 tcred_L2 tcred_L3 tcred_L4 tcred_L5, fe

// Step 5: Create the 5-year change in the debt-to-GDP ratio as an additional predictor
*gen tcred_5yr_change = D5.tcred

// Step 6: Estimate the logit model including the 5-year change as a predictor
*xtlogit crisisJST tcred_L1 tcred_L2 tcred_L3 tcred_L4 tcred_L5 tcred_5yr_change, fe




***************************************************
*** PART B ***
***************************************************

