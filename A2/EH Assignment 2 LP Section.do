clear all

cap cd "C:\Users\etien\OneDrive\Documents\GitHub\Econ-History\A2"

use JSTdatasetR6.dta

merge 1:1 iso year using RecessionDummies.dta, keep(3) nogen

replace N = 0 if missing(N)

replace F = 0 if missing(F)

gen lgdp = log(rgdpmad/(cpi*pop))*100 // Produce log(real gdp per capita)*100

lpirf lgdp N F, step(5) exog(N F)

irf set "graph.irf", replace

irf create graph, replace

irf graph irf, irf(graph) impulse(N F) response(lgdp)

count if N == 1
count if F == 1




