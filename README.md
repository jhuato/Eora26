# Eora26 / Deeper Structures

## Purpose

For industry $i, j \in [1, 26]$, country $k \in [1, ]$, and year $t \in [1990, 2015]$, to extract, plot, and analyze time series (1990-2015) of
- the rate of surplus value, $\sigma \equiv \frac{\Pi}{W}$,
- the capital composition, $h \equiv \frac{\sum_{j=1}^{26} q_{j,i}}{W_i}$, 
- the "capital productivity," $\gamma \equiv \frac{Y}{\sum_{j=1}^{26} q_{j,i}}$, and
- the profit rate, $r \equiv \frac{\Pi}{\sum_{j=1}^{26} q_{j,i}}$.

## Data

- Eora26_year_bp_T.txt: The transactions table contains, for each country $k \in [1, 160]$ and year $t \in [1990, 2015]$, the intermediate supplies of each industry $i$ to industry $j$: $q_{i,j}$, for $i, j \in [1, 26]$.
- Eora26_year_bp_VA.txt: The value added table contains, for each industry $i \in [1, 26]$, country $k \in [1, 160]$ and year $t \in [1990, 2015]$,
  - Wages ("Compensation of employees D.1"), 
  - Prod taxes ("Taxes on production D.29"), 
  - Prod subsidies ("Subsidies on production D.39"), 
  - Profits ("Net operating surplus B.2n"), 
  - Net mixed income ("Net mixed income B.3n"), and
  - Depreciation ("Consumption of fixed capital K.1").
- Eora26_year_bp_FD.txt: The final demand table contains, for each industry $i \in [1, 26]$, country $k \in [1, 160]$ and year $t \in [1990, 2015]$,
  - Household final consumption P.3h,
  - Non-profit institutions serving households P.3n,
  - Government final consumption P.3g,
  - Gross fixed capital formation P.51,
  - Changes in inventories P.52, and
  - Acquisitions less disposals of valuables P.53.



Source: https://worldmrio.com/eora26/
