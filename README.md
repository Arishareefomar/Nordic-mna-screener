# Nordic Industrial M&A Target Screener

This project screens listed Nordic industrial companies for potential M&A targets using Bloomberg data and R.
The model ranks companies across valuation, profitability, growth, leverage, and market signal factors to identify the most attractive acquisition candidates for a financial or strategic buyer.
Built as part of my self-directed learning in applied finance and R programming. Tools used: R, Bloomberg Terminal, dplyr, ggplot2, knitr.

##Key Findings

**Preferred Target: Veidekke ASA (M&A Score: 0.751)**
Veidekke ranks as the strongest candidate among the selected construction and infrastructure-related companies.
It trades at 6.35x EV/EBITDA and 0.46x EV/Sales, offering an attractive entry multiple relative to the filtered Nordic industrials universe.
Its ROIC of 32.22% is the highest among the three selected targets and the most compelling argument for the case, it suggests strong capital discipline in a sector where project execution and capital allocation are key value drivers.
Revenue growth of 6.84% is positive and supports the view that Veidekke is not a pure value trap.
A net debt/EBITDA of -1.38x means the company holds a net cash position, reducing balance sheet risk and making it more attractive to both financial and strategic buyers.
The main concern is its EBITDA margin of 2.99%, which is the lowest among the three targets and reflects the thin-margin nature of construction contracting.
This warrants further due diligence on project execution risk, cost sensitivity, and margin sustainability.

**AF Gruppen ASA (M&A Score: 0.715)**
AF Gruppen trades at a higher EV/EBITDA of 8.82x, making it the most expensive of the three targets on an earnings-based valuation measure.
However, it scores well on growth, with revenue growth of 11.91%, and maintains a solid EBITDA margin of 5.99% — nearly double that of Veidekke.
Its ROIC of 20.35% is strong, though below Veidekke. The net cash position (-0.63x net debt/EBITDA) adds further balance sheet comfort.
AF Gruppen is a credible secondary candidate where the higher multiple is partly justified by better margin quality and stronger growth momentum.

**Per Aarsleff Holding (M&A Score: 0.652)**
Per Aarsleff trades at the lowest EV/EBITDA multiple of the three (6.14x) and shows the strongest revenue growth at 15.37%.
Its EBITDA margin of 8.66% is also the highest among the selected targets, suggesting better cost discipline or a more favourable project mix.
However, its ROIC of 11.02% is significantly lower than Veidekke and AF Gruppen, indicating weaker returns on invested capital despite solid margins.
A slightly positive net debt/EBITDA of 0.27x is manageable but less attractive than the net cash positions of the other two companies.
Per Aarsleff ranks third in the model, primarily due to the weaker capital efficiency signal.

## Scoring Framework
Valuation (30%) - EV/EBITDA, EV/Sales
Profitability (25%) - EBITDA margin, net income margin, ROE, ROIC
Growth (20%) - Revenue growth, EBITDA growth, EPS growth
Leverage (15%) - Net debt / EBITDA
Market signal (10%) - Distance from 52-week high

Valuation was given the highest weight because entry multiple is the primary driver of acquisition attractiveness at the screening stage.
Profitability, growth, leverage, and market signal were included to avoid selecting companies that appear cheap only due to weak fundamentals or excessive risk.

## Limitations
Quantitative screen only. The model ranks companies on financial metrics and cannot capture strategic fit, ownership structure, regulatory constraints, or management quality.
These would be assessed in a full due diligence process.

Percentile-based scores are relative, not absolute. Scores reflect each company's position within the filtered peer universe.
If the universe changes, the scores change, not because the company changed, but because the comparison group did.

Low EBITDA margin requires sector context. Veidekke's 2.99% EBITDA margin looks weak in isolation but is partly structural in construction and contracting businesses.
Direct comparisons across sub-sectors would be misleading.

No DCF or transaction analysis. The model does not include a discounted cash flow valuation, transaction premium analysis, or deal feasibility assessment.
It is an initial screen and not a final recommendation.

Bloomberg data dependency. Results depend on the accuracy and completeness of the Bloomberg Terminal export. Raw data is not included due to licensing restrictions.

## How to Run
1. Clone or download this repository
2. Open RStudio and navigate to the project folder
3. Run requirements.R once to install all required packages
4. Open and run NM_A.R to produce the scored universe and output files
5. Open Report.Rmd in RStudio and knit to HTML to generate the final report


