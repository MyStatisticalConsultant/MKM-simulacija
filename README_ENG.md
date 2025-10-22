# Simulation FMM — Election Forensics Playground (Shiny)

An interactive Shiny app for **election forensics** built around **Walter Mebane, Jr.’s finite mixture model (FMM)**.
The app lets you **simulate precinct-level election data** under a wide range of scenarios—from clean elections 
to **extreme fraud**—by manipulating the **Beta distribution parameters** that govern turnout and the winner’s 
vote share in each mixture component. You can then **inspect numeric summaries and visual diagnostics** in 
both the **original space** (turnout, winner’s votes) and the **logit space**, estimate an FMM on the simulated data, 
and **export** the results (including a **fraud report**, **FMM estimates**, and the **raw simulated data**) 
for analysis with your preferred tools.

> Live app: **https://statisticar-mkm-simulacija.share.connect.posit.cloud**  
> Feedback: **Zlatko@MyStatisticalConsultant.com**

## Why this app?

Researchers, analysts, journalists, and anyone curious about **election integrity** can “play” with assumptions 
in a principled way:

- **Learn FMM mechanics** by seeing how mixture weights and Beta(α, β) parameters shape precinct outcomes.
- **Stress-test forensic visuals** across scenarios: clean, mild irregularities, ballot-stuffing patterns, 
vote-shaving patterns, or **extreme fraud**.
- **Bridge to workflows** you already use: export tabular results and **CSV** data for follow-up in R, 
Python, or specialized forensics packages.

The app’s backbone is the **FMM** approach (Walter Mebane, Jr.), which **models precinct outcomes as a mixture** 
of latent regimes (e.g., regular vs. fraudulent) with **component-specific Beta distributions** for turnout 
and the winner’s vote share.

## What you can do

- **Set simulation parameters**
   - For each component, set **Beta distribution parameters (α, β)** for:
      - **Turnout** (as a proportion)
      - **Winner’s vote share** (conditional on turnout)
   - Specify **mixture weights** (share of precincts per component) and **number of precincts** to simulate.
   - Optional knobs for **fraud intensity/shape** if you want to emulate stylized ballot stuffing or vote transfer patterns.
- **Run the simulation and inspect**:
   - **Tabular summaries** (component-wise and overall)
   - **Plots in original space**: scatter/density/heatmaps of turnout vs. winner’s votes, marginals, ECDFs, etc.
   - **Plots in logit space**: transform turnout and winner’s share to logits to reveal linear/compositional 
   structures typical in FMM-based diagnostics.
- **Export**
   - **raud report** (key diagnostics + narrative summary)
   - **FMM estimates** (parameters, standard errors where available)
   - **Raw simulated data** in **CSV** for downstream analysis

## App structure (at a glance)

- **Sidebar controls**
   - Simulation size & random seed
   - Mixture weights
   - **Beta(α, β)** parameters for **turnout** and **winner’s vote** per component
   - Optional fraud-specific controls (e.g., max stuffing rate)
   - Run/Reset buttons
   - Download buttons (Report, Estimates, Data)
- **Main panels**
   1. **Overview** — your selected parameters & quick stats
   2. **Plots (original space)** — turnout vs. winner’s votes and marginals
   3. **Plots (logit space)** — same variables after logit transform
   4. **Tables** — summaries by component, quantiles, cross-checks
   5. **FMM estimation** — fitted mixture results
   6. **Fraud report** — narrative + key metrics; downloadable as Word
   7. **Data** — preview + export of the simulated dataset

> **Tip**: Use the sliders and inputs to move from a “regular election” baseline 
to controlled **fraud scenarios**. Watch how diagnostics and FMM recovery change 
as you tweak the **Beta** shapes and **mixture weights**.

## Downloads

- **Fraud report**: Word (`.docx`) document combining your parameters, visuals, and summary interpretation.
- **FMM estimates**: Component weights and distribution parameters (with diagnostics).
- **Raw simulation data (CSV)**: Precinct-level turnout and winner’s votes (and any derived variables); 
bring this into other forensic tools.

## Run locally

### 1) Clone the repository

```bash
git clone https://github.com/MyStatisticalConsultant/MKM-simulacija.git
cd MKM-simulacija
```
### 2) Open in RStudio

- Open `app.R` (or the RStudio project file if included).
- Review `DESCRIPTION` / `renv.lock` if present; otherwise install the packages below.

### 3) Install packages

```r
install.packages(c(
  "shiny", "shinydashboard",
  "ggplot2", "scales", "gridExtra",
  "mixtools",     # or your chosen mixture / optimization tooling
  "stats4",       # MLE helpers if used
  "readr",        # if CSV export/import
  "dplyr", "tidyr",
  "kableExtra", "knitr", "rmarkdown"
))
```

> The exact list may differ slightly depending on your repo’s `DESCRIPTION`. Ako koristite **renv**, 
run `renv::restore()` instead.

### 4) Run the app

```r
shiny::runApp()    # or click "Run App" in RStudio
```

A local URL (e.g., `http://127.0.0.1:xxxx`) will open in your browser.

## Usage in a nutshell

1. Set **Beta(α, β)** for turnout and winner’s share in each component.
2. Choose **mixture weights** i **number of precincts**.
3. Click **Run simulation**.
4. Explore **plots** (original and logit space) and **tables**.
5. Click **Estimate FMM** to fit a mixture to your simulated data.
6. **Download**: fraud report, FMM estimates, and raw data.

## Notes & assumptions

- **Mebane FMM**: The model frames precinct outcomes as draws from a **finite 
mixture** of latent regimes with distinct **Beta** shapes for turnout and winner’s 
vote share. The app simulates from these distributions and allows you to fit 
a mixture back to the synthetic data.
- **Identifiability**: With extreme parameter overlap or small sample sizes, 
components can be hard to recover—this is a feature, not a bug; it’s part of 
the forensic learning experience.
- **Logit space**: Many forensics diagnostics are easier to interpret after 
logit transforming proportions; the app shows both spaces side-by-side.

## Citing

If you use this tool in research, please cite **Walter R. Mebane, Jr.’s** work on 
finite mixture models for election forensics, and this repository (include the 
commit or release tag).

## Feedback & issues

- Live app: https://statisticar-mkm-simulacija.share.connect.posit.cloud
- Email: Zlatko@MyStatisticalConsultant.com

Please include:

- a short description of your parameters,
- the number of simulated precincts,
- and a screenshot or the exported CSV if you’re reporting a bug.

## Acknowledgments 

- **Walter R. Mebane, Jr.** for foundational work on **finite mixture models** in election forensics.
- Contributors and the open-source community whose packages power this app.

## License

This project is licensed under the **MIT License** — see the [LICENSE](LICENSE).

<sub>© 2025 Zlatko J. Kovačić</sub>