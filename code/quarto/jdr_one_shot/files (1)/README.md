# Jury Data Report - Quarto Template

This Quarto project generates the annual Jury Data Report (JDR) memo for the Judicial Council of California.

## Installation Requirements

### 1. R (version 4.1+)
Download from: https://cran.r-project.org/

### 2. RStudio (recommended, but optional)
Download from: https://posit.co/download/rstudio-desktop/

### 3. Quarto
Download from: https://quarto.org/docs/get-started/

### 4. LaTeX Distribution
You need a LaTeX distribution to render PDFs. The easiest option is TinyTeX:

```r
# In R console:
install.packages("tinytex")
tinytex::install_tinytex()
```

Alternatively, you can use:
- **Windows**: MiKTeX (https://miktex.org/)
- **Mac**: MacTeX (https://www.tug.org/mactex/)
- **Linux**: TeX Live (`sudo apt install texlive-full` on Ubuntu)

### 5. R Packages
Run this in R to install all required packages:

```r
install.packages(c(
  "tidyverse",
  "scales",
  "knitr",
  "kableExtra"
))
```

## Project Structure

```
jdr_report/
├── jdr_fy2425_report.qmd    # Main Quarto document
├── data/
│   └── jdr_data.csv         # Your jury data (replace with actual data)
└── README.md                # This file
```

## Usage

### Rendering the Report

**Option 1: Command line**
```bash
cd jdr_report
quarto render jdr_fy2425_report.qmd
```

**Option 2: RStudio**
1. Open the .qmd file in RStudio
2. Click the "Render" button (or press Cmd/Ctrl + Shift + K)

**Option 3: R console**
```r
quarto::quarto_render("jdr_fy2425_report.qmd")
```

### Customizing for Different Fiscal Years

The report uses parameters defined in the YAML header. To change the fiscal year:

1. Edit the `params` section at the top of the .qmd file:
```yaml
params:
  fiscal_year: "2025-26"  # Change this
  data_path: "data/jdr_data.csv"
```

2. Update your data file with the new year's data

### Data Requirements

Your CSV file should include these columns (at minimum):

**Core identifiers:**
- `reporting_period` or `end_year` - Fiscal year
- `county` - Court/county name
- `beg_date`, `end_date` - Reporting period dates

**Summons and yield variables:**
- `summons` - Summonses sent
- `postin` - Postponed in from prior period
- `undel` - Undeliverable
- `fta` - Failure to appear
- `excused` - Excused
- `disqual` - Disqualified
- `dismiss_peace` - Dismissed (peace officer)
- `dismiss_dead` - Dismissed (deceased)
- `postout` - Postponed out

**Utilization variables:**
- `oncall` - On-call (not told to report)
- `jurors_sworn` - Sworn jurors
- `rel_challenge` - Released by challenge
- `rel_hardship` - Released for hardship
- `rel_perempt` - Peremptory challenge
- `not_reached` - Not reached in selection

**AAPE columns (for data confidence):**
- `potentially_available_aape`
- `tqa_aape`
- `excused_aape`
- (and other `*_aape` columns)

## Troubleshooting

### "LaTeX failed to compile"
- Make sure TinyTeX or another LaTeX distribution is installed
- Try running `tinytex::tlmgr_update()` in R to update packages
- Check that all required LaTeX packages are available

### "Package not found"
- Run the package installation commands again
- Check that you're using a recent version of R

### Charts not rendering
- Ensure all data columns exist and have numeric values
- Check for NA values in key metrics

### Colors or fonts look wrong
- The template uses standard LaTeX fonts; no special fonts required
- Colors are defined as hex values and should work universally

## Color Palette

The report uses these colors (defined in `jc_colors`):
- Blue: `#73B3E7` - Primary positive/target met
- Gold: `#C2850C` - Secondary/target not met
- Grey: `grey50` - Neutral/annotations
- Black: `black` - Text/markers
- Red: `#D32F2F` - Warnings/thresholds

## Support

For questions about the JDR or this template, contact the Research, Analytics, and Data Unit.
