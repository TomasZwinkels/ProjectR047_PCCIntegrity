# R047: PCC Data Integrity Analysis

A comprehensive R script for validating and analyzing data integrity in the Political Career Codebook (PCC) dataset, with specialized tools for detecting overlapping political episodes and suspicious date patterns.

## 📊 What This Does

This project provides:

- **Data Integrity Validation**: Comprehensive checks for parliamentary membership data consistency
- **Overlap Detection**: Identify fully overlapping, nearly overlapping, and partially overlapping membership episodes
- **Date Validation**: Detect suspicious start/end dates and formatting issues
- **Gap Analysis**: Find episodes that are suspiciously close together
- **Episode Merging**: Automatically merge overlapping parliamentary membership periods
- **Swiss Chamber Analysis**: Special handling for Swiss parliamentary chamber data (Nationalrat/Ständerat)

## 🎯 Key Features

### Data Quality Checks
- **Unique ID Validation**: Ensure all resume entry IDs are unique
- **Cross-Reference Validation**: Verify all person IDs exist in politician database
- **Date Format Validation**: Check date formatting and logical consistency
- **Overlap Detection**: Multiple algorithms for different types of overlaps

### Swiss Parliament Specialization
- **Chamber Classification**: Automatically classify Nationalrat (NR) vs Ständerat (SR) entries
- **Political Function Updating**: Generate corrected political function codes for Swiss chambers
- **Consistency Checking**: Validate chamber information against raw text descriptions

### Episode Processing
- **Intelligent Merging**: Combine overlapping membership periods while preserving data integrity
- **Gap Detection**: Identify suspiciously short gaps between episodes
- **Export Generation**: Create formatted exports for data corrections

## 🚀 Getting Started

### Prerequisites

Required R packages:
```r
install.packages(c(
  "sqldf", "stringr", "lubridate", "readr", "dplyr", 
  "writexl", "openxlsx", "testthat"
))
```

### Data Requirements

**Data Source Setup:**
The script now uses a centralized data repository. You need to:

1. **Clone the data repository:**
   ```bash
   git clone https://github.com/TomasZwinkels/PCCdata.git
   ```

2. **Update data paths in the script:**
   The script currently expects data files at `/home/tomas/projects/PCCdata/`. 
   **You must edit these paths in `R047.R` to match your local setup:**
   
   ```r
   # Lines to update in R047.R:
   POLI = read.csv("/path/to/your/PCCdata/POLI.csv", header = TRUE, sep = ";")
   RESE = read.csv("/path/to/your/PCCdata/RESE.csv", header = TRUE, sep = ";")
   PARL = read.csv("/path/to/your/PCCdata/PARL.csv", header = TRUE, sep = ";")
   ```

**Required data files from PCCdata repository:**
- `POLI.csv` - Politician information
- `RESE.csv` - Resume/membership episodes (main focus)
- `PARL.csv` - Parliament period information

## 🏃‍♂️ Running the Analysis

### Configuration
Set your country code at the top of the script:
```r
country_code <- "CH"  # Options: "NL" (Netherlands), "CH" (Switzerland)
```

### Basic Usage
```bash
Rscript R047.R
```

### What Happens
1. **Data Loading**: Import PCC datasets with integrity validation
2. **Swiss Chamber Processing**: (If CH selected) Analyze and export chamber corrections
3. **Date Preprocessing**: Standardize and validate all date formats
4. **Overlap Detection**: Run multiple overlap detection algorithms
5. **Episode Merging**: Generate merged episodes for overlapping periods
6. **Gap Analysis**: Identify suspicious gaps between episodes
7. **Suspicious Date Detection**: Flag potentially problematic dates
8. **Export Generation**: Create Excel files with corrections and analysis

## 🔧 Configuration

### Country Selection
Currently supports:
- `"NL"` - Netherlands analysis
- `"CH"` - Switzerland analysis (with special chamber handling)

### Detection Thresholds
Modify various thresholds in the script:
- **Near overlap threshold**: 2 days (for nearly identical episodes)
- **Gap detection**: 1-3 day gaps between episodes
- **Suspicious dates**: 3 days from parliament start/end dates

## 📁 Project Structure

```
ProjectR047_PCCIntegrity/
├── R047.R                    # Main analysis script
├── R047_functions.R          # Core validation functions
├── R047_unittests.R          # Function tests
├── R047_PARL_functions.R     # Parliament-specific functions
├── R047_PARL_unittests.R     # Parliament function tests
├── R047_RESE_functions.R     # Resume episode functions
├── R047_RESE_unittests.R     # Resume function tests
├── RESE_chamber_updates.csv  # Swiss chamber correction export
└── IMPORT_MERGED_*.xlsx      # Generated merge corrections

External Dependencies:
└── PCCdata/                  # Centralized data repository (clone separately)
    ├── POLI.csv
    ├── RESE.csv
    └── PARL.csv
```

## 🔍 Key Functions

### `preprocess_RESEdates(RESE)`
Standardizes date formats and creates POSIXct date columns.

### `check_RESE_parlmemeppisodes_anyfulloverlap(RESE)`
Returns TRUE if any fully overlapping episodes are found (same person, same dates).

### `check_RESE_anynear_fulloverlap(RESE)`
Returns TRUE if any nearly overlapping episodes are found (within 2 days).

### `merge_episodes(RESE, person_id)`
Intelligently merges overlapping episodes for a specific person.

### `find_gap_episodes(RESE, min_gap, max_gap)`
Finds episodes with suspicious gaps between them.

### `find_suspicious_start_dates(RESE, PARL, threshold_days)`
Identifies start dates that are suspiciously close to parliament period boundaries.

## 🧪 Testing

The project includes comprehensive unit tests:
- **Core functions**: `R047_unittests.R`
- **Parliament functions**: `R047_PARL_unittests.R`  
- **Resume functions**: `R047_RESE_unittests.R`

Run tests independently:
```r
source("R047_functions.R")
testthat::test_file("R047_unittests.R")
```

## 📈 Output

The script generates:
- **Console validation results**: Pass/fail status for all integrity checks
- **RESE_chamber_updates.csv**: Corrections for Swiss chamber classifications
- **IMPORT_MERGED_*.xlsx**: Excel files with merged episode corrections
- **Suspicious date reports**: Identified problematic dates for review

## 🏛️ Specialized Features

### Swiss Parliament Handling
- Automatically detects Nationalrat (NR) vs Ständerat (SR) based on parliament IDs
- Cross-validates chamber assignment against raw text descriptions
- Generates corrected political function codes for import

### Data Quality Assurance
- Multiple overlap detection algorithms catch different types of issues
- Comprehensive date validation prevents temporal inconsistencies
- Export generation facilitates systematic data corrections

## 🤝 Contributing

This is an academic research project focused on political career data integrity. The codebase includes extensive validation and testing to ensure reliability of political science research.

## 📝 Notes

- **Multi-country support**: Currently handles Netherlands and Switzerland data
- **Swiss specialization**: Special handling for bicameral parliament structure
- **Data-driven corrections**: Generates systematic correction files rather than making direct changes
- **Comprehensive validation**: Multiple complementary approaches to catch different types of data issues