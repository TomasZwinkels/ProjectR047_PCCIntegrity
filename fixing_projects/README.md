# Fixing Projects

This folder contains scripts that generate data fixes for the central PCC database, along with their output files.

## Fix Scripts

### `CH_chamber_function_fixes.R`
- **Purpose**: Generate corrected political function codes for Swiss parliamentary chambers
- **Output**: `CH_RESE_chamber_updates_[timestamp].csv`
- **Use case**: Fix Nationalrat (NR) vs Ständerat (SR) chamber classifications in Swiss data

### `overlapping_episodes_fixes.R` 
- **Purpose**: Generate merged episodes for overlapping parliamentary membership periods
- **Output**: `[COUNTRY]_IMPORT_MERGED_EPISODES_[timestamp].xlsx`
- **Use case**: Resolve fully overlapping parliamentary episodes for any country
- **Configuration**: Set `country_code` variable ("NL", "CH", etc.)

## Output Files

All generated fix files are saved in this folder with timestamps to track different versions:
- CSV files for simple updates
- Excel files for complex episode mergers
- Timestamped filenames prevent overwrites

## Usage

1. **Run integrity checks** first using `../R047_streamlined.R` or country deepdive scripts
2. **Identify issues** that need fixing
3. **Run appropriate fix script** from this folder
4. **Import generated files** into central database
5. **Re-run integrity checks** to validate fixes

## Notes

- All scripts reference parent directory functions via `../R047_*_functions.R`
- Fix scripts are self-contained and can be run independently
- Output files are ready for central database import
- Keep original data files as backup before applying fixes