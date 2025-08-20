# CardDemo Python Validator

This directory contains the Python-based validation tools for the CardDemo mainframe modernization project.

## Directory Structure

```
validator-python/
├── src/                    # Python source code
│   ├── requirements_validator.py
│   ├── deepeval_validator.py
│   ├── compare_validators.py
│   ├── generate_reports.py
│   └── generate_deepeval_reports.py
├── docs/                   # Documentation
│   ├── CardDemo_Requirements_Validation_Report.md
│   ├── DeepEval_Migration_Summary.md
│   ├── Validation Determinations.md
│   ├── Validator Breakdown.md
│   ├── README_Requirements_Validator.md
│   ├── RegEx Validator Overview.md
│   ├── CardDemo_Mermaid_Diagrams.md
│   ├── CardDemo_Simple_Diagrams.md
│   ├── CardDemo_Dataset_Catalog.md
│   └── CardDemo_Application_Flow.md
├── data/                   # Data files and datasets
│   └── CD-Requirements.json
├── reports/                # Generated validation reports
├── test/                   # Test scripts and results
├── carddemo-validator-env/ # Python virtual environment
├── requirements.txt        # Python dependencies
├── run_all_validators.py   # Script to run all validators
├── setup_env.bat          # Windows: Setup virtual environment
├── start_env.bat          # Windows: Activate environment
├── stop_env.bat           # Windows: Deactivate environment
├── run_validators.bat     # Windows: Run all validators with auto-activation
├── setup_env.sh           # Linux/Mac: Setup virtual environment
└── start_env.sh           # Linux/Mac: Activate environment
```

## Components

### Source Code (`src/`)

- **requirements_validator.py**: Main requirements validation logic
- **deepeval_validator.py**: DeepEval-based validation implementation
- **compare_validators.py**: Comparison utilities for different validation approaches
- **generate_reports.py**: Report generation utilities
- **generate_deepeval_reports.py**: DeepEval-specific report generation

### Documentation (`docs/`)

- Comprehensive documentation of the validation process
- Requirements specifications and validation reports
- Application flow diagrams and data models

### Data (`data/`)

- Input datasets for validation
- Configuration files
- Reference data
- **CD-Requirements.json**: Main requirements dataset for validation

### Reports (`reports/`)

- Generated validation reports
- Output files from validation runs
- Analysis results

### Tests (`test/`)

- Test scripts for validation components
- Test data and expected results
- Validation test suites

## Setup and Usage

### Quick Start (Recommended)

**Windows:**

```bash
cd validator-python
setup_env.bat          # One-time setup
start_env.bat          # Activate environment
python run_all_validators.py  # Run validators
stop_env.bat           # Deactivate when done
```

**Linux/Mac:**

```bash
cd validator-python
chmod +x *.sh          # Make scripts executable (first time)
./setup_env.sh         # One-time setup
source start_env.sh    # Activate environment
python run_all_validators.py  # Run validators
deactivate             # Deactivate when done
```

### Detailed Setup Instructions

#### 1. **Initial Environment Setup**

**Windows:**

```bash
cd validator-python
setup_env.bat
```

This script will:

- Check Python installation
- Create virtual environment (`carddemo-validator-env`)
- Install all required packages
- Display setup summary

**Linux/Mac:**

```bash
cd validator-python
chmod +x setup_env.sh
./setup_env.sh
```

#### 2. **Environment Management**

**Windows:**

```bash
# Activate environment
start_env.bat

# Deactivate environment
stop_env.bat

# Run all validators with auto-activation
run_validators.bat
```

**Linux/Mac:**

```bash
# Activate environment
source start_env.sh

# Deactivate environment
deactivate
```

#### 3. **Running Validators**

**Option 1: One-click validation (Windows):**

```bash
run_validators.bat
```

**Option 2: Manual execution:**

```bash
# Activate environment first
start_env.bat          # Windows
source start_env.sh    # Linux/Mac

# Run all validators
python run_all_validators.py

# Or run individual validators
cd src
python requirements_validator.py
python deepeval_validator.py
python generate_reports.py
python generate_deepeval_reports.py
python compare_validators.py
```

**Option 3: Manual environment management:**

```bash
# Windows
carddemo-validator-env\Scripts\activate
pip install -r requirements.txt
python run_all_validators.py
deactivate

# Linux/Mac
source carddemo-validator-env/bin/activate
pip install -r requirements.txt
python run_all_validators.py
deactivate
```

## Environment Management Scripts

### Windows Batch Files (`.bat`)

- **`setup_env.bat`**: Creates and configures the virtual environment

  - Checks Python installation
  - Creates `carddemo-validator-env` directory
  - Installs all required packages
  - One-time setup script

- **`start_env.bat`**: Activates the virtual environment

  - Opens a new command prompt with environment active
  - Checks package installation
  - Provides usage instructions

- **`stop_env.bat`**: Deactivates the virtual environment

  - Safely deactivates the environment
  - Returns to system Python

- **`run_validators.bat`**: One-click validation runner
  - Automatically activates environment
  - Runs all validators
  - Deactivates environment when complete

### Linux/Mac Shell Scripts (`.sh`)

- **`setup_env.sh`**: Creates and configures the virtual environment

  - Same functionality as Windows version
  - Uses `python3` command

- **`start_env.sh`**: Activates the virtual environment
  - Starts a new shell session with environment active
  - Provides usage instructions

## Dependencies

See `requirements.txt` for the complete list of Python dependencies.

## Troubleshooting

### Common Issues

**Virtual environment not found:**

```bash
# Run setup script first
setup_env.bat    # Windows
./setup_env.sh   # Linux/Mac
```

**Python not found:**

- Install Python 3.8 or higher
- Ensure Python is in your system PATH

**Package installation fails:**

```bash
# Activate environment and install manually
start_env.bat    # Windows
source start_env.sh  # Linux/Mac
pip install -r requirements.txt
```

**Permission denied (Linux/Mac):**

```bash
# Make scripts executable
chmod +x *.sh
```

### Environment Information

- **Python Version**: 3.8 or higher
- **Virtual Environment**: `carddemo-validator-env`
- **Key Packages**: deepeval, pandas, numpy, matplotlib, json5
- **Working Directory**: Must be `validator-python/`

## Contributing

When adding new validation components:

1. Place source code in the `src/` directory
2. Add tests to the `test/` directory
3. Update documentation in the `docs/` directory
4. Update this README if the structure changes
5. Test environment scripts if you modify the setup process
