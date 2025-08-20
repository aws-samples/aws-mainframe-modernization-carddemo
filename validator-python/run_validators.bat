@echo off
REM CardDemo Validator - Run All Validators
REM This batch file runs all validators with automatic environment activation

echo.
echo ========================================
echo CardDemo Validator - Run All Validators
echo ========================================
echo.

REM Check if we're in the right directory
if not exist "run_all_validators.py" (
    echo ERROR: run_all_validators.py not found!
    echo Please run this script from the validator-python directory.
    echo.
    pause
    exit /b 1
)

REM Check if virtual environment exists
if not exist "carddemo-validator-env\Scripts\activate.bat" (
    echo ERROR: Virtual environment not found!
    echo Please run setup_env.bat first to create the environment.
    echo.
    pause
    exit /b 1
)

REM Activate virtual environment
echo Activating virtual environment...
call carddemo-validator-env\Scripts\activate.bat

if errorlevel 1 (
    echo ERROR: Failed to activate virtual environment!
    echo.
    pause
    exit /b 1
)

echo Virtual environment activated!
echo.

REM Check if requirements are installed
echo Checking required packages...
pip list | findstr /i "deepeval pandas numpy matplotlib json5" >nul 2>&1
if errorlevel 1 (
    echo WARNING: Some required packages may not be installed.
    echo Installing requirements...
    pip install -r requirements.txt
    
    if errorlevel 1 (
        echo ERROR: Failed to install requirements!
        echo.
        pause
        exit /b 1
    )
)

echo All required packages are available.
echo.

REM Run all validators
echo Starting validation process...
echo.

python run_all_validators.py

REM Check if validation was successful
if errorlevel 1 (
    echo.
    echo ========================================
    echo Validation completed with errors!
    echo ========================================
    echo.
    echo Check the output above for details.
    echo Reports may have been generated despite errors.
    echo.
) else (
    echo.
    echo ========================================
    echo Validation completed successfully!
    echo ========================================
    echo.
    echo Check the reports/ directory for generated reports.
    echo.
)

echo Press any key to continue...
pause >nul

REM Deactivate virtual environment
echo.
echo Deactivating virtual environment...
deactivate

echo Environment deactivated.
echo.

pause
