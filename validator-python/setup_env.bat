@echo off
REM CardDemo Validator - Environment Setup
REM This batch file creates and sets up the Python virtual environment

echo.
echo ========================================
echo CardDemo Validator Environment Setup
echo ========================================
echo.

REM Check if Python is available
python --version >nul 2>&1
if errorlevel 1 (
    echo ERROR: Python is not installed or not in PATH!
    echo Please install Python 3.8 or higher and try again.
    echo.
    pause
    exit /b 1
)

echo Python found: 
python --version
echo.

REM Check if we're in the right directory
if not exist "requirements.txt" (
    echo ERROR: requirements.txt not found!
    echo Please run this script from the validator-python directory.
    echo.
    pause
    exit /b 1
)

REM Check if virtual environment already exists
if exist "carddemo-validator-env" (
    echo Virtual environment already exists!
    echo.
    set /p choice="Do you want to recreate it? (y/N): "
    if /i not "%choice%"=="y" (
        echo Setup cancelled.
        echo.
        pause
        exit /b 0
    )
    
    echo Removing existing virtual environment...
    rmdir /s /q carddemo-validator-env
    echo.
)

REM Create virtual environment
echo Creating virtual environment...
python -m venv carddemo-validator-env

if errorlevel 1 (
    echo ERROR: Failed to create virtual environment!
    echo.
    pause
    exit /b 1
)

echo Virtual environment created successfully!
echo.

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

REM Upgrade pip
echo Upgrading pip...
python -m pip install --upgrade pip

if errorlevel 1 (
    echo WARNING: Failed to upgrade pip, continuing anyway...
    echo.
)

REM Install requirements
echo Installing required packages...
pip install -r requirements.txt

if errorlevel 1 (
    echo ERROR: Failed to install requirements!
    echo.
    pause
    exit /b 1
)

echo.
echo ========================================
echo Environment setup completed!
echo ========================================
echo.
echo Virtual environment: carddemo-validator-env
echo Python: %python%
echo Pip: %pip%
echo.

REM Display installed packages
echo Installed packages:
pip list
echo.

echo ========================================
echo Next steps:
echo ========================================
echo 1. To start the environment: start_env.bat
echo 2. To run all validators: python run_all_validators.py
echo 3. To stop the environment: stop_env.bat
echo.

echo Setup completed successfully!
echo.

pause
