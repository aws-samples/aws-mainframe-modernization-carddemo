@echo off
REM CardDemo Validator - Virtual Environment Starter
REM This batch file activates the Python virtual environment for the CardDemo validator

echo.
echo ========================================
echo CardDemo Validator Environment Starter
echo ========================================
echo.

REM Check if we're in the right directory
if not exist "carddemo-validator-env" (
    echo ERROR: Virtual environment not found!
    echo Please run this script from the validator-python directory.
    echo.
    pause
    exit /b 1
)

REM Check if virtual environment exists
if not exist "carddemo-validator-env\Scripts\activate.bat" (
    echo ERROR: Virtual environment is not properly set up!
    echo Please create the virtual environment first:
    echo   python -m venv carddemo-validator-env
    echo.
    pause
    exit /b 1
)

REM Activate the virtual environment
echo Activating virtual environment...
call carddemo-validator-env\Scripts\activate.bat

REM Check if activation was successful
if errorlevel 1 (
    echo ERROR: Failed to activate virtual environment!
    echo.
    pause
    exit /b 1
)

REM Display environment information
echo.
echo Virtual environment activated successfully!
echo.
echo Environment Information:
echo - Python: %python%
echo - Pip: %pip%
echo - Working Directory: %cd%
echo.

REM Check if requirements are installed
echo Checking installed packages...
pip list | findstr /i "deepeval pandas numpy matplotlib json5" >nul 2>&1
if errorlevel 1 (
    echo.
    echo WARNING: Some required packages may not be installed.
    echo To install all requirements, run:
    echo   pip install -r requirements.txt
    echo.
) else (
    echo All required packages are installed.
)

echo.
echo ========================================
echo Environment is ready!
echo ========================================
echo.
echo Available commands:
echo - python run_all_validators.py
echo - cd src ^&^& python requirements_validator.py
echo - cd src ^&^& python deepeval_validator.py
echo.
echo To deactivate the environment, run: stop_env.bat
echo.

REM Keep the command prompt open
cmd /k
