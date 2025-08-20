@echo off
REM CardDemo Validator - Virtual Environment Stopper
REM This batch file deactivates the Python virtual environment

echo.
echo ========================================
echo CardDemo Validator Environment Stopper
echo ========================================
echo.

REM Check if virtual environment is active
if not defined VIRTUAL_ENV (
    echo No virtual environment is currently active.
    echo.
    pause
    exit /b 0
)

echo Deactivating virtual environment...
echo Current environment: %VIRTUAL_ENV%
echo.

REM Deactivate the virtual environment
deactivate

REM Check if deactivation was successful
if errorlevel 1 (
    echo WARNING: Deactivation may not have completed properly.
    echo You may need to close this command prompt window.
) else (
    echo Virtual environment deactivated successfully.
)

echo.
echo ========================================
echo Environment stopped!
echo ========================================
echo.
echo To reactivate the environment, run: start_env.bat
echo.

pause
