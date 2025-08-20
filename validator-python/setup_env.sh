#!/bin/bash
# CardDemo Validator - Environment Setup
# This script creates and sets up the Python virtual environment

echo ""
echo "========================================"
echo "CardDemo Validator Environment Setup"
echo "========================================"
echo ""

# Check if Python is available
if ! command -v python3 &> /dev/null; then
    echo "ERROR: Python3 is not installed or not in PATH!"
    echo "Please install Python 3.8 or higher and try again."
    echo ""
    exit 1
fi

echo "Python found:"
python3 --version
echo ""

# Check if we're in the right directory
if [ ! -f "requirements.txt" ]; then
    echo "ERROR: requirements.txt not found!"
    echo "Please run this script from the validator-python directory."
    echo ""
    exit 1
fi

# Check if virtual environment already exists
if [ -d "carddemo-validator-env" ]; then
    echo "Virtual environment already exists!"
    echo ""
    read -p "Do you want to recreate it? (y/N): " choice
    if [[ ! "$choice" =~ ^[Yy]$ ]]; then
        echo "Setup cancelled."
        echo ""
        exit 0
    fi
    
    echo "Removing existing virtual environment..."
    rm -rf carddemo-validator-env
    echo ""
fi

# Create virtual environment
echo "Creating virtual environment..."
python3 -m venv carddemo-validator-env

if [ $? -ne 0 ]; then
    echo "ERROR: Failed to create virtual environment!"
    echo ""
    exit 1
fi

echo "Virtual environment created successfully!"
echo ""

# Activate virtual environment
echo "Activating virtual environment..."
source carddemo-validator-env/bin/activate

if [ $? -ne 0 ]; then
    echo "ERROR: Failed to activate virtual environment!"
    echo ""
    exit 1
fi

echo "Virtual environment activated!"
echo ""

# Upgrade pip
echo "Upgrading pip..."
python -m pip install --upgrade pip

if [ $? -ne 0 ]; then
    echo "WARNING: Failed to upgrade pip, continuing anyway..."
    echo ""
fi

# Install requirements
echo "Installing required packages..."
pip install -r requirements.txt

if [ $? -ne 0 ]; then
    echo "ERROR: Failed to install requirements!"
    echo ""
    exit 1
fi

echo ""
echo "========================================"
echo "Environment setup completed!"
echo "========================================"
echo ""
echo "Virtual environment: carddemo-validator-env"
echo "Python: $(which python)"
echo "Pip: $(which pip)"
echo ""

# Display installed packages
echo "Installed packages:"
pip list
echo ""

echo "========================================"
echo "Next steps:"
echo "========================================"
echo "1. To start the environment: source start_env.sh"
echo "2. To run all validators: python run_all_validators.py"
echo "3. To stop the environment: deactivate"
echo ""

echo "Setup completed successfully!"
echo ""

# Deactivate virtual environment
deactivate
