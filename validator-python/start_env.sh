#!/bin/bash
# CardDemo Validator - Virtual Environment Starter
# This script activates the Python virtual environment for the CardDemo validator

echo ""
echo "========================================"
echo "CardDemo Validator Environment Starter"
echo "========================================"
echo ""

# Check if we're in the right directory
if [ ! -d "carddemo-validator-env" ]; then
    echo "ERROR: Virtual environment not found!"
    echo "Please run this script from the validator-python directory."
    echo ""
    exit 1
fi

# Check if virtual environment exists
if [ ! -f "carddemo-validator-env/bin/activate" ]; then
    echo "ERROR: Virtual environment is not properly set up!"
    echo "Please create the virtual environment first:"
    echo "  python3 -m venv carddemo-validator-env"
    echo ""
    exit 1
fi

# Activate the virtual environment
echo "Activating virtual environment..."
source carddemo-validator-env/bin/activate

# Check if activation was successful
if [ $? -ne 0 ]; then
    echo "ERROR: Failed to activate virtual environment!"
    echo ""
    exit 1
fi

# Display environment information
echo ""
echo "Virtual environment activated successfully!"
echo ""
echo "Environment Information:"
echo "- Python: $(which python)"
echo "- Pip: $(which pip)"
echo "- Working Directory: $(pwd)"
echo ""

# Check if requirements are installed
echo "Checking installed packages..."
if pip list | grep -i "deepeval\|pandas\|numpy\|matplotlib\|json5" > /dev/null 2>&1; then
    echo "All required packages are installed."
else
    echo ""
    echo "WARNING: Some required packages may not be installed."
    echo "To install all requirements, run:"
    echo "  pip install -r requirements.txt"
    echo ""
fi

echo ""
echo "========================================"
echo "Environment is ready!"
echo "========================================"
echo ""
echo "Available commands:"
echo "- python run_all_validators.py"
echo "- cd src && python requirements_validator.py"
echo "- cd src && python deepeval_validator.py"
echo ""
echo "To deactivate the environment, run: stop_env.sh"
echo ""

# Start a new shell with the environment activated
exec $SHELL
