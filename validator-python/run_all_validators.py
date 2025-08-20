#!/usr/bin/env python3
"""
CardDemo Validator Runner

This script runs all validation tools from the validator-python directory.
It handles the proper path setup and execution order.
"""

import os
import sys
import subprocess
from pathlib import Path

def run_command(command, description):
    """Run a command and handle errors."""
    print(f"\n{'='*60}")
    print(f"🚀 {description}")
    print(f"{'='*60}")
    print(f"Running: {command}")
    
    try:
        result = subprocess.run(command, shell=True, check=True, 
                              capture_output=True, text=True, cwd="src")
        print("✅ Success!")
        if result.stdout:
            print("Output:")
            print(result.stdout)
        return True
    except subprocess.CalledProcessError as e:
        print(f"❌ Error running {description}:")
        print(f"Exit code: {e.returncode}")
        if e.stdout:
            print("Stdout:")
            print(e.stdout)
        if e.stderr:
            print("Stderr:")
            print(e.stderr)
        return False

def main():
    """Main function to run all validators."""
    print("CardDemo Validator Suite")
    print("=" * 60)
    print("This script will run all validation tools in the correct order.")
    
    # Ensure we're in the right directory
    current_dir = Path.cwd()
    if not current_dir.name == "validator-python":
        print("❌ Error: Please run this script from the validator-python directory")
        sys.exit(1)
    
    # Check if src directory exists
    if not Path("src").exists():
        print("❌ Error: src directory not found")
        sys.exit(1)
    
    # Check if data directory and requirements file exist
    if not Path("data/CD-Requirements.json").exists():
        print("❌ Error: CD-Requirements.json not found in data/ directory")
        sys.exit(1)
    
    # Create reports directory if it doesn't exist
    Path("reports").mkdir(exist_ok=True)
    
    print("✅ Environment check passed")
    print(f"📁 Working directory: {current_dir}")
    print(f"📄 Requirements file: {Path('data/CD-Requirements.json').absolute()}")
    print(f"📊 Reports directory: {Path('reports').absolute()}")
    
    # Run validators in order
    validators = [
        ("python requirements_validator.py", "Requirements Validator"),
        ("python deepeval_validator.py", "DeepEval Validator"),
        ("python generate_reports.py", "Report Generator"),
        ("python generate_deepeval_reports.py", "DeepEval Report Generator"),
        ("python compare_validators.py", "Validator Comparison")
    ]
    
    results = []
    for command, description in validators:
        success = run_command(command, description)
        results.append((description, success))
    
    # Summary
    print(f"\n{'='*60}")
    print("📊 VALIDATION SUMMARY")
    print(f"{'='*60}")
    
    passed = 0
    failed = 0
    
    for description, success in results:
        status = "✅ PASSED" if success else "❌ FAILED"
        print(f"{status} - {description}")
        if success:
            passed += 1
        else:
            failed += 1
    
    print(f"\nResults: {passed} passed, {failed} failed")
    
    if failed == 0:
        print("\n🎉 All validators completed successfully!")
        print("📄 Check the reports/ directory for generated reports")
    else:
        print(f"\n⚠️  {failed} validator(s) failed. Check the output above for details.")
        sys.exit(1)

if __name__ == "__main__":
    main()
