#!/usr/bin/env python3
"""
CardDemo Requirements Validator - Report Generator

This script demonstrates the enhanced report generation capabilities
for the CardDemo requirements validation system.

The script provides a user-friendly interface to:
1. Initialize the requirements validator
2. Run comprehensive validation against the CardDemo codebase
3. Generate detailed reports in multiple formats (Markdown and Text)
4. Display summary statistics and progress information

This is the recommended way to run the validator for most users.
"""

# =============================================================================
# IMPORTS - Required modules for report generation
# =============================================================================

import os      # For file system operations and path handling
import sys     # For system-specific parameters and exit codes
from requirements_validator import CardDemoRequirementsValidator  # Main validator class

def main():
    """
    Generate comprehensive reports for CardDemo requirements validation.
    
    This function orchestrates the complete validation process:
    1. Validates prerequisites and file existence
    2. Initializes the requirements validator
    3. Runs comprehensive validation against the codebase
    4. Generates detailed reports in multiple formats
    5. Displays summary statistics and completion status
    
    Returns:
        bool: True if successful, False if errors occurred
    """
    
    # =============================================================================
    # SCRIPT HEADER - Display welcome message and version information
    # =============================================================================
    print("CardDemo Requirements Validator - Enhanced Report Generator")
    print("=" * 60)
    
    # =============================================================================
    # PREREQUISITE VALIDATION - Ensure required files and directories exist
    # =============================================================================
    
    # Check for AI-generated requirements file
    if not os.path.exists('docs/CD-Requirements.json'):
        print("❌ Error: CD-Requirements.json not found in docs/")
        print("Please run this script from the CardDemo root directory")
        return False
    
    # Check for CardDemo codebase structure
    if not os.path.exists('app/cbl'):
        print("❌ Error: CardDemo codebase not found")
        print("Please run this script from the CardDemo root directory")
        return False
    
    try:
        # =============================================================================
        # VALIDATOR INITIALIZATION - Set up the requirements validator
        # =============================================================================
        print("🔍 Initializing validator...")
        validator = CardDemoRequirementsValidator(
            requirements_file='docs/CD-Requirements.json',  # AI-generated requirements
            codebase_path='.'  # Current directory (CardDemo root)
        )
        
        # =============================================================================
        # VALIDATION EXECUTION - Run the complete validation process
        # =============================================================================
        print("📊 Running validation...")
        results = validator.run_validation()
        
        # =============================================================================
        # RESULTS SUMMARY - Display key validation statistics
        # =============================================================================
        print("\n📈 VALIDATION SUMMARY:")
        print("-" * 40)
        print(f"Total Requirements: {results['coverage'].total_requirements}")
        print(f"Coverage: {results['coverage'].coverage_percentage:.1f}%")
        
        # Calculate and display validation status breakdown
        pass_count = len([r for r in results['validation_results'] if r.status == 'PASS'])
        partial_count = len([r for r in results['validation_results'] if r.status == 'PARTIAL'])
        fail_count = len([r for r in results['validation_results'] if r.status == 'FAIL'])
        
        print(f"✅ Pass: {pass_count}")
        print(f"⚠️  Partial: {partial_count}")
        print(f"❌ Fail: {fail_count}")
        
        # =============================================================================
        # REPORT GENERATION - Create comprehensive reports in multiple formats
        # =============================================================================
        print("\n📄 GENERATING REPORTS:")
        print("-" * 40)
        
        # Generate Markdown report (primary format with rich formatting)
        print("1. Generating Markdown report...")
        markdown_file = "CardDemo_Requirements_Validation_Report.md"
        validator.generate_markdown_report(markdown_file)
        print(f"   ✅ Markdown report saved: {markdown_file}")
        
        # Generate text report (legacy format for compatibility)
        print("2. Generating text report...")
        text_file = "CardDemo_Requirements_Validation_Report.txt"
        validator.generate_report(text_file)
        print(f"   ✅ Text report saved: {text_file}")
        
        # =============================================================================
        # COMPLETION SUMMARY - Display final results and next steps
        # =============================================================================
        print("\n🎉 REPORT GENERATION COMPLETE!")
        print("=" * 60)
        print("Generated files:")
        
        # Display file information and sizes
        if os.path.exists(markdown_file):
            size = os.path.getsize(markdown_file)
            print(f"   📄 {markdown_file} ({size:,} bytes)")
        
        if os.path.exists(text_file):
            size = os.path.getsize(text_file)
            print(f"   📝 {text_file} ({size:,} bytes)")
        
        # Provide guidance on next steps
        print("\n💡 Next steps:")
        print("   • Open the Markdown file in any Markdown viewer")
        print("   • Review the text file for detailed analysis")
        print("   • Use browser print-to-PDF if needed")
        
        return True
        
    except Exception as e:
        # =============================================================================
        # ERROR HANDLING - Graceful error reporting and exit
        # =============================================================================
        print(f"❌ Error during report generation: {e}")
        return False

# =============================================================================
# SCRIPT EXECUTION - Entry point when run as standalone script
# =============================================================================

if __name__ == "__main__":
    success = main()
    if not success:
        sys.exit(1)  # Exit with error code if validation failed
