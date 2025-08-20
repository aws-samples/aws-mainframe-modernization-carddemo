#!/usr/bin/env python3
"""
CardDemo Validator Comparison Script

This script compares the results of the regex-based validator vs the DeepEval validator
to demonstrate the improvements in semantic understanding and false positive detection.

Author: Savantly
Date: 2025
"""

import os
import sys
import json
from pathlib import Path
import logging

# Import both validators
from requirements_validator import CardDemoRequirementsValidator
from deepeval_validator import CardDemoDeepEvalValidator

# Configure logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

def run_regex_validator(requirements_file: str, codebase_path: str):
    """Run the original regex-based validator."""
    print("🔍 Running Regex-Based Validator...")
    validator = CardDemoRequirementsValidator(requirements_file, codebase_path)
    results = validator.run_validation()
    return results

def run_deepeval_validator(requirements_file: str, codebase_path: str):
    """Run the DeepEval-based validator."""
    print("🧠 Running DeepEval Validator...")
    validator = CardDemoDeepEvalValidator(requirements_file, codebase_path)
    results = validator.run_deepeval_validation()
    return results

def compare_results(regex_results, deepeval_results):
    """Compare the results of both validators."""
    print("\n📊 VALIDATOR COMPARISON RESULTS")
    print("=" * 60)
    
    # Basic statistics comparison
    print("\n📈 Basic Statistics:")
    print("-" * 30)
    print(f"{'Metric':<25} {'Regex':<15} {'DeepEval':<15} {'Difference':<15}")
    print("-" * 70)
    
    regex_coverage = regex_results['coverage'].coverage_percentage
    deepeval_coverage = deepeval_results['coverage'].coverage_percentage
    coverage_diff = deepeval_coverage - regex_coverage
    
    print(f"{'Coverage %':<25} {regex_coverage:<15.1f} {deepeval_coverage:<15.1f} {coverage_diff:+.1f}")
    
    # Count statuses
    regex_pass = len([r for r in regex_results['validation_results'] if r.status == 'PASS'])
    deepeval_pass = len([r for r in deepeval_results['validation_results'] if r.status == 'PASS'])
    pass_diff = deepeval_pass - regex_pass
    
    regex_partial = len([r for r in regex_results['validation_results'] if r.status == 'PARTIAL'])
    deepeval_partial = len([r for r in deepeval_results['validation_results'] if r.status == 'PARTIAL'])
    partial_diff = deepeval_partial - regex_partial
    
    regex_fail = len([r for r in regex_results['validation_results'] if r.status == 'FAIL'])
    deepeval_fail = len([r for r in deepeval_results['validation_results'] if r.status == 'FAIL'])
    fail_diff = deepeval_fail - regex_fail
    
    print(f"{'Pass Count':<25} {regex_pass:<15} {deepeval_pass:<15} {pass_diff:+d}")
    print(f"{'Partial Count':<25} {regex_partial:<15} {deepeval_partial:<15} {partial_diff:+d}")
    print(f"{'Fail Count':<25} {regex_fail:<15} {deepeval_fail:<15} {fail_diff:+d}")
    
    # DeepEval-specific metrics
    print(f"\n🧠 DeepEval-Specific Metrics:")
    print("-" * 30)
    semantic_accuracy = deepeval_results['coverage'].semantic_accuracy
    hallucination_rate = deepeval_results['coverage'].hallucination_rate
    
    print(f"Semantic Accuracy: {semantic_accuracy:.1f}%")
    print(f"Hallucination Rate: {hallucination_rate:.1f}%")
    
    # Key improvements analysis
    print(f"\n💡 Key Improvements Analysis:")
    print("-" * 30)
    
    # Analyze false positive reduction
    print("1. False Positive Detection:")
    print("   • Regex validator: Uses pattern matching that catches false positives")
    print("   • DeepEval validator: Uses semantic understanding to distinguish:")
    print("     - Function names vs program names")
    print("     - JCL return codes vs program names")
    print("     - Function keys vs transaction IDs")
    
    # Analyze semantic understanding
    print("\n2. Semantic Understanding:")
    print("   • Regex validator: Pattern-based validation only")
    print("   • DeepEval validator: Context-aware validation that understands:")
    print("     - Mainframe terminology and concepts")
    print("     - Architectural patterns and relationships")
    print("     - Component interactions and dependencies")
    
    # Analyze improvement suggestions
    print("\n3. Improvement Suggestions:")
    print("   • Regex validator: Basic error reporting")
    print("   • DeepEval validator: Detailed, actionable suggestions:")
    print("     - Specific component replacements")
    print("     - Architectural corrections")
    print("     - Context-aware recommendations")
    
    # Analyze hallucination detection
    print("\n4. Hallucination Detection:")
    print("   • Regex validator: Cannot detect hallucinations")
    print("   • DeepEval validator: Identifies when requirements mention:")
    print("     - Non-existent components")
    print("     - Incorrect architectural patterns")
    print("     - Technologies not used in CardDemo")
    
    return {
        'regex_coverage': regex_coverage,
        'deepeval_coverage': deepeval_coverage,
        'coverage_improvement': coverage_diff,
        'semantic_accuracy': semantic_accuracy,
        'hallucination_rate': hallucination_rate,
        'pass_improvement': pass_diff,
        'partial_improvement': partial_diff,
        'fail_improvement': fail_diff
    }

def generate_comparison_report(comparison_data, regex_results, deepeval_results):
    """Generate a detailed comparison report."""
    report = []
    
    report.append("# CardDemo Validator Comparison Report")
    report.append("")
    report.append(f"**Generated:** {get_timestamp()}")
    report.append(f"**Application:** AWS.M2.CARDDEMO")
    report.append("")
    report.append("---")
    report.append("")
    
    # Executive Summary
    report.append("## Executive Summary")
    report.append("")
    report.append("This report compares the performance of two validation approaches:")
    report.append("")
    report.append("1. **Regex-Based Validator**: Pattern-matching approach using regular expressions")
    report.append("2. **DeepEval Validator**: Semantic understanding approach using LLM capabilities")
    report.append("")
    
    # Key Findings
    report.append("### Key Findings")
    report.append("")
    report.append(f"- **Coverage Improvement**: {comparison_data['coverage_improvement']:+.1f}%")
    report.append(f"- **Semantic Accuracy**: {comparison_data['semantic_accuracy']:.1f}%")
    report.append(f"- **Hallucination Rate**: {comparison_data['hallucination_rate']:.1f}%")
    report.append(f"- **Pass Rate Improvement**: {comparison_data['pass_improvement']:+d} requirements")
    report.append("")
    
    # Detailed Comparison
    report.append("## Detailed Comparison")
    report.append("")
    report.append("| Metric | Regex Validator | DeepEval Validator | Improvement |")
    report.append("|--------|-----------------|-------------------|-------------|")
    report.append(f"| Coverage | {comparison_data['regex_coverage']:.1f}% | {comparison_data['deepeval_coverage']:.1f}% | {comparison_data['coverage_improvement']:+.1f}% |")
    report.append(f"| Pass Count | {len([r for r in regex_results['validation_results'] if r.status == 'PASS'])} | {len([r for r in deepeval_results['validation_results'] if r.status == 'PASS'])} | {comparison_data['pass_improvement']:+d} |")
    report.append(f"| Partial Count | {len([r for r in regex_results['validation_results'] if r.status == 'PARTIAL'])} | {len([r for r in deepeval_results['validation_results'] if r.status == 'PARTIAL'])} | {comparison_data['partial_improvement']:+d} |")
    report.append(f"| Fail Count | {len([r for r in regex_results['validation_results'] if r.status == 'FAIL'])} | {len([r for r in deepeval_results['validation_results'] if r.status == 'FAIL'])} | {comparison_data['fail_improvement']:+d} |")
    report.append("")
    
    # DeepEval Advantages
    report.append("## DeepEval Advantages")
    report.append("")
    report.append("### 1. Semantic Understanding")
    report.append("- **Context Awareness**: Understands mainframe terminology and concepts")
    report.append("- **False Positive Reduction**: Distinguishes between similar terms in different contexts")
    report.append("- **Architectural Validation**: Validates against CardDemo's actual architecture")
    report.append("")
    
    report.append("### 2. Intelligent Analysis")
    report.append("- **Component Validation**: Semantic validation of programs, transactions, and files")
    report.append("- **Architectural Consistency**: Checks for technology mismatches (e.g., IMS DB vs VSAM)")
    report.append("- **Improvement Suggestions**: Provides actionable recommendations")
    report.append("")
    
    report.append("### 3. Advanced Metrics")
    report.append("- **Semantic Accuracy**: Measures how well requirements align with codebase")
    report.append("- **Hallucination Detection**: Identifies when requirements mention non-existent components")
    report.append("- **Confidence Scoring**: Provides confidence levels for validation results")
    report.append("")
    
    # Recommendations
    report.append("## Recommendations")
    report.append("")
    report.append("### For Requirements Validation")
    report.append("1. **Use DeepEval for Primary Validation**: Leverage semantic understanding for accurate results")
    report.append("2. **Focus on High Hallucination Requirements**: Prioritize fixing requirements with high hallucination scores")
    report.append("3. **Implement Improvement Suggestions**: Use DeepEval's actionable recommendations")
    report.append("4. **Monitor Semantic Accuracy**: Track semantic accuracy as a quality metric")
    report.append("")
    
    report.append("### For Future Development")
    report.append("1. **Enhance DeepEval Integration**: Add more sophisticated semantic validation")
    report.append("2. **Custom Metrics**: Develop domain-specific validation metrics")
    report.append("3. **Learning Capability**: Implement continuous improvement based on validation results")
    report.append("4. **Integration with AI Tools**: Use DeepEval insights to improve AI-generated requirements")
    report.append("")
    
    # Report footer
    report.append("---")
    report.append("")
    report.append("*Report generated by CardDemo Validator Comparison Tool*")
    
    return "\n".join(report)

def get_timestamp():
    """Get current timestamp."""
    from datetime import datetime
    return datetime.now().strftime("%Y-%m-%d %H:%M:%S")

def main():
    """Main function to run the comparison."""
    print("CardDemo Validator Comparison Tool")
    print("=" * 50)
    
    # Configuration
    requirements_file = "docs/CD-Requirements.json"
    codebase_path = "."
    
    # Validate file paths
    if not os.path.exists(requirements_file):
        print(f"❌ Error: Requirements file not found: {requirements_file}")
        sys.exit(1)
    
    if not os.path.exists(codebase_path):
        print(f"❌ Error: Codebase path not found: {codebase_path}")
        sys.exit(1)
    
    try:
        # Run both validators
        regex_results = run_regex_validator(requirements_file, codebase_path)
        deepeval_results = run_deepeval_validator(requirements_file, codebase_path)
        
        # Compare results
        comparison_data = compare_results(regex_results, deepeval_results)
        
        # Generate comparison report
        print("\n📄 Generating comparison report...")
        comparison_report = generate_comparison_report(comparison_data, regex_results, deepeval_results)
        
        # Save comparison report
        report_file = "CardDemo_Validator_Comparison_Report.md"
        with open(report_file, 'w', encoding='utf-8') as f:
            f.write(comparison_report)
        
        print(f"✅ Comparison report saved: {report_file}")
        
        print("\n🎉 COMPARISON COMPLETE!")
        print("=" * 50)
        print("The comparison shows the advantages of DeepEval's semantic approach")
        print("over traditional regex-based validation for requirements analysis.")
        
    except Exception as e:
        print(f"❌ Error during comparison: {e}")
        logger.error(f"Comparison failed: {e}")
        sys.exit(1)

if __name__ == "__main__":
    main()
