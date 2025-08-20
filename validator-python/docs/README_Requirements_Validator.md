# CardDemo Requirements Validator

A comprehensive Python test suite that validates AI-generated requirements documents against the actual CardDemo codebase to assess accuracy, completeness, and coverage.

## Overview

This validator analyzes the JSON output from AI applications that generate technical requirements for the CardDemo mainframe application. It provides:

1. **Requirement Validation**: Marks each requirement as PASS/FAIL/PARTIAL
2. **Error Detection**: Details errors in requirements assessment
3. **Coverage Analysis**: Provides percentage coverage of the codebase
4. **Missing Requirements**: Identifies requirements that failed to be captured

## Features

### ✅ **Component Validation**

- Validates COBOL program references against actual source files
- Checks CICS transaction IDs against CSD definitions
- Verifies VSAM file references against data directory
- Validates BMS mapset references against screen definitions

### ✅ **Architectural Pattern Validation**

- CICS command usage (XCTL, EXEC CICS, etc.)
- BMS screen mapping patterns
- VSAM file operations (READ, WRITE, etc.)
- JCL batch processing patterns

### ✅ **Data Model Consistency**

- Validates data structure references
- Checks field definitions in copybooks
- Verifies database schema consistency

### ✅ **User Story Validation**

- Validates user story implementability
- Checks acceptance criteria feasibility
- Assesses component availability for stories

## Installation

1. **Clone or download the validator files**
2. **Create a Python virtual environment** (recommended):

   ```bash
   python -m venv carddemo-validator-env
   source carddemo-validator-env/bin/activate  # On Windows: carddemo-validator-env\Scripts\activate
   ```

3. **Install Python dependencies**:

   ```bash
   pip install -r requirements.txt
   ```

4. **Ensure you have access to**:
   - The CardDemo codebase
   - The AI-generated requirements JSON file (`app/catlg/CD-Requirements.json`)

## Usage

### Basic Usage

```bash
python requirements_validator.py <requirements.json> <codebase_path>
```

### Enhanced Report Generation

```bash
python generate_reports.py
```

### Example

```bash
python requirements_validator.py app/catlg/CD-Requirements.json .
```

### Command Line Options

- `requirements.json`: Path to the AI-generated requirements JSON file
- `codebase_path`: Path to the CardDemo codebase root directory

## Output

The validator generates comprehensive reports in multiple formats:

### Report Formats

1. **Markdown Report** (`.md`) - Professional, formatted report with tables and sections
2. **PDF Report** (`.pdf`) - Print-ready document with proper formatting
3. **Text Report** (`.txt`) - Simple text format for easy parsing
4. **HTML Report** (`.html`) - Web-friendly format for browser viewing

### Sample Output

```
CardDemo Requirements Validator - Enhanced Report Generator
============================================================
🔍 Initializing validator...
📊 Running validation...

📈 VALIDATION SUMMARY:
----------------------------------------
Total Requirements: 804
Coverage: 70.0%
✅ Pass: 563
⚠️  Partial: 48
❌ Fail: 193

📄 GENERATING REPORTS:
----------------------------------------
1. Generating Markdown report...
   ✅ Markdown report saved: CardDemo_Requirements_Validation_Report.md
2. Converting to PDF...
   ✅ PDF report saved: CardDemo_Requirements_Validation_Report.pdf
3. Generating text report...
   ✅ Text report saved: CardDemo_Requirements_Validation_Report.txt

🎉 REPORT GENERATION COMPLETE!
============================================================
Generated files:
   📄 CardDemo_Requirements_Validation_Report.md (8,219 bytes)
   📕 CardDemo_Requirements_Validation_Report.pdf (8,045 bytes)
   📝 CardDemo_Requirements_Validation_Report.txt (8,219 bytes)
```

## Report Structure

The detailed report includes:

### 📊 **Executive Summary**

- Overall coverage assessment with status indicators
- Key metrics and statistics
- Quick status overview

### 📈 **Summary Statistics**

- Total requirements analyzed
- Validation success rate
- Coverage percentage with visual indicators

### ✅ **Validation Results Summary**

- Technical requirements breakdown
- User stories validation
- Pass/Fail/Partial status with counts

### 🔍 **Coverage Analysis**

- Missing requirements identification
- Unidentified features in codebase
- Component coverage gaps

### 💡 **Recommendations**

- Actionable suggestions for improvement
- Areas needing additional documentation
- Quality improvement tips

### 📋 **Detailed Results (Appendix)**

- Complete validation results in tabular format
- Individual requirement analysis
- Error details and evidence
- Confidence scores

## Validation Criteria

### Technical Requirements Validation

1. **Component Existence**

   - Programs mentioned must exist in `/app/cbl/`
   - Transactions must be defined in CSD file
   - Files must exist in data directories

2. **Architectural Consistency**

   - CICS patterns must include proper commands
   - BMS references must include screen mapping
   - VSAM operations must specify file handling

3. **Data Model Accuracy**
   - Data structures must match copybook definitions
   - Field references must be valid
   - Schema constraints must be respected

### User Story Validation

1. **Implementability**

   - Required programs must exist
   - Data access patterns must be feasible
   - Integration points must be available

2. **Acceptance Criteria**
   - Criteria must be technically achievable
   - Validation rules must be implementable
   - Error handling must be possible

## Configuration

### Customizing Validation Rules

You can modify the validation logic by editing:

- `_check_component_mentions()`: Component validation rules
- `_check_architectural_patterns()`: Architecture validation
- `_check_data_model_consistency()`: Data model validation
- `_check_story_implementability()`: User story validation

### Adding New Validation Types

1. Create a new validation method
2. Add it to the `_validate_single_requirement()` method
3. Update the confidence calculation logic

## Integration with Deep Delve

For enhanced code analysis, you can integrate with Deep Delve:

1. **Install Deep Delve** (if available)
2. **Uncomment Deep Delve dependencies** in `requirements.txt`
3. **Add Deep Delve analysis methods** to the validator

Example Deep Delve integration:

```python
def _analyze_with_deep_delve(self, code_path: str) -> Dict[str, Any]:
    """Use Deep Delve for advanced code analysis"""
    # Deep Delve integration code here
    pass
```

## Troubleshooting

### Common Issues

1. **File Not Found Errors**

   - Ensure codebase path is correct
   - Check file permissions
   - Verify directory structure
   - Confirm `app/catlg/CD-Requirements.json` exists

2. **JSON Parsing Errors**

   - Validate JSON syntax
   - Check for encoding issues
   - Ensure file is not corrupted
   - Verify JSON structure matches expected format

3. **PDF Generation Issues**

   - **WeasyPrint errors**: Install system dependencies or use ReportLab fallback
   - **ReportLab errors**: Ensure all dependencies are installed
   - **HTML fallback**: Open generated HTML file in browser and print to PDF

4. **Low Coverage Scores**

   - Review requirements completeness
   - Check for missing component documentation
   - Verify codebase structure
   - Examine detailed error messages in reports

5. **Encoding Issues (Windows)**
   - Ensure UTF-8 encoding is used
   - Check console encoding settings
   - Use virtual environment for isolation

### Debug Mode

Enable detailed logging:

```python
logging.basicConfig(level=logging.DEBUG)
```

## Extending the Validator

### Adding New Component Types

1. **Create extraction method**:

   ```python
   def _get_new_components(self) -> Set[str]:
       # Extract new component type
       pass
   ```

2. **Add to known_components**:

   ```python
   self.known_components['new_type'] = self._get_new_components()
   ```

3. **Create validation method**:
   ```python
   def _check_new_component_mentions(self, text: str) -> Dict[str, List[str]]:
       # Validate new component references
       pass
   ```

### Custom Validation Rules

Add custom validation logic:

```python
def _custom_validation(self, requirement_text: str) -> Dict[str, List[str]]:
    """Custom validation logic"""
    errors = []
    evidence = []

    # Your custom validation here

    return {'errors': errors, 'evidence': evidence}
```

## Performance Considerations

- **Large JSON files**: The validator processes large files efficiently (tested with 2MB+ files)
- **Memory usage**: Minimal memory footprint for large codebases
- **Processing time**: Typically completes in seconds for standard CardDemo size
- **PDF generation**: Multiple fallback methods ensure compatibility across platforms
- **Report generation**: Optimized for readability and professional presentation

## Contributing

To contribute improvements:

1. **Fork the repository**
2. **Add new validation rules**
3. **Update documentation**
4. **Submit pull request**

## License

This validator is provided as-is for CardDemo requirements validation.

## Support

For issues or questions:

1. Check the troubleshooting section
2. Review the validation criteria
3. Examine the detailed error messages in the report

## File Structure

```
CardDemo Requirements Validator/
├── requirements_validator.py      # Main validator class
├── test_validator.py              # Basic test script
├── generate_reports.py            # Enhanced report generator
├── requirements.txt               # Python dependencies
├── README_Requirements_Validator.md # This documentation
└── Generated Reports/
    ├── CardDemo_Requirements_Validation_Report.md
    ├── CardDemo_Requirements_Validation_Report.pdf
    └── CardDemo_Requirements_Validation_Report.txt
```

## Quick Start

1. **Install dependencies**: `pip install -r requirements.txt`
2. **Run validation**: `python generate_reports.py`
3. **View results**: Open the generated Markdown or PDF reports

---

**Note**: This validator is specifically designed for the CardDemo mainframe application and may need customization for other codebases.
