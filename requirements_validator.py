#!/usr/bin/env python3
"""
CardDemo Requirements Validator

This script validates AI-generated requirements against the actual CardDemo codebase
to assess accuracy, completeness, and coverage.

The validator performs the following key functions:
1. Extracts components from the CardDemo codebase (COBOL programs, CICS transactions, VSAM files, etc.)
2. Loads AI-generated requirements from a JSON file
3. Validates each requirement against the actual codebase components
4. Calculates coverage statistics and identifies gaps
5. Generates comprehensive reports in Markdown and text formats

Author: AI Assistant
Date: 2024
"""

# =============================================================================
# IMPORTS AND CONFIGURATION
# =============================================================================

# Standard library imports for file operations, JSON parsing, and system interaction
import json          # For parsing the requirements JSON file
import os            # For file system operations and path handling
import re            # For regular expression pattern matching
import sys           # For system-specific parameters and exit codes
from pathlib import Path  # For modern path handling across operating systems
from typing import Dict, List, Tuple, Set, Any  # For type hints
from dataclasses import dataclass  # For creating data classes
from collections import defaultdict  # For creating dictionaries with default values
import logging  # For logging validation progress and errors

# Configure logging to show timestamps and log levels for debugging and monitoring
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

# =============================================================================
# DATA STRUCTURES - Define the core data classes used throughout the validator
# =============================================================================

@dataclass
class ValidationResult:
    """
    Represents the result of validating a single requirement against the codebase.
    
    This data class stores all information about how well a specific requirement
    aligns with the actual CardDemo implementation.
    """
    requirement_id: str        # Unique identifier for the requirement (e.g., "TECH_REQ_001")
    requirement_text: str      # The full text of the requirement being validated
    status: str               # Validation status: 'PASS', 'FAIL', or 'PARTIAL'
    errors: List[str]         # List of specific errors found during validation
    evidence: List[str]       # List of supporting evidence for the validation result
    confidence: float         # Confidence score from 0.0 to 1.0 indicating validation certainty

@dataclass
class CoverageResult:
    """
    Represents the overall coverage analysis results for the entire requirements set.
    
    This data class aggregates statistics about how well the AI-generated requirements
    cover the actual CardDemo codebase components.
    """
    total_requirements: int           # Total number of requirements analyzed
    validated_requirements: int       # Number of requirements that passed validation
    coverage_percentage: float        # Percentage of requirements that passed (0-100)
    missing_requirements: List[str]   # List of components that should have requirements but don't
    unidentified_features: List[str]  # List of codebase features not mentioned in requirements

# =============================================================================
# MAIN VALIDATOR CLASS - Core validation logic and component discovery
# =============================================================================

class CardDemoRequirementsValidator:
    """
    Main validator class that orchestrates the entire validation process.
    
    This class is responsible for:
    1. Discovering and cataloging CardDemo codebase components
    2. Loading and parsing AI-generated requirements
    3. Validating requirements against actual codebase components
    4. Calculating coverage statistics
    5. Generating comprehensive validation reports
    """
    
    def __init__(self, requirements_file: str, codebase_path: str):
        """
        Initialize the validator with file paths and discover codebase components.
        
        Args:
            requirements_file: Path to the JSON file containing AI-generated requirements
            codebase_path: Path to the CardDemo codebase root directory
        """
        # Store file paths for later use
        self.requirements_file = requirements_file
        self.codebase_path = Path(codebase_path)
        
        # Initialize data storage
        self.requirements_data = None      # Will hold the parsed JSON requirements
        self.validation_results = []       # Will store all validation results
        self.coverage_result = None        # Will store final coverage statistics
        
        # Discover and catalog all CardDemo components for validation
        # This creates a comprehensive map of what actually exists in the codebase
        self.known_components = {
            'programs': self._get_cobol_programs(),        # COBOL program names
            'transactions': self._get_cics_transactions(), # CICS transaction IDs
            'files': self._get_vsam_files(),              # VSAM data files
            'mapsets': self._get_bms_mapsets(),           # BMS screen mapsets
            'jcl_jobs': self._get_jcl_jobs(),             # JCL batch jobs
            'data_structures': self._get_data_structures() # COBOL copybook structures
        }
        
    # =============================================================================
    # COMPONENT DISCOVERY METHODS - Extract actual codebase components for validation
    # =============================================================================
    
    def _get_cobol_programs(self) -> Set[str]:
        """
        Extract COBOL program names from the codebase.
        
        Scans the app/cbl directory for .cbl files and extracts their names
        (without extension) to create a list of all COBOL programs in the system.
        
        Returns:
            Set of COBOL program names (e.g., {'COACTUPC', 'COSGN00C', 'COMEN01C'})
        """
        programs = set()
        cbl_path = self.codebase_path / 'app' / 'cbl'  # Path to COBOL source files
        if cbl_path.exists():
            # Find all .cbl files and extract their names without extension
            for file in cbl_path.glob('*.cbl'):
                programs.add(file.stem)  # file.stem gives filename without extension
        return programs
    
    def _get_cics_transactions(self) -> Set[str]:
        """
        Extract CICS transaction IDs from the CSD (CICS System Definition) file.
        
        Parses the CARDDEMO.CSD file to find all DEFINE TRANSACTION statements
        and extracts the 4-character transaction IDs (e.g., 'CC00', 'CAUP').
        
        Returns:
            Set of CICS transaction IDs (e.g., {'CC00', 'CAUP', 'CACT'})
        """
        transactions = set()
        csd_file = self.codebase_path / 'app' / 'csd' / 'CARDDEMO.CSD'  # CICS system definition file
        if csd_file.exists():
            with open(csd_file, 'r') as f:
                content = f.read()
                # Use regex to find all DEFINE TRANSACTION statements and extract 4-character IDs
                # Pattern matches: DEFINE TRANSACTION(XXXX) where XXXX is 4 alphanumeric characters
                matches = re.findall(r'DEFINE TRANSACTION\(([A-Z0-9]{4})\)', content)
                transactions.update(matches)
        return transactions
    
    def _get_vsam_files(self) -> Set[str]:
        """
        Extract VSAM file names from the data directory.
        
        Scans the app/data/EBCDIC directory for .PS files (Physical Sequential files)
        which represent VSAM data files in the CardDemo system.
        
        Returns:
            Set of VSAM file names (e.g., {'ACCTDATA', 'CARDDATA', 'TRANDATA'})
        """
        files = set()
        data_path = self.codebase_path / 'app' / 'data' / 'EBCDIC'  # Path to VSAM data files
        if data_path.exists():
            # Find all .PS files (Physical Sequential = VSAM files)
            for file in data_path.glob('*.PS'):
                files.add(file.stem)  # Extract filename without extension
        return files
    
    def _get_bms_mapsets(self) -> Set[str]:
        """
        Extract BMS (Basic Mapping Support) mapset names from the codebase.
        
        Scans the app/bms directory for .bms files which define screen layouts
        and user interface components for the CICS application.
        
        Returns:
            Set of BMS mapset names (e.g., {'COACTUP', 'COSGN00', 'COMEN01'})
        """
        mapsets = set()
        bms_path = self.codebase_path / 'app' / 'bms'  # Path to BMS screen definition files
        if bms_path.exists():
            # Find all .bms files and extract their names
            for file in bms_path.glob('*.bms'):
                mapsets.add(file.stem)
        return mapsets
    
    def _get_jcl_jobs(self) -> Set[str]:
        """
        Extract JCL (Job Control Language) job names from the codebase.
        
        Scans the app/jcl directory for .jcl files which define batch processing jobs
        and system operations in the mainframe environment.
        
        Returns:
            Set of JCL job names (e.g., {'POSTTRAN', 'DAILYBAL', 'MONTHLY'})
        """
        jobs = set()
        jcl_path = self.codebase_path / 'app' / 'jcl'  # Path to JCL job definition files
        if jcl_path.exists():
            # Find all .jcl files and extract their names
            for file in jcl_path.glob('*.jcl'):
                jobs.add(file.stem)
        return jobs
    
    def _get_data_structures(self) -> Dict[str, Any]:
        """
        Extract data structure information from COBOL copybooks.
        
        Scans the app/cpy directory for .cpy files (COBOL copybooks) and parses each one
        to extract field definitions and data structures used throughout the application.
        
        Returns:
            Dictionary mapping copybook names to their field definitions
            (e.g., {'CSDAT01Y': {'ACCTNO': {'level': '05', 'pic': 'X(16)'}, ...}})
        """
        structures = {}
        cpy_path = self.codebase_path / 'app' / 'cpy'  # Path to COBOL copybook files
        if cpy_path.exists():
            # Process each .cpy file to extract data structure information
            for file in cpy_path.glob('*.cpy'):
                structures[file.stem] = self._parse_copybook(file)
        return structures
    
    def _parse_copybook(self, file_path: Path) -> Dict[str, Any]:
        """
        Parse a COBOL copybook file to extract field definitions.
        
        Uses regex pattern matching to identify COBOL field definitions in the format:
        '05 FIELDNAME PIC X(16)' and extracts the level, field name, and PIC clause.
        
        Args:
            file_path: Path to the .cpy file to parse
            
        Returns:
            Dictionary mapping field names to their definitions
            (e.g., {'ACCTNO': {'level': '05', 'pic': 'X(16)'}})
        """
        fields = {}
        try:
            with open(file_path, 'r') as f:
                content = f.read()
                # Regex pattern to match COBOL field definitions:
                # (\d{2}) - Two digits for level number (e.g., "05")
                # \s+(\w+) - Whitespace followed by field name
                # \s+PIC\s+ - "PIC" keyword with whitespace
                # ([^.]*) - Everything up to the next period (PIC clause)
                field_pattern = r'(\d{2})\s+(\w+)\s+PIC\s+([^.]*)'
                matches = re.findall(field_pattern, content)
                
                # Process each matched field definition
                for level, name, pic in matches:
                    fields[name] = {'level': level, 'pic': pic.strip()}
        except Exception as e:
            logger.warning(f"Error parsing copybook {file_path}: {e}")
        return fields
    
    # =============================================================================
    # REQUIREMENTS LOADING AND VALIDATION - Core validation orchestration
    # =============================================================================
    
    def load_requirements(self) -> None:
        """
        Load and parse the AI-generated requirements JSON file.
        
        Reads the JSON file containing the AI-generated requirements and stores
        the parsed data for later validation. This is the first step in the
        validation process.
        
        Raises:
            Exception: If the JSON file cannot be loaded or parsed
        """
        try:
            with open(self.requirements_file, 'r') as f:
                self.requirements_data = json.load(f)  # Parse JSON into Python dictionary
            logger.info(f"Loaded requirements from {self.requirements_file}")
        except Exception as e:
            logger.error(f"Error loading requirements file: {e}")
            raise  # Re-raise the exception to stop processing
    
    def validate_technical_requirements(self) -> List[ValidationResult]:
        """
        Validate all technical requirements against the actual codebase.
        
        This is the main validation method that:
        1. Extracts technical requirements from the JSON structure
        2. Validates each requirement against the discovered codebase components
        3. Returns detailed validation results for each requirement
        
        Returns:
            List of ValidationResult objects, one for each technical requirement
        """
        results = []
        
        # Check if requirements data is available
        if not self.requirements_data or 'data' not in self.requirements_data:
            logger.error("No requirements data found")
            return results
        
        # Extract all technical requirements from the JSON structure
        tech_reqs = self._extract_technical_requirements()
        
        # Validate each technical requirement individually
        for i, req in enumerate(tech_reqs):
            req_id = f"TECH_REQ_{i+1:03d}"  # Create unique ID (e.g., "TECH_REQ_001")
            result = self._validate_single_requirement(req_id, req)
            results.append(result)
            
        return results
    
    def _extract_technical_requirements(self) -> List[str]:
        """
        Extract technical requirements from the nested JSON structure.
        
        Recursively traverses the JSON data structure to find all technical requirements.
        The AI-generated JSON typically has a nested structure where requirements
        are embedded within various sections and subsections.
        
        Returns:
            List of technical requirement text strings
        """
        requirements = []
        
        def extract_from_section(section):
            """
            Recursive helper function to extract requirements from any section.
            
            Handles both dictionary and list structures, looking for 'technical_requirements'
            keys and extracting the 'requirement' field from them.
            """
            if isinstance(section, dict):
                # Check if this dictionary contains technical requirements
                if 'technical_requirements' in section:
                    tech_req = section['technical_requirements']
                    if isinstance(tech_req, dict) and 'requirement' in tech_req:
                        requirements.append(tech_req['requirement'])
                # Recursively check all values in this dictionary
                for key, value in section.items():
                    extract_from_section(value)
            elif isinstance(section, list):
                # Recursively check all items in this list
                for item in section:
                    extract_from_section(item)
        
        # Start the recursive extraction from the root of the requirements data
        extract_from_section(self.requirements_data)
        return requirements
    
    def _validate_single_requirement(self, req_id: str, requirement_text: str) -> ValidationResult:
        """
        Validate a single technical requirement against the codebase.
        
        This method performs comprehensive validation by checking:
        1. Component mentions (programs, transactions, files)
        2. Architectural patterns (CICS, BMS, VSAM, JCL)
        3. Data model consistency (copybook structures)
        
        Args:
            req_id: Unique identifier for the requirement
            requirement_text: The full text of the requirement to validate
            
        Returns:
            ValidationResult object with detailed validation information
        """
        errors = []
        evidence = []
        confidence = 0.0
        
        # Step 1: Check if mentioned components actually exist in the codebase
        component_checks = self._check_component_mentions(requirement_text)
        errors.extend(component_checks['errors'])
        evidence.extend(component_checks['evidence'])
        
        # Step 2: Check if architectural patterns are correctly identified
        arch_checks = self._check_architectural_patterns(requirement_text)
        errors.extend(arch_checks['errors'])
        evidence.extend(arch_checks['evidence'])
        
        # Step 3: Check if data model references are consistent
        data_checks = self._check_data_model_consistency(requirement_text)
        errors.extend(data_checks['errors'])
        evidence.extend(data_checks['evidence'])
        
        # Step 4: Determine overall validation status and confidence
        if not errors:
            status = 'PASS'      # No errors found - requirement is valid
            confidence = 0.9     # High confidence in validation
        elif len(errors) < 3:
            status = 'PARTIAL'   # Some issues but not critical
            confidence = 0.6     # Medium confidence
        else:
            status = 'FAIL'      # Multiple significant issues
            confidence = 0.3     # Low confidence in requirement accuracy
        
        return ValidationResult(
            requirement_id=req_id,
            requirement_text=requirement_text,  # Keep full text for detailed reporting
            status=status,
            errors=errors,
            evidence=evidence,
            confidence=confidence
        )
    
    # =============================================================================
    # VALIDATION CHECK METHODS - Specific validation logic for different aspects
    # =============================================================================
    
    def _check_component_mentions(self, requirement_text: str) -> Dict[str, List[str]]:
        """
        Check if components mentioned in the requirement actually exist in the codebase.
        
        Uses regex patterns to identify mentions of COBOL programs, CICS transactions,
        and VSAM files, then verifies they exist in the discovered codebase components.
        
        Args:
            requirement_text: The requirement text to analyze
            
        Returns:
            Dictionary with 'errors' and 'evidence' lists
        """
        errors = []
        evidence = []
        
        # Check for COBOL program mentions (pattern: 2-8 uppercase letters ending with 'C')
        # Examples: COACTUPC, COSGN00C, COMEN01C
        program_pattern = r'\b([A-Z]{2,8}C)\b'
        mentioned_programs = re.findall(program_pattern, requirement_text)
        for program in mentioned_programs:
            if program in self.known_components['programs']:
                evidence.append(f"PASS: Program {program} exists in codebase")
            else:
                errors.append(f"FAIL: Program {program} not found in codebase")
        
        # Check for CICS transaction mentions (pattern: exactly 4 uppercase letters/numbers)
        # Examples: CC00, CAUP, CACT, CUSR
        transaction_pattern = r'\b([A-Z0-9]{4})\b'
        mentioned_transactions = re.findall(transaction_pattern, requirement_text)
        for trans in mentioned_transactions:
            if trans in self.known_components['transactions']:
                evidence.append(f"PASS: Transaction {trans} exists in CSD")
            else:
                errors.append(f"FAIL: Transaction {trans} not found in CSD")
        
        # Check for VSAM file mentions (pattern: uppercase letters ending with DAT, SEC, or XREF)
        # Examples: ACCTDATA, CARDSEC, TRANXREF
        file_pattern = r'\b([A-Z]+(?:DAT|SEC|XREF))\b'
        mentioned_files = re.findall(file_pattern, requirement_text)
        for file in mentioned_files:
            if file in self.known_components['files']:
                evidence.append(f"PASS: VSAM file {file} exists in data directory")
            else:
                errors.append(f"FAIL: VSAM file {file} not found in data directory")
        
        return {'errors': errors, 'evidence': evidence}
    
    def _check_architectural_patterns(self, requirement_text: str) -> Dict[str, List[str]]:
        """
        Check if architectural patterns mentioned are consistent with CardDemo implementation.
        
        Validates that when mainframe technologies are mentioned, they include appropriate
        technical details and commands that would be used in a real implementation.
        
        Args:
            requirement_text: The requirement text to analyze
            
        Returns:
            Dictionary with 'errors' and 'evidence' lists
        """
        errors = []
        evidence = []
        
        # Check for CICS (Customer Information Control System) patterns
        if 'CICS' in requirement_text.upper():
            # CICS requirements should mention specific CICS commands like XCTL or EXEC CICS
            if 'XCTL' in requirement_text or 'EXEC CICS' in requirement_text:
                evidence.append("PASS: CICS patterns correctly identified")
            else:
                errors.append("FAIL: CICS mentioned but no CICS commands referenced")
        
        # Check for BMS (Basic Mapping Support) patterns
        if 'BMS' in requirement_text.upper():
            # BMS requirements should mention screen mapping concepts
            if 'MAP' in requirement_text.upper() or 'SCREEN' in requirement_text.upper():
                evidence.append("PASS: BMS screen mapping correctly identified")
            else:
                errors.append("FAIL: BMS mentioned but no screen mapping details")
        
        # Check for VSAM (Virtual Storage Access Method) patterns
        if 'VSAM' in requirement_text.upper():
            # VSAM requirements should mention file operations
            if 'READ' in requirement_text.upper() or 'WRITE' in requirement_text.upper():
                evidence.append("PASS: VSAM file operations correctly identified")
            else:
                errors.append("FAIL: VSAM mentioned but no file operations specified")
        
        # Check for JCL (Job Control Language) patterns
        if 'JCL' in requirement_text.upper() or 'BATCH' in requirement_text.upper():
            # JCL requirements should mention job structure elements
            if 'JOB' in requirement_text.upper() or 'EXEC' in requirement_text.upper():
                evidence.append("PASS: JCL/batch processing correctly identified")
            else:
                errors.append("FAIL: JCL/batch mentioned but no job structure specified")
        
        return {'errors': errors, 'evidence': evidence}
    
    def _check_data_model_consistency(self, requirement_text: str) -> Dict[str, List[str]]:
        """
        Check if data model references are consistent with actual copybook structures.
        
        Validates that when data structures or fields are mentioned in requirements,
        they actually exist in the COBOL copybooks discovered in the codebase.
        
        Args:
            requirement_text: The requirement text to analyze
            
        Returns:
            Dictionary with 'errors' and 'evidence' lists
        """
        errors = []
        evidence = []
        
        # Check for data structure mentions against discovered copybook structures
        for struct_name, fields in self.known_components['data_structures'].items():
            if struct_name in requirement_text:
                evidence.append(f"PASS: Data structure {struct_name} correctly referenced")
                
                # Check for field mentions within this data structure
                for field_name in fields.keys():
                    if field_name in requirement_text:
                        evidence.append(f"PASS: Field {field_name} in {struct_name} correctly referenced")
        
        return {'errors': errors, 'evidence': evidence}
    
    # =============================================================================
    # USER STORY VALIDATION - Validate user stories and acceptance criteria
    # =============================================================================
    
    def validate_user_stories(self) -> List[ValidationResult]:
        """
        Validate user stories against the codebase implementation.
        
        Extracts user stories from the requirements JSON and validates each one
        to ensure the described functionality can be implemented with existing
        CardDemo components.
        
        Returns:
            List of ValidationResult objects, one for each user story
        """
        results = []
        
        # Check if requirements data is available
        if not self.requirements_data or 'data' not in self.requirements_data:
            return results
        
        # Extract all user stories from the JSON structure
        user_stories = self._extract_user_stories()
        
        # Validate each user story individually
        for i, story in enumerate(user_stories):
            story_id = f"USER_STORY_{i+1:03d}"  # Create unique ID (e.g., "USER_STORY_001")
            result = self._validate_user_story(story_id, story)
            results.append(result)
        
        return results
    
    def _extract_user_stories(self) -> List[Dict[str, Any]]:
        """
        Extract user stories from the JSON structure.
        
        Looks for the 'user_stories' section in the requirements data and returns
        the list of user story objects for validation.
        
        Returns:
            List of user story dictionaries containing title, description, and acceptance criteria
        """
        stories = []
        
        # Check if user stories exist in the requirements data structure
        if 'data' in self.requirements_data and 'user_stories' in self.requirements_data['data']:
            stories = self.requirements_data['data']['user_stories']
        
        return stories
    
    def _validate_user_story(self, story_id: str, story: Dict[str, Any]) -> ValidationResult:
        """
        Validate a single user story against the codebase.
        
        Analyzes the user story title, description, and acceptance criteria to determine
        if the described functionality can be implemented with existing CardDemo components.
        
        Args:
            story_id: Unique identifier for the user story
            story: Dictionary containing the user story data
            
        Returns:
            ValidationResult object with detailed validation information
        """
        errors = []
        evidence = []
        confidence = 0.0
        
        # Extract story components for analysis
        title = story.get('title', '')           # User story title
        description = story.get('description', '') # Detailed description
        acceptance_criteria = story.get('acceptance_criteria', [])  # List of acceptance criteria
        
        # Step 1: Check if the story can be implemented with existing components
        component_check = self._check_story_implementability(title + " " + description)
        errors.extend(component_check['errors'])
        evidence.extend(component_check['evidence'])
        
        # Step 2: Check each acceptance criterion for achievability
        for criterion in acceptance_criteria:
            criterion_check = self._check_acceptance_criterion(criterion)
            errors.extend(criterion_check['errors'])
            evidence.extend(criterion_check['evidence'])
        
        # Step 3: Determine overall validation status and confidence
        if not errors:
            status = 'PASS'      # No issues found - story is implementable
            confidence = 0.8     # High confidence in implementation
        elif len(errors) < 2:
            status = 'PARTIAL'   # Minor issues but generally implementable
            confidence = 0.5     # Medium confidence
        else:
            status = 'FAIL'      # Significant implementation challenges
            confidence = 0.2     # Low confidence in achievability
        
        return ValidationResult(
            requirement_id=story_id,
            requirement_text=f"User Story: {title}",
            status=status,
            errors=errors,
            evidence=evidence,
            confidence=confidence
        )
    
    def _check_story_implementability(self, story_text: str) -> Dict[str, List[str]]:
        """
        Check if a user story can be implemented with existing CardDemo components.
        
        Analyzes the story text to identify functional areas (account, card, transaction)
        and verifies that appropriate programs exist in the codebase to support them.
        
        Args:
            story_text: Combined title and description of the user story
            
        Returns:
            Dictionary with 'errors' and 'evidence' lists
        """
        errors = []
        evidence = []
        
        # Check for account-related functionality
        if 'account' in story_text.lower():
            # Look for account-related programs (COACT prefix) in the story or codebase
            if 'COACT' in story_text or any('COACT' in prog for prog in self.known_components['programs']):
                evidence.append("PASS: Account-related programs available")
            else:
                errors.append("FAIL: Account functionality mentioned but no account programs found")
        
        # Check for card-related functionality
        if 'card' in story_text.lower():
            # Look for card-related programs (COCRD prefix) in the story or codebase
            if 'COCRD' in story_text or any('COCRD' in prog for prog in self.known_components['programs']):
                evidence.append("PASS: Card-related programs available")
            else:
                errors.append("FAIL: Card functionality mentioned but no card programs found")
        
        # Check for transaction-related functionality
        if 'transaction' in story_text.lower():
            # Look for transaction-related programs (COTRN prefix) in the story or codebase
            if 'COTRN' in story_text or any('COTRN' in prog for prog in self.known_components['programs']):
                evidence.append("PASS: Transaction-related programs available")
            else:
                errors.append("FAIL: Transaction functionality mentioned but no transaction programs found")
        
        return {'errors': errors, 'evidence': evidence}
    
    def _check_acceptance_criterion(self, criterion: str) -> Dict[str, List[str]]:
        """
        Check if an acceptance criterion is achievable with CardDemo capabilities.
        
        Validates that acceptance criteria mention functionality that can be
        implemented within the constraints of a CICS mainframe application.
        
        Args:
            criterion: Text of the acceptance criterion to validate
            
        Returns:
            Dictionary with 'errors' and 'evidence' lists
        """
        errors = []
        evidence = []
        
        # Check for input validation requirements (standard in CICS applications)
        if 'validation' in criterion.lower():
            evidence.append("PASS: Input validation is standard in CICS applications")
        
        # Check for error handling requirements (standard in CICS applications)
        if 'error handling' in criterion.lower():
            evidence.append("PASS: Error handling is standard in CICS applications")
        
        # Check for database operations (must specify VSAM or DB2)
        if 'database' in criterion.lower():
            if 'VSAM' in criterion or 'DB2' in criterion:
                evidence.append("PASS: Database operations supported")
            else:
                errors.append("FAIL: Database operations mentioned but no specific database specified")
        
        return {'errors': errors, 'evidence': evidence}
    
    # =============================================================================
    # COVERAGE ANALYSIS - Calculate statistics and identify gaps
    # =============================================================================
    
    def calculate_coverage(self) -> CoverageResult:
        """
        Calculate comprehensive coverage statistics for the requirements validation.
        
        This method analyzes all validation results to determine:
        1. Total number of requirements analyzed
        2. Number of requirements that passed validation
        3. Coverage percentage
        4. Missing requirements (components without requirements)
        5. Unidentified features (codebase components not mentioned in requirements)
        
        Returns:
            CoverageResult object with detailed coverage statistics
        """
        # Calculate total requirements (technical + user stories)
        total_requirements = len(self._extract_technical_requirements()) + len(self._extract_user_stories())
        
        # Count requirements that passed validation
        validated_requirements = len([r for r in self.validation_results if r.status == 'PASS'])
        
        # Calculate coverage percentage (avoid division by zero)
        coverage_percentage = (validated_requirements / total_requirements * 100) if total_requirements > 0 else 0
        
        # Identify components that lack specific requirements
        missing_requirements = self._identify_missing_requirements()
        
        # Identify codebase features not mentioned in requirements
        unidentified_features = self._identify_unidentified_features()
        
        # Create and store the coverage result
        self.coverage_result = CoverageResult(
            total_requirements=total_requirements,
            validated_requirements=validated_requirements,
            coverage_percentage=coverage_percentage,
            missing_requirements=missing_requirements,
            unidentified_features=unidentified_features
        )
        
        return self.coverage_result
    
    def _identify_missing_requirements(self) -> List[str]:
        """
        Identify components that should have requirements but are missing from the AI-generated requirements.
        
        Scans through all discovered codebase components and checks if they are mentioned
        in any of the validated requirements. Components without requirements are flagged.
        
        Returns:
            List of missing requirement descriptions
        """
        missing = []
        
        # Check for missing program requirements
        for program in self.known_components['programs']:
            # Look for this program name in any requirement text
            if not any(program in req.requirement_text for req in self.validation_results):
                missing.append(f"Requirements for program {program}")
        
        # Check for missing transaction requirements
        for transaction in self.known_components['transactions']:
            # Look for this transaction ID in any requirement text
            if not any(transaction in req.requirement_text for req in self.validation_results):
                missing.append(f"Requirements for transaction {transaction}")
        
        # Check for missing file requirements
        for file in self.known_components['files']:
            # Look for this file name in any requirement text
            if not any(file in req.requirement_text for req in self.validation_results):
                missing.append(f"Requirements for VSAM file {file}")
        
        return missing
    
    def _identify_unidentified_features(self) -> List[str]:
        """
        Identify codebase features that weren't mentioned in any requirements.
        
        This is similar to missing requirements but focuses on identifying
        components that exist in the codebase but were not referenced in
        the AI-generated requirements, indicating potential gaps in coverage.
        
        Returns:
            List of unidentified feature descriptions
        """
        unidentified = []
        
        # Check for programs not mentioned in requirements
        for program in self.known_components['programs']:
            if not any(program in req.requirement_text for req in self.validation_results):
                unidentified.append(f"Program {program} not mentioned in requirements")
        
        # Check for transactions not mentioned in requirements
        for transaction in self.known_components['transactions']:
            if not any(transaction in req.requirement_text for req in self.validation_results):
                unidentified.append(f"Transaction {transaction} not mentioned in requirements")
        
        return unidentified
    
    # =============================================================================
    # REPORT GENERATION - Create comprehensive validation reports
    # =============================================================================
    
    def generate_markdown_report(self, output_file: str = None) -> str:
        """
        Generate a comprehensive validation report in Markdown format.
        
        Creates a detailed, well-formatted report that includes:
        - Executive summary with coverage statistics
        - Summary statistics and breakdowns
        - Sample validation results
        - Coverage analysis with missing requirements
        - Recommendations for improvement
        - Complete validation results table
        
        Args:
            output_file: Optional file path to save the report
            
        Returns:
            Complete report text as a string
        """
        report = []
        
        # =============================================================================
        # REPORT HEADER - Basic information and metadata
        # =============================================================================
        report.append("# CardDemo Requirements Validation Report")
        report.append("")
        report.append(f"**Generated:** {self._get_timestamp()}")
        report.append(f"**Application:** AWS.M2.CARDDEMO")
        report.append(f"**Validator Version:** 1.0")
        report.append("")
        report.append("---")
        report.append("")
        
        # =============================================================================
        # EXECUTIVE SUMMARY - High-level overview for stakeholders
        # =============================================================================
        report.append("## Executive Summary")
        report.append("")
        if self.coverage_result:
            # Determine overall coverage status based on percentage
            coverage_status = "✅ Good" if self.coverage_result.coverage_percentage >= 70 else "⚠ Needs Improvement"
            report.append(f"**Overall Coverage:** {self.coverage_result.coverage_percentage:.1f}% ({coverage_status})")
            report.append("")
            # Key metrics for quick assessment
            report.append(f"- **Total Requirements:** {self.coverage_result.total_requirements}")
            report.append(f"- **Validated Requirements:** {self.coverage_result.validated_requirements}")
            report.append(f"- **Missing Requirements:** {len(self.coverage_result.missing_requirements)}")
            report.append(f"- **Unidentified Features:** {len(self.coverage_result.unidentified_features)}")
        report.append("")
        
        # =============================================================================
        # SUMMARY STATISTICS - Detailed breakdown of validation results
        # =============================================================================
        report.append("## Summary Statistics")
        report.append("")
        if self.coverage_result:
            # Calculate counts for each validation status
            pass_count = len([r for r in self.validation_results if r.status == 'PASS'])
            partial_count = len([r for r in self.validation_results if r.status == 'PARTIAL'])
            fail_count = len([r for r in self.validation_results if r.status == 'FAIL'])
            
            # Create summary table with percentages
            report.append("| Metric | Count | Percentage |")
            report.append("|--------|-------|------------|")
            report.append(f"| **Pass** | {pass_count} | {(pass_count/self.coverage_result.total_requirements*100):.1f}% |")
            report.append(f"| **Partial** | {partial_count} | {(partial_count/self.coverage_result.total_requirements*100):.1f}% |")
            report.append(f"| **Fail** | {fail_count} | {(fail_count/self.coverage_result.total_requirements*100):.1f}% |")
            report.append(f"| **Total** | {self.coverage_result.total_requirements} | 100% |")
        report.append("")
        
        # =============================================================================
        # VALIDATION RESULTS SUMMARY - Breakdown by requirement type
        # =============================================================================
        report.append("## Validation Results Summary")
        report.append("")
        
        # Technical Requirements breakdown
        report.append("### Technical Requirements")
        report.append("")
        
        # Filter and count technical requirements by status
        tech_results = [r for r in self.validation_results if r.requirement_id.startswith('TECH_REQ')]
        tech_pass = len([r for r in tech_results if r.status == 'PASS'])
        tech_partial = len([r for r in tech_results if r.status == 'PARTIAL'])
        tech_fail = len([r for r in tech_results if r.status == 'FAIL'])
        
        # Create status summary table for technical requirements
        report.append("| Status | Count |")
        report.append("|--------|-------|")
        report.append(f"| ✅ Pass | {tech_pass} |")
        report.append(f"| ⚠ Partial | {tech_partial} |")
        report.append(f"| ❌ Fail | {tech_fail} |")
        report.append("")
        
        # User Stories breakdown
        report.append("### User Stories")
        report.append("")
        
        # Filter and count user stories by status
        user_results = [r for r in self.validation_results if r.requirement_id.startswith('USER_STORY')]
        user_pass = len([r for r in user_results if r.status == 'PASS'])
        user_partial = len([r for r in user_results if r.status == 'PARTIAL'])
        user_fail = len([r for r in user_results if r.status == 'FAIL'])
        
        # Create status summary table for user stories
        report.append("| Status | Count |")
        report.append("|--------|-------|")
        report.append(f"| ✅ Pass | {user_pass} |")
        report.append(f"| ⚠ Partial | {user_partial} |")
        report.append(f"| ❌ Fail | {user_fail} |")
        report.append("")
        
        # =============================================================================
        # SAMPLE VALIDATION RESULTS - Examples of failed requirements
        # =============================================================================
        report.append("## Sample Validation Results")
        report.append("")
        report.append("### Failed Technical Requirements (Top 5)")
        report.append("")
        
        # Show top 5 failed technical requirements as examples with detailed error breakdowns
        failed_tech = [r for r in tech_results if r.status == 'FAIL'][:5]
        if failed_tech:
            report.append("| ID | Status | Confidence | Error Count | Primary Issues |")
            report.append("|----|--------|------------|-------------|----------------|")
            for result in failed_tech:
                error_count = len(result.errors)
                # Show the first 2-3 errors as primary issues
                primary_issues = []
                for error in result.errors[:3]:
                    # Extract the key part of the error message
                    if "not found" in error:
                        primary_issues.append("Missing component")
                    elif "no CICS commands" in error:
                        primary_issues.append("Missing CICS details")
                    elif "no screen mapping" in error:
                        primary_issues.append("Missing BMS details")
                    elif "no file operations" in error:
                        primary_issues.append("Missing VSAM details")
                    elif "no job structure" in error:
                        primary_issues.append("Missing JCL details")
                    else:
                        primary_issues.append("Validation error")
                
                issues_summary = ", ".join(set(primary_issues))  # Remove duplicates
                if len(result.errors) > 3:
                    issues_summary += f" (+{len(result.errors) - 3} more)"
                
                report.append(f"| {result.requirement_id} | ❌ Fail | {result.confidence:.2f} | {error_count} | {issues_summary} |")
        else:
            report.append("*No failed technical requirements found.*")
        report.append("")
        
        # Show sample partial requirements
        partial_tech = [r for r in tech_results if r.status == 'PARTIAL'][:3]
        if partial_tech:
            report.append("### Partial Technical Requirements (Sample)")
            report.append("")
            report.append("| ID | Status | Confidence | Error Count | Issues |")
            report.append("|----|--------|------------|-------------|--------|")
            for result in partial_tech:
                error_count = len(result.errors)
                # Show the specific errors
                issues_summary = "; ".join([error.split(": ")[-1] if ": " in error else error for error in result.errors[:2]])
                if len(result.errors) > 2:
                    issues_summary += f" (+{len(result.errors) - 2} more)"
                
                report.append(f"| {result.requirement_id} | ⚠ Partial | {result.confidence:.2f} | {error_count} | {issues_summary} |")
            report.append("")
        
        # =============================================================================
        # COVERAGE ANALYSIS - Detailed analysis of gaps and missing components
        # =============================================================================
        report.append("## Coverage Analysis")
        report.append("")
        if self.coverage_result:
            report.append(f"**Coverage Percentage:** {self.coverage_result.coverage_percentage:.1f}%")
            report.append("")
            
            # List components that lack specific requirements
            if self.coverage_result.missing_requirements:
                report.append("### Missing Requirements")
                report.append("")
                report.append("The following components lack specific requirements:")
                report.append("")
                for i, missing in enumerate(self.coverage_result.missing_requirements, 1):
                    report.append(f"{i}. {missing}")
                report.append("")
            
            # List codebase features not mentioned in requirements
            if self.coverage_result.unidentified_features:
                report.append("### Unidentified Features")
                report.append("")
                report.append("The following features were not mentioned in requirements:")
                report.append("")
                for i, feature in enumerate(self.coverage_result.unidentified_features, 1):
                    report.append(f"{i}. {feature}")
                report.append("")
        
        # =============================================================================
        # RECOMMENDATIONS - Actionable guidance for improvement
        # =============================================================================
        report.append("## Recommendations")
        report.append("")
        if self.coverage_result:
            if self.coverage_result.coverage_percentage < 70:
                # Provide detailed improvement guidance for low coverage
                report.append("### ⚠ Areas for Improvement")
                report.append("")
                report.append("1. **Add Missing Program Requirements**")
                report.append("   - Create requirements for all COBOL programs")
                report.append("   - Document transaction flows")
                report.append("   - Specify data file requirements")
                report.append("")
                report.append("2. **Enhance Technical Specifications**")
                report.append("   - Align technology assumptions with actual implementation")
                report.append("   - Correct architectural patterns")
                report.append("   - Update data model references")
                report.append("")
                report.append("3. **Improve Coverage**")
                report.append("   - Include requirements for all transactions")
                report.append("   - Document all VSAM files")
                report.append("   - Specify BMS mapset requirements")
            else:
                # Positive feedback for good coverage
                report.append("### ✅ Good Coverage Achieved")
                report.append("")
                report.append("The AI-generated requirements show good alignment with the CardDemo codebase.")
                report.append("Consider minor refinements for optimal coverage.")
        report.append("")
        
        # =============================================================================
        # DETAILED RESULTS - Complete validation results with error breakdowns
        # =============================================================================
        report.append("## Appendix: Detailed Results")
        report.append("")
        
        # =============================================================================
        # FAILED REQUIREMENTS DETAILS - Show exact errors for failed requirements
        # =============================================================================
        failed_requirements = [r for r in self.validation_results if r.status == 'FAIL']
        if failed_requirements:
            report.append("### Failed Requirements - Detailed Error Analysis")
            report.append("")
            report.append("The following requirements failed validation. Each failure includes the specific errors found:")
            report.append("")
            
            for result in failed_requirements:
                report.append(f"#### {result.requirement_id} - {result.status} (Confidence: {result.confidence:.2f})")
                report.append("")
                report.append("**Requirement Text:**")
                report.append(f"```")
                report.append(result.requirement_text[:1000] + ("..." if len(result.requirement_text) > 1000 else ""))
                report.append("```")
                report.append("")
                
                if result.errors:
                    report.append("**Validation Errors:**")
                    report.append("")
                    for i, error in enumerate(result.errors, 1):
                        report.append(f"{i}. ❌ {error}")
                    report.append("")
                
                if result.evidence:
                    report.append("**Supporting Evidence:**")
                    report.append("")
                    for i, evidence in enumerate(result.evidence, 1):
                        report.append(f"{i}. ✅ {evidence}")
                    report.append("")
                
                report.append("---")
                report.append("")
        
        # =============================================================================
        # PARTIAL REQUIREMENTS DETAILS - Show issues for partial requirements
        # =============================================================================
        partial_requirements = [r for r in self.validation_results if r.status == 'PARTIAL']
        if partial_requirements:
            report.append("### Partial Requirements - Issues Found")
            report.append("")
            report.append("The following requirements passed with minor issues:")
            report.append("")
            
            for result in partial_requirements:
                report.append(f"#### {result.requirement_id} - {result.status} (Confidence: {result.confidence:.2f})")
                report.append("")
                report.append("**Requirement Text:**")
                report.append(f"```")
                report.append(result.requirement_text[:800] + ("..." if len(result.requirement_text) > 800 else ""))
                report.append("```")
                report.append("")
                
                if result.errors:
                    report.append("**Issues Found:**")
                    report.append("")
                    for i, error in enumerate(result.errors, 1):
                        report.append(f"{i}. ⚠️ {error}")
                    report.append("")
                
                if result.evidence:
                    report.append("**Valid Elements:**")
                    report.append("")
                    for i, evidence in enumerate(result.evidence, 1):
                        report.append(f"{i}. ✅ {evidence}")
                    report.append("")
                
                report.append("---")
                report.append("")
        
        # =============================================================================
        # SUMMARY TABLE - All validation results in table format
        # =============================================================================
        report.append("### All Validation Results Summary")
        report.append("")
        report.append("| ID | Type | Status | Confidence | Error Count | Text Preview |")
        report.append("|----|------|--------|------------|-------------|--------------|")
        
        # Generate complete results table with error counts
        for result in self.validation_results:
            req_type = "Tech" if result.requirement_id.startswith('TECH_REQ') else "User"
            status_icon = "✅" if result.status == 'PASS' else "⚠" if result.status == 'PARTIAL' else "❌"
            error_count = len(result.errors)
            
            # Create text preview with 500+ character minimum and word boundary breaking
            preview = result.requirement_text[:500].replace('\n', ' ').replace('|', '\\|').strip()
            if len(result.requirement_text) > 500:
                # Try to break at a word boundary to avoid cutting words
                last_space = preview.rfind(' ')
                if last_space > 450:  # Only break at word if we have enough content
                    preview = preview[:last_space]
                preview = preview + "..."
            report.append(f"| {result.requirement_id} | {req_type} | {status_icon} {result.status} | {result.confidence:.2f} | {error_count} | {preview} |")
        
        # =============================================================================
        # REPORT FOOTER - Final formatting and file output
        # =============================================================================
        report.append("")
        report.append("---")
        report.append("")
        report.append("*Report generated by CardDemo Requirements Validator v1.0*")
        
        # Combine all report sections into final text
        report_text = "\n".join(report)
        
        # Save to file if output path provided
        if output_file:
            with open(output_file, 'w', encoding='utf-8') as f:
                f.write(report_text)
            logger.info(f"Markdown report saved to {output_file}")
        
        return report_text
    
    # =============================================================================
    # UTILITY METHODS - Helper functions for report generation and execution
    # =============================================================================
    
    def _get_timestamp(self) -> str:
        """
        Get current timestamp in a readable format for report headers.
        
        Returns:
            Formatted timestamp string (e.g., "2024-01-15 14:30:25")
        """
        from datetime import datetime
        return datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    
    def generate_report(self, output_file: str = None) -> str:
        """
        Generate a comprehensive validation report (legacy text format).
        
        This method maintains backward compatibility by calling the Markdown
        report generator, ensuring consistent output format.
        
        Args:
            output_file: Optional file path to save the report
            
        Returns:
            Complete report text as a string
        """
        # Keep the original text format for backward compatibility
        return self.generate_markdown_report(output_file)
    
    def convert_markdown_to_pdf(self, markdown_file: str, pdf_file: str = None) -> bool:
        """
        Convert Markdown report to PDF - Simplified version.
        
        This method was simplified to remove complex PDF generation overhead.
        Users can use browser print-to-PDF functionality instead.
        
        Args:
            markdown_file: Path to the Markdown file
            pdf_file: Optional output PDF file path
            
        Returns:
            Always returns False (PDF generation disabled)
        """
        logger.info("PDF generation removed for simplicity. Use Markdown report instead.")
        return False
    
    # =============================================================================
    # MAIN VALIDATION EXECUTION - Orchestrates the complete validation process
    # =============================================================================
    
    def run_validation(self) -> Dict[str, Any]:
        """
        Run the complete validation process from start to finish.
        
        This is the main orchestration method that:
        1. Loads the AI-generated requirements
        2. Validates technical requirements against the codebase
        3. Validates user stories for implementability
        4. Calculates comprehensive coverage statistics
        5. Generates detailed validation reports
        
        Returns:
            Dictionary containing validation results, coverage statistics, and report text
        """
        logger.info("Starting CardDemo requirements validation...")
        
        # Step 1: Load and parse the requirements JSON file
        self.load_requirements()
        
        # Step 2: Validate all technical requirements against the codebase
        logger.info("Validating technical requirements...")
        tech_results = self.validate_technical_requirements()
        self.validation_results.extend(tech_results)
        
        # Step 3: Validate user stories for implementability
        logger.info("Validating user stories...")
        user_story_results = self.validate_user_stories()
        self.validation_results.extend(user_story_results)
        
        # Step 4: Calculate comprehensive coverage statistics
        logger.info("Calculating coverage...")
        coverage = self.calculate_coverage()
        
        # Step 5: Generate detailed validation report
        logger.info("Generating validation report...")
        report = self.generate_report()
        
        # Return complete validation results
        return {
            'validation_results': self.validation_results,
            'coverage': coverage,
            'report': report
        }

# =============================================================================
# MAIN EXECUTION - Command-line interface and entry point
# =============================================================================

def main():
    """
    Main entry point for command-line execution of the requirements validator.
    
    This function handles:
    1. Command-line argument parsing and validation
    2. Initialization of the validator
    3. Execution of the complete validation process
    4. Display of summary results
    5. Generation of detailed reports
    
    Command-line usage:
        python requirements_validator.py <requirements.json> <codebase_path>
    """
    # Validate command-line arguments
    if len(sys.argv) != 3:
        print("Usage: python requirements_validator.py <requirements.json> <codebase_path>")
        sys.exit(1)
    
    # Extract file paths from command-line arguments
    requirements_file = sys.argv[1]  # Path to AI-generated requirements JSON
    codebase_path = sys.argv[2]      # Path to CardDemo codebase root
    
    # Validate that input files and directories exist
    if not os.path.exists(requirements_file):
        print(f"Error: Requirements file {requirements_file} not found")
        sys.exit(1)
    
    if not os.path.exists(codebase_path):
        print(f"Error: Codebase path {codebase_path} not found")
        sys.exit(1)
    
    # Initialize and run the validator
    validator = CardDemoRequirementsValidator(requirements_file, codebase_path)
    results = validator.run_validation()
    
    # Display validation summary to console
    print("\n" + "=" * 60)
    print("VALIDATION COMPLETE")
    print("=" * 60)
    print(f"Total Requirements: {results['coverage'].total_requirements}")
    print(f"Coverage: {results['coverage'].coverage_percentage:.1f}%")
    print(f"Pass: {len([r for r in results['validation_results'] if r.status == 'PASS'])}")
    print(f"Partial: {len([r for r in results['validation_results'] if r.status == 'PARTIAL'])}")
    print(f"Fail: {len([r for r in results['validation_results'] if r.status == 'FAIL'])}")
    
    # Generate and save detailed report
    report_file = "requirements_validation_report.txt"
    validator.generate_report(report_file)
    print(f"\nDetailed report saved to: {report_file}")

# =============================================================================
# SCRIPT EXECUTION - Entry point when run as standalone script
# =============================================================================

if __name__ == "__main__":
    main()
