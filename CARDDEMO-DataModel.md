# CARDDEMO Data Model - Mermaid Diagram

Based on the CARDDEMO drawio file, here's the data model represented in Mermaid format:

```mermaid
erDiagram
    CUSTOMER ||--o{ ACCOUNT : "owns"
    ACCOUNT ||--o{ CARD : "has"
    CARD ||--o{ TRANSACTION : "generates"
    CUSTOMER ||--o{ CROSSREF : "references"
    ACCOUNT ||--o{ CROSSREF : "links"
    TRANSACTION ||--o{ TRANSACT : "details"
    CARD ||--o{ CARDXREF : "cross-references"
    
    CUSTOMER {
        string CUST_ID PK "Customer ID"
        string CUST_FIRST_NAME "First Name"
        string CUST_MIDDLE_NAME "Middle Name"
        string CUST_LAST_NAME "Last Name"
        string CUST_ADDR_LINE_1 "Address Line 1"
        string CUST_ADDR_LINE_2 "Address Line 2"
        string CUST_ADDR_LINE_3 "Address Line 3"
        string CUST_ADDR_STATE_CD "State Code"
        string CUST_ADDR_COUNTRY_CD "Country Code"
        string CUST_ADDR_ZIP "ZIP Code"
        string CUST_PHONE_NUM_1 "Phone Number 1"
        string CUST_PHONE_NUM_2 "Phone Number 2"
        string CUST_SSN "Social Security Number"
        string CUST_GOVT_ISSUED_ID "Government ID"
        date CUST_DOB_YYYY_MM_DD "Date of Birth"
        string CUST_EFT_ACCOUNT_ID "EFT Account ID"
        string CUST_PRI_CARD_HOLDER_IND "Primary Card Holder"
        string CUST_FICO_CREDIT_SCORE "FICO Credit Score"
    }
    
    ACCOUNT {
        string ACCT_ID PK "Account ID"
        string ACCT_ACTIVE_STATUS "Active Status"
        string ACCT_CURR_BAL "Current Balance"
        string ACCT_CREDIT_LIMIT "Credit Limit"
        string ACCT_CASH_CREDIT_LIMIT "Cash Credit Limit"
        date ACCT_OPEN_DATE "Open Date"
        date ACCT_EXPIRY_DATE "Expiry Date"
        string ACCT_REISSUE_DATE "Reissue Date"
        string ACCT_CURR_CYC_CREDIT "Current Cycle Credit"
        string ACCT_CURR_CYC_DEBIT "Current Cycle Debit"
        string ACCT_GROUP_ID "Group ID"
    }
    
    CARD {
        string CARD_NUM PK "Card Number"
        string CARD_CVV_CD "CVV Code"
        string CARD_EMBOSSED_NAME "Embossed Name"
        date CARD_EXPIRY_DATE "Expiry Date"
        string CARD_ACTIVE_STATUS "Active Status"
        string ACCT_ID FK "Account ID"
    }
    
    TRANSACTION {
        string TRANS_ID PK "Transaction ID"
        string TRANS_TYPE_CD "Transaction Type"
        string TRANS_CAT_CD "Transaction Category"
        string TRANS_SOURCE "Transaction Source"
        string TRANS_DESC "Description"
        decimal TRANS_AMT "Amount"
        string MERCHANT_ID "Merchant ID"
        string MERCHANT_NAME "Merchant Name"
        string MERCHANT_CITY "Merchant City"
        string MERCHANT_ZIP "Merchant ZIP"
        date TRANS_TS "Timestamp"
        string CARD_NUM FK "Card Number"
        string TRANS_ORIG_TS "Original Timestamp"
        string TRANS_PROC_TS "Processing Timestamp"
    }
    
    CROSSREF {
        string XREF_CUST_ID FK "Customer ID"
        string XREF_ACCT_ID FK "Account ID"
        string XREF_CARD_NUM "Card Number"
        string XREF_DATA "Cross Reference Data"
    }
    
    TRANSACT {
        string TRAN_ID PK "Transaction ID"
        string TRAN_TYPE_CD "Transaction Type"
        string TRAN_CAT_CD "Category Code"
        decimal TRAN_AMT "Amount"
        string TRAN_DESC "Description"
        date TRAN_TS "Timestamp"
        string CARD_NUM FK "Card Number"
    }
    
    CARDXREF {
        string CARD_NUM FK "Card Number"
        string XREF_DATA "Cross Reference Data"
        string XREF_TYPE "Cross Reference Type"
    }
```

## Key Relationships

1. **Customer to Account**: One-to-many relationship where customers can own multiple accounts
2. **Account to Card**: One-to-many relationship where accounts can have multiple cards
3. **Card to Transaction**: One-to-many relationship where cards generate multiple transactions
4. **Cross-Reference Tables**: Support many-to-many relationships and additional data linking

## Data Model Notes

- The model supports a typical credit card system with customers, accounts, cards, and transactions
- Cross-reference tables (CROSSREF, CARDXREF) provide flexibility for complex relationships
- Transaction data is captured with merchant information and timestamps
- The model includes both current and historical transaction processing (TRANSACTION vs TRANSACT)
- Customer data includes comprehensive personal and financial information
- Account management includes credit limits, balances, and lifecycle dates

This data model supports the CARDDEMO application's core functionality for managing credit card operations, customer relationships, and transaction processing in a mainframe COBOL environment.