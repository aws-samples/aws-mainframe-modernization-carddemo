-- =========================================================================
-- V1__init_carddemo_schema.sql
-- CardDemo Relational Database Schema Initialization
-- Derived from COBOL Copybooks: CVACT01Y, CVCUS01Y, CVCRD01Y, CVTRA01Y, CVXRF01Y
-- =========================================================================

-- 1. Accounts Table (CVACT01Y)
CREATE TABLE IF NOT EXISTS cvact01y (
    acct_id BIGINT PRIMARY KEY,
    acct_active_status VARCHAR(1) NOT NULL DEFAULT 'Y',
    acct_curr_bal NUMERIC(12, 2) NOT NULL DEFAULT 0.00,
    acct_credit_limit NUMERIC(12, 2) NOT NULL DEFAULT 5000.00,
    acct_cash_credit_limit NUMERIC(12, 2) NOT NULL DEFAULT 1000.00,
    acct_open_date VARCHAR(10) NOT NULL,
    acct_expiraion_date VARCHAR(10) NOT NULL,
    acct_reissue_date VARCHAR(10),
    acct_curr_cyc_credit NUMERIC(12, 2) DEFAULT 0.00,
    acct_curr_cyc_debit NUMERIC(12, 2) DEFAULT 0.00,
    acct_group_id VARCHAR(10) DEFAULT 'STANDARD'
);

CREATE INDEX IF NOT EXISTS idx_acct_status ON cvact01y(acct_active_status);

-- 2. Customers Table (CVCUS01Y) with Multi-Occurrence Address and Phone Support
CREATE TABLE IF NOT EXISTS cvcus01y (
    cust_id INTEGER PRIMARY KEY,
    cust_first_name VARCHAR(25) NOT NULL,
    cust_middle_name VARCHAR(25),
    cust_last_name VARCHAR(25) NOT NULL,
    cust_addr_line1 VARCHAR(50) NOT NULL,
    cust_addr_line2 VARCHAR(50),
    cust_addr_line3 VARCHAR(50),
    cust_addr_state_cd VARCHAR(2) NOT NULL,
    cust_addr_country_cd VARCHAR(3) DEFAULT 'USA',
    cust_addr_zip VARCHAR(10) NOT NULL,
    cust_phone_num1 VARCHAR(15),
    cust_phone_num2 VARCHAR(15),
    cust_ssn VARCHAR(9) NOT NULL,
    cust_govt_issued_id VARCHAR(20),
    cust_dob_yyyy_mm_dd VARCHAR(10) NOT NULL,
    cust_efctv_date VARCHAR(10) NOT NULL
);

CREATE INDEX IF NOT EXISTS idx_cust_ssn ON cvcus01y(cust_ssn);

-- 3. Cards Table (CVCRD01Y)
CREATE TABLE IF NOT EXISTS cvcrd01y (
    card_num VARCHAR(16) PRIMARY KEY,
    acct_id BIGINT NOT NULL REFERENCES cvact01y(acct_id),
    card_cvv_cd VARCHAR(3) NOT NULL,
    card_emboss_first_name VARCHAR(25) NOT NULL,
    card_expiraion_date VARCHAR(10) NOT NULL,
    card_active_status VARCHAR(1) NOT NULL DEFAULT 'Y'
);

CREATE INDEX IF NOT EXISTS idx_card_acct ON cvcrd01y(acct_id);

-- 4. Transactions Table (CVTRA01Y)
CREATE TABLE IF NOT EXISTS cvtra01y (
    tran_id VARCHAR(16) PRIMARY KEY,
    tran_type_cd VARCHAR(2) NOT NULL,
    tran_cat_cd INTEGER NOT NULL DEFAULT 1,
    tran_source VARCHAR(10) DEFAULT 'ONLINE',
    tran_desc VARCHAR(100),
    tran_amt NUMERIC(11, 2) NOT NULL,
    tran_merchant_id INTEGER DEFAULT 0,
    tran_merchant_name VARCHAR(50),
    tran_merchant_city VARCHAR(50),
    tran_merchant_zip VARCHAR(10),
    card_num VARCHAR(16) NOT NULL,
    tran_orig_ts VARCHAR(26) NOT NULL,
    tran_proc_ts VARCHAR(26),
    tran_status VARCHAR(10) DEFAULT 'POSTED'
);

CREATE INDEX IF NOT EXISTS idx_tran_card ON cvtra01y(card_num);
CREATE INDEX IF NOT EXISTS idx_tran_orig_ts ON cvtra01y(tran_orig_ts);

-- 5. Account/Customer/Card Cross-Reference Table (CVXRF01Y)
CREATE TABLE IF NOT EXISTS cvxrf01y (
    card_num VARCHAR(16) PRIMARY KEY,
    cust_id INTEGER NOT NULL REFERENCES cvcus01y(cust_id),
    acct_id BIGINT NOT NULL REFERENCES cvact01y(acct_id)
);

CREATE INDEX IF NOT EXISTS idx_xrf_cust ON cvxrf01y(cust_id);
CREATE INDEX IF NOT EXISTS idx_xrf_acct ON cvxrf01y(acct_id);

-- 6. Security & Users Table (SEC_USERS / CSUTLDTC)
CREATE TABLE IF NOT EXISTS sec_users (
    user_id VARCHAR(8) PRIMARY KEY,
    password_hash VARCHAR(100) NOT NULL,
    first_name VARCHAR(20) NOT NULL,
    last_name VARCHAR(20) NOT NULL,
    user_type VARCHAR(10) NOT NULL DEFAULT 'USER',
    active_status VARCHAR(1) NOT NULL DEFAULT 'Y',
    created_ts TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
