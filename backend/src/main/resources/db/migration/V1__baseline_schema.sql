-- CardDemo baseline schema (plan §2.2) — 13 tables.
-- Source copybooks: app/cpy/CVACT01Y-03Y, CVCUS01Y, CVTRA01Y-06Y, CSUSR01Y.
-- Type rules per plan §2.1 (NUMERIC for money, DATE for dates, VARCHAR trimmed).

-- ---------------------------------------------------------------------------
-- Reference data
-- ---------------------------------------------------------------------------

-- TRANTYPE (CVTRA03Y)
CREATE TABLE transaction_types (
    type_cd     CHAR(2)      NOT NULL,
    description VARCHAR(50),
    CONSTRAINT pk_transaction_types PRIMARY KEY (type_cd)
);

-- TRANCATG (CVTRA04Y) — composite PK (type_cd, category_cd)
CREATE TABLE transaction_categories (
    type_cd     CHAR(2)      NOT NULL,
    category_cd SMALLINT     NOT NULL,
    description VARCHAR(50),
    CONSTRAINT pk_transaction_categories PRIMARY KEY (type_cd, category_cd),
    CONSTRAINT fk_trancatg_type FOREIGN KEY (type_cd) REFERENCES transaction_types (type_cd)
);

-- DISCGRP (CVTRA02Y) — composite PK; includes the DEFAULT group rows used by
-- INTCALC fallback (1200-A-GET-DEFAULT-INT-RATE).
CREATE TABLE disclosure_groups (
    group_id      VARCHAR(10) NOT NULL,
    type_cd       CHAR(2)     NOT NULL,
    category_cd   SMALLINT    NOT NULL,
    interest_rate NUMERIC(6,2) NOT NULL,
    CONSTRAINT pk_disclosure_groups PRIMARY KEY (group_id, type_cd, category_cd)
);

-- ---------------------------------------------------------------------------
-- Core entities
-- ---------------------------------------------------------------------------

-- CUSTDAT (CVCUS01Y)
-- NOTE (R5, open decision): ssn is stored as loaded for Phase 0; encryption at
-- rest + last-4-only API exposure must land before any API returns it (Phase 1).
CREATE TABLE customers (
    customer_id     BIGINT      NOT NULL,
    first_name      VARCHAR(25),
    middle_name     VARCHAR(25),
    last_name       VARCHAR(25),
    addr_line_1     VARCHAR(50),
    addr_line_2     VARCHAR(50),
    addr_line_3     VARCHAR(50),
    addr_state_cd   VARCHAR(2),
    addr_country_cd VARCHAR(3),
    addr_zip        VARCHAR(10),
    phone_num_1     VARCHAR(15),
    phone_num_2     VARCHAR(15),
    ssn             VARCHAR(9),
    govt_issued_id  VARCHAR(20),
    dob             DATE,
    eft_account_id  VARCHAR(10),
    pri_card_holder CHAR(1),
    fico_score      SMALLINT,
    version         BIGINT      NOT NULL DEFAULT 0,   -- JPA @Version (plan R3)
    CONSTRAINT pk_customers PRIMARY KEY (customer_id),
    CONSTRAINT ck_customers_pri_card_holder CHECK (pri_card_holder IN ('Y', 'N'))
);

-- ACCTDAT (CVACT01Y). Fixes ACCT-EXPIRAION-DATE typo -> expiration_date.
-- group_id: VARCHAR(10) NULL, deliberately NO FK (plan B1: disclosure_groups PK
-- is composite, nothing maintains a groups lookup; blank group -> NULL; INTCALC
-- falls back to group 'DEFAULT' at lookup time).
-- customer_id: dissolved CXACAIX xref (CVACT03Y); backfilled by ETL with a 1:1
-- cardinality assertion (plan §2.3 N8).
CREATE TABLE accounts (
    account_id        BIGINT        NOT NULL,
    active_status     CHAR(1),
    curr_bal          NUMERIC(12,2) NOT NULL DEFAULT 0,
    credit_limit      NUMERIC(12,2),
    cash_credit_limit NUMERIC(12,2),
    open_date         DATE,
    expiration_date   DATE,
    reissue_date      DATE,
    curr_cyc_credit   NUMERIC(12,2) NOT NULL DEFAULT 0,
    curr_cyc_debit    NUMERIC(12,2) NOT NULL DEFAULT 0,
    zip               VARCHAR(10),
    group_id          VARCHAR(10),
    customer_id       BIGINT,
    version           BIGINT        NOT NULL DEFAULT 0,   -- JPA @Version (plan R3)
    CONSTRAINT pk_accounts PRIMARY KEY (account_id),
    CONSTRAINT ck_accounts_active_status CHECK (active_status IN ('Y', 'N')),
    CONSTRAINT fk_accounts_customer FOREIGN KEY (customer_id) REFERENCES customers (customer_id)
);

-- CARDDAT (CVACT02Y). CVV deliberately not persisted (plan R5 recommendation:
-- drop — COCRDUPC never edits it and no API may return it).
CREATE TABLE cards (
    card_number     CHAR(16)    NOT NULL,
    account_id      BIGINT      NOT NULL,
    embossed_name   VARCHAR(50),
    expiration_date DATE,
    active_status   CHAR(1),
    version         BIGINT      NOT NULL DEFAULT 0,   -- JPA @Version (plan R3)
    CONSTRAINT pk_cards PRIMARY KEY (card_number),
    CONSTRAINT ck_cards_active_status CHECK (active_status IN ('Y', 'N')),
    CONSTRAINT fk_cards_account FOREIGN KEY (account_id) REFERENCES accounts (account_id)
);

-- Replaces the CXACAIX AIX (browse cards by account id).
CREATE INDEX idx_cards_account_id ON cards (account_id);

-- ---------------------------------------------------------------------------
-- Transactions
-- ---------------------------------------------------------------------------

-- TRANSACT (CVTRA05Y).
-- Deliberately NO FK on (type_cd, category_cd): legacy CBTRN02C posts them
-- through unchecked; reference integrity is a report-time join concern only
-- (plan §2.2 S5 decision).
CREATE TABLE transactions (
    transaction_id CHAR(16)      NOT NULL,
    type_cd        CHAR(2),
    category_cd    SMALLINT,
    source         VARCHAR(10),
    description    VARCHAR(100),
    amount         NUMERIC(11,2) NOT NULL,
    merchant_id    BIGINT,
    merchant_name  VARCHAR(50),
    merchant_city  VARCHAR(50),
    merchant_zip   VARCHAR(10),
    card_number    CHAR(16)      NOT NULL,
    orig_ts        TIMESTAMP,
    proc_ts        TIMESTAMP,
    CONSTRAINT pk_transactions PRIMARY KEY (transaction_id),
    CONSTRAINT fk_transactions_card FOREIGN KEY (card_number) REFERENCES cards (card_number)
);

-- Replaces the TRANIDX AIX (transactions by card number).
CREATE INDEX idx_transactions_card_number ON transactions (card_number);

-- DALYTRAN (CVTRA06Y) — sequential staging input to the POSTTRAN batch job.
-- No FK on card_number: POSTTRAN validation (reject reason 100) must be able to
-- see daily transactions whose card does not exist.
CREATE TABLE daily_transactions (
    daily_tran_id CHAR(16)      NOT NULL,
    type_cd       CHAR(2),
    category_cd   SMALLINT,
    source        VARCHAR(10),
    description   VARCHAR(100),
    amount        NUMERIC(11,2) NOT NULL,
    merchant_id   BIGINT,
    merchant_name VARCHAR(50),
    merchant_city VARCHAR(50),
    merchant_zip  VARCHAR(10),
    card_number   CHAR(16),
    orig_ts       TIMESTAMP,
    proc_ts       TIMESTAMP,
    CONSTRAINT pk_daily_transactions PRIMARY KEY (daily_tran_id)
);

-- TCATBALF (CVTRA01Y)
CREATE TABLE transaction_category_balances (
    account_id  BIGINT        NOT NULL,
    type_cd     CHAR(2)       NOT NULL,
    category_cd SMALLINT      NOT NULL,
    balance     NUMERIC(11,2) NOT NULL DEFAULT 0,
    CONSTRAINT pk_transaction_category_balances PRIMARY KEY (account_id, type_cd, category_cd),
    CONSTRAINT fk_tcatbal_account FOREIGN KEY (account_id) REFERENCES accounts (account_id)
);

-- DALYREJS replacement (CBTRN02C reject record).
-- Deliberately NO CHECK limiting fail_reason to 100-103: CBTRN02C also emits
-- 109 (account REWRITE failure, line 556) and says "ADD MORE VALIDATIONS HERE".
CREATE TABLE transaction_rejects (
    id               BIGSERIAL     NOT NULL,
    daily_tran_id    CHAR(16),
    type_cd          CHAR(2),
    category_cd      SMALLINT,
    source           VARCHAR(10),
    description      VARCHAR(100),
    amount           NUMERIC(11,2),
    merchant_id      BIGINT,
    merchant_name    VARCHAR(50),
    merchant_city    VARCHAR(50),
    merchant_zip     VARCHAR(10),
    card_number      CHAR(16),
    orig_ts          TIMESTAMP,
    proc_ts          TIMESTAMP,
    fail_reason      SMALLINT      NOT NULL,
    fail_reason_desc VARCHAR(76),
    job_run_id       BIGINT,
    created_at       TIMESTAMP     NOT NULL DEFAULT now(),
    CONSTRAINT pk_transaction_rejects PRIMARY KEY (id)
);

-- ---------------------------------------------------------------------------
-- Security
-- ---------------------------------------------------------------------------

-- USRSEC (CSUSR01Y). password_hash replaces plaintext SEC-USR-PWD PIC X(8);
-- bcrypt output fits in 72.
CREATE TABLE users (
    user_id       VARCHAR(8)  NOT NULL,
    first_name    VARCHAR(20),
    last_name     VARCHAR(20),
    password_hash VARCHAR(72) NOT NULL,
    user_type     CHAR(1)     NOT NULL,
    version       BIGINT      NOT NULL DEFAULT 0,   -- JPA @Version (plan R3)
    CONSTRAINT pk_users PRIMARY KEY (user_id),
    CONSTRAINT ck_users_user_type CHECK (user_type IN ('A', 'U'))
);

-- ---------------------------------------------------------------------------
-- Batch outputs (new tables replacing dataset/spool outputs)
-- ---------------------------------------------------------------------------

-- CBSTM03A output index (plain-text + HTML statement files)
CREATE TABLE statements (
    id         BIGSERIAL NOT NULL,
    account_id BIGINT    NOT NULL,
    cycle_date DATE      NOT NULL,
    txt_path   VARCHAR(255),
    html_path  VARCHAR(255),
    created_at TIMESTAMP NOT NULL DEFAULT now(),
    CONSTRAINT pk_statements PRIMARY KEY (id),
    CONSTRAINT fk_statements_account FOREIGN KEY (account_id) REFERENCES accounts (account_id)
);

-- CORPT00C / CBTRN03C report run metadata (backs GET /reports + download)
CREATE TABLE report_runs (
    id           BIGSERIAL   NOT NULL,
    report_type  VARCHAR(10) NOT NULL,
    from_date    DATE,
    to_date      DATE,
    status       VARCHAR(20) NOT NULL,
    file_path    VARCHAR(255),
    requested_by VARCHAR(8),
    created_at   TIMESTAMP   NOT NULL DEFAULT now(),
    CONSTRAINT pk_report_runs PRIMARY KEY (id),
    CONSTRAINT ck_report_runs_type CHECK (report_type IN ('monthly', 'yearly', 'custom'))
);
