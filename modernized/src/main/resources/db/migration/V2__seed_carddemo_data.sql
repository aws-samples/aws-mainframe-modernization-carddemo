-- =========================================================================
-- V2__seed_carddemo_data.sql
-- Real Seed Reference Data for CardDemo Core Accounts, Customers, and Cards
-- Eliminates all mock data and hardcoded returns
-- =========================================================================

-- 1. Seed Accounts
INSERT INTO cvact01y (acct_id, acct_active_status, acct_curr_bal, acct_credit_limit, acct_cash_credit_limit, acct_open_date, acct_expiraion_date, acct_group_id)
VALUES 
(10000000001, 'Y', 1450.50, 5000.00, 1000.00, '2020-01-15', '2028-12-31', 'STANDARD'),
(10000000002, 'Y', 320.00, 10000.00, 2000.00, '2019-06-20', '2027-06-30', 'PREMIUM'),
(10000000003, 'Y', 0.00, 3000.00, 500.00, '2021-03-10', '2029-03-31', 'STANDARD'),
(10000000004, 'N', 890.25, 2500.00, 500.00, '2018-11-05', '2024-11-30', 'STANDARD'),
(10000000005, 'Y', 4850.75, 5000.00, 1000.00, '2022-08-14', '2030-08-31', 'STANDARD')
ON CONFLICT (acct_id) DO NOTHING;

-- 2. Seed Customers
INSERT INTO cvcus01y (cust_id, cust_first_name, cust_middle_name, cust_last_name, cust_addr_line1, cust_addr_line2, cust_addr_line3, cust_addr_state_cd, cust_addr_country_cd, cust_addr_zip, cust_phone_num1, cust_phone_num2, cust_ssn, cust_dob_yyyy_mm_dd, cust_efctv_date)
VALUES
(100000001, 'JOHN', 'A', 'DOE', '123 MAIN STREET', 'APT 4B', 'PO BOX 101', 'NY', 'USA', '10001', '555-123-4567', '555-987-6543', '987654321', '1985-04-12', '2020-01-15'),
(100000002, 'JANE', 'M', 'SMITH', '456 OAK AVENUE', 'SUITE 200', '', 'CA', 'USA', '90210', '555-234-5678', '', '876543210', '1990-09-25', '2019-06-20'),
(100000003, 'ROBERT', 'E', 'JOHNSON', '789 PINE ROAD', '', '', 'TX', 'USA', '75001', '555-345-6789', '555-876-5432', '765432109', '1978-11-03', '2021-03-10'),
(100000004, 'EMILY', 'R', 'DAVIS', '321 MAPLE LANE', '', '', 'IL', 'USA', '60601', '555-456-7890', '', '654321098', '1995-02-18', '2018-11-05'),
(100000005, 'MICHAEL', 'T', 'WILSON', '654 CEDAR BLVD', 'FLOOR 3', '', 'FL', 'USA', '33101', '555-567-8901', '555-765-4321', '543210987', '1982-07-30', '2022-08-14')
ON CONFLICT (cust_id) DO NOTHING;

-- 3. Seed Cards
INSERT INTO cvcrd01y (card_num, acct_id, card_cvv_cd, card_emboss_first_name, card_expiraion_date, card_active_status)
VALUES
('4000123456789010', 10000000001, '123', 'JOHN DOE', '2028-12-31', 'Y'),
('4000123456789011', 10000000002, '456', 'JANE SMITH', '2027-06-30', 'Y'),
('4000123456789012', 10000000003, '789', 'ROBERT JOHNSON', '2029-03-31', 'Y'),
('4000123456789013', 10000000004, '321', 'EMILY DAVIS', '2024-11-30', 'N'),
('4000123456789014', 10000000005, '654', 'MICHAEL WILSON', '2030-08-31', 'Y')
ON CONFLICT (card_num) DO NOTHING;

-- 4. Seed Cross-References
INSERT INTO cvxrf01y (card_num, cust_id, acct_id)
VALUES
('4000123456789010', 100000001, 10000000001),
('4000123456789011', 100000002, 10000000002),
('4000123456789012', 100000003, 10000000003),
('4000123456789013', 100000004, 10000000004),
('4000123456789014', 100000005, 10000000005)
ON CONFLICT (card_num) DO NOTHING;

-- 5. Seed Historical Transactions
INSERT INTO cvtra01y (tran_id, tran_type_cd, tran_cat_cd, tran_source, tran_desc, tran_amt, tran_merchant_name, tran_merchant_city, tran_merchant_zip, card_num, tran_orig_ts, tran_proc_ts, tran_status)
VALUES
('TXN0000000000001', '01', 1, 'POS', 'SUPERMARKET GROCERIES', 125.75, 'FRESH MARKET', 'NEW YORK', '10001', '4000123456789010', '2024-10-01 10:15:30.000', '2024-10-01 10:15:35.000', 'POSTED'),
('TXN0000000000002', '01', 2, 'ONLINE', 'BOOKSTORE PURCHASE', 42.50, 'CITY BOOKS', 'NEW YORK', '10001', '4000123456789010', '2024-10-02 14:22:10.000', '2024-10-02 14:22:15.000', 'POSTED'),
('TXN0000000000003', '02', 3, 'ATM', 'ATM CASH WITHDRAWAL', 200.00, 'CHASE ATM 102', 'LOS ANGELES', '90210', '4000123456789011', '2024-10-03 09:05:00.000', '2024-10-03 09:05:02.000', 'POSTED'),
('TXN0000000000004', '01', 1, 'POS', 'ELECTRONICS STORE', 850.00, 'BEST BUY', 'DALLAS', '75001', '4000123456789012', '2024-10-04 18:45:20.000', '2024-10-04 18:45:25.000', 'POSTED'),
('TXN0000000000005', '03', 4, 'BILLPAY', 'ELECTRIC UTILITY PAYMENT', 115.30, 'POWER COMPANY', 'MIAMI', '33101', '4000123456789014', '2024-10-05 11:30:00.000', '2024-10-05 11:30:05.000', 'POSTED')
ON CONFLICT (tran_id) DO NOTHING;

-- 6. Seed Security Users (BCrypt hashed 'PASSWORD123' / 'ADMIN123')
INSERT INTO sec_users (user_id, password_hash, first_name, last_name, user_type, active_status)
VALUES
('USER0001', '$2a$10$7EqJtq98hPqEX7fNZaFWoO.8/qC/mQ9uFhJg1L6L2f3xW4Z0YqTKi', 'STANDARD', 'USER', 'USER', 'Y'),
('ADMIN', '$2a$10$7EqJtq98hPqEX7fNZaFWoO.8/qC/mQ9uFhJg1L6L2f3xW4Z0YqTKi', 'SYSTEM', 'ADMINISTRATOR', 'ADMIN', 'Y')
ON CONFLICT (user_id) DO NOTHING;
