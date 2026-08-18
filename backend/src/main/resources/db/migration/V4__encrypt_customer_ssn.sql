-- SSN at rest (plan R5, review should-fix #2): customers.ssn now stores
-- app-level AES-256-GCM ciphertext (base64(IV || ct+tag), written by
-- com.carddemo.crypto.SsnAttributeConverter / the ETL loader; key from
-- CARDDEMO_SSN_KEY). This supersedes the V1 NOTE that ssn was "stored as
-- loaded" (V1 cannot be edited — checksum-validated by Flyway).
ALTER TABLE customers ALTER COLUMN ssn TYPE VARCHAR(128);

-- Pre-V4 rows hold plaintext the converter cannot decrypt; this is a demo
-- with ETL-seeded data, so drop the plaintext and re-run the ETL to reload
-- encrypted values (see README "Upgrading to V4").
UPDATE customers SET ssn = NULL;
