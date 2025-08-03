/* Copyright Amazon.com, Inc. or its affiliates.                   */
/* All Rights Reserved.                                            */
/*                                                                 */
/* Licensed under the Apache License, Version 2.0 (the "License"). */
/* You may not use this file except in compliance with the License.*/
/* You may obtain a copy of the License at                         */
/*                                                                 */
/*    http://www.apache.org/licenses/LICENSE-2.0                   */
/*                                                                 */
/* Unless required by applicable law or agreed to in writing,      */
/* software distributed under the License is distributed on an     */
/* "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    */
/* either express or implied. See the License for the specific     */
/* language governing permissions and limitations under the License*/
  SET CURRENT SQLID = 'SYSADM';

      CREATE DATABASE CARDDEMO
             STOGROUP AWST1STG
             BUFFERPOOL BP0
             CCSID EBCDIC;

      COMMIT ;

      CREATE TABLESPACE CARDSPC1
        IN CARDDEMO
        USING STOGROUP AWST1STG
        SEGSIZE 4
        LOCKSIZE TABLE
        BUFFERPOOL BP0
        CLOSE NO
        CCSID EBCDIC;

      COMMIT ;

      CREATE TABLE  CARDDEMO.TRANSACTION_TYPE
       (       TR_TYPE   CHAR(2)      NOT NULL,
        TR_DESCRIPTION   VARCHAR(50)  NOT NULL,
                 PRIMARY KEY(TR_TYPE))
         IN CARDDEMO.CARDSPC1
         CCSID EBCDIC;

      COMMIT ;

   CREATE UNIQUE INDEX CARDDEMO.XTRAN_TYPE
                    ON CARDDEMO.TRANSACTION_TYPE
                        (TR_TYPE   ASC)
                    USING STOGROUP AWST1STG
                    ERASE NO
                    BUFFERPOOL BP0
                    CLOSE NO;

      GRANT DBADM ON DATABASE CARDDEMO
            TO PUBLIC;
      GRANT USE OF TABLESPACE CARDDEMO.CARDSPC1
            TO PUBLIC;

      GRANT DELETE, INSERT, SELECT, UPDATE
            ON TABLE CARDDEMO.TRANSACTION_TYPE
            TO PUBLIC;

  CREATE TABLESPACE CARDSTTC
    IN CARDDEMO
    USING STOGROUP AWST1STG
    SEGSIZE 4
    LOCKSIZE TABLE
    BUFFERPOOL BP0
    CLOSE NO
    CCSID EBCDIC;

  COMMIT ;

  GRANT USE OF TABLESPACE CARDDEMO.CARDSTTC
        TO PUBLIC;

  CREATE TABLE CARDDEMO.TRANSACTION_TYPE_CATEGORY
       (TRC_TYPE_CODE     CHAR(2)        NOT NULL,
        TRC_TYPE_CATEGORY CHAR(4)        NOT NULL,
        TRC_CAT_DATA      VARCHAR(50)    NOT NULL,
  PRIMARY KEY(TRC_TYPE_CODE, TRC_TYPE_CATEGORY))
     IN CARDDEMO.CARDSTTC
     CCSID EBCDIC;

  COMMIT;

  CREATE UNIQUE INDEX CARDDEMO.X_TRAN_TYPE_CATG
     ON CARDDEMO.TRANSACTION_TYPE_CATEGORY
       (TRC_TYPE_CODE    ASC,
         TRC_TYPE_CATEGORY ASC)
     USING STOGROUP AWST1STG
     ERASE NO
     BUFFERPOOL BP0
     CLOSE NO;

  COMMIT;

  ALTER TABLE CARDDEMO.TRANSACTION_TYPE_CATEGORY
    FOREIGN KEY (TRC_TYPE_CODE)
      REFERENCES CARDDEMO.TRANSACTION_TYPE (TR_TYPE)
  ON DELETE RESTRICT;

  COMMIT;

  GRANT DELETE, INSERT, SELECT, UPDATE
        ON TABLE CARDDEMO.TRANSACTION_TYPE_CATEGORY
        TO PUBLIC;

