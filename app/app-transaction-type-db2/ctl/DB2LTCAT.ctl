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
 INSERT INTO CARDDEMO.TRANSACTION_TYPE_CATEGORY
      (TRC_TYPE_CODE    ,
       TRC_TYPE_CATEGORY,
       TRC_CAT_DATA     )
 WITH DMY AS (SELECT * FROM SYSIBM.SYSDUMMY1)
 SELECT '01','0001','REGULAR SALES DRAFT'           FROM DMY UNION ALL
 SELECT '01','0002','REGULAR CASH ADVANCE'          FROM DMY UNION ALL
 SELECT '01','0003','CONVENIENCE CHECK DEBIT'       FROM DMY UNION ALL
 SELECT '01','0004','ATM CASH ADVANCE'              FROM DMY UNION ALL
 SELECT '01','0005','INTEREST AMOUNT'               FROM DMY UNION ALL
 SELECT '02','0001','CASH PAYMENT'                  FROM DMY UNION ALL
 SELECT '02','0002','ELECTRONIC PAYMENT'            FROM DMY UNION ALL
 SELECT '02','0003','CHECK PAYMENT'                 FROM DMY UNION ALL
 SELECT '03','0001','CREDIT TO ACCOUNT'             FROM DMY UNION ALL
 SELECT '03','0002','CREDIT TO PURCHASE BALANCE'    FROM DMY UNION ALL
 SELECT '03','0003','CREDIT TO CASH BALANCE'        FROM DMY UNION ALL
 SELECT '04','0001','ZERO DOLLAR AUTHORIZATION'     FROM DMY UNION ALL
 SELECT '04','0002','ONLINE PURCHASE AUTHORIZATION' FROM DMY UNION ALL
 SELECT '04','0003','TRAVEL BOOKING AUTHORIZATION'  FROM DMY UNION ALL
 SELECT '05','0001','REFUND CREDIT'                 FROM DMY UNION ALL
 SELECT '06','0001','FRAUD REVERSAL'                FROM DMY UNION ALL
 SELECT '06','0002','NON FRAUD REVERSAL'            FROM DMY UNION ALL
 SELECT '07','0001','SALES DRAFT CREDIT ADJUSTMENT' FROM DMY

 COMMIT;
