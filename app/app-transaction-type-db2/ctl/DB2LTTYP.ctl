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
 INSERT INTO CARDDEMO.TRANSACTION_TYPE
        (TR_TYPE,TR_DESCRIPTION)
 SELECT '01','PURCHASE'      FROM SYSIBM.SYSDUMMY1 UNION ALL
 SELECT '02','PAYMENT'       FROM SYSIBM.SYSDUMMY1 UNION ALL
 SELECT '03','CREDIT'        FROM SYSIBM.SYSDUMMY1 UNION ALL
 SELECT '04','AUTHORIZATION' FROM SYSIBM.SYSDUMMY1 UNION ALL
 SELECT '05','REFUND'        FROM SYSIBM.SYSDUMMY1 UNION ALL
 SELECT '06','REVERAL'       FROM SYSIBM.SYSDUMMY1 UNION ALL
 SELECT '07','ADJUSTMENT'    FROM SYSIBM.SYSDUMMY1
 COMMIT;
