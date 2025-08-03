#!/bin/bash
# Check if tunnel is running on the system
if [ $(ps f | grep -c "2121:") == 1 ]
then 
  echo "FTP Tunnel to Ensono not running."
  exit 
fi

tnftp localhost 2121 <<EOF
pwd
quote site filetype=JES
!echo "Job 1: Close files in CICS "
put jcl/CLOSEFIL.jcl
!echo "Wait for 5 seconds" && sleep 5

!echo "Job 2: Refresh Account Data file "
put jcl/ACCTFILE.jcl

!echo "Job 3: Refresh Card Data "
put jcl/CARDFILE.jcl

!echo "Job 4: Refresh Card Cross Reference "
put jcl/XREFFILE.jcl

!echo "Job 5: Refresh Customer Data "
put jcl/CUSTFILE.jcl

!echo "Job 6: Refresh Transaction Data "
put jcl/TRANBKP.jcl

!echo "Job 7: Refresh Disclosure Group "
put jcl/DISCGRP.jcl

!echo "Job 8: Refresh Transaction Category Balance "
put jcl/TCATBALF.jcl

!echo "Job 9: Refresh Transaction Type File "
put jcl/TRANTYPE.jcl

!echo "Job 11: Refresh User Security  File "
put jcl/DUSRSECJ.jcl

!echo "Data Refresh Complete" && sleep 5
!echo "-------------------------------------------------"

!echo "Job 12 : Core processing job"
put jcl/POSTTRAN.jcl
!echo "Wait for 10 seconds" && sleep 10

!echo "Job 13: Run interest calculations "
put jcl/INTCALC.jcl
!echo "Wait for 5 seconds" && sleep 5

!echo "Job 14: Backup transaction Data "
put jcl/TRANBKP.jcl

!echo "Job 15: Combine system transactions with daily ones "
put jcl/COMBTRAN.jcl
!echo "Wait for 5 seconds" && sleep 5

!echo "Job 15: Define alternate index on transaction file"
put jcl/TRANIDX.jcl
!echo "Wait for 5 seconds" && sleep 5

!echo "Job 16: Open files in CICS "
put jcl/OPENFIL.jcl


bye
EOF