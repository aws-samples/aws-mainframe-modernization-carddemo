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

!echo "Job 2: Refresh Account Master file "
put jcl/ACCTFILE.jcl

!echo "Job 3: Refresh Card Master "
put jcl/CARDFILE.jcl

!echo "Job 4: Refresh Card Cross Reference "
put jcl/XREFFILE.jcl

!echo "Job 5: Refresh Customer Master "
put jcl/CUSTFILE.jcl

!echo "Job 6: Refresh Transaction Master "
put jcl/TRANFILE.jcl

!echo "Job 7: Refresh Disclosure Group "
put jcl/DISCGRP.jcl

!echo "Job 8: Refresh Transaction Category Balance "
put jcl/TCATBALF.jcl

!echo "Job 9: Refresh Transaction Category File "
put jcl/TRANCATG.jcl

!echo "Job 10: Refresh Transaction Type File "
put jcl/TRANTYPE.jcl

!echo "Job 11: Refresh User Security  File "
put jcl/DUSRSECJ.jcl

!echo "Data Refresh Complete" && sleep 5
!echo "-------------------------------------------------"

!echo "Job 11: Open files in CICS   "
put jcl/OPENFIL.jcl
bye
EOF  