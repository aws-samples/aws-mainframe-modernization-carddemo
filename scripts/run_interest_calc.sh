#!/bin/bash
# Check if tunnel is running on the system
if [ $(ps f | grep -c "2121:") == 1 ]
then 
  echo "FTP Tunnel to Ensono not running."
  exit 
fi

echo "Running Interest Calculation Cycle"

tnftp localhost 2121 <<EOF
pwd
quote site filetype=JES
!echo "Job 1: Close files in CICS "
put jcl/CLOSEFIL.jcl
!echo "Wait for 5 seconds" && sleep 5

!echo "Job 2: Run interest calculations "
put jcl/INTCALC.jcl
!echo "Wait for 5 seconds" && sleep 5

!echo "Job 3: Backup transaction master "
put jcl/TRANBKP.jcl

!echo "Job 4: Combine system transactions with daily ones "
put jcl/COMBTRAN.jcl
!echo "Wait for 5 seconds" && sleep 5

!echo "Job 5: Define alternate index on transaction file"
put jcl/TRANIDX.jcl
!echo "Wait for 5 seconds" && sleep 5

!echo "Job 6: Open files in CICS "
put jcl/OPENFIL.jcl

bye
EOF