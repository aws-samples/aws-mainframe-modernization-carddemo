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

!echo "Job 3: Refresh Transaction Category Balance "
put jcl/TCATBALF.jcl

!echo "Job 4: Refresh Transaction Master "
put jcl/TRANBKP.jcl

!echo "Job 5: Core processing job"
put jcl/POSTTRAN.jcl
!echo "Wait for 10 seconds" && sleep 10

!echo "Job 6: Define alternate index on transaction file"
put jcl/TRANIDX.jcl
!echo "Wait for 5 seconds" && sleep 5

!echo "Job 7: Open files in CICS "
put jcl/OPENFIL.jcl

bye
EOF