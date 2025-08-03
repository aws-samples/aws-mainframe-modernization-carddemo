#!/bin/bash
# Check if tunnel is running on the system
if [ $(ps f | grep -c "2121:") == 1 ]
then 
  echo "FTP Tunnel to Ensono not running."
  exit 
fi

file_name=$1
file_extension=$2

echo $file_name $file_extension

if [ $file_extension != ".jcl" ]
then
 echo "Only files with jcl extension can be submitted on mainframe"
 exit
fi

tnftp localhost 2121 <<EOF
pwd
quote site filetype=JES
put $file_name
bye
EOF