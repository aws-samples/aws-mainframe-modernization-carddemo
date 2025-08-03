#!/bin/bash
# Check if tunnel is running on the system
if [ $(ps f | grep -c "2121:") == 1 ]
then 
  echo "FTP Tunnel to Ensono not running."
  exit 
fi
module_path=$1
module_type=$2

if [[ $module_path == "" ]]
then 
  echo "Missing module_path parameter"
  exit
fi 

if [[ $module_type == "" ]] 
then 
  echo "Missing module_type parameter"
  exit
fi 

# extract module name 
module_name=$(echo $module_path | sed -E 's/^(.+\/)(.+)(\.[a-z]{3,4})$/\2/g')
echo "Uploading $module_name"

# Pad spaces to make it mainframe pds member with recln = 80
# echo $module_path $module_name $PWD
awk -f scripts/pad.awk $module_path > $module_name.tmp && cp $module_name.tmp $module_path && rm $module_name.tmp
if [[ $? != 0 ]] 
then
  say "awk transformation failed"
  exit
fi
tnftp localhost 2121 <<EOF
pwd
put $module_path "'AWS.M2.CARDDEMO.$module_type($module_name)'" 
bye
EOF