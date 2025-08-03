#!/bin/bash
# Check if tunnel is running on the system
if [ $(ps f | grep -c "2121:") == 1 ]
then 
  echo "FTP Tunnel to Ensono not running."
  exit 
fi

# # Make will ensure the latest modules are uploaded to mainframe
pwd
make -f Makefile

file_name=$1
file_extension=$2
file_basename_no_extension=$3
template_jcl=./compile_batch.jcl.template

echo $file_name $file_extension $file_basename_no_extension

if [ $file_extension != ".cbl" ]
then
 echo "Only files with cbl extension can be compiled on mainframe"
 exit
fi

# Substitute file base name in the compile jcl member name and create temp jcl
sed -e "s/ZZZZZZZZ/$file_basename_no_extension/g" $template_jcl > $file_basename_no_extension.tmp 
rc=$?
if [[ rc -ne 0 ]]
then
    echo "JCL template substition failed rc = $rc"
    exit
fi 

tnftp localhost 2121 <<EOF
pwd
quote site filetype=JES
put $file_basename_no_extension.tmp 
bye
EOF

# Remove temporary jcl
rm $file_basename_no_extension.tmp 
