#!/bin/bash
#
#----------------------------------------------------------------------------------------
# This script can be used to insert or update the Applicaiton version information 
# in the soruce programs that are in Git repository.
#----------------------------------------------------------------------------------------

#
# Important: Make sure the app_name and app_version are correct efore executing this script
#
app_name=CardDemo
app_version=v2.0

# Input parameter
file_name=$1

# Check if file_name was passed as input parameter
if [ -z "$file_name" ]; then
    echo "file_name must be passed as input parameter..."
    exit 99
fi

# Check if the input file exist
if [ ! -f "$file_name" ]; then
    echo "File $file_name does NOT exist..."
    exit 99
fi

# Delete tags
for git_tag in `git tag`
do
    if [ "$git_tag" != "$app_version" ]; then
        git tag -d $git_tag > /dev/null 2>&1 
    fi
done

git tag $app_version > /dev/null 2>&1

projectversion=`git describe --tags --long`
revisioncount=`git log --oneline | wc -l | tr -d ' '`
src_version="$projectversion-$revisioncount"
date=$(date '+%Y-%m-%d %H:%M:%S %Z')
version_info="Ver: ${app_name}_${src_version} Date: $date"

base_filename=$(basename -- "$file_name")
file_extension="${base_filename##*.}"
file_extension=$(echo $file_extension | tr '[:upper:]' '[:lower:]')

# Check source code type based on file extension and determine the soruce comment syntax 
case $file_extension in
    cbl|cob|cpy)
        src_comment="      *"
        ;;
    jcl|prc|proc)
        src_comment="//*"
        ;;
    bms)
        src_comment="*"
        ;;
    py)
        src_comment="##"
        ;;
    *)
        echo "Unknown file type..."
        exit 99
        ;;
esac

echo "--------------------------------------------------------------------------"
echo ""
echo "Adding version number and release date for srouce file : $file_name"
echo ""

# Temp file
tmp_filename="/tmp/tmp_$base_filename"
rm -f $tmp_filename

# Check if the file is a DOS file. If so, convert to Unix file
grep -c $'\r$' $file_name > /dev/null

if [ $? -eq 0 ] ; then
    cat $file_name | sed 's/\r$//' > $tmp_filename
else
    cp $file_name $tmp_filename
fi

# Check if the verion number is already available
grep -c "Ver: ${app_name}_" $file_name > /dev/null 

if [ $? -eq 0 ] ; then
    cat $file_name | sed "s/Ver:.*$/$version_info/" | sed 's/\r$//' > $tmp_filename
else
    echo "$src_comment" >> $tmp_filename
    echo "$src_comment $version_info" >> $tmp_filename
    echo "$src_comment" >> $tmp_filename
fi

diff $file_name $tmp_filename
mv $tmp_filename $file_name
echo ""

exit 0

