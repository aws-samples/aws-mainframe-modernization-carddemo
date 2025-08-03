# pad the records in file to be exactly 80 characters
{
        gsub("\r"," ");
        printf("%-80s\n", $0);
}
