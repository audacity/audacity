#!/bin/sh

# Prints all class references made by all classes in a Jar file
# Depends on the output formatting of javap

# create a temporary working directory
dir=`mktemp -d $TMPDIR/classrefs.XXXXXX`

asm_dump="$dir/asm_dump"
all_classes="$dir/all_classes"

# for each class in a Jar file, dump the full assembly
javap -c -classpath "$1" `/usr/bin/jar tf "$1" | grep "\.class" | sort | xargs | sed -e 's/\.class//g'` > $asm_dump

# dump the initial list of all classes in the Jar file
/usr/bin/jar tf $1 | grep "\.class" | sed -e 's/\.class//g' >> $all_classes

# dump all static class references
cat $asm_dump | grep //class | awk -F"//class " '{print $2}' | sort | uniq >> $all_classes

# dump all references to classes made in methods
cat $asm_dump | grep //Method | awk -F"//Method " '{print $2}' | sort | uniq | grep "\." | awk -F"." '{print $1}' | sort | uniq >> $all_classes

# dump all references to classes by direct field access
cat $asm_dump | grep //Field | awk -F"//Field " '{print $2}' | sort | uniq | grep "\:L" | awk -F"\:L" '{print $2}' | sort | uniq | awk -F"\;" '{print $1}' >> $all_classes

# sort and reformat
sort $all_classes | uniq | grep -v "\"" | sed -e 's/\//\./g'

# cleanup
rm -rf $dir
