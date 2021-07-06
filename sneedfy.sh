#!/bin/sh
set -e

if [ ! "$#" -eq 2 ]; then
    printf '%s\n' "usage: $0 fork commit_id"
    printf '%s\n' "Sneedfies audacity's botnet code"
    printf '\n'
    printf '%s\n' "fork is the USER on https://github.com/USER/sneedacity"
    printf '%s\n' "commit_id is the latest commit merged from audacity"
    exit 1
fi

fork="$1" commit="$2"
if [ ! -d audacity ]; then
    git clone "https://github.com/audacity/audacity"
else
    cd audacity; git pull; cd ..
fi
if [ ! -d sneedacity ]; then
    git clone "https://github.com/$fork/sneedacity"
else
    cd sneedacity; git pull; cd ..
fi

cd audacity
git format-patch "$commit"
mv *.patch ../sneedacity
cd ..

cd sneedacity
sed -i 's/audacity/sneedacity/g; 
        s/AUDACITY/SNEEDACITY/g;
        s/Audacity/Sneedacity/g' *.patch
git am *.patch
rm *.patch

echo "Done! Don't forget to test it before pushing and making a PR"
