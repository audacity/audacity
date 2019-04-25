for i in *.po; do
    sed -i '/^Project/d' $i
done