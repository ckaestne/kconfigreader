cd ..
ls *.config -1 > nest/files
cd nest
cat files | while read i; do 
  sed "s/SOURCETARGET/..\/$i/" < template > $i
done
