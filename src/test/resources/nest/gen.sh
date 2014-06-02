cd ..
ls *.config -1 > nest/files
cd nest
cat files | while read i; do 
  sed "s/SOURCETARGET/src\/test\/resources\/$i/" < template > $i
  sed "s/SOURCETARGET/src\/test\/resources\/$i/" < _template > _$i
done
