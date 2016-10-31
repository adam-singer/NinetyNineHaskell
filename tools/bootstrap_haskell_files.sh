
for i in $( seq -w 1 1 99 ); do
  echo "H99_$i.hs"
  echo "module H99_$i where\n"
  echo "solution$1 = undefined"
done
