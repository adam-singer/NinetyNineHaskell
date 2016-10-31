
for i in `seq -w 1 1 99`; do
  file="H99_$i.hs"
  echo "module H99_$i where" > src/$file
  echo "solution$i = undefined" >> src/$file
done

