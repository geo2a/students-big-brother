cd 'out'
for dir in */
do
  base=$(basename "$dir")
  zip -r "../${base}.zip" "$dir"
done
