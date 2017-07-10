
for file in $(ls testprograms/lab5/valid/*.p0)
do
	echo "Processing $file"
	scala -cp "lib/cafebabe_2.11-1.2.jar;target/scala-2.11/classes/" punkt0.Main -d dest $file 
	scala -cp dest Main > ourOut
	rm -rf dest
	scala -cp lib/cafebabe_2.11-1.2.jar punkt0_2.11-1.2.jar -d dest $file
	scala -cp dest Main > refOut
	diff ourOut refOut
done
rm ourOut
