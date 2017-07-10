
for file in $(ls testprograms/lab3/valid/*.p0)
do
	echo "Processing $file"
	scala -cp target/scala-2.11/classes punkt0.Main --ast $file > ourAST
	cat ourAST | dos2unix > tmpAST # fix windows line breaks
	mv tmpAST ourAST 
	diff ourAST $file.ast
done
rm ourAST