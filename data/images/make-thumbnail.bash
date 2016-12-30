#! /bin/bash

if [ -n "$1" ]
then
	file="$1"
else
	echo "make-thumbnail.bash usage: ./make-thumbnail.bash image"
	exit
fi

# strip extension from file
newfile=$(echo $file | sed 's/\..*$//')_thumb.png

convert ${file} -thumbnail 84x150 ${newfile}
