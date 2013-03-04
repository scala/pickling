#!/bin/bash
# a little helper script to simplify working with the Cloud9 IDE
(cd src/reflect/scala ; unzip ~/Downloads/scala-pickling.zip)
# remove zip file, so that subsequent "Download Project" actions save to the same file
rm ~/Downloads/scala-pickling.zip
# move tests to the right dir
cp src/reflect/scala/test/* test/files/pickling/
