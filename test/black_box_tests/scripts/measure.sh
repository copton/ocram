#!/bin/bash

app_section_size() {
	echo "# size of sections of application"
	${TOOLCHAIN}size *.exe | awk '{print $6", "$1", "$2", "$3}'
}

avrora_stack() {
	echo "# maximum stack size"
	export CLASSPATH=/home/alex/scm/avrora/bin/
	java avrora.Main -action=analyze-stack -seconds=5 ./native.od
	java avrora.Main -action=analyze-stack -seconds=5 ./ec.od
}

for app in $ROOT/application/*; do
	if [ -d $app ]; then
		cd $app
		echo "# measuring `basename $app`"
		app_section_size
		if [ $PLATFORM = avrora ]; then
			avrora_stack
		fi
	fi
done
