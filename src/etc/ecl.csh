#!/bin/csh -f
set dir = DIRECTORY
set ecl = ecl
set a = "" a0 = U a1 = 1 a2 = 0 a3 = 0 a4 = 0
set ifile = "" ofile = ""
while ($#argv > 0)
	switch ($1)
	case -c:
		set a1 = 0 a2 = 1 a3 = 1 a4 = 1
		breaksw
	case -C:
		set a2 = 1 a3 = 1 a4 = 1
		breaksw
	case -o:
		shift
		set ofile = $1
		breaksw
	case -d:
		shift
		set dir = $1
		breaksw
	case -l:
		shift
		set ecl = $1
		breaksw
	case -s:
		set a0 = S
		breaksw
	case -*:
		echo ${1}: Unknown flag.
		echo 'Usage: ecl [-d dir] [-l ecl] [ [-c] [-C] [-s]' \
				'[-o ofile] ifile ]'
		echo '	-d dir		System directory, defaults to'
		echo '				'\"$dir\"
		echo '	-l ecl		Ecl binary, defaults to' \"$ecl\"
		echo '	-c		Produce C files only'
		echo '	-C		Produce C files as well'
		echo '	-s		Compile for system'
		echo '	-o ofile	Output file name, defaults to ifile'
		echo '	ifile		File to compile'
		exit 1
	case *:
		set ifile = $1
		breaksw
	endsw
	shift
end
if ($ifile != "" && $ofile == "") set ofile = $ifile
if ($ifile != "") set a = $a0$a1$a2$a3$a4
if ($ecl !~ */*) set ecl = $dir/$ecl
exec $ecl -dir $dir/ $ifile $ofile $a
exit $status

