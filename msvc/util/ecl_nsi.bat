@ECHO off
REM Converted from ecl_nsi.sh

SETLOCAL

SET source=%1
SET where=%2
CD %where%
SET dest=ecl.nsi
SET where=%CD%
ECHO %where%

REM set -x

sed -e "/!@INSTALLFILES@/,$d" -e "s,@ECLDIR@,%where:\=\\%," %source% > %dest%
DIR /B /A:-D | sed -e "/^.*ecl.exe.*$/d" -e "/^.*ecl.nsi.*$/d" -e "s,^%where:\=\\%\\,,g" -e "s,^\(.*\)$,  File \"${ECLDIR}\\\1\",g" >> %dest%
DIR /B /A:D | sed -e "s,^%where:\=\\%\\,,g" -e "s,^\(.*\)$,  File /r \"${ECLDIR}\\\1\",g" >> %dest%
sed -e "1,/!@INSTALLFILES@/d;/!@DELETEFILES@/,$d" %source% >> %dest%
DIR /B /A:-D | sed -e "/^.*ecl.exe.*$/d" -e "/^.*ecl.nsi.*$/d" -e "s,^%where:\=\\%\\,,g" -e "s,^\(.*\)$,  Delete \"$INSTDIR\\\1\",g" >> %dest%
DIR /B /A:D | sed -e "s,^%where:\=\\%\\,,g" -e "s,^\(.*\)$,  RMDir /r \"$INSTDIR\\\1\",g" >> %dest%
sed -e "1,/!@DELETEFILES@/d" %source% >> %dest%

ENDLOCAL
