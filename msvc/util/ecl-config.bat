@ECHO OFF
REM Script converted from ecl-config
REM (Michael Goffioul)

IF %1 == --cflags (
	ECHO @ECL_CFLAGS@ -I@includedir@
	GOTO END
) ELSE IF %1 == --libs (
	ECHO /LIBDIR:@libdir@ ecl.lib @LDFLAGS@ @CLIBS@
	GOTO END
)

ECHO Usage: ecl-config [OPTIONS]
ECHO Options:
ECHO      [--cflags]
ECHO      [--libs]

:END
