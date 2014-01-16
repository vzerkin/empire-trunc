@rem
@rem	Script ported from Linux to Windows by V.Zerkin@iaea.org
@rem	Project:	Portable Empire-3.2 for Windows
@rem	Created:	18-Nov-2013
@rem	Last modified:	18-Dec-2013
@rem	Organization:	Nuclear Data Section
@rem			International Atomic Energy Agency (IAEA)
@rem			Wagramer Strasse 5, P.O.Box 100, A-1400
@rem			Vienna, Austria
@rem 
@echo.
@echo.
@echo SCRIPT TO CREATE zvd FILE WITH POINT AND CURVES (in %3)
@echo AND TO CALL ZVV
@echo.
@echo CD=%cd%
@echo EMPIREDIR=%EMPIREDIR%
@echo par0=%0
@echo par1=%1
@echo par2=%2
@echo par3=%3
@echo par4=%4
set scriptDir=%~dp0
@if "%scriptDir:~-1%"=="\" set scriptDir=%scriptDir:~0,-1%

@if .%EMPIREDIR%==. (
    echo.
    echo Please set the 'EMPIREDIR' environment variable.
    echo See Empire v3 documentation for more information.
    echo.
    exit /b
)


echo pntzvv file.pnt file.cur file.zvd
echo %1 %2 %3 %4
del %3
del tmp.dat
del pnt.zvd
del cur.zvd
dir %1 %2 %3 %4
%EMPIREDIR:/=\%\util\c4zvd\pntdatl.exe %1 tmp.dat
%EMPIREDIR:/=\%\util\c4zvd\datzvdl.exe tmp.dat pnt.zvd
%EMPIREDIR:/=\%\util\c4zvd\curzvd3l.exe %2 cur.zvd
@rem pause
echo #!zvview.exe >%3
type pnt.zvd >>%3
type cur.zvd >>%3
@if "%4"=="" (
   %EMPIREDIR:/=\%\util\c4zvd\zvview.exe %3 ps01.tit/c
@rem pause
   exit /b
)
@rem next line makes use of the ddx.tit file with plot settings
%EMPIREDIR:/=\%\util\c4zvd\zvview.exe -p:tmp %3 ddx.tit
del tmp.dat pnt.zvd cur.zvd
@rem pause
