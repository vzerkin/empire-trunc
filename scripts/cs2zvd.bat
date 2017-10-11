cls
@echo      +-----------------------------------------------------+
@echo      I                   Empire-3.2.3                      I
@echo      I               adopted for Windows                   I
@echo      I          and prepared to run from DVD-ROM           I
@echo      I         by Viktor Zerkin, V.Zerkin@iaea.org         I
@echo      I                                                     I
@echo      I                WORKING MATERIALS                    I
@echo      I       UNDER DEVELOPMENT, NOT FOR DISTRIBUTION       I
@echo      I                                                     I
@echo      I                Nuclear Data Section                 I
@echo      I         International Atomic Energy Agency          I
@echo      I                 Vienna, 2014-2017                   I
@echo      +-----------------------------------------------------+
@echo.
@echo par0=%0
@if not %1.==. echo par1=%1
@if not %2.==. echo par2=%2
@if not %3.==. echo par3=%3
@if not %4.==. echo par4=%4
@if not %5.==. echo par5=%5
@if not %6.==. echo par6=%6
@if not %7.==. echo par7=%7
@if not %8.==. echo par8=%8
@if not %9.==. echo par9=%9
@echo EMPIREDIR=%EMPIREDIR%
@echo Working dir=%CD%

bash.exe %0 %1 %2 %3 %4 %5 %6 %7 %8 %9

@rem pause
exit
