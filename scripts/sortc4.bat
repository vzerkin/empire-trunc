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
@echo      I                   Vienna, 2014                      I
@echo      +-----------------------------------------------------+
@echo.
@echo par0=%0
@echo par1=%1
@echo par2=%2
@echo par3=%3
@echo par4=%4
@echo par5=%5
@echo par6=%6
@echo par7=%7
@echo par8=%8
@echo par9=%9
@echo EMPIREDIR=%EMPIREDIR%
@echo Working dir=%CD%

bash.exe %0 %1 %2 %3 %4 %5 %6 %7 %8 %9

@rem pause
exit
