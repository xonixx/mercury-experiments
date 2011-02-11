@echo off
rem mmc.bat --make --infer-all -s hlc.gc --warn-non-tail-recursion %*
rem mmc.bat --make --infer-all -s hlc.gc %*

set OPTS=%OPTS% --make --infer-all 
set OPTS=%OPTS% -O6
set OPTS=%OPTS% --optimize-constructor-last-call
set OPTS=%OPTS% --grade=hlc.gc --structure-reuse

mmc.bat %OPTS% %*