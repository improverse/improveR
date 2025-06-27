@echo off
setlocal enabledelayedexpansion

rem Determine the directory of this script
set "BASEDIR=%~dp0"

rem Check if the first parameter is -install
if /I "%1"=="-install" (
    echo Adding %BASEDIR% to your user PATH...
    rem Note: -install updates the user PATH; changes take effect in new CMD windows.
    setx PATH "%PATH%;%BASEDIR%"
    echo Done. Please restart your command prompt.
    goto :EOF
)

rem Search for the first JAR file in the lib folder
for %%f in ("%BASEDIR%lib\improve-cli-*.jar") do (
    echo Starting JAR: "%%f" with parameters: %*
    "%BASEDIR%jre\bin\java" -jar "%%f" %*
    goto :EOF
)

echo No matching JAR found!
goto :EOF
