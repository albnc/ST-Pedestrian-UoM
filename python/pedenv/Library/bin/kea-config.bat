@echo off

IF "%1"=="" (
   echo kea-config.bat [OPTIONS]
   echo Options:
   echo     [--prefix]
   echo     [--version]
   echo     [--libs]
   echo     [--cflags]
   echo     [--includes]
   EXIT /B 1
) ELSE (
:printValue
    if "%1" neq "" (
	    IF "%1"=="--prefix" echo C:/Users/engan/GitHub/ST-Pedestrian-UoM/python/pedenv/Library
	    IF "%1"=="--version" echo 1.4.13
	    IF "%1"=="--cflags" echo -IC:/Users/engan/GitHub/ST-Pedestrian-UoM/python/pedenv/Library/include
	    IF "%1"=="--libs" echo -LIBPATH:C:/Users/engan/GitHub/ST-Pedestrian-UoM/python/pedenv/Library/lib libkea.lib 
	    IF "%1"=="--includes" echo C:/Users/engan/GitHub/ST-Pedestrian-UoM/python/pedenv/Library/include
		shift
		goto :printValue
    )
	EXIT /B 0
)
