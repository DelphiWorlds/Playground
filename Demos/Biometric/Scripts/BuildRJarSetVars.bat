@echo off

:: ****** Modify this file to suit your project and environment
set KastriPath=Z:\Kastri
set PlatformConfig=Android
set BuildConfig=Debug
::: if %IsRelease% == 1 ( set BuildConfig=Debug ) else ( set BuildConfig=Release)

:: Project related paths
:: SPN = SanitizedProjectName
set SPN=BiometricDemoD11
set ProjectPath=%~dp0..
set ProjectBinPath=%ProjectPath%\%PlatformConfig%\%BuildConfig%\%SPN%
set MergedResPath=%ProjectPath%\Resources\Merged\res
set BuildPath=%ProjectPath%\Build

set ResMergeExe=%KastriPath%\Tools\resmerge.exe
set TrimRJavaExe=%KastriPath%\Tools\trimrjava.exe
:: Root path to target package and its dependencies
set PackagePath=%~dp0..\..\..\ThirdParty\Android\androidx-biometric-1.1.0

:: Tools/library paths
set PlatformPath=C:\Android\SDK\platforms\android-31
set JDKPath=C:\Program Files\Java\jdk1.8.0_191\bin
set AAPT2Exe=C:\Android\SDK\build-tools\31.0.0\aapt2.exe
set AAPTExe=C:\Android\SDK\build-tools\31.0.0\aapt.exe
