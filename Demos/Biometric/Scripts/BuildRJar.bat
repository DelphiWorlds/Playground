@echo off

set IsRelease=%1
call BuildRJarSetVars.bat

if not exist "%ProjectBinPath%\res" (
  echo It appears the project has not been deployed. Please ensure that it is, then re-run %~nx0
  goto :END
)

:: Create merged resources folder
if not exist "%MergedResPath%" mkdir "%MergedResPath%"
:: Create a folder to put the R jar in
if not exist "%ProjectPath%"\Lib mkdir "%ProjectPath%\Lib"

@echo Copying resource files...
:: Copy the PROJECT resources to the merged path
%ResMergeExe% "%ProjectBinPath%\res" "%MergedResPath%"

:: Merge resources for the target package and its dependencies
@echo Merging resource files...
:: Calling ResMerge only for packages that have a \res folder in them. The rest do not have one
%ResMergeExe% "%PackagePath%\appcompat-1.2.0\res" "%MergedResPath%"
%ResMergeExe% "%PackagePath%\appcompat-resources-1.2.0\res" "%MergedResPath%"
%ResMergeExe% "%PackagePath%\biometric-1.1.0\res" "%MergedResPath%"
%ResMergeExe% "%PackagePath%\core-1.5.0-rc02\res" "%MergedResPath%"
%ResMergeExe% "%PackagePath%\fragment-1.2.5\res" "%MergedResPath%"

:: Create a temp folder for building
if exist "%BuildPath%" rmdir /Q /S "%BuildPath%"
mkdir "%BuildPath%"
mkdir "%BuildPath%\obj"

call BuildRJarGenJava.bat appcompat-1.2.0
call BuildRJarGenJava.bat appcompat-resources-1.2.0
call BuildRJarGenJava.bat biometric-1.1.0
call BuildRJarGenJava.bat core-1.5.0-rc02

:: Create a sources file for javac to compile
dir "%BuildPath%"\R.java /S /b > "%BuildPath%\sourcefiles.txt"

@echo Compiling classes from R.java files..
"%JDKPath%\javac" -d "%BuildPath%\obj" -classpath "%PlatformPath%;%BuildPath%\obj" @"%BuildPath%\sourcefiles.txt"

:: Generate R jar
@echo Compiling %ProjectPath%\Lib\%SPN%.R.jar..
"%JDKPath%\jar" cf "%ProjectPath%\Lib\%SPN%.R.jar" -C "%BuildPath%\obj" .

:END
if exist "%BuildPath%" rmdir /Q /S "%BuildPath%"