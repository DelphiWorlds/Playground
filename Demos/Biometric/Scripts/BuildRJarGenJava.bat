@echo off

set RPackage=%1
set RPath=%BuildPath%\%RPackage%
mkdir %RPath%
mkdir "%RPath%\src"

:: Generate R.java using ALL merged resources
@echo Generating R.java file for %RPackage%
if "%IsAAB%" == "A" (
  "%AAPT2Exe%" compile --dir "%MergedResPath%" -o "%BuildPath%\compiled_res.flata"
  "%AAPT2Exe%" link --proto-format -o "%BuildPath%\linked_res.ap_" -I "%PlatformPath%\android.jar" --manifest "%PackagePath%\%RPackage%\AndroidManifest.xml" -R "%BuildPath%\compiled_res.flata" --auto-add-overlay --java "%RPath%\src"
) else (
  "%AAPTExe%" package -f -m -I "%PlatformPath%\android.jar" -M "%PackagePath%\%RPackage%\AndroidManifest.xml" -S "%MergedResPath%" -J "%RPath%\src" --auto-add-overlay
)

@echo Compacting R.java file for %RPackage%
%TrimRJavaExe% "%BuildPath%\%RPackage%\src" "%PackagePath%\%RPackage%\R.txt"
