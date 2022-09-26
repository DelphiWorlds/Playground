# Biometric Beta Demo

## Description

**For Delphi 11 only.**. Support for earlier versions is very unlikely to happen.

This demo represents a potential update to the Biometric demo that [already exists in Kastri](https://github.com/DelphiWorlds/Kastri/tree/master/Demos/Biometric)

It was created as a result of [this issue](https://github.com/DelphiWorlds/Kastri/issues/88) reported in the original demo, which apparently affects devices running Android 9 or earlier.

**NOTE: This "beta" of the demo is provided only in order to resolve any unforeseen issues. The plan is to sometime in the near future make it easier to include the support in your own project, unless you feel capable of doing it yourself.**

## Steps to build this demo

Due to [missing support for Android packages](https://quality.embarcadero.com/browse/RSP-20000) in Delphi, there are extra steps required to build the demo. _These steps only need to be done the first time the project is built, or when the resources and/or packages change, which hopefully should not be often._

1. Modify `BuildRJarSetVars.bat` in the `Scripts` folder to suit your environment. See below for details
2. Load the `BiometricDemoD11` project in Delphi
3. Build the project
4. Deploy the project - **note that this will error at the end of the process - this is expected**
5. In a command line window, navigate to the `Scripts` folder, and run `BuildRJar.bat`
6. Build and Deploy the project again - this time it should succeed

### Changes required for `BuildRJarSetVars.bat`

The value for KastriPath will need to be changed to where Kastri is located on your machine, e.g. it might be in `C:\Source\Kastri`, so the line would become:

```
set KastriPath=C:\Source\Kastri
```

Some or all of the values under:
```
:: Tools\Library paths
```
May need to be changed, depending on where these are located on your machine. If you are using the AdoptOpenJDK that is recommended for Delphi 11, the line for `JDKPath` will be:

```
set JDKPath=C:\Program Files\Eclipse Adoptium\jdk-11.0.14.101-hotspot\bin
```

## Longer Term Plan

The longer term plan for the kind of requirements to build projects that rely on Android packages such as this demo is to better automate the build process (most likely via [Codex](https://github.com/DelphiWorlds/Codex)), so that developers don't need to worry about tweaking batch files (and perhaps not even relying on them).

## Some details about the changes

As described above, Delphi currently does not support easy integration of Android packages, like tools such as Android Studio do. In order to fill this gap, it is necessary to download the package, and any of its dependencies. Those dependencies may have their own dependencies as well.

In this demo, the work of downloading and extracting the package and dependencies has been done and included with this repo. This was achieved by using Gradle (which Android Studio uses), and through some automation. The result is in the `ThirdParty\Android\androidx-biometric-1.1.0` folder, as well as some of the `.jar` files that appear in `ThirdParty\Android`.

The batch files in the `Scripts` folder use these resources which are processed with tools from Kastri, Android Build Tools and the JDK to first merge the resources, then create the final `BiometricDemoD11.R.jar` library that appears in the `Lib` folder. The merged resources have then been added to the project deployment.









