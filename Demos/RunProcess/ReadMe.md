# Run Process Demo

## Purpose

Potential replacement for the `TRunProcess` code that already exists in [Kastri, in the Core folder](https://github.com/DelphiWorlds/Kastri/tree/master/Core).
As per [this issue](https://github.com/DelphiWorlds/Codex/issues/26) raised in the Codex repo, the intention is to remove the dependence on JCL/JVCL.

## Description

This is essentially a test app for using `TRunProcess`. Click the button and the code should execute the given command. See the Status section regarding the issue(s)

## Supported Versions

The code compiles in Delphi 11.x. It may or may not work on earlier versions

## Feedback

Please provide feedback about this demo as per [the main ReadMe.](https://github.com/DelphiWorlds/Playground/blob/main/Readme.md)

## Status

Mar 21st, 2023:

Peformed major refactoring, and the code now works. Still needs more testing

Jan 30th, 2023:

Currently, there's an issue with an AV in [line 214 of `DW.Process.Win.pas`](https://github.com/DelphiWorlds/Playground/blob/c19f7767d0d784b116ece0c2f67beed49c79c3c3/Demos/RunProcess/DW.RunProcess.Win.pas#L214), so help is being sought on how to resolve this.
