# Location "Java" Demo

## Purpose

To seek a solution to [this issue](https://github.com/DelphiWorlds/Playground/issues/1).

## Feedback

Please provide feedback about this demo as per [the main ReadMe.](https://github.com/DelphiWorlds/Playground/blob/main/Readme.md)

## Description

As per the Purpose section, this project is intended to seek a solution to the issue where a combination of Application and Service (based on JobIntentService) causes the app to crash after a few minutes. 

The idea in this project demo is to have a completely Java-based service, that loads a library written in Delphi (in the project called Service) and executes its methods. At present however, I'm yet to work out how to access parameters that are not simple types, See the Status section.

## Status of this demo (Sept 16th, 2022)

The Java service (LocationDemoService) successfully loads the Delphi service library, and is able to call the Test method OK, however attempting to call the ReceivedLocation method causes a crash when attempting to convert the json member (JNIString) to a Delphi string.

