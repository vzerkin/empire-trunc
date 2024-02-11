# VS Code support for EMPIRE scripts

...requires  Visual Studio Code extension "cmd exec" and... VS Code of course :)

settings.json file is a configuration file for "cmd exec" extension to run Empire scripts from inside the VS Code. Contents of this file should be added to the existing .vscode/settings.json by copying it inside. If vscode/settings.json is not present in the project directory it should be simply copied to the newly created .vscode directory at  project directory. If newProject script has been run this directory and settings.json file should be automatically created.

If not already done by the newProject script the configuration file needs to be personalized to the current project:

- "$dir": has to be set to the full(!) path to the project
- "$file": has to be set to the name of the project e.g., Pt195 if the input file is Pt195.inp

Current set of scripts covered in the configuration file is not complete but can be easily
extended following included examples. At this time, scripts cover basic runs to make isotopic evalaution (runE, format, verify,  process, drawPlots, and archive), full set of scripts to make elemental evaluation consiting of  three scripts that make full elemental evalutaion (see elem: in front of the name), and full set of 16 scripts covering generation of covariances with Kalman (see kal: in  front of the name).   The " cmd exec" extention allows for passing different options to the scripts (shown in the extention description). To invoke the list of scripts click on "cmd exec" in the bottom bar of the VS code window close to the right hand corner.

NOTE: this is not a repalcement for a GUI. "cmd exec" allows for a single pull down menu that can become very long and difficult to use if all scripts are included. Editing common files is left to the file explorer of VS Code. If there are many files created finding a simple *.inp might be cumbersome. Also plotting requires creation of the *-zvv.yaml file out of *-log.plotc4 file, rather than clicking on an item in the list. As a benefit, we get publication quality eps in addition to interactive zvd files.
