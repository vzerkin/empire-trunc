# VS Code support for EMPIRE scripts

...requires  Visual Studio Code extension "cmd exec":

settings.json file is a configuration file for "cmd exec" extension to run Empire scripts from inside the VS Code. This file should be added to the existing .vscode/settings.json by copying it inside. If vscode/settings.json is not present
in the project directory it should be simply copied to the newly created .vscode
directory at the top of the project directory. If newProject script has been run
this directory and settings.json file should be automatically created.

If not already done by the newProject script the configuration file needs to be
personalize to the spcific project:

- "$workDir": has to be set to the full(!) path to the project
- "$rootName": has to be set to the name of the project e.g., Pt195 if the input file is Pt195.inp

Current set of scripts covered in the configuration file is not complete but can be easily
extended following included examples. The extention allows for passing different options
to the scripts (shown in the extention description).

NOTE: not all of the scripts need to be copied e.g., format only needs MAT number to be passed to the script as a second argument.  This should be possible when calling the script from new GUI or under VS code extension.

