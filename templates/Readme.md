# templates

This directory contains input files or scripts that are project dependent.

The newProject script should copy each of these files to a new created project directory and possibly do the necessary adjustments. Some of the adjustments can be done only after the first Empire run has been executed or may require user to manually edit these files in the project directory.

Generally, a project .inp file will always require user inspection and manual editing.

Once the development of automated creation of the project is completed all necessary info should be contained in the project yaml file and be automatically propagated each time the yaml file is edited and saved.
