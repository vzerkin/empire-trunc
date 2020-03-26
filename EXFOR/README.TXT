    You may  update the EXFOR files yourself using the latest version     
    available from the IAEA at https://www-nds.iaea.org/x4toc4-master/?C=M;O=D .
    The C4 formatted source files have names like "C4-2019-12-12.zip".
    Download it and unzip it in the empire/EXFOR directory. This will create
    a subdirectory C4-2019-12-12 (note that 2019-12-12 date will change with
    subsequent releases). This directory contains file  C4-2019-12-12.xc4 which
    contains all necessary data. This file needs to be cleaned from the comment 
    lines and split among the projectile/target combinations. All these operations 
    are executed with the following line commands issued inside the empire/EXFOR
    subdirectory:
       
	unzip ./C4-2019-12-12.zip (if needed)
	mv ./C4-2019-12-12/C4-2019-12-12.xc4 .. 
	grep -v '^#' C4-2019-12-12.xc4 >>C4-2019-12-12.c4 (to remove comment lines)
	python parseC4.py ./C4-2019-12-12.c4 (to split the file)
