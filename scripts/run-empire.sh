#set -x
#clear

source $EMPIREDIR/Tcl/tcltk-vars.sh
setTclTk $EMPIREDIR/Tcl
#echo "___TCLDIR  = $TCLDIR"
#echo "___DISPLAY = $DISPLAY"

#MacOS: start X11 and set DISPLAY as following
#export DISPLAY=:0.0

#wish $EMPIREDIR/../welcome32.tcl
wish $EMPIREDIR/scripts/Xrun.tcl &
errCode=$?
if [ $errCode -ne 0 ]; then
    echo "___________Error=$errCode. Check installation of Tcl/Tk."
    myos=`uname -s`
    echo "___OS: $myos"
    if test "$myos" = "Darwin" ; then
	echo "___On MacOS start X11 and set DISPLAY, for example"
	echo "       export DISPLAY=:0.0"
	echo -n "___Pause. Press any key..."; read aaa
    fi
fi
#clear
exit
