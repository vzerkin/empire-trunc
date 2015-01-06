#PBS -m a
#PBS -l nodes=1#shared
#PBS -S /bin/bash
#PBS -j oe
#
# could use -m e above to mail me when script finishes
# right now only mails on abort
#
#SCRIPT TO RUN FULL EMPIRE PACKAGE

origDir=`pwd`

echo " Running on node "$HOSTNAME

# do a couple of sanity checks to make sure this 
# was run from qsubEmpire. If not, abort.

if [ -z $dir ]; then
  echo " Working directory not specified!"
  echo " Was this run from qsubEmpire?"
  exit
fi

if [ -z $PBS_JOBID ]; then
  echo " PBS JOB_ID not defined!"
  echo " Was this run from qsubEmpire?"
  exit
fi

# setup working directory on local scratch disk
work=/dev/shm/${PBS_JOBID%%.*}
if [ -e $work ]; then
        rm -r $work
fi
mkdir $work
cd $work
cp -r $dir/* ./

# RUN X-SEC CALCULATIONS & EXFOR PROCESSING

~/empire/scripts/runE $file

# check if everything completed properly:
if [ ! `tail -n 2 $file.out | grep -c "SUCCESSFULLY"` = 1 ]; then
    echo crash: $dir >> $origDir/crashlog.txt
fi

# all done - copy local files back to orig dir
if [ -z $clean ]; then
   clean="N"
fi
if [ $clean = Y ]; then
    # only copy .xsc & pfns.out files
   if [ -e $file.xsc ]; then
      cp $file.xsc $dir
   fi
   if [ -e $file-pfns.out ]; then
      cp $file-pfns.out $dir
   fi
   if [ -e $file.out ]; then
      cp $file.out $dir
   fi
else
   # copy everything
   cp *.xsc $dir 2> /dev/null
   cp *.out $dir 2> /dev/null
   cp *.xvd $dir 2> /dev/null
   cp *.lst $dir 2> /dev/null
   cp *.sum $dir 2> /dev/null
   cp *.war $dir 2> /dev/null
   if [ ! -e ${dir%/*}/${file}-tl ]; then
      cp -r ${file}-tl $dir
   fi
fi

# delete working dir
rm -r $work

exit
