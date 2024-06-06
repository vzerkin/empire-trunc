#!/bin/bash
## Script to run EMPIRE on the new NNDC Linux cluster nlc2
## using SLURM Workload Manager/Job Scheduler
## Based on MCNP6.2 input run_mcnp_mpi.sh.serial from Arantxa

## Set partition/queue to use for submitting jobs
#SBATCH --partition=SERIAL

## Set name of job
#SBATCH --job-name=%x.%j

## Mail alert at start, end and abortion of execution
#####SBATCH --mail-type='END,FAIL'

## Send mail to this address
#####SBATCH --mail-user='gnobre@bnl.gov'

## Set the name of the output file
#SBATCH --output=%x.%j.out

#SBATCH --ntasks=1
#SBATCH --ntasks-per-node=1
#SBATCH --export=ALL

########SBATCH --ntasks-per-node=36

##########PBS -m a
##########PBS -l nodes=1#shared
##########PBS -S /bin/bash
##########PBS -j oe
#
# could use -m e above to mail me when script finishes
# right now only mails on abort
#
#SCRIPT TO RUN FULL EMPIRE PACKAGE

origDir=`pwd`
export EMPIREDIR=$empirepath

echo " Running on node "$HOSTNAME

# do a couple of sanity checks to make sure this 
# was run from qsubEmpire. If not, abort.

if [ -z $dir ]; then
  echo " Working directory not specified!"
  echo " Was this run from qsubEmpire?"
  exit
fi

if [ -z ${SLURM_JOBID} ]; then
  echo " SLURM JOB_ID not defined!"
  echo " Was this run from qsubEmpire?"
  exit
fi

# setup working directory on local scratch disk

work=/dev/shm/${SLURM_JOBID}
if [ -e $work ]; then
        rm -r $work
fi
mkdir $work
cd $work
cp -r $dir/* ./

# RUN X-SEC CALCULATIONS & EXFOR PROCESSING

#~/empire/scripts/runE $file
$EMPIREDIR/scripts/runE $file

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
