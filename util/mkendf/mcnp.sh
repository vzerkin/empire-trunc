#PBS -m a
#PBS -S /bin/bash
#PBS -j oe
#PBS -o ${workdir##*/}/${script##*/}.log

echo "Running MCNP on node "$HOSTNAME

# setup working directory on local scratch disk
#locdir=/dev/shm/${PBS_JOBID%%.*}
locdir=/state/partition1/${PBS_JOBID%%.*}
if [ -e $locdir ]
then
	rm -r $locdir
fi
mkdir $locdir
cd $locdir

# create local xsdir file. Start with VII.1 file and
# replace the current media being modeled
$EMPIREDIR/util/mkendf/mk_xsdir ${workdir}/ ${proj}

# copy over local ACE file
cp ${workdir}/${proj}_300K.ace ./

# copy over mcnp script
cp ${script} ./mcnp.i

# run MCNP job
/home/arcilla/bin/mcnp5 inp=mcnp.i xsdir=xsdir eol > out.dat

# move output to original directory
scp=${script##/*/}
scp=${scp%.*}
mv out.dat ${workdir}/${scp}.out
mv outp ${workdir}/${scp}.outp

# cleanup
rm -r $locdir

exit
