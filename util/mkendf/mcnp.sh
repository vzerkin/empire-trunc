#PBS -m a
#PBS -S /bin/bash
#PBS -j oe
#PBS -o ${workdir##/*/}/${proj}_mcnp.log
echo 'Running MCNP'

# setup working directory on local scratch disk
locdir=/dev/shm/${PBS_JOBID%%.*}
if [ -e $locdir ]
then
	rm -r $locdir
fi
mkdir $locdir
cd $locdir

# create the xsdir based on integral experiment and local xsdir file
$EMPIREDIR/util/mkendf/mk_xsdir ${workdir}/${proj}

# copy over ACE file
cp ${workdir}/${proj}_300K.ace ./

# copy over mcnp script
cp ${workdir%/*}/mcnp.i ./

/home/arcilla/bin/mcnp5 inp=mcnp.i xsdir=xsdir eol > out.dat

# move output to original directory & clean up
mv out.dat ${workdir}/${proj}_mcnp.out
mv outp ${workdir}/${proj}_mcnp.outp
rm -r $locdir

exit
