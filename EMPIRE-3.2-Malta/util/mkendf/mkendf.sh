#!/bin/bash

if [ -z $EMPIREDIR ]; then
    echo "Please set the 'EMPIREDIR' environment variable."
    echo "See Empire v3 documentation for more information."
    exit
fi

# par 1 = project (file) name

fil=${1%.*}

# check that all the required files are present

outfil=${fil}.out

if [ ! -e ${outfil} ]
then
   echo ' Empire output file not found: '${outfil}
   exit
fi

if [ -e ${fil}.res ]
then
   resfil=${fil}.res
elif [ -e ../${fil}.res ]
   then resfil=../${fil}.res
else
   echo ' Resonance file (MF2/32) not found!'
   echo ' Please create file '${fil}.res
   exit
fi

if [ -e ${fil}-orig.endf ]
then
   origfil=${fil}-orig.endf
elif [ -e ../${fil}-orig.endf ]
   then origfil=../${fil}-orig.endf
else
   echo ' Donor ENDF file not found!'
   echo ' Please create file '${fil}-orig.endf
   exit
fi

matnum=$($EMPIREDIR/util/mkendf/getmtn ${resfil})
matnum=${matnum:?" undefined!"}
echo 'MAT = '$matnum

echo ' Processing Empire output file '${outfil}
echo ''

# clean up old temp files

if [ -e ${fil}.endf ]
then
    rm -r ${fil}.endf
fi

if [ -e ${fil}_1.endf ]
then
    rm -r ${fil}_1.endf
fi

if [ -e ${fil}_2.endf ]
then
    rm -r ${fil}_2.endf
fi

if [ -e ${fil}_2.endfadd ]
then
    rm -r ${fil}_2.endfadd
fi

if [ -e ${fil}_3.endf ]
then
    rm -r ${fil}_3.endf
fi

if [ -e ${fil}.njoy ]
then
    rm -r ${fil}.njoy
fi

# convert Empire output to ENDF format & add resonances

cat > input <<EOF
${outfil}
${fil}_1.endf
1
-1.0
$matnum
EOF

$EMPIREDIR/util/empend/empend < input

cat > input <<EOF
${fil}_1.endf
${resfil}
${fil}_2.endf
1
EOF

$EMPIREDIR/util/endres/endres < input

rm ${fil}_1.endf
rm empmf1.tmp
rm endres.scr
rm angdis.*
rm input

# add in nubar, fission spectra(MF5,MT18) and ang dist
# (MF4,MT18) from donor file containing these MFs
# only do this if it's a fissile material!

if ! $EMPIREDIR/util/mkendf/add_endf ${fil}
then
    echo 'Error adding sections to ENDF file'
    exit
fi
rm ${fil}_2.endf
# if not adding, just move _2 -> _2.endfadd
# mv ${fil}_2.endf ${fil}_2.endfadd

# add 103-107 if data available and re-make elastic
# FIXUP reads from local file, NOT input stream

cat > FIXUP.INP <<EOF
10101111111001
${fil}_2.endfadd
${fil}_3.endf
S  4=+( 51, 91)
S103=+(600,649)
S104=+(650,699)
S105=+(700,749)
S106=+(750,799)
S107=+(800,849)
S  3=+(  4,  5)+( 11, 18)+( 22, 26)+( 28, 37)+( 41, 45)+( 102, 117)
S  2=+(  1,  1)-(  3,  3)

 0.00000+00 0.00000+00          0          09237103

 0.00000+00 0.00000+00          0          09237107







EOF

$EMPIREDIR/util/fixup/fixup
echo ' Done with FIXUP'
rm ${fil}_2.endfadd
rm FIXUP.INP
echo ' Done cleaning FIXUP'

# run STAN

$EMPIREDIR/util/stan/stan ${fil}_3.endf
mv ${fil}_3.STN ${fil}.endf
rm ${fil}_3.endf

exit