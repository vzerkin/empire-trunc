#!/bin/bash

echo ' Checking /dev/shm on cluster'
echo ''

echo ' On mother node'
df /dev/shm
echo ''

echo 'On nukedata-0-0'
ssh -x nukedata-0-0 df /dev/shm
echo 'On nukedata-0-1'
ssh -x nukedata-0-1 df /dev/shm
echo 'On nukedata-0-2'
ssh -x nukedata-0-2 df /dev/shm
echo 'On nukedata-0-3'
ssh -x nukedata-0-3 df /dev/shm
echo 'On nukedata-0-4'
ssh -x nukedata-0-4 df /dev/shm
#echo 'On nukedata-0-5'
#ssh -x nukedata-0-5 df /dev/shm
echo 'On nukedata-0-6'
ssh -x nukedata-0-6 df /dev/shm
echo 'On nukedata-0-7'
ssh -x nukedata-0-7 df /dev/shm
echo 'On nukedata-0-8'
ssh -x nukedata-0-8 df /dev/shm
echo 'On nukedata-0-9'
ssh -x nukedata-0-9 df /dev/shm
echo 'On nukedata-0-10'
ssh -x nukedata-0-10 df /dev/shm
echo 'On nukedata-0-11'
ssh -x nukedata-0-11 df /dev/shm
echo 'On nukedata-0-12'
ssh -x nukedata-0-12 df /dev/shm

exit
