#!/bin/bash

echo ' Checking /state/partition1/ on cluster'
echo ''

echo 'On nukedata-0-0'
ssh -x nukedata-0-0 df /state/partition1/
echo 'On nukedata-0-1'
ssh -x nukedata-0-1 df /state/partition1/
echo 'On nukedata-0-2'
ssh -x nukedata-0-2 df /state/partition1/
echo 'On nukedata-0-3'
ssh -x nukedata-0-3 df /state/partition1/
echo 'On nukedata-0-4'
ssh -x nukedata-0-4 df /state/partition1/
echo 'On nukedata-0-5'
ssh -x nukedata-0-5 df /state/partition1/
echo 'On nukedata-0-6'
ssh -x nukedata-0-6 df /state/partition1/
echo 'On nukedata-0-7'
ssh -x nukedata-0-7 df /state/partition1/
#echo 'On nukedata-0-8'
#ssh -x nukedata-0-8 ls /state/partition1/
#echo 'On nukedata-0-9'
#ssh -x nukedata-0-9 ls /state/partition1/
#echo 'On nukedata-0-10'
#ssh -x nukedata-0-10 ls /state/partition1/
#echo 'On nukedata-0-11'
#ssh -x nukedata-0-11 ls /state/partition1/
#echo 'On nukedata-0-12'
#ssh -x nukedata-0-12 ls /state/partition1/

exit
