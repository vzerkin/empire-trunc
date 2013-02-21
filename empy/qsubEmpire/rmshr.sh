#!/bin/bash

echo -n ' Clear /dev/shm on entire cluster? (Y/N) > '
read repl
if [ ${repl} = Y ] || [ ${repl} = y ]; then
   echo ' All dev/shm/ files will be removed'
else
   echo ' No files removed'
   exit
fi

echo ''

echo ' On mother node:'
rm -r /dev/shm/*
echo ''

echo ' On compute nodes:'
ssh -x nukedata-0-0  rm -r /dev/shm/*
ssh -x nukedata-0-1  rm -r /dev/shm/*
ssh -x nukedata-0-2  rm -r /dev/shm/*
ssh -x nukedata-0-3  rm -r /dev/shm/*
ssh -x nukedata-0-4  rm -r /dev/shm/*
ssh -x nukedata-0-5  rm -r /dev/shm/*
ssh -x nukedata-0-6  rm -r /dev/shm/*
ssh -x nukedata-0-7  rm -r /dev/shm/*
ssh -x nukedata-0-8  rm -r /dev/shm/*
ssh -x nukedata-0-9  rm -r /dev/shm/*
ssh -x nukedata-0-10 rm -r /dev/shm/*
ssh -x nukedata-0-11 rm -r /dev/shm/*
ssh -x nukedata-0-12 rm -r /dev/shm/*

exit
