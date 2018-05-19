#!/bin/bash -v

PYC=/mnt/c/Users/jdree/PycharmProjects/AWS_Utils/export.py

DIR=/mnt/c/Users/jdree/Desktop/MyProjects/EM-Assessment/data

PYTHONUNBUFFERED=1

python  ${PYC} --directory ${DIR} --log-file export.log

exit
