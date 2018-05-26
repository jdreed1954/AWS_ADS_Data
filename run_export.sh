#!/bin/bash -v

PYC=/mnt/c/Users/jdree/PycharmProjects/AWS_Utils/export.py

DIR=/mnt/c/Users/jdree/Desktop/MyProjects/EM-Assessment/data

PYTHONUNBUFFERED=1

declare -a AGENTS=("o-10811obiw9eie9c9z" "o-15p810oyrd09unsoy" \
                   "o-17vbev18sdyslf98g" "o-1uhe06ocvqzgg3juv" \
                   "o-1xns12efjorz827mb" "o-1z8xtzcqe7w0fa28x" \
                   "o-2egn2tmcilnkvgqd3" "o-2hvdh4os5n69fhlfr" \
                   "o-2rcllgl4kmhofuje4" "o-2wljnejhpbriizyq8" \
                   "o-3j4ftzagcxdyxro8j" "o-3owubnjzljdsg8l42" \
                   "o-4hca3adua5ibubvrl" "o-4j3e01hjivpvgv4wo" \
                   "o-4t5xlzfuo6cn2noou" "o-4ybz2yofaj6epht52" \
                   "o-5psvqgqbxgdts8rv8" "o-66v4hbwwqqovqhoi3" \
                   "o-910scsu50d58mqp7e" "o-3gs0l8zsxncd68g2r" \
                   "o-dec17htkilkp8md6z")


for i in "${AGENTS[@]}"
do
  echo "$i"
  python  ${PYC} --directory ${DIR} --filter ${i} --log-file ${i}.log
done

exit
