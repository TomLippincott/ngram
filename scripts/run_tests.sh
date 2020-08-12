#!/bin/bash

N=4
TMP=/tmp
WORK=${TMP}/ngram-work

mkdir -p ${WORK}
for TYPE in bytes chars words
do
    echo "Building ${TYPE} model on 500 instances..."
    zcat data/train.txt.gz | head -n 500 | stack exec -- ngramClassifier train --modelType ${TYPE} --n ${N} --modelFile ${WORK}/model_1
    echo "Testing model..."
    zcat data/dev.txt.gz | stack exec -- ngramClassifier evaluate --modelType ${TYPE} --modelFile ${WORK}/model_1 --n ${N}
    echo "Augmenting model with 500 more instances..."
    zcat data/train.txt.gz | tail -n 500 | stack exec -- ngramClassifier update --modelType ${TYPE} --n ${N} --modelFile ${WORK}/model_1 --updatedModelFile ${WORK}/model_2
    echo "Testing updated model..."
    zcat data/dev.txt.gz | stack exec -- ngramClassifier evaluate --modelType ${TYPE} --modelFile ${WORK}/model_2 --n ${N}
    echo "Augmenting model with 500 more instances..."
    zcat data/train.txt.gz | head -n 1000 | tail -n 500 | stack exec -- ngramClassifier update --modelType ${TYPE} --n ${N} --modelFile ${WORK}/model_2 --updatedModelFile ${WORK}/model_3
    echo "Testing updated model..."
    zcat data/dev.txt.gz | stack exec -- ngramClassifier evaluate --modelType ${TYPE} --modelFile ${WORK}/model_3 --n ${N}
    echo "Testing updated model of lower order..."
    zcat data/dev.txt.gz | stack exec -- ngramClassifier evaluate --modelType ${TYPE} --modelFile ${WORK}/model_3 --n 2
    rm ${WORK}/*
done
rm -rf ${WORK}
