#!/bin/bash

N=4
TMP=/tmp
WORK=${TMP}/haskseg-work

mkdir -p ${WORK}
echo "Building model on 500 instances..."
zcat data/train.txt.gz | head -n 500 | docker run -v ${WORK}:${WORK} --rm -i hltcoe/ngram train --n ${N} --modelFile ${WORK}/model_1.gz
echo "Testing model..."
zcat data/dev.txt.gz | docker run -v ${WORK}:${WORK} --rm -i hltcoe/ngram evaluate --modelFile ${WORK}/model_1.gz --n ${N}
echo "Augmenting model with 500 more instances..."
zcat data/train.txt.gz | tail -n 500 | docker run -v ${WORK}:${WORK} --rm -i hltcoe/ngram update --modelFile ${WORK}/model_1.gz --n ${N} --updatedModelFile ${WORK}/model_2.gz
echo "Testing updated model..."
zcat data/dev.txt.gz | docker run -v ${WORK}:${WORK} --rm -i hltcoe/ngram evaluate --modelFile ${WORK}/model_2.gz --n ${N}
echo "Augmenting model with 500 more instances..."
zcat data/train.txt.gz | tail -n 500 | docker run -v ${WORK}:${WORK} --rm -i hltcoe/ngram update --modelFile ${WORK}/model_2.gz --n ${N} --updatedModelFile ${WORK}/model_3.gz
echo "Testing updated model..."
zcat data/dev.txt.gz | docker run -v ${WORK}:${WORK} --rm -i hltcoe/ngram evaluate --modelFile ${WORK}/model_3.gz --n ${N}
echo "Testing updated model of lower order (n=2)..."
zcat data/dev.txt.gz | docker run -v ${WORK}:${WORK} --rm -i hltcoe/ngram evaluate --modelFile ${WORK}/model_3.gz --n 2
rm -rf ${WORK}
