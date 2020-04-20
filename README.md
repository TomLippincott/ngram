# NGram

This is a code base for experimenting with various approaches to n-gram-based
text modeling.  The easiest way to use it to immediately train and apply models
is via Docker:

```
docker pull hltcoe/ngram
docker run --name ngram -p 8080:8080 -d hltcoe/ngram serve --modelType chars
```

This starts a container to train and apply character-based models, and exposes 
a REST API on port 8080.  You can see example usage in `scripts/ngram_rest_client.py` 
for each end-point:

1.  GET /info {} -> {"modelType" : string, "n" : int}
2.  PUT /reset {"n" : int} -> {}
3.  PATCH /train {"docs" : [doc]} -> {}
4.  GET /apply {"docs" : [doc], "n" : int} -> {"results" : [result]}
5.  GET /pull_model {} -> {"state" : string}
6.  PUT /push_model {"state" : string} -> {}

A "doc" is a JSON object with "id", "label", and "text" fields, and a "result" is a
JSON object with the same, and an additional "probabilities" field that maps each
label to a log-probability.

5. and 6. together allow you to save and restore models.  Note that the server is 
started with a particular *type* of model (byte, character, or word) and this
cannot be changed dynamically (yet): runtime errors will occur trying to restore
incompatible models.

## Compiling

To compile the code directly, first install [Stack](https://docs.haskellstack.org) somewhere on 
your `PATH`.  For example, using `~/.local/bin`:

```
wget https://get.haskellstack.org/stable/linux-x86_64.tar.gz -O -|tar xpfz - -C /tmp
cp /tmp/stack-*/stack ~/.local/bin
rm -rf /tmp/stack-*
```

Then, while in the directory of this README file, run:

```
stack build
stack install
```

The first time this runs will take a while, 10 or 15 minutes, as it builds an 
entire Haskell environment from scratch: subsequent compilations are very fast.  
The `ngramClassifier` binary will be installed in `~/.local/bin`, though you can 
also forego the `install` command and replace `ngramClassifier` with 
`stack exec -- ngramClassifier` in this guide.

## Testing

You can run a handful of tests by invoking `scripts/run_tests.sh`, which uses small 
data files included in this repository:

```
$ scripts/run_tests.sh 
Building model on 500 instances...
Testing model...
Accuracy: 0.665
Augmenting model with 500 more instances...
Testing updated model...
Accuracy: 0.754
Augmenting model with 500 more instances...
Testing updated model...
Accuracy: 0.785
Testing updated model of lower order...
Accuracy: 0.773
```

### Data format for command-line tool

The command-line invocations expect data to be text where each line has the format:

```
${id}<TAB>${label}<TAB>${text}
```

When a model is applied to data, the output will have a header of format:

```
ID<TAB>GOLD<TAB>${label_1_name}<TAB>${label_2_name}<TAB>...
```

and lines with the corresponding format:

```
${doc_id}<TAB>${gold_label_name}<TAB>${label_1_prob}<TAB>${label_2_prob}<TAB>...
```

where probabilities are represented as natural logarithms.

### Command-line operation

Functionality is comprised of four sub-commands: `train`, `update`, `apply`, 
and `evaluate`.  The sub-commands accept the `-h` and `--help` switch for
more information.  Each sub-command expects one or two input files, and produces
one output file.  These can be fully specified as arguments, or if one of the
inputs or the output are omitted, piped to/from the command.  For example, the
following (and other combinations) are equivalent:

```
ngramClassifier apply --inputFile my_data.txt --modelFile my_model.gz --scoresFile my_output.txt
cat my_data.txt | ngramClassifier apply --modelFile my_model.gz --scoresFile my_output.txt
zcat my_model.gz | ngramClassifier apply --inputFile my_data.txt --scoresFile my_output.txt
ngramClassifier apply --modelFile my_model.gz > my_output.txt
```

Note also that any argument-file specified with a `gz` suffix will be treated as
compressed (stdin/stdout are never treated as compressed, the example scripts 
show this in action).

The `train` and `apply` sub-commands are mostly self-explanatory: one less-obvious 
feature of the latter is that when applying a model you may specify a smaller value 
of `n` than it was trained with, and get the expected behavior (perhaps sacrificing 
accuracy for generalization).

The `update` sub-command takes an existing model and augments it with new data.  In 
general, `n` should be the same as for the original model to get the expected
result, though this is not mandated.

The `evaluate` sub-command is a convenient way to test a model quickly against 
labeled data, basically running `apply` and printing accuracy and F1 to `stderr`.

### Negative or otherwise-unusual labelings (EXPERIMENTAL)

In the data format:

```
${id}<TAB>${label}<TAB>${text}
```

the `label` field can actually be a composite of positive and negative labels: `-`
and `,` are special characters.  For example, these are valid:

```
en,tr
-de,-es,en
-uk,-ru
```

However, the interpretation and use of such labels is an area of active research.
The only guarantee at the moment is that a single positive label will *always* be
treated the same way (in other words, stick to that!).

## Implemented models

The remainder of this document describes the implemented models.  The library aims
to be parametric over sequence types, and forthcoming features include the ability to
specify whether to consider bytes, unicode characters, or whitespace-delimited 
tokens.

### Prediction by Partial Matching

Currently, the only implemented model (and the package's original raison d'être), PPM 
is essentially an n-gram model with a particular backoff logic that can't quite be 
reduced to more widespread approaches to smoothing, but empirically tends to 
outperform them on classifying short documents (and compressing natural language).

## Using Docker

A compact (95mb) Docker image of this system is available on Docker Hub:

```
docker pull hltcoe/ngram
```

or can be built directly from this repository:

```
docker build --force-rm -t hltcoe/ngram .
```

See `scripts/run_tests_docker.sh`: it illustrates how to invoke Docker to accomplish the same actions as the non-Docker version.

## TODO

1. Implementing additional models
2. Support RESTful server mode
3. Improvements to run-time performance
