# NGrams

This is a code base for experimenting with various approaches to n-gram-based
text modeling.  To get started, run:

```bash
stack build
stack install
```

This will build and install the library and binary commands.  Generally,
the commands expect data to be text files where each line has the format:

```
${id}<TAB>${label}<TAB>${text}
```

When a model is applied to data, the output will generally have a header
with the format:

```
ID<TAB>GOLD<TAB>${label_1_name}<TAB>${label_2_name}<TAB>...
```

and lines with the corresponding format:

```
${doc_id}<TAB>${gold_label_name}<TAB>${label_1_prob}<TAB>${label_2_prob}<TAB>...
```

where probabilities are represented as natural logarithms.

The remainder of this document describes the implemented models, most of which
have a corresponding command that *stack* will have installed.  The library aims
to be parametric over the sequence types, and most commands allow users to 
specify whether to consider bytes, unicode characters, or whitespace-delimited 
tokens.

## Prediction by Partial Matching

PPM is essentially an n-gram model with a particular backoff logic that can't 
quite be reduced to more widespread approaches to smoothing, but empirically 
tends to outperform them on short documents.  To create a PPM model, run:

```bash
sh> ppm train --train train.txt --dev dev.txt --n 4 --modelFile model.gz
Dev accuracy: 0.8566666666666667
```

The model can then be applied to new data:

```bash
sh> ppm apply --test test.txt --modelFile model.gz --n 4 --scoresFile scores.txt
```

The value of `--n` can also be less than the model size, which will run a bit 
faster, and (perhaps) less tuned to the original training data.
