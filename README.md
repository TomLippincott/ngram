# NGram

This is a code base for experimenting with various approaches to n-gram-based
text modeling.

## Compiling

First install [Stack](https://docs.haskellstack.org) somewhere on your `PATH`.  For example, for `~/.local/bin`:

```
wget https://get.haskellstack.org/stable/linux-x86_64.tar.gz -O -|tar xpfz - -C /tmp
cp /tmp/stack-*/stack ~/.local/bin
rm -rf /tmp/stack-*
```

Then, while in the directory of this README file, run:

```
stack build
```

The first time this runs will take a while, 10 or 15 minutes, as it builds an entire Haskell environment from scratch.  Subsequent compilations are very fast.

## Running

Generally, the commands expect data to be text files where each line has the format:

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
sh> stack exec -- ngramClassifier train --train train.txt --dev dev.txt --n 4 --modelFile model.gz
Dev accuracy: 0.8566666666666667
```

The model can then be applied to new data:

```bash
sh> stack exec -- ngramClassifier apply --test test.txt --modelFile model.gz --n 4 --scoresFile scores.txt
```

The value of `--n` can also be less than the model size, which will run a bit 
faster, and (perhaps) less tuned to the original training data.
