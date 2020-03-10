import requests
import argparse
import logging
import gzip


def info_func(args):
    resp = requests.get("{}://{}:{}/info".format(args.protocol, args.host, args.port))
    logging.info(resp.json())
    return resp

def reset_func(args):
    return requests.put("{}://{}:{}/reset".format(args.protocol, args.host, args.port), json={"n" : args.n})

def train_func(args):
    docs = []
    resp = None
    with (gzip.open if args.input.endswith("gz") else open)(args.input, "rt") as ifd:
        for i, line in enumerate(ifd):
            try:
                cid, label, text = line.strip().split("\t")
            except:
                continue
            if len(docs) == args.docs_per_call:
                resp = requests.patch("{}://{}:{}/train".format(args.protocol, args.host, args.port), json={"docs" : docs})
                docs = []
            docs.append({"id" : cid, "label" : label, "text" : text})
    if len(docs) > 0:
        resp = requests.patch("{}://{}:{}/train".format(args.protocol, args.host, args.port), json={"docs" : docs})
    return resp

def apply_func(args):
    results = []
    docs = []
    with (gzip.open if args.input.endswith("gz") else open)(args.input, "rt") as ifd:
        for line in list(ifd):
            cid, label, text = line.strip().split("\t")
            if len(docs) == args.docs_per_call:
                resp = requests.get("{}://{}:{}/apply".format(args.protocol, args.host, args.port), json={"docs" : docs})
                docs = []
                results += resp.json().get("results", [])
            docs.append({"id" : cid, "label" : label, "text" : text})
    if len(docs) > 0:
        resp = requests.get("{}://{}:{}/apply".format(args.protocol, args.host, args.port), json={"docs" : docs})
        results += resp.json().get("results", [])
    total = 0
    correct = 0
    with (gzip.open if args.output.endswith("gz") else open)(args.output, "wt") as ofd:
        for res in results:
            try:
                guess = list(reversed(sorted(res["probabilities"].items(), key=lambda x : x[1])))[0][0]
            except:
                guess = None
            ofd.write("{}\t{}\n".format(res["label"], guess))
            if guess == res["label"]:
                correct += 1
            total += 1
    logging.info("Accuracy: %.3f", correct / total)
    return resp

def pull_func(args):
    resp = requests.get("{}://{}:{}/pull_model".format(args.protocol, args.host, args.port))
    with (gzip.open if args.output.endswith("gz") else open)(args.output, "wt") as ofd:
        ofd.write(resp.json()["state"])
    return resp

def push_func(args):
    with (gzip.open if args.input.endswith("gz") else open)(args.input, "rt") as ifd:
        model = ifd.read()
    return requests.put("{}://{}:{}/push_model".format(args.protocol, args.host, args.port), json={"state" : model})


if __name__ == "__main__":

    parser = argparse.ArgumentParser()
    subs = parser.add_subparsers()

    parser.add_argument("--host", dest="host", default="localhost", help="Host name of REST service")
    parser.add_argument("--port", dest="port", default=8080, type=int, help="Port number of REST service")
    parser.add_argument("--protocol", dest="protocol", default="http", choices=["http", "https"], help="Protocol (either http or https)")
    
    info_parser = subs.add_parser("info")
    info_parser.set_defaults(func=info_func)
    reset_parser = subs.add_parser("reset")
    reset_parser.add_argument("--n", dest="n", required=True, type=int, help="Maximum n-gram length")
    reset_parser.set_defaults(func=reset_func)
    train_parser = subs.add_parser("train")
    train_parser.add_argument("--input", dest="input", help="Data file of tab-separated lines in form 'ID<TAB>LABEL<TAB>TEXT'")
    train_parser.add_argument("--docs_per_call", dest="docs_per_call", type=int, default=1000, help="Number of documents to send per REST call")
    train_parser.set_defaults(func=train_func)
    apply_parser = subs.add_parser("apply")
    apply_parser.add_argument("--input", dest="input", help="Data file of one document per line")
    apply_parser.add_argument("--docs_per_call", dest="docs_per_call", type=int, default=1000, help="Number of documents to send per REST call")
    apply_parser.add_argument("--output", dest="output", help="Output file for results")
    apply_parser.set_defaults(func=apply_func)
    pull_parser = subs.add_parser("pull_model")
    pull_parser.add_argument("--output", dest="output", help="Where to store the model pulled from the REST API")
    pull_parser.set_defaults(func=pull_func)
    push_parser = subs.add_parser("push_model")
    push_parser.add_argument("--input", dest="input", help="Model file previously downloaded from the REST API")
    push_parser.set_defaults(func=push_func)
    args = parser.parse_args()

    logging.basicConfig(level=logging.INFO)

    if hasattr(args, "func"):
        resp = args.func(args)
    else:
        print("Run with '-h' for usage information.")
