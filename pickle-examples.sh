#! /bin/sh

./pickle-dump.py "{}" --output example.pickle
runghc bin/pickle-read.hs example.pickle

./pickle-dump.py "{'type': 'cache-query', 'metric': 'some.metric.name'}" --output example.pickle
runghc bin/pickle-read.hs example.pickle

rm -f example.pickle
