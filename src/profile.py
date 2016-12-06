import os
import subprocess
from time import time

# This script runs games between all AIs and outputs the number wins and durations
# It requires `swipl` in your environment
# Run it with `python profile.py`

SRC_DIR = os.path.dirname(os.path.abspath(__file__))


ais = [0, 1, 2, 3]
pairs = []

for idx, p1 in enumerate(ais):
    for p2 in ais[idx:]:
        pairs.append((p1, p2))
        pairs.append((p2, p1))

for (p1, p2) in pairs:
    nbRuns = 100
    if p1 == 3 or p2 == 3:
        nbRuns = 1

    startTime = time()
    process = subprocess.Popen(['swipl', "profile.pl", str(p1), str(p2), str(nbRuns)], stdout=subprocess.PIPE, cwd=SRC_DIR)
    outBytes, errBytes = process.communicate()
    endTime = time()
    out = outBytes.decode("utf-8")
    p2Wins = out.count("1")
    p1Wins = out.count("2")
    draws = nbRuns - p1Wins - p2Wins
    duration = endTime - startTime
    print("{} VS {}\n p1:{}, p2:{}, draw:{} (tot: {}, duration/game: {})".format(p1, p2, p1Wins, p2Wins, draws, nbRuns, duration / nbRuns))
