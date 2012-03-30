#! /usr/bin/env python

import fileinput
import re
import sys
import json

logline_pattern = re.compile(r'^log output: ([0-9]*): ([0-9]): trace: (.*)$')
def logline(text):
    mo = logline_pattern.match(text)
    if mo:
        time = int(mo.group(1))
        node = int(mo.group(2))
        action = json.loads(mo.group(3))
        return time, node, action
    else:
        return None

expect = [
    [],
    [
        {"type":"call", "seq":1, "fun":"RPC_TELL", "peer":"fe80::212:7403:3:303", "call":{"type":"call", "seq":0, "fun":"RPC_READ_SLOW_SENSOR", "sensor":1}},
        {"type":"call", "seq":2, "fun":"RPC_READ_SLOW_SENSOR", "sensor":2},
        {"type":"call", "seq":3, "fun":"RPC_READ_SLOW_SENSOR", "sensor":3},
        {"type":"call", "seq":4, "fun":"RPC_READ_FAST_SENSOR", "sensor":1},
        {"type":"response", "seq":4, "fun":"RPC_READ_FAST_SENSOR", "value":2309},
        {"type":"response", "seq":1, "fun":"RPC_TELL", "call":{"type":"response", "seq":0, "fun":"RPC_READ_SLOW_SENSOR", "value":27771}},
        {"type":"response", "seq":2, "fun":"RPC_READ_SLOW_SENSOR", "value":-13429},
        {"type":"response", "seq":3, "fun":"RPC_READ_SLOW_SENSOR", "value":12417},
    ], [
        {"type":"call", "seq":1, "fun":"RPC_TELL", "peer":"fe80::212:7403:3:303", "call":{"type":"call", "seq":0, "fun":"RPC_READ_SLOW_SENSOR", "sensor":1}},
        {"type":"call", "seq":2, "fun":"RPC_READ_SLOW_SENSOR", "sensor":2},
        {"type":"call", "seq":3, "fun":"RPC_READ_SLOW_SENSOR", "sensor":3},
        {"type":"call", "seq":4, "fun":"RPC_READ_FAST_SENSOR", "sensor":1},
        {"type":"response", "seq":4, "fun":"RPC_READ_FAST_SENSOR", "value":2309},
        {"type":"response", "seq":1, "fun":"RPC_TELL", "call":{"type":"response", "seq":0, "fun":"RPC_READ_SLOW_SENSOR", "value":27771}},
        {"type":"response", "seq":2, "fun":"RPC_READ_SLOW_SENSOR", "value":-13429},
        {"type":"response", "seq":3, "fun":"RPC_READ_SLOW_SENSOR", "value":12417},
    ], [
        {"type":"call", "seq":0, "fun":"RPC_READ_SLOW_SENSOR", "sensor":1},
        {"type":"response", "seq":0, "fun":"RPC_READ_SLOW_SENSOR", "value":27771},
    ],
    ]

def verify(logs):
    loop = 0
    time = [-1,0,0,0]
    for line in logs:
        log = logline(line.strip())
        if log != None:
            _, node, action = log
            if node == 2 and time[node] == 4:
                expect[2][4]["value"] = action["value"]
                expect[1][4]["value"] = action["value"]
            if node == 2 and time[node] == 6:
                expect[2][6]["value"] = action["value"]
                expect[1][6]["value"] = action["value"]
            if node == 2 and time[node] == 7:
                expect[2][7]["value"] = action["value"]
                expect[1][7]["value"] = action["value"]
            if node == 3 and time[node] == 1:
                expect[3][1]["value"] = action["value"]
                expect[2][5]["call"]["value"] = action["value"]
                expect[1][5]["call"]["value"] = action["value"]

            assert action == expect[node][time[node]], "\naction=%s\nexpect=%s\nnode=%d\ntime=%d" % (action, expect[node][time[node]], node, time[node])
            time[node] += 1

            if node == 1 and time[node] == 8:
                loop += 1
                time = [-1, 0, 0, 0]
                for i in range(len(expect)):
                    for j in range(len(expect[i])):
                        expect[i][j]["seq"] += 5
                        if expect[i][j].has_key("call"):
                            expect[i][j]["call"]["seq"] += 5

    assert loop != 0, "not enough log data"
    sys.stdout.write("verification succeeded.\n")

if __name__ == '__main__':
    verify(fileinput.input())
