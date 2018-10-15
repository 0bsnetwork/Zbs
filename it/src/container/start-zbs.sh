#!/bin/bash

trap 'kill -TERM $PID' TERM INT
echo Options: $ZBS_OPTS
java $ZBS_OPTS -jar /opt/zbs/zbs.jar /opt/zbs/template.conf &
PID=$!
wait $PID
trap - TERM INT
wait $PID
EXIT_STATUS=$?
