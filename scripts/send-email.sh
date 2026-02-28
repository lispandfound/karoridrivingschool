#!/bin/bash
# $1 is the path to the job file provided by 'filed'
set -eou pipefail
msmtp -C /etc/msmtprc -t < "$1" && rm -f "$1"
