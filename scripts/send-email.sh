#!/bin/bash
# $1 is the path to the job file provided by 'filed'
msmtp -C /etc/msmtprc -t < "$1"
