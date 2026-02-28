#!/bin/bash
# Delete /tmp/email-* files older than one hour
find /tmp -name 'email-*' -mmin +60 -delete
