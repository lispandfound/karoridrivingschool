#!/bin/bash

cp /run/secrets/msmtp_config /etc/msmtprc
chmod 600 /etc/msmtprc
chown appuser:appuser /etc/msmtprc

echo su appuser -c "FILED_STATE_FILE=/db/filed.db /usr/local/bin/filed /jobs" \&
su appuser -c "FILED_STATE_FILE=/db/filed.db /usr/local/bin/filed /jobs" &


echo su -s /bin/bash appuser -c '/usr/local/bin/karoridrivingschool'
exec su -s /bin/bash appuser -c '/usr/local/bin/karoridrivingschool'
