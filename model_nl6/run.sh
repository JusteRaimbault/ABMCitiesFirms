#!/bin/sh
cd /root/model
(cat <<END
oml
oml
END
) | openmole --script Calibration.oms
cp -r calibration /data/outputs
