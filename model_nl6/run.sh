#!/bin/sh
(cat <<END
oml
oml
END
) | openmole --script Calibration.oms
cp -r calibration /data/outputs
