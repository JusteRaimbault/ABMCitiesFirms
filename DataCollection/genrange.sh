
START=1
END=30
STEP=10
FILE=testranges

#START=1
#END=563389
#STEP=8900
#FILE=ranges

for i in `seq $START $STEP $END`
do
  echo $i"-"$((i + STEP - 1)) >> $FILE
done
