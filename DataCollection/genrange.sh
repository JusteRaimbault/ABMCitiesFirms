
#START=1
#END=30
#STEP=10
#FILE=testranges

# 1
#START=1
#END=19081827
#STEP=8000
#FILE=ranges

# 2
START=1
END=13569685
STEP=12000
FILE=ranges

rm $FILE

for i in `seq $START $STEP $END`
do
  echo $i"-"$((i + STEP - 1)) >> $FILE
done
