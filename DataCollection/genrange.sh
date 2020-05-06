
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
#START=1
# actual end 13560001-13572000
#END=13569685
#END=13548001
#STEP=12000
#FILE=ranges
#FILE=rangesaggreg

#2bis
START=1
END=363600
#451449
STEP=90900
#90909
FILE=ranges

rm $FILE

for i in `seq $START $STEP $END`
do
  echo $i"-"$((i + STEP - 1)) >> $FILE
done

#echo "13560001-13569685" >> $FILE
echo "363601-451449" >> $FILE

