
DATADIR=data2
EXPORT=processed/FAME_complete.tsv
RANGEFILE=rangesaggreg

# assumes ranges are as before collection
#cat $RANGEFILE | awk '{print "ls '$DATADIR' | grep "$0" "}'

rm $EXPORT
head -n 1 data2/1-12000_1587918752.txt >> $EXPORT
ls -tr data2 | awk '{print "tail -n +2 data2/"$0" >> '$EXPORT'"}' | sh

gzip $EXPORT

