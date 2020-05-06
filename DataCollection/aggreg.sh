
DATADIR=data
EXPORT=processed/AMADEUS_complete.tsv
HEADERFILE=1-8000_1571226960.txt

#DATADIR=data2
#EXPORT=processed/FAME_complete.tsv
#RANGEFILE=rangesaggreg
#HEADERFILE=1-12000_1587918752.txt

# assumes ranges are as before collection
#cat $RANGEFILE | awk '{print "ls '$DATADIR' | grep "$0" "}'

rm $EXPORT
head -n 1 $DATADIR/$HEADERFILE >> $EXPORT
ls -tr $DATADIR | awk '{print "tail -n +2 '$DATADIR'/"$0" >> '$EXPORT'"}' | sh

gzip $EXPORT

