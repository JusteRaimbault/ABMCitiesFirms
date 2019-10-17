

def add_to_file(string,filename):
    data = open(filename,'r').readlines()
    #print(data)
    newcontent = [l.replace('\n','') for l in data]
    filewriter = open(filename,'w')
    filewriter.write(string+'\n')
    for l in newcontent:
        filewriter.write(l+'\n')

def read_from_file(filename):
    data = open(filename,'r').readlines()
    #print(data)
    newcontent = [l.replace('\n','') for l in data[1:]]
    filewriter = open(filename,'w')
    for l in newcontent:
        filewriter.write(l+'\n')
    if len(data)>0:
        return(data[0].replace('\n',''))
    else:
        return(None)



def get_param(param_name,filename='params.csv',delimiter=';'):
    pfile=import_csv(filename,delimiter)
    value = 0
    for line in pfile:
        if line[0]==param_name :
            value = line[1]
    return(value)




def import_csv(csvfile,delimiter):
    infile = open(csvfile,'r')
    res = []
    for line in infile.readlines():
        if line[0]!="#" :
            res.append(line.replace('\n','').split(delimiter))
    return(res)
