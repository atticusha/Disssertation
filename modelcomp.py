def modelcomp(vii,vai,vti,vta):
    with open(vii) as f:
        vii=f.read().split('\n')
    with open(vai) as f:
        vai=f.read().split('\n')
    with open(vti) as f:
        vti=f.read().split('\n')
    with open(vta) as f:
        vta=f.read().split('\n')
    stats=[vii[39],vii[13],vai[39],vai[13],vti[39],vti[13],vta[39],vta[13]]
    processed=[re.sub('\[1\] ', '', i) for i in stats] 
    fl_list=[float(i) for i in processed]
    formatted=['%.2f' % i for i in fl_list]
    print(*formatted, sep = ' & ')
