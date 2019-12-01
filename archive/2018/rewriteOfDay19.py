def main():
    a=0
    b=0
    c=0
    d=0
    e=1
    a+=2
    a=((a*a)*19)*11
    c=((c+6)*22)+15
    a=a+c
    if e!=0:
        c=((27*28)+29)*30*14*32
        a=a+c
        e=0
    b = 1
    while(b<=a):
        d = 1
        if(a % b==0):
            e+=b
        b+=1
    print(e)
main()