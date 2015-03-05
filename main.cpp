#include <fstream>
#include <string>
#include <iostream>
#include <cstdlib>
#include <map>
using namespace std;
class SIC{
    public:
map<string, map<string,string> >  optab;
map<string,map<string,string> > symtab;
string locctr;
string plength;
string text;
string t,sa,aline;
public:
void otab()
{
optab["ADD"]["code"]="18";
optab["AND"]["code"]="40";
optab["COMP"]["code"]="28";
optab["DIV"]["code"]="24";
optab["JEQ"]["code"]="30";
optab["J"]["code"]="3C";
optab["JGT"]["code"]="34";
optab["JLT"]["code"]="38";
optab["JSUB"]["code"]="48";
optab["LDA"]["code"]="00";
optab["LDCH"]["code"]="50";
optab["LDL"]["code"]="08";
optab["LDX"]["code"]="04";
optab["MUL"]["code"]="20";
optab["OR"]["code"]="44";
optab["RD"]["code"]="D8";
optab["WD"]["code"]="DC";
optab["RSUB"]["code"]="4C";
optab["STA"]["code"]="0C";
optab["STCH"]["code"]="54";
optab["STL"]["code"]="14";
optab["STSW"]["code"]="E8";
optab["STX"]["code"]="10";
optab["SUB"]["code"]="1C";
optab["TD"]["code"]="E0";
optab["TIX"]["code"]="2C";
}
string write(string s,string g,string h)
{   int d=text.length();
    string f;
    f=dth(d/2);
    string k=t+f+s;
    text=h;
    t="T"+g;
    return k;
}
string gtr(string s,int f,string l)
{  if(t.length()==0)
    t="T"+l;
    string b;
    if(f==1)
     b=write(text,l,s);
    else
    if((text.length()+s.length())>60)
    b=write(text,l,s);
    else
    if(text.length()==60)
    b=write(text,l,s);
    else

    text=text+s;
    return b;


}
string its(int i)
{string k="0123456789";
string b="";
for(int m=i;i>0;i/=10)
{
    b=k[m%10]+b;
}
return b;
}

int retnum(string s)
{
    string c=s;
    int n;
    n=atoi(c.c_str());
    return n;

}
string sitb(char s)
{ string h1="01234567";
 int d=h1.find(s);

  string k="";
  int n=d;
  int p;
    for(int i=0;i<=2;i++)
    {
        p=n%2;
       k=h1[p]+k;
       n/=2;
    }
    return k;


}
int htd(string s)
{
    string h="0123456789ABCDEF";

  int d;
  int k=0;
  int p=1;
    for(int i=s.length()-1;i>=0;i--)
    {
       d=h.find(s[i]);
       k=k+p*d;
       p*=16;

    }//cout<<"htd  AAAA"<<s<<" l"<<k;
    return k;
}

int btd(string s)
{
    string h="01";

  int d;
  int k=0;
  int p=1;
    for(int i=s.length()-1;i>=0;i--)
    {
       d=h.find(s[i]);
       k=k+p*d;
       p*=2;

    }//cout<<"htd  AAAA"<<s<<" l"<<k;
    return k;
}



 string dth(int s)
 {
  string h="0123456789ABCDEF";
  string f="";
  int q;
     for(int i=s;i>0;i/=16)
     {
        q=i%16;
        f=h[q]+f;
     }
     return f;
 }
 string add(string a,string b)

 {
 int a1=htd(a);

 int a2=htd(b);
 int a3=a1+a2;
//cout<<"DTH "<<dth(a3)<<" a +"<<a<<" "<<b<<" +"<<a3;
 return dth(a3);
 }
string process2(string loc,string lab,string com,string tar,string ins,int ln)
{



    //string acc="";
    string pname;
    string sa,temp,sp,gdf;
    sp="";
    int fr=0;
    string fry=symtab[lab]["tr"];
    if(fry.compare("break")==0)
        {fr=1;

        }

    string lc1,lc2;

    if(com.compare("START")==0)
        {

            int k1=6-symtab[lab]["address"].length();
            int k2=6-locctr.length();
            int k=6-lab.length();
            for(int i=0;i<3;i++)
            {
             if(i <k1)
                    lc1+="0";
             if(i <k2)
                lc2+="0";
             if(i<k)
                sp+=" ";
            }
            gdf="H"+lab+sp+lc1+symtab[lab]["address"]+lc2+plength;

           aline=ins;

        }






    if(ins[0]!='-')
    {
     string op=optab[com]["code"];
     if(op.length())
     {   string tr;
         string opr=symtab[tar]["address"];
         if(opr.length())
         {
         aline=ins+" "+op+opr;
         gdf=gtr(op+opr,fr,loc);}
         else
         {

         if(tar.find(",")<tar.length())
         {
             string h=tar.substr(0,tar.find(","));
             string hop=symtab[h]["address"];

             string x="1";
             string hop2=x+sitb(hop[0]);
             int y=btd(hop2);
             hop=hop.substr(1,hop.length());
             aline=ins+" "+op+its(y)+hop;
             gdf=gtr(op+its(y)+hop,fr,loc);

         }else
         {aline=ins+" "+op+"0000";
         gdf=gtr(op+"0000",fr,loc);}
         }



     }
     if(com.compare("BYTE")==0)
       {
        string dev;
        if(tar[0]=='X')
        dev=tar.substr(2,(tar.length()-3));
        if(tar[0]=='C')
            for(int i=2;i<(int)tar.length()-1;i++)
            dev=dev+dth((int)tar[i]);
       aline=ins+" "+dev;
       gdf=gtr(dev,fr,loc);
       }
       if(com.compare("WORD")==0)
       {
           string fg;
           string r=dth(retnum(tar));
           int m=r.length();
           for(int i=0;i<6-m;i++)
           fg+="0";
           fg=fg+r;
       aline=ins+" "+fg;
     gdf=gtr(fg,fr,loc);
       }
       if(com.compare("RESB")==0||com.compare("RESW")==0)
        aline=ins;
     }
     else
    aline="     "+ins.substr(ins.find(" ")+1,ins.length());
     if(com.compare("END")==0)
     {

      gdf=gtr("",1,loc);
      string sad=symtab[tar]["address"];
      string dg="";
      for(int i=0;i<6-(int)sad.length();i++)
        dg+="0";
    //aline=ins;
      gdf="E"+dg+sad;}
return gdf;


}
string process(string lab,string com,string tar,string ins,int ln)
{




    string pname;
    string temp;
    if(ln==1)
    {  //cout<<ln<<"----"<<lab<<"----"<<com<<"----"<<tar<<endl;
        if(com.compare("START")==0)
        {
            sa=tar;
            pname=lab;
            locctr=tar;
            //myfile<<locctr<<" "<<ins<<endl;
           // cout<<locctr<<" "<<ins<<endl;

        }else
    locctr="0000";


    }

    //do{
            if((ins.find(" ")+1)==ins.find("."))
            {//myfile<<"COMMENT "<<ins<<endl;
            temp="----"+ins.substr(ins.find(" "),ins.length());
            }
            else
            { if(com.compare("END")!=0)
    {
     temp=locctr+" "+ins.substr(ins.find(" ")+1,ins.length());


    }
    else

  temp="---- "+ins.substr(ins.find(" ")+1,ins.length());
                if(lab.length())
                {
                    string t=symtab[lab]["flag"];
                    if(t.length()==1)
                        symtab[lab]["flag"]="Duplicate Symbol";
                    else
                    {symtab[lab]["address"]=locctr;
                     symtab[lab]["flag"]="0";
                     if(com.compare("JSUB")==0)
                        symtab[tar]["tr"]="break";
                    }


                }
                if(com.length())
                {
                    string t1=optab[com]["code"];
                   // cout<<"-----"<<t1<<"-"<<com<<"-"<<endl;
                    if(t1.length())
                    {//cout<<"-----"<<locctr<<endl;
                     locctr=add(locctr,"3");
                     //cout<<"-----"<<locctr<<endl;

                    }
                    else if (com.compare("WORD")==0)
                        locctr=add(locctr,"3");
                    else if(com.compare("RESW")==0)
                        locctr=add(locctr,dth((3*retnum(tar))));
                    else if(com.compare("RESB")==0)
                        locctr=add(locctr,dth(retnum(tar)));
                    else if(com.compare("BYTE")==0)
                       {
                           if(tar[0]=='C')

                        locctr=add(locctr,dth((tar.length()-3)));
                        else
                        locctr=add(locctr,dth(1));

                        //cout<<"TAS"<<locctr;
                        }
                    else
                        optab["Error"]["flag"]="1";
                }
           }
        //myfile<<locctr<<" "<<ins<<endl;
        //}while (com.compare("END")!=0);
    //myfile<<locctr<<" "<<ins<<endl;
    plength=dth(htd(locctr)-htd(sa));
    //cout<<plength;
    //cout<<"program length:"<<plength<<endl;
    return temp;
}
void readinput(){
    int lno=5;
    ofstream myfile;
    myfile.open("code.txt");
    cout<<"Line\t Command\n";
    string command;
    //cout<<command.find("END")<<endl;
    do
    {
    cout<<lno<<" ";
    getline(cin,command);
    //cout<<command.find("END")<<" "<<command.length()<<endl;
    myfile<<lno<<" "<<command<<endl;
    lno+=5;
    if(command.compare("END")==0&&command.length()==3)
    break;
    if(command.find("END ")<command.length())
    break;
    }while(1);
    myfile.close();
}
void gen2()
{   string  line;
    int pos[3],c,f, i=5;
     ofstream mfile,afile;
     afile.open("AssemblerListing.txt");
     mfile.open("objectcode.txt");
  //  mfile.open("end.txt");
    ifstream input("intermediate.txt");
    if(input.is_open())
    {
       while (!input.eof())
       { string command,target,label;

         getline(input,line);
         pos[0]=0,pos[1]=0,pos[2]=0,c=0,f=0;
         while(f!=-1)
         {
             f=line.find(" ",f+1);
             if(f!=-1)
            pos[c++]=f;
         }
        if(c==1)
        {command=line.substr((pos[0]+1));}
       else if
        (c==2)
        {command=line.substr((pos[0]+1),(pos[1]-pos[0]-1));
        target=line.substr((pos[1]+1));}
       else
        if(c==3)
        {label=line.substr((pos[0]+1),(pos[1]-pos[0]-1));
        command=line.substr((pos[1]+1),(pos[2]-pos[1]-1));
        target=line.substr((pos[2]+1));}
        else
            continue;
        //mfile<<process(label,command,target,line,i++)<<endl;
        string lc=line.substr(0,(pos[0]));
        //cout<<lc<<"!     l"<<label<<"            c"<<command<<" t"<<target<<endl;
        string h=process2(lc,label,command,target,line,i);
        if(h.length())
        mfile<<h<<endl;
        i+=5;
        afile<<aline<<endl;
       }
    }
    mfile.close();
    afile.close();
}

void gen()
{   string  line;
    int pos[3],c,f, i=1;
     ofstream mfile;
    mfile.open("intermediate.txt");
    ifstream input("code.txt");
    if(input.is_open())
    {
       while (!input.eof())
       { string command,target,label;
         getline(input,line);
         pos[0]=0,pos[1]=0,pos[2]=0,c=0,f=0;
         while(f!=-1)
         {
             f=line.find(" ",f+1);
             if(f!=-1)
             pos[c++]=f;

         }
        if(c==1)
        {command=line.substr((pos[0]+1));}
       else if
        (c==2)
        {command=line.substr((pos[0]+1),(pos[1]-pos[0]-1));
        target=line.substr((pos[1]+1));}
       else
        if(c==3)
        {label=line.substr((pos[0]+1),(pos[1]-pos[0]-1));
        command=line.substr((pos[1]+1),(pos[2]-pos[1]-1));
        target=line.substr((pos[2]+1));}
        else
            continue;
       mfile<<process(label,command,target,line,i++)<<endl;
        //cout<<c<<"     l"<<label<<"            c"<<command<<" t"<<target<<endl;
       }
    }
    mfile.close();
}
};
int main()
{  cout<<"Comment out 'c.readinput()' and copy your code to 'code.text' if you have input problems."<<endl;
    SIC c;
   c.otab();

   //c.readinput();//comment this line if needed
   c.gen();
   c.gen2();
   cout<<"Open 'objectcode.text' ";
    return 0;
}
