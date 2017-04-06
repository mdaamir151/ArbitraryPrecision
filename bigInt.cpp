#include <bits/stdc++.h>

using namespace std;
  /*best block-size , since two nine digits numbers multiply to give product which can be stored in long long without overflow*/
const int base=1000000000;

/* below 43 digits O(n^2) multiplication performs better than Karatsuba's multiplication algo*/ 
const int optimal=43;


class bigInt
{
    public:
    vector<int> v;
    bool sign;
public:
    bigInt(){}
    bigInt(long long x)  //constructor for bigint from int , long , long long numbers
    {
        if(x<0) { sign=1; x*=-1; }
        else sign = 0;
        v.push_back(x%base);
        x/=base;
        if(x) v.push_back(x);
    }

    bigInt(const string& s) //constructs bigint from large strings
    {
        int l=s.size(),L,pos=0;
        sign=0;
        if(s[0]=='+') { l-=1; pos=1; }
        if(s[0]=='-') { l-=1; sign=1; pos=1; }

        L=l;

        v.resize(ceil(l/9.0));
        l-=int(l/9)*9;

        int x=0,k=v.size()-1;
        for( int i=0; i<l; ++i) { x=(x<<1)+(x<<3) ; x+=s[pos++]-'0'; }
        if(x)  v[k--]=x;

        while(pos<L)
        {
            x=0;
            for( int i=0; i<9; ++i)
            {
                x=(x<<1)+(x<<3), x+=s[pos++]-'0';
            }
            v[k--]=x;
        }
        this->trim();
    }

    bigInt operator-() const  //negation operator / inverts sign of big int
    {
        bigInt c;
        c.sign=sign^1;
        c.v=v;
        return c;
    }

    bool operator==(const bigInt& b)
    {
        if( (sign!=b.sign) || (v.size()!=b.v.size()) ) return false;
        for( int i=v.size()-1; i>=0; --i) if(v[i]!=b.v[i]) return false;
        return true;
    }

    bool operator<(const bigInt& b)
    {
        if(sign!=b.sign) return sign>b.sign;
        if(v.size()!=b.v.size())
        {
            if(sign) return v.size()>b.v.size();
            return v.size()<b.v.size();
        }
        int l=v.size();
        for(int i=l-1; i>=0; --i)
        {
            if(v[i]!=b.v[i])
            {
                if(sign) return v[i]>b.v[i];
                return v[i]<b.v[i];
            }
        }
        return false;

    }

    bool operator>(const bigInt& b)
    {
        if(sign!=b.sign) return sign<b.sign;
        if(v.size()!=b.v.size())
        {
            if(sign) return v.size()<b.v.size();
            return v.size()>b.v.size();
        }
        int l=v.size();
        for(int i=l-1; i>=0; --i)
        {
            if(v[i]!=b.v[i])
            {
                if(sign) return v[i]<b.v[i];
                return v[i]>b.v[i];
            }
        }
        return false;
    }

    bigInt operator+(const bigInt& b) const
    {
        if(sign!=b.sign) return *this-(-b);
        bigInt c;
        c.sign=sign;
        int vs=v.size(), bs=b.v.size();

        c.v.resize(max(bs,vs));
        int carry=0;
        int l=min(bs,vs);

        for( int i=0; i<l; ++i )
        {
            c.v[i]=v[i]+b.v[i]+carry;
            carry=0;
            if(c.v[i]>=base)
            {
                c.v[i]-=base;
                carry=1;
            }
        }
        while(l<vs) { c.v[l]=v[l]+carry; carry=0; if(c.v[l]>=base) { c.v[l]-=base; carry=1;} l++; }
        while(l<bs) { c.v[l]=b.v[l]+carry; carry=0; if(c.v[l]>=base) { c.v[l]-=base; carry=1;} l++; }
        if(carry) c.v.push_back(carry);
        return c;
    }

    friend bigInt abs(const bigInt& b)  //absolute value function
    {
        bigInt c=b;
        c.sign=0;
        return c;
    }


	//trim leading zeroes that might have resulted e.g. through subtraction or multiplication

    void trim() 
    {
        int i=v.size()-1;
        while((v[i]==0) && (i>0) ) i--;
        v.resize(i+1);
    }

    bigInt operator-(const bigInt& b) const
    {
        bigInt c;
        if(sign!=b.sign) return *this+(-b);
        int l=min(v.size(),b.v.size());
        if(abs(*this)>abs(b))
        {
            c.sign=sign;
            int carry=0;
            for( int i=0; i<l;++i)
            {
                c.v.push_back(v[i]-b.v[i]-carry);
                carry=0;
                if(c.v[i]<0) { c.v[i]+=base; carry=1; }
            }
            while(l<v.size()){ c.v.push_back(v[l]-carry); carry=0; if(c.v[l]<0) { c.v[l]+=base; carry=1; } l++; }
            c.trim();
            return c;
        }
        else if(abs(b)>abs(*this))
        {
            c=-(b-*this);
            return c;
        }
        else
        {
            c.v.push_back(0);
            c.sign=0;
            return c;
        }

    }

    bigInt operator*(int x) const
    {
        bigInt c;
        c.sign=x<0 ? sign^1 : sign ;
        x=abs(x);
        c.v.resize(v.size());
        int carry=0;
        long long ans;
        for(int i=0; i<v.size(); ++i )
        {
            ans=(long long)v[i]*x+carry;
            carry=0;
            if(ans>=base)
            {
                carry=ans/base;
                ans%=base;
            }
            c.v[i]=ans;
        }
        if(carry) c.v.push_back(carry);
        c.trim();
        return c;
    }


/*shift the number left appending 'n' BLOCKS( 9 digits maake a block ) of zeroes at the end of bigint,
 equivalent to multplying by 1000000000(=base)^n */
 
    void shiftl(int n)
    {
        if(n<=0) return ;
        v.resize(v.size()+n);
        int i=v.size()-1, k=v.size()-n-1;
        while(k>=0) v[i--]=v[k--];
        while(i>=0) v[i--]=0;
    }

/* trail the big Int with 'n' zeroes,
equivalent to multiplying by 10^n */

    void trailz(int n)
    {
        if(n==0) return ;
        int m=n/9;
        int r=n-m*9;
        if(r) *this=(*this)*(int)pow(10,r);
        if(m) this->shiftl(m);
    }

    friend bigInt mult(const bigInt&a, const bigInt& b, int ai, int aj, int bi, int bj)
    {
       // cout<<"a = "<<a<<" b = "<<b;
        bigInt c; //cout<<" prod of "<<ai<<" "<<aj<<endl;
        c.sign=0;
        int sb=bj-bi+1, sa=aj-ai+1;

        if(sa<sb) return mult(b,a,bi,bj,ai,aj);
            int carry=0;
            long long prod;
            c.v.resize(sa+sb,0);
            int i,j;
            for(i=0; i<sb; ++i)
            {
                for(j=0; j<sa; ++j)
                {
                    prod=(long long)b.v[bi+i]*a.v[ai+j]+carry;
                    carry=0;
                    if(prod>=base)
                    {
                        carry=prod/base;
                        prod%=base;
                    }
                        c.v[i+j]+=prod;
                        if(c.v[i+j]>=base){ c.v[i+j]-=base; carry+=1; }
                }
                if(carry) c.v[i+j]+=carry;
                carry=0;
            }
            c.trim(); //cout<<" prod : "<<c;
            return c;
    }

    friend bigInt karatsuba(const bigInt& P,const bigInt& Q, int pi, int pj)
    {

        int sm=pj-pi+1;
        int hp=sm>>1;
        if(sm<=optimal) return mult(P,Q,pi,pj,pi,pj);

        bigInt PS,QS,P1,P2,P0,D;
        PS.v.resize(hp), QS.v.resize(hp);
        int carry,k,i,j;
        PS.sign=0,QS.sign=0, P0.sign=0, P1.sign=0, P2.sign=0, D.sign=0;


        carry=0,k=0;
        int ans;
        for( i=pi,j=0; j<hp; ++i,++j)
        {
            ans=P.v[i]+P.v[i+hp]+carry;
            carry=0;
            if(ans>=base)
            {
                ans-=base;
                carry=1;
            }
            PS.v[k]=ans;
            k++;
        }
        if(sm&1) PS.v.push_back(P.v[pj]+carry);
        else if(carry) PS.v.push_back(carry);

        carry=0; k=0;
        for( i=pi,j=0; j<hp; ++i,++j)
        {
            ans=(long long)Q.v[i]+Q.v[i+hp]+carry;
            carry=0;
            if(ans>=base)
            {
                ans-=base;
                carry=1;
            }
            QS.v[k]=ans;
            k++;
        }
        if(sm&1) QS.v.push_back(Q.v[pj]+carry);
        else if(carry) QS.v.push_back(carry);
        /* equate sizes of PS and QS for Karatsuba algo to work! */
        if(PS.v.size()<QS.v.size()) PS.v.push_back(0);
        if(QS.v.size()<PS.v.size()) QS.v.push_back(0);
        P2=karatsuba(PS,QS,0,PS.v.size()-1);
        P0=karatsuba(P,Q,pi,pi+hp-1);
        P1=karatsuba(P,Q,pi+hp,pj);
        D=P2-P1-P0;
        D.shiftl(hp), P1.shiftl(hp<<1);
        D=P1+D+P0; D.trim(); // cout<<" for "<<pi<<" "<<pj<<"  karatsuba of "<<P<<Q<<" returns  "<<D;
        return D;

    }

/* power ( exponentiation ) function for big Int */
    friend bigInt operator^(bigInt b, int e)
    {
        bigInt r("1");
        while(e>0)
        {
            if(e&1) r=r*b;
            b=b*b;
            e>>=1;
        }
        return r;
    }

    bigInt operator*(bigInt b)
    {
        int l=max(v.size(),b.v.size());
        this->padl(l-v.size());
        b.padl(l-b.v.size());
        bigInt c=karatsuba(*this,b,0,v.size()-1);
        this->trim();
        b.trim();
        c.trim();
        c.sign=sign^b.sign;
        return c;
    }

// add 'n' blocks of leading zeroes to bigInt used in Karatsuba multiplication
    void padl(int n)
    {
        if(n==0) return ;
        v.resize(v.size()+n);
        for( int i=v.size()-1,j=0; j<n; ++j,--i) v[i]=0;
    }

    friend ostream& operator<<(ostream& os, const bigInt& b)
    {
        int k=0;
        if(b.sign) os<<'-';
        if(b.v.size()) { os<<b.v[b.v.size()-1]<<' '; ++k; }
        for(int i=b.v.size()-2; i>=0; --i) { os<<setfill('0')<<setw(9)<<b.v[i]<<' '; ++k ; if( !(k^10) ) { os<<'\n'; k^=10; } }
        return os;
    }
};


int main()
{
    ios_base::sync_with_stdio(false);
    string s;
    //freopen("output","w+",stdout);
    cin>>s;  //string with |s|<4096
    int x;
    cin>>x;
    bigInt A(s);
    bigInt r=A^x;
    cout<<r;

    return 0;
}
