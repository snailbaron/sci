input geometry
pickup pencircle scaled semibold;

beginfig(13);
path p[];
z0=(0,0);
z1=(2.5mv,0);
z2=z1 rotated 90;
z3=z1 rotated 30;
z4=.5z3 rotated 10;

p0=z0..z4{dir 30}..z3;
pickup pencircle scaled light;
draw subpath (0,0.5) of p0;
draw subpath (1.5,2) of p0;
pickup pencircle scaled semibold;
draw z0--z3;
drawarrow 1.2[z1,z0]--z1;
drawarrow 1.2[z2,z0]--z2;
pickup pencircle scaled extrabold;
dotlabel.llft(btex $O$ etex,z0);
label.lft(btex $y$ etex,z2);
label.bot(btex $x$ etex,z1);
label.rt(btex $z$ etex,z3);
label(btex $r=|z|$ etex rotated 30, z4);
marks_angle(z1,z0,z3,1);
z5=text_point(z1,z0,z3,1);
label.rt(btex $ \varphi = \hbox{arg } z$ etex rotated 15,z5+(0,1.5mm));
endfig;

beginfig(15);
path p[];
save mv;
mv:=39cm/60;

z.a=(-2mv,  0);
z.b=(   0,2mv);
z.c=(1.5mv,1.5mv);
circumscribe(p0,z0,r,z.a,z.b,z.c);
pickup pencircle scaled semibold;
draw p0;
z.d=point 3.6 of p0;
z1=whatever[z.a,z.b];
z1=whatever[z.c,z.d];
draw z.a--z.b--1.1[z.b,z1];
draw z.c--z.d--1.1[z.c,z1];
pickup pencircle scaled extrabold;
dotlabel.lrt(btex $a$ etex,z.a);
dotlabel.lrt(btex $b$ etex,z.b);
dotlabel.urt(btex $c$ etex,z.c);
dotlabel.rt(btex $d$ etex,z.d);
dotlabel.rt(btex $e$ etex,z1);
endfig;

beginfig(14);
path p[];
save mv;
mv:=39cm/60;

z.a=(-2mv,  0);
z.b=(   0,2mv);
z.c=(1.5mv,1.5mv);
save r;
circumscribe(p0,z0,r,z.a,z.b,z.c);
pickup pencircle scaled semibold;
draw p0;
z.d=point 3.6 of p0;
z1=whatever[z.a,z.b];
z1=whatever[z.c,z.d];
tangent(z1,z0,r)(z2,z3);
draw 1.6[z1,z2]--z2;
draw 1.6[z1,z3]--z3;

pickup pencircle scaled bold;
label.rt(btex $z$ etex,z1);
dotlabel.ulft(btex $\zeta$ etex,z2);
dotlabel.rt(btex $\zeta'$ etex,z3);
draw z2--z1;
draw z3--z1;
endfig;


beginfig(16);
path p[];
save mv;
mv:=0.6667cm;
z0=(0,0);
circum(p0,z0,1.5mv);

z5=(0.8mv,-1.5mv);
tangent(z5,z0,1.5mv)(z1,z4);
z6=2.3[z5,z1];
tangent(z6,z0,1.5mv)(z2,z100);
z7=2.25[z6,z2];
tangent(z7,z0,1.5mv)(z3,z200);
z8= whatever[z7,z3];
z8= whatever[z4,z5];

pickup pencircle scaled bold;
draw p0;
pickup pencircle scaled semibold;
draw z5--z6--z7--z8--cycle;
draw z0--z4;
draw z0--z3;
draw z0--z2;
draw z0--z1;
label.rt(btex $\zeta_1$ etex,z5);
label.rt(btex $\zeta_2$ etex,z6);
label.top(btex $\zeta_3$ etex,z7);
label.lft(btex $\zeta_4$ etex,z8);
label.lrt(btex $z_1$ etex,z1);
label.urt(btex $z_2$ etex,z2);
label.ulft(btex $z_3$ etex,z3);
label.bot(btex $z_4$ etex,z4);

marks_angle(z1,z0,z2,1);
marks_angle(z2,z0,z3,2);
marks_angle(z3,z0,z4,3);

z10=text_point(z1,z0,z2,0.5);
z11=text_point(z2,z0,z3,0.5);
z12=text_point(z3,z0,z4,0.5);

label.rt(btex $ \varphi_1$ etex,z10);
label.top(btex $ \varphi_2$ etex,z11);
label.lft(btex $ \varphi_3$ etex,z12);

endfig;

beginfig(17);
save mv;
mv:=7cm/8;

path p[];
z.c=(0,0);
z.a=(4.8mv,0) rotated 40;
z.b=(4.6mv,0);
save r;
circumscribe(p0,z2,r,z.a,z.b,z.c);
(z1-z.b)dotprod(z.c-z.a)=0;
(z1-z.c)dotprod(z.b-z.a)=0;
z3=whatever[z.a,.5[z.b,z.c]];
z3=whatever[z.c,.5[z.b,z.a]];
pickup pencircle scaled bold;
draw z.a--z.b--z.c--cycle;
pickup pencircle scaled semibold;
draw z1--z2--z3;
pickup pencircle scaled light;
draw z.a--.5[z.b,z.c];
draw z.b--.5[z.a,z.c];
z4=whatever[z.c,z1];
z4=whatever[z.a,z.b];
z5=whatever[z.b,z1];
z5=whatever[z.a,z.c];
draw z4--z.c;
draw z5--z.b;

pickup pencircle scaled extrabold;
dotlabel.lft(btex $C=0$ etex,z.c);
dotlabel.rt(btex $B=a$ etex,z.b);
dotlabel.urt(btex $A=b e^{i\varphi}$ etex,z.a);
dotlabel.lft(btex $\zeta_2$ etex,z2);
dotlabel.ulft("",z3);
label.ulft(btex $\zeta_3$ etex,z3+(2mm,2mm));
dotlabel.top(btex  $\zeta_1$ etex,z1);

endfig;

beginfig(18);
path p[];
save mv;
mv:=52cm/56;

z.a=(0,0);
z.b=(5.6mv,0) rotated 40;
z.c=(5.2mv,0);
save r;
circumscribe(p0,z.O,r,z.a,z.b,z.c);
z.d= point 0.1 of p0;

(z1-z.d)dotprod(z.b-z.c)=0;
z1=whatever[z.b,.z.c];
(z2-z.d)dotprod(z.a-z.c)=0;
z2=whatever[z.a,z.c];
(z3-z.d)dotprod(z.b-z.a)=0;
z3=whatever[z.b,.z.a];

pickup pencircle scaled bold;

draw p0;
draw z.a--z.b--z.c--cycle;
pickup pencircle scaled semibold;
draw z.a--z3;
draw z.d--z1;
draw z.d--z2;
draw z.d--z3;
dotlabel.lft(btex $a$ etex,z.a);
%dotlabel.urt(btex $A$ etex,z.a);
dotlabel.rt(btex $d$ etex,z.d);
dotlabel.top(btex $b$ etex,z.b);
%dotlabel.bot(btex $B$ etex,z.b);
dotlabel.lrt(btex $c$ etex,z.c);
%dotlabel.ulft(btex $C$ etex,z.c);
label.lft(btex $a_0$ etex,z1);
label.ulft(btex $c_0$ etex,z3);
label.urt(btex  $b_0$ etex,z2);

pickup pencircle scaled light;
draw 1.2[z1,z2]--1.4[z1,z3] dashed evenly;
draw z.c--1.4[z.c,z2];

endfig;

beginfig(19);
save mv;
mv:=23cm/30;

z.A=(0,0);
z.B=(3.2mv,0)rotated 15;
z.C=(3.3mv,0)rotated -25;
z.D=(3mv,0)rotated -60;
z.E =whatever[z.A,z.B];
z.E =whatever[z.C,z.D];
z.F =whatever[z.C,z.B];
z.F =whatever[z.A,z.D];
z.M=.5[z.A,z.C];
z.N=.5[z.B,z.D];
z.O=.5[z.E,z.F];

pickup pencircle scaled semibold;
draw z.A--z.E--z.D--cycle;
draw z.A--z.F--z.B--cycle;
draw z.A--z.C;
draw z.E--z.F;
draw z.B--z.D;
pickup pencircle scaled light;
draw z.M--z.N--z.O dashed evenly;

pickup pencircle scaled extrabold;
dotlabel.lft(btex $A$ etex,z.A);
dotlabel.top(btex $B$ etex,z.B);
dotlabel.top(btex $E$ etex,z.E);
dotlabel.top(btex $M$ etex,z.M);
dotlabel.ulft(btex $N$ etex,z.N);
dotlabel.lrt(btex $C$ etex,z.C);
dotlabel.lrt(btex $O$ etex,z.O);
dotlabel.llft(btex $D$ etex,z.D);
dotlabel.llft(btex $F$ etex,z.F);


endfig;

beginfig(20);
path p[];

z11=(0,0);
z12=(mv,0);
z16=(0.1mv,-2mv);
save r;
circumscribe(p0,z0,r,z11,z12,z16);
z13= point 0.2 of p0;
z14= point 3.5 of p0;
z15= point 3.1 of p0;

for i=1 upto 6:
  z[i] = z[i+10] rotatedaround (z0,-54);
endfor

z.a= whatever[z1,z2];
z.a= whatever[z4,z5];
z.b= whatever[z2,z3];
z.b= whatever[z6,z5];
z.c= whatever[z4,z3];
z.c= whatever[z6,z1];





pickup pencircle scaled semibold;
draw p0;
draw z1--z2--z3--z4--z5--z6--cycle;
draw z.a--z1;
draw z.a--z5;
draw z.b--z2;
draw z.b--z6;
draw z.c--z3;
draw z.c--z1;
pickup pencircle scaled light;
draw z.a--z.b--z.c dashed evenly;
endfig;


end
