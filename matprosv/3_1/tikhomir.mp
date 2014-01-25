input geometry
pickup pencircle scaled semibold;

beginfig(1);
path p[];

z0=(-mv,0);
z1=(mv,0);
z2=(0,2mv);
for i =0 upto 2 :
z[i+3]=z[i] shifted (3mv,0);
z[i+6]=z[i] shifted (-3mv,0);
endfor

p1=z6--z7--z8--cycle;
p2=z3--z4--z5--cycle;
pickup pencircle scaled bold;
draw p1; draw p2;
pickup pencircle scaled extrabold;
dotlabel.lft(btex $A$ etex,z6);
dotlabel.top(btex $B$ etex,z8);
dotlabel.rt(btex $C$ etex,z7);
dotlabel.lft(btex $C$ etex,z3);
dotlabel.top(btex $B$ etex,z5);
dotlabel.rt(btex $A$ etex,z4);
drawarrow (-0.7mv,mv)--(0.7mv,mv);
endfig;

beginfig(2);
z1=(0,1.2mv);
z2=z1 rotated -120;
z3=1.5z2 rotated -120;
z4=1.2[z3,z1];
z5=1.2[z1,z2];
z6=1.2[z2,z3];
pickup pencircle scaled extrabold;
dotlabel.ulft(btex $A$ etex,z1);
dotlabel.urt(btex $B$ etex,z2);
dotlabel.lrt(btex $C$ etex,z3);
pickup pencircle scaled bold;
draw z1--z5;
draw z3--z4;
draw z2--z6;
pickup pencircle scaled semibold;
marks_angle(z2,z1,z4,1);
marks_angle(z3,z2,z5,2);
marks_angle(z1,z3,z6,3);
pair ss;
ss=(4*xpart z5,0);
z7=.5(z4-z3)+ss;
z8=.5(z5-z1)+ss;
z9=.5(z6-z2)+ss;
pickup pencircle scaled bold;
draw ss--z7;
draw ss--z8;
draw ss--z9;
pickup pencircle scaled semibold;
marks_angle(z8,ss,z7,1);
marks_angle(z9,ss,z8,2);
marks_angle(z7,ss,z9,3);
endfig;

beginfig(3);
path p[];
save mv;
mv:=1.3cm;
circum(p0,(0,0),1.5mv)

z0=(0,0);
z1= point 0 of p0;
z2= point 2 of p0;
z3= point 3.14159 of p0;

pickup pencircle scaled semibold;
draw p0;
draw z1--z2--z3--cycle;
draw z0--z3;
pickup pencircle scaled extrabold;
dotlabel.lft(btex $A$ etex,z2);
dotlabel.rt(btex $B$ etex,z1);
dotlabel.lrt(btex $C$ etex,z3);
dotlabel.top(btex $ O$ etex,z0);
pickup pencircle scaled semibold;
marks_angle(z3,z2,z1,1);
marks_angle(z0,z3,z2,1);
marks_angle(z0,z1,z3,2);
marks_angle(z1,z3,z0,2);

endfig;

beginfig(4);
path p[];
save mv;
mv:=1.5cm;

circum(p0,(0,0),1.5mv)

z0=(0,0);
z1= point 0 of p0;
z2= point 1.41 of p0;
z3= point 2.22 of p0;
z4= point 3.41 of p0;

pickup pencircle scaled semibold;
draw p0;
draw z1--z2--z3--z0--cycle;
draw z2--z4;
pickup pencircle scaled extrabold;
dotlabel.llft(btex $A$ etex,z3);
dotlabel.rt(btex $B$ etex,z1);
dotlabel.ulft(btex $C$ etex,z2);
dotlabel.urt(btex $ O$ etex,z0);
dotlabel.lrt(btex $D$ etex,z4);
pickup pencircle scaled semibold;
marks_angle(z3,z2,z0,1);
marks_angle(z0,z3,z2,1);
marks_angle(z0,z2,z1,2);
marks_angle(z2,z1,z0,2);
mark_angle(z3,z0,z4,1);
mark_angle(z4,z0,z1,2);


endfig;

beginfig(6);
pickup pencircle scaled semibold;
  z.A=(-3mv,0);
  z.C=(0,-mv);
  z1 =z.A rotatedaround (z.C,-150);
  z.M= whatever[z.C,z1];
  ypart z.M =0;
  (z.B-.5[z.A,z.C])dotprod(z.A-z.C)=0;
  z2= z.M rotatedaround (z.A,30);
  z.B=whatever[z.A,z2];
  z.b=z.B rotatedaround(z.A,-60);

draw z.C--z.A--z.M--z.b--z.A--z.B--z.M;
pickup pencircle scaled light;
draw z.B--z.b dashed evenly;
pickup pencircle scaled extrabold;
dotlabel.lft(btex $A$ etex,z.A);
dotlabel.top(btex $B$ etex,z.B);
dotlabel.lrt(btex $C$ etex,z.C);
dotlabel.rt(btex $M$ etex,z.M);
dotlabel.bot(btex $B'$ etex,z.b);
endfig;

beginfig(7);
save mv;
mv:=0.707cm;

begingroup
z.A=(0,0);
z.B=(3mv,0);
z.B1=(3mv,3mv);
z.A1=(0,3mv);
z.O=(1.5mv,1.5mv);
z.C=(mv,-0.707mv);
z.N=.5[z.C,z.B];
z.M=.5[z.A,z.C];

pickup pencircle scaled semibold;
draw z.A--z.B--z.B1--z.A1--cycle;
draw z.A--z.C--z.B;
pickup pencircle scaled light;
draw z.A--z.B1;
draw z.B--z.A1;
draw z.M--z.O--z.N;
draw z.A1--z.C--z.B1;
pickup pencircle scaled extrabold;
dotlabel.lft(btex $A$ etex,z.A);
dotlabel.lft(btex $A'$ etex,z.A1);
dotlabel.rt(btex $B$ etex,z.B);
dotlabel.rt(btex $B'$ etex,z.B1);
dotlabel.lrt(btex $C$ etex,z.C);
dotlabel.llft(btex $M$ etex,z.M);
dotlabel.lrt(btex $N$ etex,z.N);
dotlabel.top(btex $O$ etex,z.O);
endgroup ;

picture a;
a = currentpicture;
undraw currentpicture;

begingroup
z.A=(0,0);
z.B=(3mv,0);
z.B1=(3mv,3mv);
z.A1=(0,3mv);
z.O=(1.5mv,1.5mv);
z.C=(mv,-0.707mv);
z.N=.5[z.C,z.B];
z.M=.5[z.A,z.C];
z.B2=z.C rotatedaround(z.B,90);
z.C2=z.B rotatedaround(z.C,-90);
z.A3=z.C rotatedaround(z.A,-90);
z.C3=z.A rotatedaround(z.C,90);

pickup pencircle scaled semibold;
draw z.A--z.B--z.B1--z.A1--cycle;
draw z.A--z.C--z.B;
pickup pencircle scaled light;
draw z.C--z.B1;
draw z.A--z.B2;
draw z.B--z.B2--z.C2--z.C--z.C3--z.A3--z.A;
pickup pencircle scaled extrabold;
dotlabel.ulft(btex $A$ etex,z.A);
dotlabel.lft(btex $A'$ etex,z.A1);
dotlabel.lft(btex $A'''$ etex,z.A3);
dotlabel.urt(btex $B$ etex,z.B);
dotlabel.rt(btex $B'$ etex,z.B1);
dotlabel.rt(btex $B''$ etex,z.B2);
dotlabel.bot("",z.C);
label.bot(btex $C$ etex,z.C+(-1mm,-3mm));
dotlabel.bot(btex $C''$ etex,z.C2);
dotlabel.bot(btex $C'''$ etex,z.C3);
angle_radius:=12pt;
marks_angle(z.A,z.B,z.B2,1);
angle_radius:=8pt;
marks_angle(z.B1,z.B,z.C,1);
endgroup  ;
picture b;
b = currentpicture;
undraw currentpicture;



begingroup
      path p[];

z0=(0,0);
z.B=(3mv,0);
z1=(3mv,3mv);
z.CC=(0,3mv);
circum(p0,z.CC,mv);
z.AA = point 1.5 of p0;
z2=point 1 of p0;
z3=(-mv,4mv);

pickup pencircle scaled semibold;
draw z0--z.B--z1--z.CC--cycle;
draw z3--z.B;

pickup pencircle scaled light;
drawarrow subpath (0,0.5) of p0;
draw subpath (0.4,3) of p0;

pickup pencircle scaled semibold;
draw z2--z.B dashed evenly;
dotlabel.rt(btex $B$ etex,z.B);
dotlabel.top(btex $A$ etex,z.AA);
dotlabel.llft(btex $C$ etex,z.CC);

endgroup   ;

picture c;
c = currentpicture;
undraw currentpicture;

draw a;
draw b shifted (6mv,0);
draw c shifted (12mv,0);
endfig;


beginfig(8);
%mv:=1.2cm;
%save mv;
%mv:=0.707cm;

begingroup
z.A=(0,0);
z.D=(4mv,0);
z.C=(4mv,4mv);
z.B=(0,4mv);
z.a=z.C-(0.95mv,0);
z.c=z.A+(0.95mv,0);
z.b=z.B-(0,0.95mv);
z.d=z.D+(0,0.95mv);

pickup pencircle scaled semibold;
draw z.A--z.B--z.C--z.D--cycle;
draw z.a--z.b--z.c--z.d--cycle;
pickup pencircle scaled extrabold;
dotlabel.lft(btex $A$ etex,z.A);
dotlabel.top(btex $A'$ etex,z.a);
dotlabel.lft(btex $B$ etex,z.B);
dotlabel.lft(btex $B'$ etex,z.b);
dotlabel.rt(btex $C$ etex,z.C);
dotlabel.bot(btex $C'$ etex,z.c);
dotlabel.rt(btex $D$ etex,z.D);
dotlabel.rt(btex $D'$ etex,z.d);
label.rt(btex $a$ etex,.5[z.d,z.D]);
label.rt(btex $b$ etex,.5[z.d,z.C]);
label.top(btex $a$ etex,.5[z.a,z.C]);
label.top(btex $b$ etex,.5[z.a,z.B]);
label.bot(btex $a$ etex,.5[z.A,z.c]);
label.bot(btex $b$ etex,.5[z.c,z.D]);
label.lft(btex $a$ etex,.5[z.B,z.b]);
label.lft(btex $b$ etex,.5[z.b,z.A]);
endgroup ;

picture a;
a = currentpicture;
undraw currentpicture;

begingroup
z.A=(0,0);
z.D=(4mv,0);
z.C=(4mv,4mv);
z.B=(0,4mv);
z.a=z.C-(0.95mv,0);
z.c=z.A+(0.95mv,0);
z.b=z.B-(0,0.95mv);
z.d=z.D+(0,0.95mv);
z1=z.D-(0.95mv,0);
z2=z.C-(0,0.95mv);


pickup pencircle scaled semibold;
draw z.A--z.B--z.C--z.D--cycle;
draw z.b--z2;
draw z.a--z1;
pickup pencircle scaled extrabold;
label.rt(btex $a$ etex,.5[z2,z.C]);
label.lft(btex $b$ etex,.5[z.A,z.b]);
endgroup ;

picture c;
c = currentpicture;
undraw currentpicture;

draw a;
draw c shifted (7mv,0);
endfig;


beginfig(11);
  z.A=(-3mv,0);
  z.C=(0,-mv);
  z1 =z.A rotatedaround (z.C,-150);
  z.M= whatever[z.C,z1];
  ypart z.M =0;
  (z.B-.5[z.A,z.C])dotprod(z.A-z.C)=0;
  z2= z.M rotatedaround (z.A,30);
  z.B=whatever[z.A,z2];
  z.b=z.B rotatedaround(z.A,-60);

pickup pencircle scaled semibold;
draw z.C--z.A--z.M--z.B--z.C;
draw z.B--z.A;
draw z.M--z.C;
pickup pencircle scaled extrabold;
dotlabel.lft(btex $A$ etex,z.A);
dotlabel.top(btex $B$ etex,z.B);
dotlabel.lrt(btex $C$ etex,z.C);
dotlabel.rt(btex $M$ etex,z.M);
label.ulft(btex $ a$ etex,.5[z.A,z.B]);
label.urt(btex $ b$ etex,.5[z.M,z.B]);
angle_radius:=0.5mv;
marks_angle(z.M,z.A,z.B,1);
z5=text_point(z.M,z.A,z.B,0.2);
angle_radius:=1.1mv;
marks_angle(z.B,z.M,z.A,1);
z4=text_point(z.B,z.M,z.A,0.1);
angle_radius:=1.5mv;
marks_angle(z.A,z.M,z.C,1);
z3=text_point(z.A,z.M,z.C,0.15);
label.rt (btex $ 30^\circ$ etex rotated 15, z5+(-1mm,1mm));
label.lft(btex $ \varphi$ etex, z4);
label.lft(btex $ \psi$ etex, z3);
endfig;

angle_radius:=8pt;

beginfig(12);
save mv;
mv:=45cm/40;
%mv:=0.707cm;
z.A=(0,0);
z.B=(3mv,0);
z.B1=(3mv,3mv);
z.A1=(0,3mv);
z.O=(1.5mv,1.5mv);
z.C=(mv,-0.707mv);
z.N=.5[z.C,z.B];
z.M=.5[z.A,z.C];

pickup pencircle scaled semibold;
draw z.A--z.B--z.B1--z.A1--cycle;
draw z.A--z.C--z.B;
draw z.C--z.B1;
pickup pencircle scaled extrabold;
dotlabel.lft(btex $A$ etex,z.A);
dotlabel.lft(btex $A'$ etex,z.A1);
dotlabel.rt(btex $B$ etex,z.B);
dotlabel.rt(btex $B'$ etex,z.B1);
dotlabel.lrt(btex $C$ etex,z.C);

label.lrt(btex $ a$ etex,.5[z.C,z.B]);
label.lft(btex $ c$ etex,.5[z.B1,z.B]);
label.llft(btex $ b$ etex,.5[z.A,z.C]);
marks_angle(z.B,z.C,z.A,1);
marks_angle(z.A,z.B,z.C,1);
z5=text_point(z.B,z.C,z.A,0.8);
z4=text_point(z.A,z.B,z.C,1.5);
label.lft (btex $ \varphi$ etex, z5);
label     (btex $\chi$ etex, z4);

endfig;


end
