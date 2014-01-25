input geometry
pickup pencircle scaled semibold;

beginfig(5);

path p[];
numeric rd,ro;
z.A=(-2mv,0);
z.O=(mv,0);
circum(p0,z.O,mv);
tangent(z.A,z.O,mv)(z1,z2);
xpart z.B=-0.3mv;
z.B=whatever[z.A,z2];

(z.D-.5[z.A,z.B])dotprod(z.A-z.B)=0;
(z.D-.5[z.O,z.B])dotprod(z.O-z.B)=0;
rd:=distance(z.D,z.A);
circum(p1,z.D,rd);

tangent(z.B,z.O,mv)(z3,z4);

z.C=whatever[z.A,z1];
z.C=whatever[z.B,z3];
pickup pencircle scaled bold;
draw z.A--z.B--z.C--cycle;
pickup pencircle scaled semibold;
draw z.B--2.5[z.B,z4];
draw z.C--4[z.C,z1];
draw p0;
draw p1;
(z.E-.5[z.A,z.B])dotprod(z.A-z.B)=0;
(z.E-.5[z.C,z.B])dotprod(z.C-z.B)=0;
ro:=distance(z.E,z.A);
circum(p2,z.E,ro);
pickup pencircle scaled light;
draw p2 dashed evenly;

pickup pencircle scaled extrabold;
dotlabel.ulft(btex $A$ etex,z.A);
dotlabel.top(btex $B$ etex,z.B);
   label.bot(btex $C$ etex,1.1[z.B,z.C]);
dotlabel.bot("",z.C);
dotlabel.urt(btex $ O$ etex,z.O);
dotlabel.lrt(btex $D$ etex,z.D);

pickup pencircle scaled semibold;

endfig;


beginfig(9);
path p[];
numeric rd,ro;
z.O=(0,0);
circum(p0,z.O,2mv);
z.A=point 2.3 of p0;
z.C=point 3.8 of p0;
z.B=point 1   of p0;
z.B1=point   3 of p0;

pickup pencircle scaled semibold;
draw p0;
draw z.A--z.B--z.C--cycle;
draw z.A--z.B--z.B1--cycle;

marks_angle(z.B,z.B1,z.A,1);
marks_angle(z.B,z.C,z.A,1);
mark_rt_angle(z.B1,z.A,z.B);

pickup pencircle scaled extrabold;
dotlabel.llft(btex $A$ etex,z.A);
dotlabel.top(btex $B$ etex,z.B);
dotlabel.rt(btex $C$ etex,z.C);
dotlabel.rt(btex $ O$ etex,z.O);
dotlabel.bot(btex $B'$ etex,z.B1);

endfig;

beginfig(10);
path p[];
numeric rd,ro;
z.O=(0,0);
circum(p0,z.O,2mv);
z.A=point 2.3 of p0;
z.C=point 3.8 of p0;
z.B=point 1   of p0;
z.B1=point   3 of p0;

pickup pencircle scaled semibold;
draw p0;
draw z.A--z.B--z.C--cycle;

z.D=whatever[z.A,z.C];
(z.D-z.B)dotprod(z.A-z.C)=0;

draw z.B--z.D;

marks_angle(z.B,z.C,z.A,1);
mark_rt_angle(z.B,z.D,z.A);

%z1=z.C+2.5angle_radius*(unitvector(z.A-z.C)+unitvector(z.B-z.C));
z1=text_point(z.B,z.C,z.A,1.1);

pickup pencircle scaled extrabold;
dotlabel.llft(btex $A$ etex,z.A);
dotlabel.top(btex $B$ etex,z.B);
dotlabel.rt(btex $C$ etex,z.C);
dotlabel.bot(btex $ D$ etex,z.D);
   label(btex $\varphi$ etex,z1);
endfig;



end
