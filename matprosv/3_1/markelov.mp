input geometry
pickup pencircle scaled semibold;

beginfig(1);
path p[];
numeric r,R,u[];
mv:=0.8cm;
r:=0.5sqrt3*mv;
z.B=(-2mv,0);
z.C=(mv,0);
z.O=(0,r);
circum(p0,z.O,r);
tangent(z.B,z.O,r)(z1,z2);
tangent(z.C,z.O,r)(z3,z4);
z.A=whatever[z.B,z2];
z.A=whatever[z.C,z3];
circumscribe(p1,z5,R,z.A,z.B,z.C);

z6=unitvector(-z.A);
z7=unitvector( z.B);
z8=unitvector( z.C);
z10=unitvector(z5);

z9 =unitvector(z.B-z.A);
z11=unitvector(z.C-z.A);
z12=unitvector(z9+(1,0));
z13=unitvector(z11+(-1,0));
z14=whatever[z.B,z.B+ z12];
z14=whatever[z.C,z.C+ z13];
u1=-ypart z14;
z15=unitvector(z6+z7);
z16=unitvector(z6+z8);
u2=u1/(-ypart z15);
circum(p2,u2*z15,u1);
u3=u1/(-ypart z16);
circum(p3,u3*z16,u1);

pickup pencircle scaled bold;
draw z.A--z.B--z.C--cycle;

pickup pencircle scaled semibold;
draw p0;
draw p1;
draw p2 dashed evenly;
draw p3 dashed evenly;
z20=whatever[z.C,z.B];
xpart z20=xpart (u2*z15)-u1;
z21=whatever[z.C,z.B];
xpart z21=xpart (u3*z16)+u1;
z22=whatever[z.A,-z.A];
ypart z22 = -2u1;
draw z.B--z20;
draw z.C--z21;
draw z.A--z22;

dotlabel.ulft("",z.B);
label.ulft(btex $B$ etex,z.B+(-0.2mv,0));
dotlabel.urt("",z.B);
label.urt(btex $C$ etex,z.C+(0.2mv,0));
dotlabel.urt(btex $A$ etex,z.A);
dotlabel("",(0,0));
label.llft(btex $K$ etex,(-0.1mv,0));

endfig;


end
