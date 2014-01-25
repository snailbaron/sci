input geometry
pickup pencircle scaled semibold;

beginfig(1);
path p[];
z0=(0,0);

circum(p0,z0,3mv);
z.L =point 3.8 of p0;
z.M =point 0.9 of p0;
z.K =point 1.2 of p0;
z.N =point 2.7 of p0;
z.O = whatever[z.N,z.M];
z.O = whatever[z.K,z.L];
ypart z.A =ypart z.B =ypart z.O;
xpart z.B= 3mv+-+(ypart z.O);
xpart z.A=-xpart z.B;
z1=whatever[z.A,z.B];
z2=whatever[z.A,z.B];
z1=whatever[z.K,z.N];
z2=whatever[z.M,z.L];

p1=z.O--z.K--z.N--cycle;
p2=z.O--z.M--z.L--cycle;

fill p1 withcolor 0.8white;
fill p2 withcolor 0.8white;

pickup pencircle scaled bold;
draw p0;
draw z.N--z.M--z.L--z.K--z.N;
draw z.A--z.B;
dotlabel.bot(btex $O$ etex,z.O);
dotlabel.rt(btex $L$ etex,z.L);
dotlabel.urt(btex $B$ etex,z.B);
dotlabel.top(btex $M$ etex,z.M);
dotlabel.top(btex $K$ etex,z.K);
dotlabel.ulft(btex $A$ etex,z.A);
dotlabel.llft(btex $N$ etex,z.N);
dotlabel.ulft("",z1);
dotlabel.ulft("",z2);
draw_marked (z1--z.O,1);
draw_marked (z2--z.O,1);


endfig;

beginfig(2);
pickup pencircle scaled bold;
z.A=(0,0);
z.D=(2.5mv,0);
z1=(5.5mv,0);
z2=(2.2mv,4.5mv);
z.B=.7[z.A,z2];
z.C=whatever[z.B,z1];
z.C=whatever[z.D,z2];
draw z.A--z1--z.B;
draw z.A--z.D--z2--cycle;
draw z.A--z.C;
draw z.B--z.D;
dotlabel.bot(btex $A$ etex,z.A);
dotlabel.bot(btex $D$ etex,z.D);
dotlabel.urt(btex $C$ etex,z.C);
dotlabel.ulft(btex $B$ etex,z.B);
dotlabel.ulft("",z1);
dotlabel.ulft("",z2);
dotlabel.ulft("",.5[z.A,z.D]);
dotlabel.ulft("",.5[z.A,z.C]);
dotlabel.ulft("",.5[z.A,z.B]);
dotlabel.ulft("",.5[z.C,z.B]);
dotlabel.ulft("",.5[z.D,z.B]);
dotlabel.ulft("",.5[z.D,z.C]);


endfig;

end
