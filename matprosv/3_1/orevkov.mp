input knots
input geometry
pickup pencircle scaled semibold;
sthick:=4mm;
gap:=2mm;

beginfig(1);
path p[];

pickup pencircle scaled bold;
p0 = (-2.5cm,-1.5cm){dir 75}..(0,cm){right}.. (2.5cm,-1.5cm){dir -75};
p1 = p0 yscaled -1;

for i = -1 upto 1:
  drawarrow (point 1+0.85*i of p0)--(point 1+0.85*i of p0 shifted (0,-.5cm));
  drawarrow (point 1+0.85*i of p1)--(point 1+0.85*i of p1 shifted (0, .5cm));
endfor

thickdraw (subpath (0,1) of p1, 1.0,bold);
thickdraw (subpath (1,2) of p0, 1.0,bold);
underwhite(p0,p1,(1,2),1.0,bold);
underwhite(p1,p0,(0,1),1.0,bold);

z0=(7cm,0);
for i = 1 upto 3:
z[i]-z0=(2.5cm,0) rotated (-140+120*i);
z[i+3]-z0=(2.5cm,0) rotated (-160+120*i);
endfor

p2=z6--z1;
p3=z4--z2;
p4=z5--z3;
z7=.5cm*unitvector((z6-z1) rotated -90);
z8=.5cm*unitvector((z4-z2) rotated -90);
z9=.5cm*unitvector((z5-z3) rotated -90);
drawarrow .05[z1,z6]--.05[z1,z6]+z7;
drawarrow .95[z1,z6]--.95[z1,z6]+z7;
drawarrow .05[z2,z4]--.05[z2,z4]+z8;
drawarrow .95[z2,z4]--.95[z2,z4]+z8;
drawarrow .05[z3,z5]--.05[z3,z5]+z9;
drawarrow .95[z3,z5]--.95[z3,z5]+z9;

thickdraw(subpath (0,0.5) of p2,1.0,bold);
thickdraw(subpath (0.5,1) of p3,1.0,bold);
thickdraw(subpath (0.5,1) of p4,1.0,bold);
underwhite(p2,p3,(0,0.5),1.0,bold);
underwhite(p3,p4,(0,0.5),1.0,bold);
underwhite(p4,p2,(0.5,1),1.0,bold);


endfig;

beginfig(2);
path p[];
z0=(0,0);
z1=(10mv,0);
z2=z1 rotated 90 scaled .15;

drawarrow 1.2[z1,z0]--z1;
drawarrow 2[z2,z0]--z2;
label.urt(btex $\dot\varphi$ etex,z2);
label.bot(btex $\varphi$ etex,z1);

hc:= 1;
 for i=0 upto 2:
drawarrow ((0,-.6mv){left}..(-0.6mv,0){up}..(0,.6mv){right}) shifted (3.6*mv*i,0);
drawarrow ((0,-.6mv){left}..(-0.6mv,0){up}..(0,.6mv){right}) shifted (3.6*mv*i,0);
drawarrow ((0,.6mv){right}..(0.6mv,0){down}..(0,-.6mv){left}) shifted (3.6*mv*i,0);
drawarrow ((0,.6mv){right}..(0.6mv,0){down}..(0,-.6mv){left}) shifted (3.6*mv*i,0);
 endfor

 z3=unitvector(1.8/3.14159,0.6);
 z4 =z3 yscaled -1;
 z5 =-z3;
 z6=z5 yscaled -1;
 for i=0 upto 2:
drawarrow ((-1.8mv,0){z3}..(0,0.6*sqrt(2)*mv){right}) shifted (3.6*mv*i,0);
drawarrow ((0,0.6*sqrt(2)*mv){right}..( 1.8mv,0){z4}) shifted (3.6*mv*i,0);
drawarrow ((1.8mv,0){z5}..(0,-0.6*sqrt(2)*mv){left}) shifted (3.6*mv*i,0);
drawarrow ((0,-0.6*sqrt(2)*mv){left}..(-1.8mv,0){z6}) shifted (3.6*mv*i,0);
 endfor

for hc=3 upto 4:
 for i=0 upto 2:
drawarrow ((-1.8mv,0.6*sqrt(hc-2)*mv){right}..(0,0.6*sqrt(hc)*mv){right}) shifted (3.6*mv*i,0);
drawarrow ((0,0.6*sqrt(hc)*mv){right}..(1.8mv,0.6*sqrt(hc-2)*mv){right}) shifted (3.6*mv*i,0);
drawarrow ((1.8mv,-0.6*sqrt(hc-2)*mv){left}..(0,-0.6*sqrt(hc)*mv){left}) shifted (3.6*mv*i,0);
drawarrow ((0,-0.6*sqrt(hc)*mv){left}..(-1.8mv,-0.6*sqrt(hc-2)*mv){left}) shifted (3.6*mv*i,0);
 endfor
endfor


endfig;

beginfig(3);
path p[];
save mv;
mv:=20cm/25;


z0=(0,0);
z.A=(0,-2.5mv)rotated -15;
z.B=(0,-2.5mv)rotated  15;
z1=z.A yscaled -1;
z2=z.B yscaled -1;

p0=z.A{dir -30}..z.B{dir 30}..
   z0{dir135}..z1{dir30}..z2{dir-30}..
   z0{dir-135}..cycle;
p1=z.A{dir -30}..tension 2.3 and 1..z1{up}..
   z2{down}..tension 1 and 2.3..
   z.B{dir 30};
p2=z.A{dir -30}..(0,-0.5mv)..z1{up}..
   z2{down}..(0,-0.5mv)..
   z.B{dir 30};


   for i=0 upto 3:
      dotlabel.llft(btex$ A$etex,z.A+(3.5mv*i,0));
      dotlabel.lrt(btex$B$etex,z.B+(3.5mv*i,0));
   endfor;
   draw      subpath (1,6) of p0 dashed evenly;
    pickup pencircle scaled semibold;
   drawarrow subpath (0,1) of p0;

   drawarrow p1 shifted (3.5mv,0);
   drawarrow p2 shifted (7.0mv,0);

   drawarrow subpath (0,1) of p0 shifted(10.5mv,0);
   draw      subpath (1,6) of p0 shifted(10.5mv,0);



endfig;


end
