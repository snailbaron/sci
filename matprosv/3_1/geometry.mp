%27/01/97 Вялый: определения констант размерности длины
marksize:=4pt;
angle_radius:=8pt;
numeric pen_size;
light=0.8pt;
semibold=1.2pt;
bold=1.6pt;
extrabold=3.2pt;
mv:=1cm;

%27/01/97 Вялый: определения пометок на линиях и углах
%27/01/97 Вялый: macros from MP manual

def draw_mark(expr p,a) =
    begingroup
        save t,dm; pair dm;
pickup pencircle scaled light;
        pen_size:=light;
        t = arctime a of p;
        dm = marksize*unitvector direction t of p rotated 90;
        draw (-.5dm.. .5dm) shifted point t of p;
    endgroup
enddef;

def draw_marked (expr p,n) =
        begingroup
         save amid;
pickup pencircle scaled light;
         amid=.5*arclength p;
         for i=-(n-1)/2 upto (n-1)/2:
          draw_mark(p, amid+.6marksize*i);
         endfor
         draw p;
        endgroup
enddef;

def mark_angle(expr a,b,c,n) =
 begingroup
  save p,s; path p;
pickup pencircle scaled light;
  p = unitvector(a-b){(a-b)rotated 90}..unitvector(c-b);
  s = .9marksize/length(point 1 of p - point 0 of p);
  if s < angle_radius: s:=angle_radius; fi
  draw_marked(p scaled s shifted b, n);
 endgroup
enddef;

def mark_rt_angle(expr a,b,c) =
 begingroup
pickup pencircle scaled light;
 draw ((1,0)--(1,1)--(0,1)) zscaled
 (angle_radius*unitvector(a-b)) shifted b
 endgroup
enddef;

%27/01/97 Вялый: new macro
% отмечает угол несколькими дугами
def marks_angle(expr a,b,c,n) =
 begingroup
  save p,s; path p;
pickup pencircle scaled light;
  p = unitvector(a-b){(a-b)rotated 90}..unitvector(c-b);
  s = .9marksize/length(point 1 of p - point 0 of p);
  if s < angle_radius: s:=angle_radius; fi
         for i=-(n-1)/2 upto (n-1)/2:
          draw p scaled (s+.5marksize*i) shifted b;
         endfor
 endgroup
enddef;


%30/01/97 Вялый: new macro
% определяет точку для метки угла
def text_point(expr a,b,c,n) =
 begingroup
  save p,s,pp; path p; pair pp;
  p = unitvector(a-b){(a-b)rotated 90}..unitvector(c-b);
  s = .9marksize/length(point 1 of p - point 0 of p);
  if s < angle_radius: s:=angle_radius; fi
b+(s+n*angle_radius)*unitvector((unitvector(a-b)+unitvector(c-b)))
 endgroup
enddef;




%27/01/97 Вялый: текстовые пометки

%27/01/97 Вялый: касательные и вписанные окружности
%28/01/97 Вялый: окружности
def circum(expr p,c,r) =%28/01/97 Вялый: путь, центр, радиус
  p= c+(r,0)..c+(0,r)..c+(-r,0)..c+(0,-r)..cycle;
enddef;


def distance(expr a,b)=
  (arclength(a--b))
enddef;

def tangent(expr p,c,r)(suffix tp,tn) =
 begingroup
   save sa,sb,sc;
   pair sa,sb,sc;
   sa=(r,distance(c,p)+-+r);
   sb=unitvector(sa);
   sc=c+r*unitvector(p-c);
   tp = sc rotatedaround (c,angle(sb));
   tn = sc rotatedaround (c,-angle(sb));
 endgroup
enddef;

def circumscribe(expr q,c,r,x,y,u) =
 begingroup
  (c-.5[x,y])dotprod(x-y)=0;
  (c-.5[x,u])dotprod(x-u)=0;
  r=distance(c,x);
  circum(q,c,r);
 endgroup
enddef;

%26/05/97 Вялый: пересечение прямой и окружнности

def linecirc(expr c,r,x,y)(suffix tx,ty) =
 begingroup
   save u, l,ll;
   pair u;
   numeric l,ll;
   (c-u)dotprod(x-y)=0;
   u=whatever[x,y];
   l=distance(c,u);
   ll=r+-+l;
   tx=u+ll*unitvector(x-u);
   ty=u+ll*unitvector(y-u);
 endgroup
enddef;
