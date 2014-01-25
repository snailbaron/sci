%08/09/96 Вялый: В первой скобке стоит путь. Он рисуется с разрывами,
%которые управляются параметром gap. Точки разрывов определяются в перечне,
%стоящем во второй скобке. Конец и начало также подвергаются разрыву.

vardef breaks(expr p)(text t) =
%Начинаем с точки пересечения по нижней ветви!!
draw subpath (
arctime gap of p,
  for i=t:
arctime ((arclength subpath (0,i) of p)-gap) of p) of p;
draw subpath (
arctime ((arclength subpath (0,i) of p)+gap) of p,
  endfor
arctime ((arclength subpath (0,length p) of p)-gap) of p) of p;
enddef;

%08/09/96 Вялый:  Вариант предыдущего макроса, необходимый для
%ленточного рисования. Проводит кривую очень светлым цветом (можно и белым).
%Текущий парметр равен 0.95.

vardef breakswhite(expr p)(text t) =
%Начинаем с точки пересечения по нижней ветви!!
draw subpath (
arctime gap of p,
  for i=t:
arctime ((arclength subpath (0,i) of p)-gap) of p) of p
withcolor 0.95white;
draw subpath (
arctime ((arclength subpath (0,i) of p)+gap) of p,
  endfor
arctime ((arclength subpath (0,length p) of p)-gap) of p) of p
withcolor 0.95white;
enddef;


%08/09/96 Вялый: Еще один способ проводить линии с пробелами.
%Первый аргумент -- путь, который считается
%расположенным выше второго аргумента.  Второй аргумент рассматривается на
%интервале, задаваемом парой, задаваемой третьим аргументом. Рисуется этот
%кусок с необходимыми разрывами под первой кривой.

vardef under(expr pix,  pv, sp)=
path pp;
pp=subpath sp of pv;
draw subpath
(
arctime gap of (pp cutbefore pix),
length(pp cutbefore pix)
)
of
(pp cutbefore pix);
draw subpath
(
0, length(pp cutafter pix)-
arctime gap of (pp cutafter pix)
)
of
(pp cutafter pix);
enddef;

vardef thickdraw(expr p,n,wl) =
linecap:=butt;
pickup pencircle scaled sthick;
draw p;
pickup pencircle scaled (sthick-2*wl);
draw p withcolor n*white;
pickup pencircle scaled wl;
linecap:=rounded;

enddef;

%06/02/97 Вялый: вариант для рисования разными цветами
vardef underwhite(expr pix,  pv, sp,n,wl)=
path pp;
pp=subpath sp of pv;
thickdraw( subpath
(
arctime (gap+.5sthick) of (pp cutbefore pix),
length(pp cutbefore pix)
)
of
(pp cutbefore pix),
n,wl);
thickdraw( subpath
(
0, length(pp cutafter pix)-
arctime (gap+.5sthick) of (pp cutafter pix)
)
of
(pp cutafter pix),
n,wl);
enddef;


