%08/09/96 ���: � ��ࢮ� ᪮��� �⮨� ����. �� ������ � ࠧ�뢠��,
%����� �ࠢ������ ��ࠬ��஬ gap. ��窨 ࠧ�뢮� ��।������� � ���筥,
%���饬 �� ��ன ᪮���. ����� � ��砫� ⠪�� �����࣠���� ࠧ���.

vardef breaks(expr p)(text t) =
%��稭��� � �窨 ����祭�� �� ������ ��⢨!!
draw subpath (
arctime gap of p,
  for i=t:
arctime ((arclength subpath (0,i) of p)-gap) of p) of p;
draw subpath (
arctime ((arclength subpath (0,i) of p)+gap) of p,
  endfor
arctime ((arclength subpath (0,length p) of p)-gap) of p) of p;
enddef;

%08/09/96 ���:  ��ਠ�� �।��饣� �����, ����室��� ���
%����筮�� �ᮢ����. �஢���� �ਢ�� �祭� ᢥ�� 梥⮬ (����� � ����).
%����騩 ��ଥ�� ࠢ�� 0.95.

vardef breakswhite(expr p)(text t) =
%��稭��� � �窨 ����祭�� �� ������ ��⢨!!
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


%08/09/96 ���: �� ���� ᯮᮡ �஢����� ����� � �஡�����.
%���� ��㬥�� -- ����, ����� ��⠥���
%�ᯮ������� ��� ��ண� ��㬥��.  ��ன ��㬥�� ��ᬠ�ਢ����� ��
%���ࢠ��, ���������� ��ன, ���������� ���쨬 ��㬥�⮬. ������� ���
%��᮪ � ����室��묨 ࠧ�뢠�� ��� ��ࢮ� �ਢ��.

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

%06/02/97 ���: ��ਠ�� ��� �ᮢ���� ࠧ�묨 梥⠬�
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


