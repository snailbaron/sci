% 17/03/97 20:23 Рабочая версия стиля \лк МатПросвещения\пк.
% Этот стиль LaTeX'а создан для нужд сборника \лк МатПросвещение\пк
% М.Н. Вялым путем творческой компиляции стандартного стиля
% book.sty и стиля С.М. Львовского russcorr.sty. Ответственность за
% имеющиеся в нем ошибки целиком лежит на М. Вялом. Связаться со мной
% (с ним)  можно по адресу:
% vyalyi@ium.ips.ras.ru
% (см. также адреса редакции МатПросвещения)
%
% КАК ХОДИТЬ ПО ЭТОМУ ТЕКСТУ?
% Основные блоки разделены строками вида
%%%<ИМЯ-БЛОКА>%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  ПЕРЕЧЕНЬ ОСНОВНЫХ БЛОКОВ
% 1. ОПРЕДЕЛЕНИЕ ОСНОВНЫХ ЭЛЕМЕНТОВ ОФОРМЛЕНИЯ
% 2. РУСИФИКАЦИИ
% 3. РАСШИРЕННАЯ ПОДДЕРЖКА МАТЕМАТИКИ
% 4. ОПРЕДЕЛЕНИЕ ПЕРВЫХ СТРАНИЦ (включая состав редколлегии)
%
%%ОБЩИЕ ОПРЕДЕЛЕНИЯ %%%%07/02/97 Вялый: добавлены из Львовского%%%%%%%%%%%

\NeedsTeXFormat{LaTeX2e}[1995/12/01]
\ProvidesClass{matpros}
              [13/04/97  document class MATPROS]
\newcommand\@ptsize{}
\newif\if@restonecol
\newif\if@titlepage
\@titlepagetrue
\newif\if@openright
\newif\if@mainmatter \@mainmattertrue
\if@compatibility\else
\DeclareOption{a4paper}
   {\setlength\paperheight {297mm}%
    \setlength\paperwidth  {210mm}}
\DeclareOption{a5paper}
   {\setlength\paperheight {210mm}%
    \setlength\paperwidth  {148mm}}
\DeclareOption{b5paper}
   {\setlength\paperheight {250mm}%
    \setlength\paperwidth  {176mm}}
\DeclareOption{letterpaper}
   {\setlength\paperheight {11in}%
    \setlength\paperwidth  {8.5in}}
\DeclareOption{legalpaper}
   {\setlength\paperheight {14in}%
    \setlength\paperwidth  {8.5in}}
\DeclareOption{executivepaper}
   {\setlength\paperheight {10.5in}%
    \setlength\paperwidth  {7.25in}}
\DeclareOption{landscape}
   {\setlength\@tempdima   {\paperheight}%
    \setlength\paperheight {\paperwidth}%
    \setlength\paperwidth  {\@tempdima}}
\fi
\if@compatibility
  \renewcommand\@ptsize{0}
\else
\DeclareOption{10pt}{\renewcommand\@ptsize{0}%
\font\boldsymbol=cmmib10 scaled 1000
}
\fi
\DeclareOption{11pt}{\renewcommand\@ptsize{1}%
\font\boldsymbol=cmmib10 scaled 1095
}
\DeclareOption{12pt}{\renewcommand\@ptsize{2}%
\font\boldsymbol=cmmib10 scaled 1200
}
\if@compatibility\else
\DeclareOption{oneside}{\@twosidefalse \@mparswitchfalse}
\fi
\DeclareOption{twoside}{\@twosidetrue  \@mparswitchtrue}
\DeclareOption{draft}{\setlength\overfullrule{5pt}}
\if@compatibility\else
\DeclareOption{final}{\setlength\overfullrule{0pt}}
\fi
\DeclareOption{titlepage}{\@titlepagetrue}
\if@compatibility\else
\DeclareOption{notitlepage}{\@titlepagefalse}
\fi
\if@compatibility
\@openrighttrue
\else
\DeclareOption{openright}{\@openrighttrue}
\DeclareOption{openany}{\@openrightfalse}
\fi
\if@compatibility\else
\DeclareOption{onecolumn}{\@twocolumnfalse}
\fi
\DeclareOption{twocolumn}{\@twocolumntrue}
\DeclareOption{leqno}{\input{leqno.clo}}
\DeclareOption{fleqn}{\input{fleqn.clo}}
\DeclareOption{openbib}{%
  \AtEndOfPackage{%
   \renewcommand\@openbib@code{%
      \advance\leftmargin\bibindent
      \itemindent -\bibindent
      \listparindent \itemindent
      \parsep \z@
      }%
   \renewcommand\newblock{\par}}%
}
\newif\if@longtoc
\DeclareOption{longtoc}{\@longtoctrue}
\DeclareOption{shorttoc}{\@longtocfalse}
\newif\if@indentheadings
\DeclareOption{indentheadings}{\@indentheadingstrue}
\DeclareOption{noindentheadings}{\@indentheadingsfalse}

\ExecuteOptions{letterpaper,10pt,twoside,onecolumn,final,openany,%
                shorttoc,noindentheadings}
\ProcessOptions
\input{bk1\@ptsize.clo}
\setlength\lineskip{1\p@}
\setlength\normallineskip{1\p@}
\renewcommand\baselinestretch{}
\setlength\parskip{0\p@ \@plus \p@}
\@lowpenalty   51
\@medpenalty  151
\@highpenalty 301
\setcounter{topnumber}{2}
\renewcommand\topfraction{.7}
\setcounter{bottomnumber}{1}
\renewcommand\bottomfraction{.5}
\setcounter{totalnumber}{3}
\renewcommand\textfraction{.2}
\renewcommand\floatpagefraction{.5}
\setcounter{dbltopnumber}{2}
\renewcommand\dbltopfraction{.7}
\renewcommand\dblfloatpagefraction{.5}


%%РАЗМЕРЫ%%%%%%%%07/02/97 Вялый: 170 на 240%%%%%%%%%%%%%%%%%%%
\widowpenalty=10000%19/03/97 Вялый: запрет на висячие строки
\hfuzz=0.3pt
%07/02/97 Вялый: переопределения размеров из *.clo
  \setlength\@tempdima{170mm}
  \addtolength\@tempdima{-4cm}
  \setlength\@tempdimb{390\p@}
  \if@twocolumn
    \ifdim\@tempdima>2\@tempdimb\relax
      \setlength\textwidth{2\@tempdimb}
    \else
      \setlength\textwidth{\@tempdima}
    \fi
  \else
    \ifdim\@tempdima>\@tempdimb\relax
      \setlength\textwidth{\@tempdimb}
    \else
      \setlength\textwidth{\@tempdima}
    \fi
  \fi
\if@compatibility\else
  \@settopoint\textwidth
\fi
\if@compatibility
  \setlength\textheight{36\baselineskip}
\else
  \setlength\@tempdima{240mm}
  \addtolength\@tempdima{-4.5cm}
%07/02/97 Вялый:   \addtolength\@tempdima{-1.5in}
  \divide\@tempdima\baselineskip
  \@tempcnta=\@tempdima
  \setlength\textheight{\@tempcnta\baselineskip}
\fi
\def\changebaselineskip#1{
\advance\baselineskip by#1}

%07/02/97 Вялый: \addtolength\textheight{\topskip}
\if@twocolumn
 \setlength\marginparsep {10\p@}
\else
  \setlength\marginparsep{7\p@}
\fi
\setlength\marginparpush{7\p@}
\if@compatibility
  \setlength\topmargin{.73in}
\else
  \setlength\topmargin{240mm}
  \addtolength\topmargin{-4.5cm}
  \addtolength\topmargin{-\headheight}
  \addtolength\topmargin{-\headsep}
  \addtolength\topmargin{-\textheight}
  \addtolength\topmargin{-\footskip}     % this might be wrong!
  \addtolength\topmargin{-.5\topmargin}
  \@settopoint\topmargin
\fi
   \setlength\oddsidemargin   {1in}
   \setlength\evensidemargin  {1in}


%07/02/97 Вялый: конец переопределений
%%%HEADINGS%%%%%%%%%07/02/97 Вялый: из старого%%%%%%%%%
\def\ps@headings{\let\@mkboth\markboth
 \def\@oddfoot{}\def\@evenfoot{}%       No feet.
 \def\@evenhead%
{\raisebox{0pt}[\headheight][0pt]{%
\vbox{\hbox to\textwidth{ \textsf{\thepage}\hfil\strut \slshape\leftmark}% Left heading.
\hrule}%
}}
 \def\@oddhead%
{\raisebox{0pt}[\headheight][0pt]{%
\vbox{\hbox to\textwidth{\strut{\slshape \rightmark}\hfil \textsf{\thepage}}% Right heading.
\hrule}%
}}
 \def\chaptermark##1{\ifx##1\relax
\markboth {\@author}{\@title}
\else \markboth {\@author}{##1}\fi}
}

\def\ps@sheadings{\let\@mkboth\markboth
 \def\@oddfoot{}\def\@evenfoot{}%       No feet.
 \def\@evenhead%
{\global\partbeginfalse\raisebox{0pt}[\headheight][0pt]{%
\vbox{\hbox to\textwidth{ \textsf{\thepage}\hfil\strut }% Left heading.
\hrule}%
}}
 \def\@oddhead%
{\global\partbeginfalse\raisebox{0pt}[\headheight][0pt]{%
\vbox{\hbox to\textwidth{\strut{}\hfil \textsf{\thepage}}% Right heading.
\hrule}%
}}
 \def\chaptermark##1{\ifx##1\relax
\markboth {\@author}{\@title}
\else \markboth {\@author}{##1}\fi}
}


\def\ps@paperheadings{\let\@mkboth\markboth
 \def\@oddfoot{}\def\@evenfoot{}%       No feet.
 \def\@evenhead%
{\global\partbeginfalse\raisebox{0pt}[\headheight][0pt]{%
\vbox{\hbox to\textwidth{\textsf{Математическое просвещение,
сер. 3, вып. \выпуск, \Year
(\pageref{boa@\thechapter}--\pageref{eoa@\thechapter})
}\hfil\strut}% Left heading.
\hrule}%
}}
 \def\@oddhead%
{\global\partbeginfalse\raisebox{0pt}[\headheight][0pt]{%
\vbox{\hbox to\textwidth{\strut\textsf{Математическое просвещение,
сер. 3, вып. \выпуск, \Year
(\pageref{boa@\thechapter}--\pageref{eoa@\thechapter})
}\hfil}% Right heading.
\hrule}%
}}
 \def\chaptermark##1{\ifx##1\relax
\markboth {\@author}{\@title}
\else \markboth {\@author}{##1}\fi}
}

\def\ps@myheadings{\let\@mkboth\@gobbletwo
 \def\@oddfoot{}\def\@evenfoot{}%       No feet.
 \def\@evenhead{\global\partbeginfalse}% No head
 \def\@oddhead{\global\partbeginfalse}%    No head
}

%%PARS and SECTIONS%%%%%%%07/02/97 Вялый: %%%%%%%%%%%%%%%%%%%%%%%%
\newcommand*\chaptermark[1]{}
\setcounter{secnumdepth}{2}
%07/02/97 Вялый:
\newcounter {part}                    %%% Часть соответствует рубрике,
\newcounter {chapter}                 %   глава -- статье
%07/02/97 Вялый: правило состоит в том, что статья располагается
%в отдельном файле, имеющем расширение .i<номер>
% 05-08-95 Вялый: section соответствует разделу
\newcounter {section}[chapter]
% 05-08-95 Вялый: subsection соответствует пункту
%принимаем сплошную нумерацию пунктов безотносительно к разделу
%как в МП-II
\newcounter {subsection}[chapter]
\newcounter {subsubsection}[subsection]
\newcounter {paragraph}[subsubsection]
\newcounter {subparagraph}[paragraph]


% 05-08-95 Вялый: Названия частей сборника
%\def\thepart{\headpart{part}}
%\def\headpart#1{\@headpart{\@nameuse {c@#1}}}
%\def\@headpart#1{
%\ifcase #1\or
%          \or
%Студенческие чтения        \or
%Тема номера                \or
%$\phantom{\rm Заметки}$    \or
%Школьный отдел             \or
%Задачный отдел             \else\fi
%}
\def\thepart{}

% Was changed to title and authors!!!Вялый 09-05-94 05:29pm
\def\thechapter{}%Вялый 09-05-94 10:50pm {\markboth{\@title}{\@author}}

\def\thesection       {\arabic{section}}
\def\thesubsection    {\arabic{subsection}}% 05-08-95 Вялый
\def\thesubsubsection {\thesubsection .\arabic{subsubsection}}
\def\theparagraph     {\thesubsubsection.\arabic{paragraph}}
\def\thesubparagraph  {\theparagraph.\arabic{subparagraph}}

%\def\@chapapp{\chaptername} Вялый 09-05-94 04:28pm
% 05-08-95 Вялый: необходимо для размещения сокращенных названий статей
% в колонтитулах
% 05-08-95 Вялый: далее идут специальные определения для поддержки полного
% и краткого наименования авторов и статей
\def\shorttitle#1{\gdef\@shorttitle{#1}}
\let\@shorttitle\relax
\newif\if@full\@fullfalse
\def\и{\if@full\and\else,\space\fi}
\def\из#1{\if@full\space(#1)\else\relax\fi}
\def\прим#1{\if@full\thanks{\hskip0.4em #1}\else\relax\fi}
\def\ак#1ъ{\if@full#1\space\else\space\fi}
\def\статья#1#2#3{\global\def\@author{#1}\global\def\@title{#2}%
%\let\Label\label \def\label##1{\Label{#3:##1}}%
%\let\Ref\ref \def\ref##1{\Ref{#3:##1}}%
%\let\Pageref\pageref \def\pageref##1{\Pageref{#3:##1}}%
\chapter{}
\label{boa\arabic{chapter}}
\begingroup \input #3.\issue\endgroup%
\label{eoa\arabic{chapter}}
\global\def\@author{}\global\def\@title{}\global\let\@shorttitle\relax%
}
\def\chapter{
% 05-08-95 Вялый: в оглавление заносим в отдельной строке
% автора и в отдельной --- название
\addtocontents{toc}{\protect\noindent\@author\protect\nopagebreak}%09/02/97 02:14
\addcontentsline{toc}{chapter}{\textit{\@title}}
\markboth{}{}
\refstepcounter{chapter}%
\arttitle
\chaptermark\@shorttitle}
\def\arttitle{\par
 \begingroup
   \def\thefootnote{\fnsymbol{footnote}}%
   \def\@makefnmark{\hbox
       to\z@{$\m@th^{\@thefnmark}$\hss}}%
   \if@twocolumn
     \twocolumn[\@arttitle]%
     \else
     \global\@topnum\z@
     \@arttitle \fi
   \ifpartbegin\thispagestyle{myheadings}\global\partbeginfalse
   %\else\thispagestyle{paperheadings}\fi
   \else\thispagestyle{sheadings}\fi
\@thanks
  \endgroup
 \setcounter{footnote}{0}%
\global\language=0
 \gdef\@thanks{}
%\let\thanks\relax
}

\def\@arttitle{
 \null
 \vskip 2em\@fulltrue
 \begin{center}%
  {\LARGE \@title \par}%
  \vskip 1.5em
  {\large
   \lineskip .5em
   \begin{tabular}[t]{c}\@author
   \end{tabular}\par}%
 \end{center}%
 \par
 \vskip 1.5em
\@fullfalse}

\def\abstract{
\if@twocolumn
\section*{\abstractname}%
\else \small
\begin{center}
\quotation
\fi}

\def\endabstract{
\if@twocolumn\else\endquotation\end{center}\fi\bigskip}


%07/02/97 Вялый: из новой версии, пока не используется
\newcommand\frontmatter{%
  \if@openright
    \cleardoublepage
  \else
    \clearpage
  \fi
  \@mainmatterfalse
  \pagenumbering{roman}}
\newcommand\mainmatter{%
  \if@openright
    \cleardoublepage
  \else
    \clearpage
  \fi
  \@mainmattertrue
  \pagenumbering{arabic}}
\newcommand\backmatter{%
  \if@openright
    \cleardoublepage
  \else
    \clearpage
  \fi
  \@mainmatterfalse}
\newif\ifpartbegin\partbeginfalse
\newcommand\part{%
  \if@openright
    \cleardoublepage
  \else
    \clearpage
  \fi
 \global\partbegintrue
  \thispagestyle{myheadings}%
  \if@twocolumn
    \onecolumn
    \@tempswatrue
  \else
    \@tempswafalse
  \fi
%%07/02/97 Вялый:   \null\vfil
  \secdef\@part\@spart}


\def\@part[#1]#2{
        \refstepcounter{part}%
    %14/05/97 Вялый: bug? \ifnum\c@part>1
        \addtocontents{toc}{\protect\bigskip
\protect\textbf{#2}\protect\\[1mm] }%
    %\fi% 05-08-95 Вялый
   {\centering
    \interlinepenalty \@M
    \vskip -20\p@
    \huge \bfseries #2
    \par\vskip0.05in\hrule height1.4pt\vskip2.8\p@\hrule height 0.8pt}
\vskip0.95in\@endpart}

\def\@endpart{
   \if@twoside
       \hbox{}%
       \thispagestyle{empty}%
   \fi
   \if@tempswa
     \twocolumn
   \fi}

\def\@spart#1{{\centering
   \interlinepenalty \@M
   \Huge \bf   #1\par\hrule height4pt\vskip1\p@\hrule}
\vskip1in\@endpart}

\def\section{\@startsection {section}{1}{\z@}%
                                   {-2.5ex \@plus -1ex \@minus -.2ex}%
                                   {10.3ex \@plus.2ex}%
                                   {\normalfont\large\mdseries\scshape}}
\newcommand\subsection{\@startsection{subsection}{2}{\z@}%
                                     {-2.25ex\@plus -1ex \@minus -.2ex}%
                                     {0.75ex \@plus .2ex}%
                                     {\normalfont\normalsize\scshape}}
\newcommand\subsubsection{\@startsection{subsubsection}{3}{\z@}%
                                     {-2.25ex\@plus -1ex \@minus -.2ex}%
                                     {0.75ex \@plus .2ex}%
                                     {\normalfont\normalsize\scshape}}
\newcommand\paragraph{\@startsection{paragraph}{4}{\z@}%
                                    {1.25ex \@plus1ex \@minus.2ex}%
                                    {-1em}%
                                    {\normalfont\normalsize\scshape}}
\newcommand\subparagraph{\@startsection{subparagraph}{5}{\parindent}%
                                       {1.25ex \@plus1ex \@minus .2ex}%
                                       {-1em}%
                                      {\normalfont\normalsize\bfseries}}
\if@indentheadings
\def\section{\@startsection {section}{1}{\parindent}%
                                   {2.5ex \@plus 1ex \@minus .2ex}%
                                   {1.3ex \@plus.2ex}%
                                   {\reset@font\large\scshape}}
\def\subsection{\@startsection{subsection}{2}{\parindent}%
                                     {2.25ex\@plus 1ex \@minus .2ex}%
                                     {0.75ex \@plus .2ex}%
                                     {\reset@font\normalsize\scshape}}
\def\subsubsection{\@startsection{subsubsection}{3}{\parindent}%
                                     {2.25ex\@plus 1ex \@minus .2ex}%
                                     {0.75ex \@plus .2ex}%
                                     {\reset@font\normalsize\scshape}}
\fi



\def\@seccntformat#1{\csname pre#1\endcsname\csname the#1\endcsname
                \csname post#1\endcsname}
\let\@Tocseccntformat\@seccntformat
\def\@postskip@{\hskip.5em\relax}
\def\postsection{.\@postskip@}
\def\postsubsection{.\@postskip@}
\def\postsubsubsection{.\@postskip@}
\def\postparagraph{.\@postskip@}
\def\postsubparagraph{.\@postskip@}
\def\presection{}
\def\presubsection{}
\def\presubsubsection{}
\def\preparagraph{}
\def\presubparagraph{}
%%% Unfortunately, \@seccntformat is not reflected in toc.
%%% Hence, I had to hack up the definition of \@sect a little.
\def\@sect#1#2#3#4#5#6[#7]#8{%
     \ifnum #2>\c@secnumdepth
       \let\@svsec\@empty\else
       \refstepcounter{#1}%
       \let\@@protect\protect
       \def\protect{\noexpand\protect\noexpand}%
       \edef\@svsec{\@seccntformat{#1}}%
       \let\protect\@@protect
     \fi
     \@tempskipa #5\relax
      \ifdim \@tempskipa>\z@
        \begingroup\centering%13/04/97 Вялый
          #6\relax
          \@hangfrom{\hskip #3\relax\@svsec}%
                    {\interlinepenalty \@M #8\par}%
        \endgroup
       \csname #1mark\endcsname{#7}\addcontentsline
         {toc}{#1}{\ifnum #2>\c@secnumdepth \else
                      \protect\numberline{\@Tocseccntformat{#1}}\fi
%%%%%%%                                   ^^^^^^^^^^^^^^^^^^^^^
         \if@longtoc#8\else#7\fi}\else
%%%%%%%  ^^^^^^^^^^^^^^^^^^^^^^^
        \def\@svsechd{#6\hskip #3\relax  %% \relax added 2 May 90
                   \@svsec #8\csname #1mark\endcsname
                      {#7}\addcontentsline
                           {toc}{#1}{\ifnum #2>\c@secnumdepth \else
                           \protect\numberline{\@Tocseccntformat{#1}}\fi
%%%%%                                          ^^^^^^^^^^^^^^^^^^^^^
         \if@longtoc#8\else#7\fi}}\fi
%%%%%    ^^^^^^^^^^^^^^^^^^^^^^^
     \@xsect{#5}}

%08/02/97 Вялый: из старого LATeXa
\def\@ssect#1#2#3#4#5{\@tempskipa #3\relax
   \ifdim \@tempskipa>\z@
     \begingroup #4
 \leftskip=#1\relax \rightskip=#1\relax
          \centering
\interlinepenalty \@M \ignorespaces#5\par
     \endgroup
% добавил \ignorespaces перед #5
   \else \def\@svsechd{#4\hskip #1\relax
\ignorespaces #5}\fi
% добавил \ignorespaces перед #5
    \@xsect{#3}}




\def\@mainmattertrue{\let\if@mainmatter\iftrue}
\def\@mainmatterfalse{\let\if@mainmatter\iffalse}
  \@mainmattertrue

%%LISTS%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\if@twocolumn
  \setlength\leftmargini  {2em}
\else
  \setlength\leftmargini  {2.5em}
\fi
\leftmargin  \leftmargini
\setlength\leftmarginii  {2.2em}
\setlength\leftmarginiii {1.87em}
\setlength\leftmarginiv  {1.7em}
\if@twocolumn
  \setlength\leftmarginv  {.5em}
  \setlength\leftmarginvi {.5em}
\else
  \setlength\leftmarginv  {1em}
  \setlength\leftmarginvi {1em}
\fi
%%%Set vspaces for lists
\setlength\partopsep{3\p@ \@plus 2\p@ \@minus 2\p@}
\def\@listi{\leftmargin\leftmargini
            \parsep 2\p@  \@plus1.5\p@ \@minus\p@
            \topsep \itemsep
            \partopsep 4\p@ \@plus2\p@   \@minus2\p@
            \itemsep4\p@  \@plus2.5\p@ \@minus\p@}
\let\@listI\@listi
\@listi
\def\@listii {\leftmargin\leftmarginii
              \labelwidth\leftmarginii
              \advance\labelwidth-\labelsep
              \topsep    2.5\p@\@plus\p@\@minus\p@
              \parsep   \z@
              \itemsep   \parsep}
\def\@listiii{\leftmargin\leftmarginiii
              \labelwidth\leftmarginiii
              \advance\labelwidth-\labelsep
              \topsep    2.5\p@\@plus\p@\@minus\p@
              \parsep    \z@
              \partopsep \p@ \@plus\z@ \@minus\p@
              \itemsep   \parsep}
\def\@listiv {\leftmargin\leftmarginiv
              \labelwidth\leftmarginiv
              \advance\labelwidth-\labelsep}
\def\@listv  {\leftmargin\leftmarginv
              \labelwidth\leftmarginv
              \advance\labelwidth-\labelsep}
\def\@listvi {\leftmargin\leftmarginvi
              \labelwidth\leftmarginvi
              \advance\labelwidth-\labelsep}

%%%end of Set vspaces for lists
\setlength  \labelsep  {.5em}
\setlength  \labelwidth{\leftmargini}
\addtolength\labelwidth{-\labelsep}
\@beginparpenalty -\@lowpenalty
\@endparpenalty   -\@lowpenalty
\@itempenalty     -\@lowpenalty
\renewcommand\theenumi{\@arabic\c@enumi}
\renewcommand\theenumii{\@alph\c@enumii}
\renewcommand\theenumiii{\@roman\c@enumiii}
\renewcommand\theenumiv{\@Alph\c@enumiv}
\newcommand\labelenumi{\theenumi.}
\newcommand\labelenumii{(\theenumii)}
\newcommand\labelenumiii{\theenumiii.}
\newcommand\labelenumiv{\theenumiv.}
\renewcommand\p@enumii{\theenumi}
\renewcommand\p@enumiii{\theenumi(\theenumii)}
\renewcommand\p@enumiv{\p@enumiii\theenumiii}
\newcommand\labelitemi{$\triangleright$}
\newcommand\labelitemii{--}
\newcommand\labelitemiii{$\m@th\ast$}
\newcommand\labelitemiv{$\m@th\cdot$}

\def\ralph#1{\@ralph{\@nameuse {c@#1}}}
\def\@ralph#1{\ifcase #1\or а\or б\or в\or г\or д\or е\or ж\or з\or
и\else\@iralph{#1}\fi
}
\def\@iralph#1{\ifcase #1\or\or\or\or\or\or\or\or\or\or
     к\or л\or м\or н\or о\or п\or р\or с\or т\or у\or
     ф\or х\or ц\or ч\or ш\or щ\or э\or ю\or я\else\@ctrerr
     \fi}
\def\Ralph#1{\@Ralph{\@nameuse {c@#1}}}
\def\@Ralph#1{\ifcase #1%
     \or А\or Б\or В\or Г\or Д\or Е\or Ж\or З\or
     И\else\@iRalph{#1}\fi}
\def\@iRalph#1{\ifcase #1%
     \or\or\or\or\or\or\or\or\or\or К\or Л\or М\or
     Н\or О\or П\or Р\or С\or Т\or У\or Ф\or Х\or Ц\or
     Ч\or Ш\or Щ\or Э\or Ю\or Я\else\@ctrerr
     \fi}

\message{lists}
% Теперь "русифицируем" окружение enumerate:
\def\labelenumi{\theenumi)}
\def\theenumii{\ralph{enumii}}
\def\labelenumii{\theenumii)}
\def\p@enumii{\theenumi}
\def\labelenumiii{{\bf--}}
\let\theenumiii\relax
\def\p@enumiii{\theenumi\theenumii}
%
% И еще запретим вкладывать четыре enumerate друг в друга ...
%
\def\enumerate{\ifnum \@enumdepth >2% было 3, а не 2
\@toodeep\else
      \advance\@enumdepth \@ne
      \edef\@enumctr{enum\romannumeral\the\@enumdepth}\list
      {\csname label\@enumctr\endcsname}{\usecounter
        {\@enumctr}\def\makelabel##1{\hss\llap{##1}}}\fi}
%
% Теперь определим окружение rlist
% для создания списков, в которых
% \item'ы нумеруются русскими буквами
% (строчными для rlist, прописными для rlist[u])
%
\@definecounter{rlistctr}
\newif\if@rlistsnested\@rlistsnestedfalse
\def\rlist{\@ifnextchar[{\@rlist}{\@rlist[l]}}
\def\@rlist[#1]{\if u#1\def\@tempa{R}\else\def\@tempa{r}\fi
                \if@rlistsnested\@toodeep\else
                \@rlistsnestedtrue
                \edef\therlistctr{\expandafter\noexpand\csname
                                @\@tempa alph\endcsname\noexpand\c@rlistctr}%
                \list{\labelrlist}{\usecounter
                   {rlistctr}}\fi}
\let\endrlist\endlist
\def\labelrlist{\therlistctr)}


%%ENVIRONMENTS%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\newenvironment{description}
               {\list{}{\labelwidth\z@ \itemindent-\leftmargin
                        \let\makelabel\descriptionlabel}}
               {\endlist}
\newcommand*\descriptionlabel[1]{\hspace\labelsep
                                \normalfont\bfseries #1}
\newenvironment{verse}
               {\let\\\@centercr
                \list{}{\itemsep      \z@
                        \itemindent   -1.5em%
                        \listparindent\itemindent
                        \rightmargin  \leftmargin
                        \advance\leftmargin 1.5em}%
                \item\relax}
               {\endlist}
\newenvironment{quotation}
               {\list{}{\listparindent 1.5em%
                        \itemindent    \listparindent
                        \rightmargin   \leftmargin
                        \parsep        \z@ \@plus\p@}%
                \item\relax}
               {\endlist}
\newenvironment{quote}
               {\list{}{\itemindent\parindent%
                        \leftmargin\parindent%10/02/97 22:06 hack
                        \rightmargin\leftmargin}%
                \item\relax}
               {\endlist}
\if@compatibility
\newenvironment{titlepage}
    {%
      \cleardoublepage
      \if@twocolumn
        \@restonecoltrue\onecolumn
      \else
        \@restonecolfalse\newpage
      \fi
      \thispagestyle{empty}%
      \setcounter{page}\z@
    }%
    {\if@restonecol\twocolumn \else \newpage \fi
    }
\else
\newenvironment{titlepage}
    {%
      \cleardoublepage
      \if@twocolumn
        \@restonecoltrue\onecolumn
      \else
        \@restonecolfalse\newpage
      \fi
      \thispagestyle{empty}%
      \setcounter{page}\@ne
    }%
    {\if@restonecol\twocolumn \else \newpage \fi
     \if@twoside\else
        \setcounter{page}\@ne
     \fi
    }
\fi
\newcommand\appendix{\par
  \setcounter{chapter}{0}%
  \setcounter{section}{0}%
  \renewcommand\@chapapp{\appendixname}%
  \renewcommand\thechapter{\@Alph\c@chapter}}
\setlength\arraycolsep{5\p@}
\setlength\tabcolsep{6\p@}
\setlength\arrayrulewidth{.4\p@}
\setlength\doublerulesep{2\p@}
\setlength\tabbingsep{\labelsep}
\skip\@mpfootins = \skip\footins
\setlength\fboxsep{3\p@}
\setlength\fboxrule{.4\p@}
\@addtoreset{equation}{chapter}
\renewcommand\theequation{\thechapter.\@arabic\c@equation}
\newcounter{figure}[chapter]
\renewcommand\thefigure{\thechapter.\@arabic\c@figure}
\def\fps@figure{tbp}
\def\ftype@figure{1}
\def\ext@figure{lof}
\def\fnum@figure{\figurename~\thefigure}
\newenvironment{figure}
               {\@float{figure}}
               {\end@float}
\newenvironment{figure*}
               {\@dblfloat{figure}}
               {\end@dblfloat}
\newcounter{table}[chapter]
\renewcommand\thetable{\thechapter.\@arabic\c@table}
\def\fps@table{tbp}
\def\ftype@table{2}
\def\ext@table{lot}
\def\fnum@table{\tablename~\thetable}
\newenvironment{table}
               {\@float{table}}
               {\end@float}
\newenvironment{table*}
               {\@dblfloat{table}}
               {\end@dblfloat}
\newlength\abovecaptionskip
\newlength\belowcaptionskip
\setlength\abovecaptionskip{10\p@}
\setlength\belowcaptionskip{0\p@}
\long\def\@makecaption#1#2{%
  \vskip\abovecaptionskip
  \sbox\@tempboxa{#1: #2}%
  \ifdim \wd\@tempboxa >\hsize
    #1: #2\par
  \else
    \global \@minipagefalse
    \hb@xt@\hsize{\hfil\box\@tempboxa\hfil}%
  \fi
  \vskip\belowcaptionskip}
%%OLDFONTS%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\DeclareOldFontCommand{\rm}{\normalfont\rmfamily}{\mathrm}
\DeclareOldFontCommand{\sf}{\normalfont\sffamily}{\mathsf}
\DeclareOldFontCommand{\tt}{\normalfont\ttfamily}{\mathtt}
\DeclareOldFontCommand{\bf}{\normalfont\bfseries}{\mathbf}
\DeclareOldFontCommand{\it}{\normalfont\itshape}{\mathit}
\DeclareOldFontCommand{\sl}{\normalfont\slshape}{\@nomath\sl}
\DeclareOldFontCommand{\sc}{\normalfont\scshape}{\@nomath\sc}
\DeclareRobustCommand*\cal{\@fontswitch\relax\mathcal}
\DeclareRobustCommand*\mit{\@fontswitch\relax\mathnormal}
%%TABLE OF CONTENTS%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\newcommand\@pnumwidth{1.55em}
\newcommand\@tocrmarg{2.55em}
\newcommand\@dotsep{4.5}
\setcounter{tocdepth}{1}
\def\tableofcontents{\@restonecolfalse
  \if@twocolumn\@restonecoltrue\onecolumn\fi
  \section*{\contentsname
        \@mkboth{\uppercase{\contentsname}}{\uppercase{\contentsname}}}%
  \@starttoc{toc}\if@restonecol\twocolumn\fi}


\def\l@part#1#2{\addpenalty{-\@highpenalty}%
   \addvspace{2.25em plus\p@}% space above part line
   \begingroup
   \@tempdima 3em
     \parindent \z@ \rightskip \@pnumwidth
     \parfillskip -\@pnumwidth
     {\large \bf
     \leavevmode
     #1\hfil \hbox to\@pnumwidth{\hss #2}}\par
     \nobreak
    \global\@nobreaktrue
    \everypar{\global\@nobreakfalse\everypar{}}%% suggested by
   \endgroup}

%\def\l@chapter#1#2{\addpenalty{-\@highpenalty}%
%   \vskip 1.0em plus\p@
%  \@tempdima 1.5em
%  \begingroup
%    \parindent \z@ \rightskip \@pnumwidth
%    \parfillskip -\@pnumwidth
%    \bf
%    \leavevmode
%     \advance\leftskip\@tempdima
%     \hskip -\leftskip
%    #1\nobreak\hfil \nobreak\hbox to\@pnumwidth{\hss #2}\par
%    \penalty\@highpenalty
%  \endgroup}
\def\l@chapter{\@dottedtocline{1}{0.0em}{2.3em}}
\def\l@section{\@dottedtocline{2}{1.5em}{2.3em}}
\def\l@subsection{\@dottedtocline{2}{3.8em}{3.2em}}
\def\l@subsubsection{\@dottedtocline{3}{7.0em}{4.1em}}
\def\l@paragraph{\@dottedtocline{4}{10em}{5em}}
\def\l@subparagraph{\@dottedtocline{5}{12em}{6em}}


%07/02/97 Вялый: из исходного файла
\newcommand\listoffigures{%
    \if@twocolumn
      \@restonecoltrue\onecolumn
    \else
      \@restonecolfalse
    \fi
    \chapter*{\listfigurename
      \@mkboth{\MakeUppercase\listfigurename}%
              {\MakeUppercase\listfigurename}}%
    \@starttoc{lof}%
    \if@restonecol\twocolumn\fi
    }
\newcommand*\l@figure{\@dottedtocline{1}{1.5em}{2.3em}}
\newcommand\listoftables{%
    \if@twocolumn
      \@restonecoltrue\onecolumn
    \else
      \@restonecolfalse
    \fi
    \chapter*{\listtablename
      \@mkboth{%
          \MakeUppercase\listtablename}{\MakeUppercase\listtablename}}%
    \@starttoc{lot}%
    \if@restonecol\twocolumn\fi
    }
\let\l@table\l@figure
\newdimen\bibindent
\setlength\bibindent{1.5em}
\newenvironment{thebibliography}[1]
     {\section*{\bibname}%%07/02/97 Вялый:
      \list{\@biblabel{\@arabic\c@enumiv}}%
           {\settowidth\labelwidth{\@biblabel{#1}}%
            \leftmargin\labelwidth
            \advance\leftmargin\labelsep
            \@openbib@code
            \usecounter{enumiv}%
            \let\p@enumiv\@empty
            \renewcommand\theenumiv{\@arabic\c@enumiv}}%
      \sloppy\clubpenalty4000\widowpenalty4000%
      \sfcode`\.\@m}
     {\def\@noitemerr
       {\@latex@warning{Empty `thebibliography' environment}}%
      \endlist}
\newcommand\newblock{\hskip .11em\@plus.33em\@minus.07em}
\let\@openbib@code\@empty
\newenvironment{theindex}
               {\if@twocolumn
                  \@restonecolfalse
                \else
                  \@restonecoltrue
                \fi
                \columnseprule \z@
                \columnsep 35\p@
                \twocolumn[\@makeschapterhead{\indexname}]%
                \@mkboth{\MakeUppercase\indexname}%
                        {\MakeUppercase\indexname}%
                \thispagestyle{plain}\parindent\z@
                \parskip\z@ \@plus .3\p@\relax
                \let\item\@idxitem}
               {\if@restonecol\onecolumn\else\clearpage\fi}
\newcommand\@idxitem{\par\hangindent 40\p@}
\newcommand\subitem{\@idxitem \hspace*{20\p@}}
\newcommand\subsubitem{\@idxitem \hspace*{30\p@}}
\newcommand\indexspace{\par \vskip 10\p@ \@plus5\p@ \@minus3\p@\relax}
\renewcommand\footnoterule{%
  \kern-3\p@
  \hrule\@width.4\columnwidth
  \kern2.6\p@}
\@addtoreset{footnote}{chapter}
\newcommand\@makefntext[1]{%
    \parindent 1em%
    \noindent
    \hb@xt@1.8em{\hss\@makefnmark}#1}
%%NAMES (НАЗВАНИЯ ВСЯКОГО РАЗНОГО)%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\def\contentsname{Содержание}
\def\figurename{Рис.}
\def\partname{Часть}
\def\listfigurename{Список рисунков}
\def\listtablename{Список таблиц}
\def\refname{Список литературы}
\def\bibname{Список литературы}
%\def\refname{Литература}
%\def\bibname{Литература}
\def\indexname{Предметный указатель}
\def\tablename{Таблица}
\def\abstractname{От редакции.}
%%ОПРЕДЕЛЕНИЯ (в том числе из russcorr.sty)%%%%%%%%%%%%%%%%%%%%%%%
\newcommand\today{}
\edef\today{\ifcase\month\or
  January\or February\or March\or April\or May\or June\or
  July\or August\or September\or October\or November\or December\fi
  \space\number\day, \number\year}
\setlength\columnsep{10\p@}
\setlength\columnseprule{0\p@}
\def\сегодня{\number\day\space
                \ifcase\month\or
                 января\or февраля\or марта\or апреля\or мая\or
                 июня\or июля\or августа\or сентября\or октября\or
                 ноября\or декабря\fi\space\number\year}
\frenchspacing
\righthyphenmin 2

\pagestyle{headings}
\pagenumbering{arabic}
\if@twoside
\else
  \raggedbottom
\fi
\if@twocolumn
  \twocolumn
  \sloppy
  \flushbottom
\else
  \onecolumn
\fi
% !!!! Отсюда и до второго вхождения !!!! все сперто из german.sty
\def\@allowhyphens{\penalty\@M\hskip\z@skip}
\def\set@low@box#1{\setbox\tw@\hbox{,}\setbox\z@\hbox{#1}\dimen\z@\ht\z@
     \advance\dimen\z@ -\ht\tw@
     \setbox\z@\hbox{\lower\dimen\z@ \box\z@}\ht\z@\ht\tw@ \dp\z@\dp\tw@ }
%    (this lowers the german left quotes to the same level as the comma.)
\def\@glqq{{\ifhmode \edef\@SF{\spacefactor\the\spacefactor}\else
     \let\@SF\empty \fi \leavevmode
     \set@low@box{''}\box\z@\kern-.04em\@allowhyphens\@SF\relax}}
\def\glqq{\protect\@glqq}
\def\@grqq{\ifhmode \edef\@SF{\spacefactor\the\spacefactor}\else
     \let\@SF\empty \fi \kern-.07em``\kern.07em\@SF\relax}
\def\grqq{\protect\@grqq}
\def\@glq{{\ifhmode \edef\@SF{\spacefactor\the\spacefactor}\else
     \let\@SF\empty \fi \leavevmode
     \set@low@box{'}\box\z@\kern-.04em\@allowhyphens\@SF\relax}}
\def\glq{\protect\@glq}
\def\@grq{\kern-.07em`\kern.07em}
\def\grq{\protect\@grq}
% !!!!
\let\ллл\glqq
\let\плл\grqq
\let\лл\glq
\let\пл\grq

%% 3. РАСШИРЕННАЯ ПОДДЕРЖКА МАТЕМАТИКИ

%
% Следующая проблема - поставить точки после номеров "теорем".
% Мы опять определим \afterthmseparator и инициализируем
% его точкой. Для возврата к стандарту - переопределить
% его на \relax
%
\let\theoremshape\itshape
\def\afterthmseparator{.}
\expandafter\ifx\csname theoremstyle\endcsname\relax
  \def\@begintheorem#1#2{\trivlist\itemindent=\parindent
     \item[\hskip \labelsep{\scshape #1\ #2\unskip\afterthmseparator
  }]\itshape}
  \def\@opargbegintheorem#1#2#3{\trivlist\itemindent=\parindent
        \item[\hskip \labelsep{\scshape #1\ #2\ (#3)\afterthmseparator
  }]\itshape}
\fi

\@addtoreset{equation}{chapter}
% 05-08-95 Вялый: уравнения нумеруются сплошь по статье
\def\theequation{\arabic{equation}}


%
%%%%                    MATH SYMBOLS
%
\def\KK{{\mathbb K}}  \def\QQ{{\mathbb Q}}  \def\ZZ{{\mathbb Z}}  \def\NN{{\mathbb N}}
\def\RR{{\mathbb R}}  \def\CC{{\mathbb C}}  \def\LL{{\mathbb L}}  \def\FF{{\mathbb F}}
\def\Ss{{\mathbb S}}

\newcommand \bydef {\stackrel{\mbox{\scriptsize def}}{=}}
\newcommand \Iff
 {\ifmmode \mathrel{\Longleftrightarrow} \else $\Longleftrightarrow$\fi}
\newcommand \oper[1] {\mathop{\operator@font  #1}\nolimits}
\def\const{\oper{const}}
\def\tan{\oper{tg}}
\def\towhen{\mathop{\to}\limits}
\def\Spec{\oper{Spec}}
\newcommand*{\tg}{\mathop{\operator@font tg}\nolimits}
\newcommand*{\ctg}{\mathop{\operator@font ctg}\nolimits}
\newcommand*{\cosec}{\mathop{\operator@font cosec}\nolimits}
\newcommand*{\arctg}{\mathop{\operator@font arctg}\nolimits}
\newcommand*{\arcctg}{\mathop{\operator@font arcctg}\nolimits}
\newcommand*{\sh}{\mathop{\operator@font sh}\nolimits}
\newcommand*{\ch}{\mathop{\operator@font ch}\nolimits}
\newcommand*{\id}{\mathop{\operator@font id}\nolimits}
\renewcommand*{\Re}{\mathop{\operator@font Re}\nolimits}
\renewcommand*{\Im}{\mathop{\operator@font Im}\nolimits}
\DeclareTextCommand{\th}{OT1}{\mathop{\operator@font th}\nolimits}
\newcommand*{\cth}{\mathop{\operator@font cth}\nolimits}
\let\eps\varepsilon
\let\phi\varphi
\let\vphi\varphi
\DeclareSymbolFont{AMSa}{U}{msa}{m}{n}
\DeclareSymbolFont{AMSb}{U}{msb}{m}{n}
\DeclareMathSymbol{\varkappa}{\mathord}{AMSb}{"7B}
\DeclareMathSymbol{\emptyset}{\mathord}{AMSb}{"3F}
\DeclareMathSymbol{\leqslant}{\mathrel}{AMSa}{"36}
\DeclareMathSymbol{\geqslant}{\mathrel}{AMSa}{"3E}
\let\kkk\varkappa
\let\le\leqslant
\let\leq\leqslant
\let\ge\geqslant
\let\geq\geqslant
%10/02/97 19:32 hack\let\emptyset\varnothing
%
%13/12/96 Вялый: макросы для размещения одного под другим
\def\under#1#2{\def\tempmathop{\mathop{\rm #1}}%
$\tempmathop\limits_{\mbox{\tiny #2}}$}



%%% 4. ОПРЕДЕЛЕНИЕ ПЕРВЫХ СТРАНИЦ (включая состав редколлегии)
\def\titlepage{
\language=1
\@restonecolfalse\if@twocolumn\@restonecoltrue\onecolumn
     \else \newpage \fi \pagestyle{empty}\c@page\z@}

\def\endtitlepage{\if@restonecol\twocolumn \else \newpage \fi
\language=0}

\newbox\titlebox
\font\nf= cminch
\font\hf=cmdunh10 scaled 3432
% Титульные страницы!Вялый 09-05-94 05:30pm
\long\def\maketitle{\begin{titlepage}%
 \let\footnotesize\small
 \let\footnoterule\relax
\setbox\titlebox\vbox to 24cm{
 \vskip 30\p@
\hbox to17cm{\hss\hf МАТЕМАТИЧЕСКОЕ \hss}%
 \vskip 45\p@
\hbox to17cm{\hss\hf ПРОСВЕЩЕНИЕ \hss}%
 \vskip 30\p@
\hbox to17cm{\hfill\rule{15cm}{2pt}\hfill}
 \vskip 1\p@
\hbox to17cm{\hfill\rule[12pt]{5.44cm}{1pt}\hskip3mm
{\Large\sc Третья серия}\hskip3mm
\rule[12pt]{5.44cm}{1pt}\hfill}
\hbox to17cm{\hfill\rule[16pt]{5.44cm}{1pt}\hskip3mm
$\phantom{\mbox{\Large\sc Третья серия}} $\hskip3mm
\rule[16pt]{5.44cm}{1pt}\hfill}
\vfill
\hbox to17cm{\hss\nf\выпуск\hss}%
\vskip 0.2in
\hbox to17cm{\hfill\rule[1.2pt]{6.13cm}{1pt}\hskip3mm
{\Large\sc выпуск }\hskip3mm
\rule[1.2pt]{6.13cm}{1pt}\hfill}
\hbox to17cm{\hfill\rule[10pt]{15cm}{2pt}\hfill}
\vskip30pt
\hbox to17cm{\hss\Huge\Year\hss}%
\vskip100pt
}
%\advance\hoffset by -0.8in
\shipout\box\titlebox%
%\advance\hoffset by 0.8in
 \begin{center}%
   {\Huge МАТЕМАТИЧЕСКОЕ \par}%
   {\Huge ПРОСВЕЩЕНИЕ \par}%
   \vfill{\Large Третья серия\par}\vfill
   {\large\sc выпуск \выпуск\par}
   \vfill\vfill\vfill
\vfill
{\LARGE МЦНМО \Year}\par
\vskip10pt
\end{center}
\newpage
\centerline{\LARGE Редакционная коллегия}\par
\begin{center}
{
\begin{tabular}{lll}
Бугаенко В.О.        &
Васильев Н.Б.        &
Винберг Э.Б.         \\
Вялый М.Н.           &
Глейзер Г.Д.         &
Гусейн-Заде С.М.     \\
Егоров А.А.          &
Ильяшенко Ю.С.       &
Канель-Белов А.Я.    \\
Константинов Н.Н.    &
Розов Н.Х.           &
Савин А.П.           \\
Соловьев Ю.П.        &
Сосинский А.Б.       &
Тихомиров~В.М.       \\
Шарыгин И.Ф.         &
Ященко И.В.          &
\end{tabular}
}
\end{center}
\vfil
\noindent{\sc Адрес  редакции:}

{\tt
\noindent
121002, Москва, Б. Власьевский пер., д.11, к. 211 \\
(с пометкой \лк{}Математическое просвещение\пк{})\\[3mm]
{\sc Email:} matpros@mccme.ru
}
\vfil
\vfil
{\small
\language=0
Полный вариант первого номера новой серии сборника
\лк Математическое просвещение\пк{} (предварительная публикация
первого номера: Математическое просвещение, сер. 3, вып. 1,
М.: изд-во МК~НМУ, 1995).

Предполагается, что сборники новой серии
\лк Математического просвещения\пк{} будут содержать
материалы по следующим разделам:
проблемы современной  математики,
 популярные лекции для
школьников и студентов,
материалы по истории и методологии математики,
проблемы математического образования, научно-методические сообщения,
хроника математической жизни, олимпиады и другие математические
соревнования, задачи и проблемы.
Большинство этих разделов представлено и в данном сборнике.

Хотя сборник и рассчитан на
широкий круг любителей математики: школь\-ни\-ков, студентов, преподавателей,
уровень некоторых статей в н\"ем, впрочем,
требует зна\-чи\-тель\-ных усилий со стороны
читателя. Подобное чтение может быть рекомендовано тем, кто хочет
всерь\"ез
разбираться с математическими вопросами, затрагиваемыми в статьях сборника.


}
\vfil
\vfil
\vfil
\noindent{\bfseries ISBN} 5-900916-15-4\par
\hfill\noindent{\copyright МЦНМО, \Year~г.}\par
%\advance\textheight by-100pt
%\advance\topmargin by30pt

\vfil

\centerline{\parbox{96mm}{
Выпуск данного сборника поддержан грантом\\
Российского Фонда Фундаментальных Исследований\\
(номер проекта 96-01-14087)
}}

\setcounter{page}{2}%
\setcounter{footnote}{0}%
%\let\thanks\relax
\gdef\@thanks{}\gdef\@author{}\gdef\@title{}\let\maketitle\relax
%07/02/97 Вялый: убраны заморочки (далеко не все, к сожалению)
\end{titlepage}%
}

%%ПЕРЕНОСЫ%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\def\выступ{\discretionary{-\kern-1pt}{}{}}
\def\double#1{#1\discretionary{}{#1}{}}

\hyphenation{су-ще-с-т-ву-ет функ-цию ут-ве-р-ж-де-ние
ра-ди-ус ра-ди-уса ра-ди-усом ра-ди-усы ра-ди-уса-ми
все-го стра-те-гии вы-чис-лять сред-не-го
мно-же-ст-во мно-же-ст-ва мно-же-ст-вом мно-же-ст-ву
пе-ри-о-ди-че-ские
ус-ло-ви-ям ус-ло-ви-ями
про-ст-ран-ст-вом про-ст-ран-ст-во про-ст-ран-ст-ва-ми
ес-те-ст-вен-но
ос-нов-ные ос-нов-ной ос-нов-но-му
ок-руж-ность ок-руж-но-сти
об-ра-зо-ва-нию об-ра-зо-ва-ние об-ра-зо-ва-ни-ем  об-ра-зо-ва-ния
}



\endinput
%%
%% End of file `matpros.cls'.