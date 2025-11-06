;;;; latex class template;;;; -*- coding: utf-8-emacs; mode: lisp-data -*-

(("zh-latex-report"
  "\\documentclass{report}
    \\usepackage[usenames]{color}
    \\usepackage{xeCJK}
    \\setCJKmainfont{LXGW WenKai}
    \[DEFAULT-PACKAGES]
    \[PACKAGES]
    \\pagestyle{empty}             % do not remove
    % The settings below are copied from fullpage.sty
    \\setlength{\\textwidth}{\\paperwidth}
    \\addtolength{\\textwidth}{-3cm}
    \\setlength{\\oddsidemargin}{1.5cm}
    \\addtolength{\\oddsidemargin}{-2.54cm}
    \\setlength{\\evensidemargin}{\\oddsidemargin}
    \\setlength{\\textheight}{\\paperheight}
    \\addtolength{\\textheight}{-\\headheight}
    \\addtolength{\\textheight}{-\\headsep}
    \\addtolength{\\textheight}{-\\footskip}
    \\addtolength{\\textheight}{-3cm}
    \\setlength{\\topmargin}{1.5cm}
    \\addtolength{\\topmargin}{-2.54cm}
    \\setcounter{tocdepth}{1}"
  ("\\chapter{%s}" . "\\chapter*{%s}")
  ("\\section{%s}" . "\\section*{%s}")
  ("\\subsection{%s}" . "\\subsection*{%s}")
  ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))

 ("zh-latex-article"
  "\\documentclass{ctexart}
       \\usepackage[usenames]{color}
       \\setCJKmainfont{LXGW WenKai}
       \[DEFAULT-PACKAGES]
       \[PACKAGES]
       \\pagestyle{empty}             % do not remove
       % The settings below are copied from fullpage.sty
       \\setlength{\\textwidth}{\\paperwidth}
       \\addtolength{\\textwidth}{-3cm}
       \\setlength{\\oddsidemargin}{1.5cm}
       \\addtolength{\\oddsidemargin}{-2.54cm}
       \\setlength{\\evensidemargin}{\\oddsidemargin}
       \\setlength{\\textheight}{\\paperheight}
       \\addtolength{\\textheight}{-\\headheight}
       \\addtolength{\\textheight}{-\\headsep}
       \\addtolength{\\textheight}{-\\footskip}
       \\addtolength{\\textheight}{-3cm}
       \\setlength{\\topmargin}{1.5cm}
       \\addtolength{\\topmargin}{-2.54cm}
       \\setcounter{tocdepth}{2}"
  ("\\section{%s}" . "\\section*{%s}")
  ("\\subsection{%s}" . "\\subsection*{%s}")
  ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))

 ("zh-latex-report"
  "\\documentclass{ctexrep}
       \\usepackage[usenames]{color}
       \\setCJKmainfont{LXGW WenKai}
       \[DEFAULT-PACKAGES]
       \[PACKAGES]
       \\pagestyle{empty}             % do not remove
       % The settings below are copied from fullpage.sty
       \\setlength{\\textwidth}{\\paperwidth}
       \\addtolength{\\textwidth}{-3cm}
       \\setlength{\\oddsidemargin}{1.5cm}
       \\addtolength{\\oddsidemargin}{-2.54cm}
       \\setlength{\\evensidemargin}{\\oddsidemargin}
       \\setlength{\\textheight}{\\paperheight}
       \\addtolength{\\textheight}{-\\headheight}
       \\addtolength{\\textheight}{-\\headsep}
       \\addtolength{\\textheight}{-\\footskip}
       \\addtolength{\\textheight}{-3cm}
       \\setlength{\\topmargin}{1.5cm}
       \\addtolength{\\topmargin}{-2.54cm}
       \\setcounter{tocdepth}{2}"
  ("\\chapter{%s}" . "\\chapter*{%s}")
  ("\\section{%s}" . "\\section*{%s}")
  ("\\subsection{%s}" . "\\subsection*{%s}")
  ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))

 ("zh-latex-book"
  "\\documentclass{ctexbook}
       \\usepackage[usenames]{color}
       \\setCJKmainfont{LXGW WenKai}
       \[DEFAULT-PACKAGES]
       \[PACKAGES]
       \\pagestyle{empty}             % do not remove
       % The settings below are copied from fullpage.sty
       \\setlength{\\textwidth}{\\paperwidth}
       \\addtolength{\\textwidth}{-3cm}
       \\setlength{\\oddsidemargin}{1.5cm}
       \\addtolength{\\oddsidemargin}{-2.54cm}
       \\setlength{\\evensidemargin}{\\oddsidemargin}
       \\setlength{\\textheight}{\\paperheight}
       \\addtolength{\\textheight}{-\\headheight}
       \\addtolength{\\textheight}{-\\headsep}
       \\addtolength{\\textheight}{-\\footskip}
       \\addtolength{\\textheight}{-3cm}
       \\setlength{\\topmargin}{1.5cm}
       \\addtolength{\\topmargin}{-2.54cm}
       \\setcounter{tocdepth}{2}"
  ("\\chapter{%s}" . "\\chapter*{%s}")
  ("\\section{%s}" . "\\section*{%s}")
  ("\\subsection{%s}" . "\\subsection*{%s}")
  ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))

 ("ews"
  "\\documentclass[11pt, twoside, hidelinks]{memoir}
        \\setstocksize{9.25in}{7.5in}
        \\settrimmedsize{\\stockheight}{\\stockwidth}{*}
        \\setlrmarginsandblock{1.5in}{1in}{*}
        \\setulmarginsandblock{1in}{1.5in}{*}
        \\checkandfixthelayout
        \\layout
        \\setcounter{tocdepth}{0}
        \\renewcommand{\\baselinestretch}{1.25}
        \\setheadfoot{0.5in}{0.75in}
        \\setlength{\\footskip}{0.8in}
        \\chapterstyle{bianchi}
        \\setsecheadstyle{\\normalfont \\raggedright \\textbf}
        \\setsubsecheadstyle{\\normalfont \\raggedright \\emph}
        \\setsubsubsecheadstyle{\\normalfont\\centering}
        \\pagestyle{myheadings}
        \\usepackage[font={small, it}]{caption}
        \\usepackage{ccicons}
        \\usepackage{ebgaramond}
        \\usepackage[authoryear]{natbib}
        \\bibliographystyle{apalike}
        \\usepackage{svg}
        \\hyphenation{mini-buffer}"
  ("\\chapter{%s}" . "\\chapter*{%s}")
  ("\\section{%s}" . "\\section*{%s}")
  ("\\subsection{%s}" . "\\subsection*{%s}")
  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
