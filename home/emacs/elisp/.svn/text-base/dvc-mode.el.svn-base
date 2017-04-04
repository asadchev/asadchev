;;; dvc-mode.el 
;; This is free software.

;; A mode for editing DVC programs.
;; Modified version of Fred White's dvc-mode.el

;; Copyright (C) 1996 Fred White <fwhite@alum.mit.edu>
;; Copyright (C) 1998 Free Software Foundation, Inc.
;;   (additions by Dave Love)
;; Copyright (C) 2008-2009 Free Software Foundation, Inc.
;;   (additions by Randolph Fritz and Vincent Belaiche (VB1) )

;; Author: Fred White <fwhite@alum.mit.edu>
;; Adapted-by: Dave Love <d.love@dl.ac.uk>
;;           : Kevin Whitefoot <kevin.whitefoot@nopow.abb.no>
;;           : Randolph Fritz <rfritz@u.washington.edu>
;;           : Vincent Belaiche (VB1) <vincentb1@users.sourceforge.net>
;; Version: 1.4.8 (2009-09-29)
;; Serial Version: %Id: 18%
;; Keywords: languages, basic, Evil


;; (Old) LCD Archive Entry:
;; basic-mode|Fred White|fwhite@alum.mit.edu|
;; A mode for editing DVC programs.|
;; 18-Apr-96|1.0|~/modes/basic-mode.el.Z|

;; This file is NOT part of GNU Emacs but the same permissions apply.
;;
;; GNU Emacs  is free software;  you can redistribute it and/or modify
;; it under the terms of  the GNU General  Public License as published
;; by  the Free Software  Foundation;  either version  2, or (at  your
;; option) any later version.
;;
;; GNU  Emacs is distributed  in the hope that  it will be useful, but
;; WITHOUT    ANY  WARRANTY;  without even the     implied warranty of
;; MERCHANTABILITY or FITNESS FOR A  PARTICULAR PURPOSE.  See the  GNU
;; General Public License for more details.
;;
;; You should have received  a copy of  the GNU General Public License
;; along with GNU Emacs; see  the file COPYING.  If  not, write to the
;; Free Software Foundation, 675  Mass Ave, Cambridge, MA 02139,  USA.
;; This  program  is free  software;  you  can  redistribute it and/or
;; modify it  under  the terms of the  GNU  General Public License  as
;; published by the Free Software  Foundation; either version 2 of the
;; License, or (at your option) any later version.

;;; Commentary:

;; Purpose of this package:
;;  This is a mode for editing programs written in The World's Most
;;  Successful Programming Language.  It features automatic
;;  indentation, font locking, keyword capitalization, and some minor
;;  convenience functions.

;; Installation instructions
;;  Put dvc-mode.el somewhere in your path, compile it, and add 
;;  the following to your init file:

;;  (autoload 'dvc-mode "dvc-mode" "DVC mode." t)
;;  (setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\)$" .
;;                                  dvc-mode)) auto-mode-alist))
;;
;;  If you are doing Rhino scripts, add:
;;  (setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\|rvb\\)$" .
;;                                  dvc-mode)) auto-mode-alist))

;;  If you had dvc-mode already installed, you may need to call
;;  dvc-upgrade-keyword-abbrev-table the first time that 
;;  dvc-mode is loaded.

;; Of course, under Windows 3.1, you'll have to name this file
;; something shorter than dvc-mode.el

;; Revisions:
;; 1.0 18-Apr-96  Initial version
;; 1.1 Accomodate emacs 19.29+ font-lock-defaults
;;     Simon Marshall <Simon.Marshall@esrin.esa.it>
;  1.2 Rename to dvc-mode
;; 1.3 Fix some indentation bugs.
;; 1.3+ Changes by Dave Love: [No attempt at compatibility with
;;      anything other than Emacs 20, sorry, but little attempt to
;;      sanitize for Emacs 20 specifically.]
;;      Change `_' syntax only for font-lock and imenu, not generally;
;;      provide levels of font-locking in the current fashion;
;;      font-lock case-insensitively; use regexp-opt with the font-lok
;;      keywords; imenu support; `dvc-split-line', bound to
;;      C-M-j; account for single-statement `if' in indentation; add
;;      keyword "Global"; use local-write-file-hooks, not
;;      write-file-hooks.
;; 1.4 September 1998
;; 1.4 KJW Add begin..end, add extra keywords
;;     Add customisation for single line if.  Disallow by default.
;;     Fix if regexp to require whitespace after if and require then.
;;     Add more VB keywords.  Make begin..end work as if..endif so
;;     that forms are formatted correctly.
;; 1.4.1 KJW Merged Dave Love and KJW versions.
;;     Added keywords suggested by Mickey Ferguson
;;     <MFerguson@peinc.com>
;;     Fixed imenu variable to find private variables and enums

;;     Changed syntax class of =, <, > to punctuation to allow dynamic
;;     abbreviations to pick up only the word at point rather than the
;;     whole expression.

;;     Fixed bug introduced by KJW adding suport for begin...end in
;;     forms whereby a single end outdented.

;;     Partially fixed failure to recognise if statements with
;;     continuations (still fails on 'single line' if with
;;     continuation, ugh).
;; 1.4.2 RF added "class" and "null" keywords, "Rhino" script note.
;; 1.4.3 VB1 added 
;;     1) function dvc-if-not-on-single-line to recognize single line
;;      if statements, even when line is broken.  variable
;;      dvc-allow-single-line-if default set to t again.  
;;     2) use of 'words in calling regexp-opt rather than concat \\< ...\\>
;;     3) new keywords Preserve and Explicit
;; 1.4.4 VB1 added function dvc-close-block
;; 1.4.5 VB1, (expand-abbrev) within (save-excusion...)
;; 1.4.6 VB1 correct dvc-close-block (single line If case)
;; 1.4.7 VB1 correct dvc-close-block (For/Next)
;; 1.4.8 VB1 correct dvc-close-block (Property, + add With /End With)
;;           add command dvc-insert-item

;;
;; Notes:
;; Dave Love
;; BTW, here's a script for making tags tables that I (Dave Love) have
;; used with reasonable success.  It assumes a hacked version of etags
;; with support for case-folded regexps.  I think this is now in the
;; development version at <URL:ftp://fly.cnuce.cnr.it/pub/> and should
;; make it into Emacs after 20.4.

;; #! /bin/sh

;; # etags-vb: (so-called) Visual (so-called) Basic TAGS generation.
;; # Dave Love <d.love@dl.ac.uk>.  Public domain.
;; # 1997-11-21

;; if [ $# -lt 1 ]; then
;;     echo "Usage: `basename $0` [etags options] VBfile ... [etags options] " 1>&2
;;     exit 1
;; fi

;; if [ $1 = "--help" ] || [ $1 = "-h" ]; then
;;     echo "Usage: `basename $0` [etags options] VBfile ... [etags options]

;; "
;;     etags --help
;; fi

;; exec etags --lang=none -c '/\(global\|public\)[ \t]+\(\(const\|type\)[ \t]+\)*\([a-z_0-9]+\)/\4/' \
;;     -c '/public[ \t]+\(sub\|function\|class\)[ \t]+\([a-z_0-9]+\)/\2/' \
;;   "$@"

;; End Notes Dave Love


;; Known bugs:
;;  Doesn't know about ":" separated stmts



;; todo:
;;  fwd/back-compound-statement
;;  completion over OCX methods and properties.
;;  IDE integration
;;  Change behaviour of ESC-q to recognise words used as paragraph
;;  titles and prevent them being dragged into the previous
;;  paragraph.
;;  etc.


;;; Code:

(provide 'dvc-mode)

(defvar dvc-xemacs-p (string-match "XEmacs\\|Lucid" (emacs-version)))
(defvar dvc-winemacs-p (string-match "Win-Emacs" (emacs-version)))
(defvar dvc-win32-p (eq window-system 'w32))

;; Variables you may want to customize.
(defvar dvc-mode-indent 8 "*Default indentation per nesting level.")
(defvar dvc-fontify-p t "*Whether to fontify Basic buffers.")
(defvar dvc-capitalize-keywords-p t
  "*Whether to capitalize BASIC keywords.")
(defvar dvc-wild-files "*.frm *.bas *.cls"
  "*Wildcard pattern for BASIC source files.")
(defvar dvc-ide-pathname nil
  "*The full pathname of your DVC exe file, if any.")
;; VB
(defvar dvc-allow-single-line-if t
  "*Whether to allow single line if")


(defvar dvc-defn-templates
  (list "Public Sub ()\nEnd Sub\n\n"
        "Public Function () As Variant\nEnd Function\n\n"
        "Public Property Get ()\nEnd Property\n\n")
  "*List of function templates though which dvc-new-sub cycles.")

(defvar dvc-imenu-generic-expression
   '((nil "^\\s-*\\(public\\|private\\)*\\s-+\\(declare\\s-+\\)*\\(sub\\|function\\)\\s-+\\(\\sw+\\>\\)"
         4)
    ("Constants"
     "^\\s-*\\(private\\|public\\|global\\)*\\s-*\\(const\\s-+\\)\\(\\sw+\\>\\s-*=\\s-*.+\\)$\\|'"
     3)
    ("Variables"
     "^\\(private\\|public\\|global\\|dim\\)+\\s-+\\(\\sw+\\>\\s-+as\\s-+\\sw+\\>\\)"
     2)
    ("Types" "^\\(public\\s-+\\)*type\\s-+\\(\\sw+\\)" 2)))



(defvar dvc-mode-syntax-table nil)
(if dvc-mode-syntax-table
    ()
  (setq dvc-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\' "\<" dvc-mode-syntax-table) ; Comment starter
  (modify-syntax-entry ?\n ">" dvc-mode-syntax-table)
  (modify-syntax-entry ?\\ "w" dvc-mode-syntax-table)
  (modify-syntax-entry ?\= "." dvc-mode-syntax-table)
  (modify-syntax-entry ?\< "." dvc-mode-syntax-table)
  (modify-syntax-entry ?\> "." dvc-mode-syntax-table)) ; Make =, etc., punctuation so that dynamic abbreviations work properly


(defvar dvc-mode-map nil)
(if dvc-mode-map
    ()
  (setq dvc-mode-map (make-sparse-keymap))
  (define-key dvc-mode-map "\t" 'dvc-indent-line)
  (define-key dvc-mode-map "\r" 'dvc-newline-and-indent)
  (define-key dvc-mode-map "\M-\r" 'dvc-insert-item)
  (define-key dvc-mode-map "\C-c\C-j" 'dvc-insert-item)
  (define-key dvc-mode-map "\M-\C-a" 'dvc-beginning-of-defun)
  (define-key dvc-mode-map "\M-\C-e" 'dvc-end-of-defun)
  (define-key dvc-mode-map "\M-\C-h" 'dvc-mark-defun)
  (define-key dvc-mode-map "\M-\C-\\" 'dvc-indent-region)
  (define-key dvc-mode-map "\M-q" 'dvc-fill-or-indent)
  (define-key dvc-mode-map "\M-\C-j" 'dvc-split-line)
  (define-key dvc-mode-map "\C-c]" 'dvc-close-block)
   (cond (dvc-winemacs-p
         (define-key dvc-mode-map '(control C) 'dvc-start-ide))
        (dvc-win32-p
         (define-key dvc-mode-map (read "[?\\S-\\C-c]") 'dvc-start-ide)))
  (if dvc-xemacs-p
      (progn
        (define-key dvc-mode-map "\M-G" 'dvc-grep)
        (define-key dvc-mode-map '(meta backspace) 'backward-kill-word)
        (define-key dvc-mode-map '(control meta /) 'dvc-new-sub))))


;; These abbrevs are valid only in a code context.
(defvar dvc-mode-abbrev-table nil)

(defvar dvc-mode-hook ())


;; Is there a way to case-fold all regexp matches?
;; Change KJW Add enum, , change matching from 0 or more to zero or one for public etc.
(eval-and-compile
  (defconst dvc-defun-start-regexp
    (concat
     "^[ \t]*\\([Pp]ublic \\|[Pp]rivate \\|[Ss]tatic\\|[Ff]riend \\)?"
     "\\({\\|[Ss]ub\\|[Ff]unction\\|[Pp]roperty +[GgSsLl]et\\|[Tt]ype\\|[Ee]num\\|[Cc]lass\\)"
     "[ \t]+\\(\\w+\\)[ \t]*(?")))

(defconst dvc-defun-end-regexp
  "^[ \t]*[Ee]nd \\([Ss]ub\\|[Ff]unction\\|[Pp]roperty\\|[Tt]ype\\|[Ee]num\\|[Cc]lass\\)")

(defconst dvc-dim-regexp
  "^[ \t]*\\([Cc]onst\\|[Dd]im\\|[Pp]rivate\\|[Pp]ublic\\)\\_>"  )


;; Includes the compile-time #if variation.
;; KJW fixed if to require a whitespace so as to avoid matching, for
;; instance, iFileName and to require then.

;; Two versions; one recognizes single line if just as though it were
;; a multi-line and the other does not.  Modified again to remove the
;; requirement for then so as to allow it to match if statements that
;; have continuations -- VB1 further elaborated on this for single line
;; if statement to be recognized on broken lines.
;;(defconst dvc-if-regexp
;;   "^[ \t]*#?[Ii]f[ \t]+.*[ \t]+[Tt]hen[ \t]*.*\\('\\|$\\)")
(defconst dvc-if-regexp
   "^[ \t]*#?[Ii]f[ \t]+.*[ \t_]+")

(defconst dvc-ifthen-regexp "^[ \t]*#?[Ii]f.+\\<[Tt]hen\\>\\s-\\S-+")

(defconst dvc-else-regexp "^[ \t]*#?[Ee]lse\\([Ii]f\\)?")
(defconst dvc-endif-regexp "[ \t]*#?[Ee]nd[ \t]*[Ii]f")

(defconst dvc-looked-at-continuation-regexp   "_[ \t]*$")

(defconst dvc-continuation-regexp 
  (concat "^.*" dvc-looked-at-continuation-regexp))

(eval-and-compile
  (defconst dvc-label-regexp "^[ \t]*[a-zA-Z0-9_]+:$"))

(defconst dvc-select-regexp "^[ \t]*[Ss]elect[ \t]+[Cc]ase")
(defconst dvc-case-regexp "^[ \t]*[Cc]ase")
(defconst dvc-select-end-regexp "^[ \t]*[Ee]nd[ \t]+[Ss]elect")


(defconst dvc-for-regexp "^[ \t]*[Ff]or\\b")
(defconst dvc-next-regexp "^[ \t]*[Nn]ext\\b")

(defconst dvc-do-regexp "^[ \t]*[Dd]o\\b")
(defconst dvc-loop-regexp "^[ \t]*[Ll]oop\\b")

(defconst dvc-while-regexp "^[ \t]*\\(MENU\\|STATE\\|SCRIPT\\|LIST\\|COMMAND\\|KEYS\\).*{")
(defconst dvc-wend-regexp "^[ \t]*}")

;; Added KJW Begin..end for forms
(defconst dvc-begin-regexp "^[ \t]*\\(MENU\\|STATE\\|SCRIPT\\|LIST\\).*{[ \t]*$")
;; This has created a bug.  End on its own in code should not outdent.
;; How can we fix this?  They are used in separate Lisp expressions so
;; add another one.
(defconst dvc-end-begin-regexp "^[ \t]*\[Ee]nd")

(defconst dvc-with-regexp "^[ \t]*[Ww]ith\\b")
(defconst dvc-end-with-regexp "^[ \t]*[Ee]nd[ \t]+[Ww]ith\\b")

(defconst dvc-blank-regexp "^[ \t]*$")
(defconst dvc-comment-regexp "^[ \t]*\\s<.*$")


;; This is some approximation of the set of reserved words in DVC.
(eval-and-compile
  (defvar dvc-all-keywords
  '("Add" "Aggregate" "And" "App" "AppActivate" "Application" "Array" "As"
    "Asc" "AscB" "Atn" "Attribute"
    "Beep" "Begin" "BeginTrans" "Boolean" "ByVal" "ByRef"
    "CBool" "CByte" "CCur"
    "CDate" "CDbl" "CInt" "CLng" "CSng" "CStr" "CVErr" "CVar" "Call"
    "Case" "ChDir" "ChDrive" "Character" "Choose" "Chr" "ChrB" "Class"
    "ClassModule" "Clipboard" "Close" "Collection" "Column" "Columns"
    "Command" "CommitTrans" "CompactDatabase" "Component" "Components"
    "Const" "Container" "Containers" "Cos" "CreateDatabase" "CreateObject"
    "CurDir" "Currency"
    "DBEngine" "DDB" "Data" "Database" "Databases"
    "Date" "DateAdd" "DateDiff" "DatePart" "DateSerial" "DateValue" "Day"
    "Debug" "Declare" "Deftype" "DeleteSetting" "Dim" "Dir" "Do"
    "DoEvents" "Domain"
    "Double" "Dynaset" "EOF" "Each" "Else" "Empty" "End" "EndProperty"
    "Enum" "Environ" "Erase" "Err" "Error" "Exit" "Exp" "Explicit" "FV" "False" "Field"
    "Fields" "FileAttr" "FileCopy" "FileDateTime" "FileLen" "Fix" "Font" "For"
    "Form" "FormTemplate" "Format" "Forms" "FreeFile" "FreeLocks" "Friend"
    "Function"
    "Get" "GetAllSettings" "GetAttr" "GetObject" "GetSetting" "Global" "GoSub"
    "GoTo" "Group" "Groups" "Hex" "Hour" "IIf" "IMEStatus" "IPmt" "IRR"
    "If" "Implements" "InStr" "Input" "Int" "Integer" "Is" "IsArray" "IsDate"
    "IsEmpty" "IsError" "IsMissing" "IsNull" "IsNumeric" "IsObject" "Kill"
    "LBound" "LCase" "LOF" "LSet" "LTrim" "Left" "Len" "Let" "Like" "Line"
    "Load" "LoadPicture" "LoadResData" "LoadResPicture" "LoadResString" "Loc"
    "Lock" "Log" "Long" "Loop" "MDIForm" "MIRR" "Me" "MenuItems"
    "MenuLine" "Mid" "Minute" "MkDir" "Month" "MsgBox" "NPV" "NPer" "Name"
    "New" "Next" "Not" "Now" "Nothing" "Null" "Object" "Oct" "On" "Open"
    "OpenDatabase"
    "Operator" "Option" "Optional"
    "Or" "PPmt" "PV" "Parameter" "Parameters" "Partition"
    "Picture" "Pmt" "Preserve" "Print" "Printer" "Printers" "Private" 
	"ProjectTemplate" "Property"
    "Properties" "Public" "Put" "QBColor" "QueryDef" "QueryDefs"
    "RSet" "RTrim" "Randomize" "Rate" "ReDim" "Recordset" "Recordsets"
    "RegisterDatabase" "Relation" "Relations" "Rem" "RepairDatabase"
    "Reset" "Resume" "Return" "Right" "RmDir" "Rnd" "Rollback" "RowBuffer"
    "SLN" "SYD" "SavePicture" "SaveSetting" "Screen" "Second" "Seek"
    "SelBookmarks" "Select" "SelectedComponents" "SendKeys" "Set"
    "SetAttr" "SetDataAccessOption" "SetDefaultWorkspace" "Sgn" "Shell"
    "Sin" "Single" "Snapshot" "Space" "Spc" "Sqr" "Static" "Step" "Stop" "Str"
    "StrComp" "StrConv" "String" "Sub" "SubMenu" "Switch" "Tab" "Table"
    "TableDef" "TableDefs" "Tan" "Then" "Time" "TimeSerial" "TimeValue"
    "Timer" "To" "Trim" "True" "Type" "TypeName" "UBound" "UCase" "Unload"
    "Unlock" "Val" "Variant" "VarType" "Verb" "Weekday" "Wend"
    "While" "Width" "With" "Workspace" "Workspaces" "Write" "Year"
    "MENU" "STATE" "LIST" "SCRIPT" "KEYS")))

(defvar dvc-font-lock-keywords-1
  (eval-when-compile
    (list
     ;; Names of functions.
     (list dvc-defun-start-regexp
           '(1 font-lock-keyword-face nil t)
           '(2 font-lock-keyword-face nil t)
           '(3 font-lock-function-name-face))

     ;; Statement labels
     (cons dvc-label-regexp 'font-lock-keyword-face)

     ;; Case values
     ;; String-valued cases get font-lock-string-face regardless.
     (list "^[ \t]*case[ \t]+\\([^'\n]+\\)" 1 'font-lock-keyword-face t)

     ;; Any keywords you like.
     (list (regexp-opt
                          '("Dim" "If" "Then" "Else" "ElseIf" "End If") 'words)
           1 'font-lock-keyword-face))))

(defvar dvc-font-lock-keywords-2
  (append dvc-font-lock-keywords-1
          (eval-when-compile
            `((, (regexp-opt dvc-all-keywords 'words)
                   1 font-lock-keyword-face)))))

(defvar dvc-font-lock-keywords dvc-font-lock-keywords-1)


(put 'dvc-mode 'font-lock-keywords 'dvc-font-lock-keywords)

(defun dvc-mode ()
  "A mode for editing Microsoft DVC programs.
Features automatic indentation, font locking, keyword capitalization,
and some minor convenience functions.
Commands:
\\{dvc-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map dvc-mode-map)
  (setq major-mode 'dvc-mode)
  (setq mode-name "DVC")
  (set-syntax-table dvc-mode-syntax-table)

  (add-hook 'local-write-file-hooks 'dvc-untabify)

  (setq local-abbrev-table dvc-mode-abbrev-table)
  (if dvc-capitalize-keywords-p
      (progn
        (make-local-variable 'pre-abbrev-expand-hook)
        (add-hook 'pre-abbrev-expand-hook 'dvc-pre-abbrev-expand-hook)
        (abbrev-mode 1)))

  (make-local-variable 'comment-start)
  (setq comment-start "# ")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "'+ *")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'comment-end)
  (setq comment-end "")

  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'dvc-indent-line)

  (dvc-enable-font-lock)

  (make-local-variable 'imenu-generic-expression)
  (setq imenu-generic-expression dvc-imenu-generic-expression)

  (set (make-local-variable 'imenu-syntax-alist) `((,(string-to-char "_") . "w")))
  (set (make-local-variable 'imenu-case-fold-search) t)

  ;;(make-local-variable 'dvc-associated-files)
  ;; doing this here means we need not check to see if it is bound later.
  (add-hook 'find-file-hooks 'dvc-load-associated-files)

  (run-hooks 'dvc-mode-hook))


(defun dvc-enable-font-lock () (font-lock-mode 1))

;; KJW should add some odds and bobs here to cover "end if" one way
;; could be to create the abbreviations by removing whitespace then we
;; could put "end if", "end with" and so on in the keyword table
;; Another idea would be to make it intelligent enough to substitute
;; the correct end for the construct (with, select, if)
;; Is this what the abbrev table hook entry is for?
(defun dvc-construct-keyword-abbrev-table ()
  (if dvc-mode-abbrev-table
      nil
    (let ((words dvc-all-keywords)
          (word nil)
          (list nil))
      (while words
        (setq word (car words)
              words (cdr words))
        (setq list (cons (list (downcase word) word) list)))

      (define-abbrev-table 'dvc-mode-abbrev-table list))))

;; Would like to do this at compile-time.
(dvc-construct-keyword-abbrev-table)


(defun dvc-upgrade-keyword-abbrev-table ()
  "Use this in case of upgrading dvc-mode.el"
  (interactive)
  
  (let ((words dvc-all-keywords)
		(word nil)
		(list nil))
	(while words
	  (setq word (car words)
			words (cdr words))
	  (setq list (cons (list (downcase word) word) list)))
	(define-abbrev-table 'dvc-mode-abbrev-table list)))


(defun dvc-in-code-context-p ()
  (if (fboundp 'buffer-syntactic-context) ; XEmacs function.
      (null (buffer-syntactic-context))
    ;; Attempt to simulate buffer-syntactic-context
    ;; I don't know how reliable this is.
    (let* ((beg (save-excursion
                  (beginning-of-line)
                  (point)))
           (list
            (parse-partial-sexp beg (point))))
      (and (null (nth 3 list))          ; inside string.
           (null (nth 4 list))))))      ; inside comment


(defun dvc-pre-abbrev-expand-hook ()
  ;; Allow our abbrevs only in a code context.
  (setq local-abbrev-table
        (if (dvc-in-code-context-p)
            dvc-mode-abbrev-table)))


(defun dvc-newline-and-indent (&optional count)
  "Insert a newline, updating indentation."
  (interactive)
  (save-excursion
    (expand-abbrev)
    (dvc-indent-line))
  (call-interactively 'newline-and-indent))

(defun dvc-beginning-of-defun ()
  (interactive)
  (re-search-backward dvc-defun-start-regexp))

(defun dvc-end-of-defun ()
  (interactive)
  (re-search-forward dvc-defun-end-regexp))

(defun dvc-mark-defun ()
  (interactive)
  (beginning-of-line)
  (dvc-end-of-defun)
  (set-mark (point))
  (dvc-beginning-of-defun)
  (if dvc-xemacs-p
      (zmacs-activate-region)))

(defun dvc-indent-defun ()
  (interactive)
  (save-excursion
    (dvc-mark-defun)
    (call-interactively 'dvc-indent-region)))


(defun dvc-fill-long-comment ()
  "Fills block of comment lines around point."
  ;; Derived from code in ilisp-ext.el.
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((comment-re "^[ \t]*\\s<+[ \t]*"))
      (if (looking-at comment-re)
          (let ((fill-prefix
                 (buffer-substring
                  (progn (beginning-of-line) (point))
                  (match-end 0))))

            (while (and (not (bobp))
                        (looking-at dvc-comment-regexp))
              (forward-line -1))
            (if (not (bobp)) (forward-line 1))

            (let ((start (point)))

              ;; Make all the line prefixes the same.
              (while (and (not (eobp))
                          (looking-at comment-re))
                (replace-match fill-prefix)
                (forward-line 1))

              (if (not (eobp))
                  (beginning-of-line))

              ;; Fill using fill-prefix
              (fill-region-as-paragraph start (point))))))))


(defun dvc-fill-or-indent ()
  "Fill long comment around point, if any, else indent current definition."
  (interactive)
  (cond ((save-excursion
           (beginning-of-line)
           (looking-at dvc-comment-regexp))
         (dvc-fill-long-comment))
        (t
         (dvc-indent-defun))))


(defun dvc-new-sub ()
  "Insert template for a new subroutine.  Repeat to cycle through alternatives."
  (interactive)
  (beginning-of-line)
  (let ((templates (cons dvc-blank-regexp
                         dvc-defn-templates))
        (tem nil)
        (bound (point)))
    (while templates
      (setq tem (car templates)
            templates (cdr templates))
      (cond ((looking-at tem)
             (replace-match (or (car templates)
                                ""))
             (setq templates nil))))

    (search-backward "()" bound t)))


(defun dvc-untabify ()
  "Do not allow any tabs into the file."
  (if (eq major-mode 'dvc-mode)
      (untabify (point-min) (point-max)))
  nil)

(defun dvc-default-tag ()
  (if (and (not (bobp))
           (save-excursion
             (backward-sexp)
             (looking-at "\\w")))
      (backward-word 1))
  (let ((s (point))
        (e (save-excursion
             (forward-sexp)
             (point))))
    (buffer-substring s e)))

(defun dvc-grep (tag)
  "Search BASIC source files in current directory for TAG."
  (interactive
   (list (let* ((def (dvc-default-tag))
                (tag (read-string
                      (format "Grep for [%s]: " def))))
           (if (string= tag "") def tag))))
  (grep (format "grep -n %s %s" tag dvc-wild-files)))



;;; Indentation-related stuff.

(defun dvc-indent-region (start end)
  "Perform dvc-indent-line on each line in region."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (beginning-of-line)
    (while (and (not (eobp))
                (< (point) end))
      (if (not (looking-at dvc-blank-regexp))
          (dvc-indent-line))
      (forward-line 1)))

  (cond ((fboundp 'zmacs-deactivate-region)
         (zmacs-deactivate-region))
        ((fboundp 'deactivate-mark)
         (deactivate-mark))))



(defun dvc-previous-line-of-code ()
  (if (not (bobp))
      (forward-line -1))        ; previous-line depends on goal column
  (while (and (not (bobp))
              (or (looking-at dvc-blank-regexp)
                  (looking-at dvc-comment-regexp)))
    (forward-line -1)))


(defun dvc-find-original-statement ()
  "If the current line is a continuation, move back to the original stmt."
  (let ((here (point)))
    (dvc-previous-line-of-code)
    (while (and (not (bobp))
                (looking-at dvc-continuation-regexp))
      (setq here (point))
      (dvc-previous-line-of-code))
    (goto-char here)))

(defun visual-find-matching-stmt (open-p close-p)
  ;; Searching backwards
  (let ((level 0))
    (while (and (>= level 0) (not (bobp)))
      (dvc-previous-line-of-code)
      (dvc-find-original-statement)
      (cond ((funcall close-p)
             (setq level (+ level 1)))
            ((funcall open-p)
             (setq level (- level 1)))))))

(defun dvc-find-matching-stmt (open-regexp close-regexp)
  (visual-find-matching-stmt 
   (lambda () (looking-at open-regexp))
   (lambda () (looking-at close-regexp))))

(defun dvc-get-complete-tail-of-line ()
  "Return the tail of the current statement line, starting at
  point and going up to end of statement line. If you want the
  complete statement line, you have to call functions
  `dvc-find-original-statement' and then
  `beginning-of-line' before" 
  (let* ((start-point (point)) 
	 complete-line 
	 (line-beg start-point) 
	 line-end)
    (while (null line-end)
      (end-of-line)
      (setq line-end (point))
      (if (search-backward "_" line-beg t)
	  (if (looking-at  dvc-looked-at-continuation-regexp)
	      ;; folded line
	      (progn
		(setq line-end (1- (point))
		      complete-line (cons 
				     (buffer-substring-no-properties 
				      line-beg line-end) 
				     complete-line)
		      line-end nil)
		(beginning-of-line 2)
		(setq line-beg (point)))
	    ;; _ found, but not a folded line (this is a syntax error)
	    (setq complete-line 
		  (cons (buffer-substring-no-properties line-beg line-end) complete-line)))
	;; not a folded line
	(setq complete-line 
	      (cons (buffer-substring-no-properties line-beg line-end) 
		    complete-line))))
    (mapconcat 'identity (nreverse complete-line) " ")))

(defun dvc-if-not-on-single-line ()
  "Return non-`nil' when the If statement is not on a single statement
line, i.e. requires a matching End if. Note that a statement line may
be folded over several code lines."
  (if (looking-at dvc-if-regexp)
      (save-excursion
	(beginning-of-line)
	(let (p1 
	      p2
	      ;; 1st reconstruct complete line
	      (complete-line (dvc-get-complete-tail-of-line)) )

	  ;; now complete line has been reconstructed, drop confusing elements

	  ;; remove any VB string from complete line, as strings may disrupt : and ' detection
	  (while (and (setq p1 (string-match "\"" complete-line))
		      (setq p2 (string-match "\"" complete-line (1+ p1))))
	    (setq complete-line (concat (substring complete-line 0 p1)
					(substring complete-line (1+ p2)))))
	  ;; now drop tailing comment if any
	  (when (setq p1 (string-match "'" complete-line))
	    (setq complete-line (substring complete-line p1)))
	  ;; now drop 1st concatenated instruction is any
	  (when (setq p1 (string-match ":" complete-line))
	    (setq complete-line (substring complete-line p1)))
	  ;;
	  (string-match "Then\\s-*$" complete-line))); end (save-excursion ...)
    ;; else, not a basic if
    nil))

(defun dvc-find-matching-if ()
  (visual-find-matching-stmt 'dvc-if-not-on-single-line
							 (lambda () (looking-at dvc-endif-regexp))))

(defun dvc-find-matching-select ()
  (dvc-find-matching-stmt dvc-select-regexp
                                   dvc-select-end-regexp))

(defun dvc-find-matching-for ()
  (dvc-find-matching-stmt dvc-for-regexp
                                   dvc-next-regexp))

(defun dvc-find-matching-do ()
  (dvc-find-matching-stmt dvc-do-regexp
                                   dvc-loop-regexp))

(defun dvc-find-matching-while ()
  (dvc-find-matching-stmt dvc-while-regexp
                                   dvc-wend-regexp))

(defun dvc-find-matching-with ()
  (dvc-find-matching-stmt dvc-with-regexp
                                   dvc-end-with-regexp))

;;; If this fails it must return the indent of the line preceding the
;;; end not the first line because end without matching begin is a
;;; normal simple statement
(defun dvc-find-matching-begin ()
  (let ((original-point (point)))
    (dvc-find-matching-stmt dvc-begin-regexp
                                     dvc-end-begin-regexp)
    (if (bobp) ;failed to find a matching begin so assume that it is
               ;an end statement instead and use the indent of the
               ;preceding line.
        (progn (goto-char original-point)
               (dvc-previous-line-of-code)))))


(defun dvc-calculate-indent ()
  (let ((original-point (point)))
    (save-excursion
      (beginning-of-line)
      ;; Some cases depend only on where we are now.
      (cond ((or (looking-at dvc-defun-start-regexp)
                 (looking-at dvc-label-regexp)
                 (looking-at dvc-defun-end-regexp))
             0)

            ;; The outdenting stmts, which simply match their original.
            ((or (looking-at dvc-else-regexp)
                 (looking-at dvc-endif-regexp))
             (dvc-find-matching-if)
             (current-indentation))

            ;; All the other matching pairs act alike.
            ((looking-at dvc-next-regexp) ; for/next
             (dvc-find-matching-for)
             (current-indentation))

            ((looking-at dvc-loop-regexp) ; do/loop
             (dvc-find-matching-do)
             (current-indentation))

            ((looking-at dvc-wend-regexp) ; while/wend
             (dvc-find-matching-while)
             (current-indentation))

            ((looking-at dvc-end-with-regexp) ; with/end with
             (dvc-find-matching-with)
             (current-indentation))

            ((looking-at dvc-select-end-regexp) ; select case/end select
             (dvc-find-matching-select)
             (current-indentation))

            ;; A case of a select is somewhat special.
            ((looking-at dvc-case-regexp)
             (dvc-find-matching-select)
             (+ (current-indentation) dvc-mode-indent))

            ;; Added KJW: Make sure that this comes after the cases
            ;; for if..endif, end select because end-regexp will also
            ;; match "end select" etc.
            ((looking-at dvc-end-begin-regexp) ; begin/end
             (dvc-find-matching-begin)
             (current-indentation))

            (t
             ;; Other cases which depend on the previous line.
             (dvc-previous-line-of-code)

             ;; Skip over label lines, which always have 0 indent.
             (while (looking-at dvc-label-regexp)
               (dvc-previous-line-of-code))

             (cond
              ((looking-at dvc-continuation-regexp)
               (dvc-find-original-statement)
               ;; Indent continuation line under matching open paren,
               ;; or else one word in.
               (let* ((orig-stmt (point))
                      (matching-open-paren
                       (condition-case ()
                           (save-excursion
                             (goto-char original-point)
                             (beginning-of-line)
                             (backward-up-list 1)
                             ;; Only if point is now w/in cont. block.
                             (if (<= orig-stmt (point))
                                 (current-column)))
                         (error nil))))
                 (cond (matching-open-paren
                        (1+ matching-open-paren))
                       (t
                        ;; Else, after first word on original line.
                        (back-to-indentation)
                        (forward-word 1)
                        (while (looking-at "[ \t]")
                          (forward-char 1))
                        (current-column)))))
              (t
               (dvc-find-original-statement)

               (let ((indent (current-indentation)))
                 ;; All the various +indent regexps.
                 (cond ((looking-at dvc-defun-start-regexp)
                        (+ indent dvc-mode-indent))

                       ((or (dvc-if-not-on-single-line)
							(and (looking-at dvc-else-regexp)
								 (not (and dvc-allow-single-line-if
										   (looking-at dvc-ifthen-regexp)))))
                        (+ indent dvc-mode-indent))

                       ((or (looking-at dvc-select-regexp)
                            (looking-at dvc-case-regexp))
                        (+ indent dvc-mode-indent))

                       ((or (looking-at dvc-do-regexp)
                            (looking-at dvc-for-regexp)
                            (looking-at dvc-while-regexp)
                            (looking-at dvc-with-regexp)
                            (looking-at dvc-begin-regexp))
                        (+ indent dvc-mode-indent))

                       (t
                        ;; By default, just copy indent from prev line.
                        indent))))))))))

(defun dvc-indent-to-column (col)
  (let* ((bol (save-excursion
                (beginning-of-line)
                (point)))
         (point-in-whitespace
          (<= (point) (+ bol (current-indentation))))
         (blank-line-p
          (save-excursion
            (beginning-of-line)
            (looking-at dvc-blank-regexp))))

    (cond ((/= col (current-indentation))
           (save-excursion
             (beginning-of-line)
             (back-to-indentation)
             (delete-region bol (point))
             (indent-to col))))

    ;; If point was in the whitespace, move back-to-indentation.
    (cond (blank-line-p
           (end-of-line))
          (point-in-whitespace
           (back-to-indentation)))))


(defun dvc-indent-line ()
  "Indent current line for BASIC."
  (interactive)
   (dvc-indent-to-column (dvc-calculate-indent)))


(defun dvc-split-line ()
  "Split line at point, adding continuation character or continuing a comment.
In Abbrev mode, any abbrev before point will be expanded."
  (interactive)
  (let ((pps-list (parse-partial-sexp (save-excursion
                                        (beginning-of-line)
                                        (point))
                                      (point))))
    ;; Dispatch on syntax at this position.
    (cond ((equal t (nth 4 pps-list))  ; in comment
           (indent-new-comment-line))
          ((equal t (nth 4 pps-list))   ; in string
           (error "Can't break line inside a string"))
          (t (just-one-space)           ; leading space on next line
                                        ; doesn't count, sigh
             (insert "_")
             (dvc-newline-and-indent)))))

(defun dvc-detect-idom ()
  "Detects whether this is a VBA or VBS script. Returns symbol
  `vba' if it is VBA, `nil' otherwise" 
  (let (ret)
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(cond
	 ((looking-at "^[ \t]*Attribute\\s-+VB_Name\\s-+= ") (setq ret 'vba)))
	 ))
    ret))

(defun dvc-close-block ()
  "Insert `End If' is current block is a `If Then ...', `End
With' if the block is a `With ...', etc..."
  (interactive)
  (let (end-statement end-indent)
    (save-excursion
      (save-match-data
	(while
	    (unless  (bobp)
	      (dvc-previous-line-of-code)
	      (dvc-find-original-statement)
	      (cond
	       ;; Cases where the current statement is a start-of-smthing statement
	       ((looking-at dvc-defun-start-regexp)
		(let ((smt (match-string 2)))
		  (when (string-match "\\`Prop" smt)
		    (setq smt "Property"))
		  (setq end-statement (concat "End " smt)
			end-indent 0))
		nil)
	       ((looking-at dvc-select-regexp)
		(setq  end-statement "End Select"
		       end-indent (current-indentation))
		nil)
	       ((looking-at dvc-with-regexp)
		(setq  end-statement "End With"
		       end-indent (current-indentation))
		nil)
	       ((looking-at dvc-case-regexp)
		(setq  end-statement  "End Select"
		       end-indent (max 0 (- (current-indentation) dvc-mode-indent)))
		nil)
	       ((looking-at dvc-begin-regexp)
		(setq  end-statement "End"
		       end-indent (current-indentation))
		nil)
	       ((or (dvc-if-not-on-single-line)
		    (looking-at dvc-else-regexp))
		(setq  end-statement "End If"
		       end-indent (current-indentation))
		nil)

	       ((looking-at dvc-do-regexp)
		(setq  end-statement "Loop"
		       end-indent (current-indentation))
		nil)

	       ((looking-at dvc-for-regexp)
		(goto-char (match-end 0))
		(setq  end-statement "Next"
		       end-indent (current-indentation))
		(let ((vb-idom (dvc-detect-idom)))
		  (cond
		   ;; for VBA add the variable name after Next.
		   ((eq vb-idom 'vba)
		    (when (looking-at "\\s-+\\(Each\\s-+\\|\\)\\([^ \t\n\r]+\\)")
		      (setq end-statement (concat end-statement " " (match-string 2)))))))
		nil)
	       ;; Cases where the current statement is an end-of-smthing statement
	       ((or (looking-at dvc-else-regexp)
		    (looking-at dvc-endif-regexp))
		(dvc-find-matching-if)
		t)
	       ((looking-at dvc-next-regexp) ; for/next
		(dvc-find-matching-for)
		t)
	       ((looking-at dvc-loop-regexp) ; do/loop
		(dvc-find-matching-do)
		t)
	       ((looking-at dvc-wend-regexp) ; while/wend
		(dvc-find-matching-while)
		t)
	       ((looking-at dvc-end-with-regexp) ; with/end with
		(dvc-find-matching-with)
		t)
	       ((looking-at dvc-select-end-regexp) ; select case/end select
		(dvc-find-matching-select)
		t)
	       
	       
	       ;; default is to loop again back to previous line of code.
	       (t t))))))
    (when end-statement
      (insert end-statement)
      (dvc-indent-to-column end-indent))))

(defun dvc-insert-item ()
  "Insert a new item in a block. 

This function is under developement, and for the time being only Dim items are handled.

Interting an item means:

* Add a `Case' or `Case Else' into a `Select ... End Select'
  block. Pressing again toggles between `Case' and `Case
  Else'. `Case Else' is possible only if there is not already a `Case Else'.

* split a Dim declaration over several lines.

* Add an `Else' or `ElseIf ... Then' into an `If ... Then ... End
  If' block. Pressing again toggles between `Else' and `ElseIf
  ... Then'. `Else' is possible only if therei s not already an
  `Else'.
"
  (interactive)
  ;; possible cases are
  ;; dim-split-before => split before variable name
  ;; dim-split-after => split after type name if any
  ;; if-with-else
  ;; if-without-else
  ;; select-with-else
  ;; select-without-else
  ;; not-itemizable
  (let (item-case 
	item-ident
	split-point
	cur-point-mark
	prefix
	tentative-split-point
	block-stack (cur-point (point)) previous-line-of-code)
    (save-excursion
      (save-match-data
	(beginning-of-line)
	(while 
	    (progn
	      (dvc-find-original-statement)	      
	      (cond
	       ;; dim case
	       ;;--------------------------------------------------------------
	       ((and (null previous-line-of-code)
		     (looking-at dvc-dim-regexp)
		     (null (save-match-data (looking-at dvc-defun-start-regexp))))
		(setq prefix (buffer-substring-no-properties
			      (point)
			      (goto-char (setq split-point (match-end 0)))))
		(while 
		    (progn
		      (if
			  (looking-at "\\s-*\\sw+\\s-*")
			  (progn
			    (goto-char (setq tentative-split-point (match-end 0)))
			    (if (>= tentative-split-point cur-point)
				  nil
			      (while (or 
				      (looking-at "([^)\n]+)\\s-*")
				      (looking-at dvc-looked-at-continuation-regexp))
				(goto-char (setq tentative-split-point (match-end 0))))
			      (when (looking-at "As\\s-+\\sw+\\s-*")
				(goto-char (setq tentative-split-point (match-end 0))))
			      (when (looking-at dvc-looked-at-continuation-regexp)
				(beginning-of-line 2))
			      (if (looking-at ",")
				  (goto-char (setq split-point (match-end 0)))
				(setq split-point (point))
				nil)))
			nil)))
		(goto-char split-point)
		(setq item-case (if (<= split-point cur-point) 'dim-split-before 'dim-split-after)
		      delta-split-to-cur-point (- split-point cur-point))
		(setq cur-point-mark (make-marker))
		(set-marker cur-point-mark cur-point)
		(looking-at "\\s-*")
		(setq delta-split-to-cur-point (- delta-split-to-cur-point
						  (- (match-end 0) (match-beginning 0))))
		(delete-region (point) (match-end 0))
		(when (looking-back ",")
		  (delete-region split-point (1- split-point)))
		(insert "\n" prefix " ")
		(setq cur-point (marker-position cur-point-mark))
		(set-marker cur-point-mark nil)
		nil)
	       ;; next
	       ((looking-at dvc-next-regexp)
		(push (list 'next) block-stack))
	       ;; default
	       ;;--------------------------------------------------------------
	       (t (if (bobp) 
		      (setq item-case 'not-itemizable)))
	       )
	      (when (null item-case)
		(dvc-previous-line-of-code)
		(setq previous-line-of-code t))
	      (null item-case)))))
    (cond
     ((eq item-case 'dim-split-after)
      (goto-char cur-point))
    )
    ))

;;; Some experimental functions

;;; Load associated files listed in the file local variables block
(defun dvc-load-associated-files ()
  "Load files that are useful to have around when editing the source of the file that has just been loaded.
The file must have a local variable that lists the files to be loaded.
If the file name is relative it is relative to the directory
containing the current buffer.  If the file is already loaded nothing
happens, this prevents circular references causing trouble.  After an
associated file is loaded its associated files list will be
processed."
  (if (boundp 'dvc-associated-files)
      (let ((files dvc-associated-files)
            (file nil))
        (while files
          (setq file (car files)
                files (cdr files))
          (message "Load associated file: %s" file)
          (dvc-load-file-ifnotloaded file default-directory)))))



(defun dvc-load-file-ifnotloaded (file default-directory)
  "Load file if not already loaded.
If file is relative then default-directory provides the path"
  (let((file-absolute (expand-file-name file default-directory)))
    (if (get-file-buffer file-absolute); don't do anything if the buffer is already loaded
        ()
      (find-file-noselect file-absolute ))))


(setq auto-mode-alist (append '(("\\.dvc$" . dvc-mode)) auto-mode-alist))


;;; dvc-mode.el ends here


;External Links
;* [http://visualbasic.freetutes.com/ DVC tutorials]

