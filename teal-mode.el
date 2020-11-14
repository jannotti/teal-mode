;;; teal-mode.el --- mode for editing Algorand's TEAL language  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Algorand, Inc

;; Author: John Jannotti <jj@jannotti.com>
;; Maintainer: jj@jannotti.com
;; Keywords: languages

;;; Commentary:

;; A TEAL mode to make editing TEAL a bit easier.  It indents opcodes,
;; and outdents branck labels and pragmas.  It autocompletes many of
;; the builtin constants that are used with specific opcodes, and
;; labels when used as the argument to a branch, or as an identifier
;; starting in column 0.

;;; Code:

(defconst teal-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; " is a string delimiter
    (modify-syntax-entry ?\" "\"" table)

    ;; / is punctuation, but // is a comment starter
    (modify-syntax-entry ?/ ". 12" table)

    ;; \n is a comment ender
    (modify-syntax-entry ?\n ">" table)
    table))

(defvar teal-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for Teal mode.")

(defvar teal-pragmas
  '("version"))

(defvar teal-txna-fields
  '("ApplicationArgs" "Accounts"))

(defvar teal-txn-fields
  '("Sender" "Fee"
    "FirstValid" "FirstValidTime"
    "LastValid"
    "Note"
    "Lease"
    "Receiver"
    "Amount"
    "CloseRemainderTo"
    "VotePK" "SelectionPK"
    "VoteFirst" "VoteLast" "VoteKeyDilution"
    "Type" "TypeEnum"
    "XferAsset"
    "AssetAmount" "AssetSender" "AssetReceiver" "AssetCloseTo"
    "GroupIndex" "TxID"
    "ApplicationID" "OnCompletion" "ApplicationArgs" "NumAppArgs"
    "Accounts" "NumAccounts"
    "ApprovalProgram" "ClearStateProgram"
    "RekeyTo"
    "ConfigAsset" "ConfigAssetTotal" "ConfigAssetDecimals"
    "ConfigAssetDefaultFrozen" "ConfigAssetUnitName"
    "ConfigAssetName" "ConfigAssetURL" "ConfigAssetMetadataHash"
    "ConfigAssetManager" "ConfigAssetReserve" "ConfigAssetFreeze"
    "ConfigAssetClawback"
    "FreezeAsset" "FreezeAssetAccount" "FreezeAssetFrozen"
    ))

(defvar teal-txn-types
  '("unknown" "pay" "keyreg" "acfg" "axfer" "afrz" "appl"))

(defvar teal-global-fields
  '("MinTxnFee" "MinBalance" "MaxTxnLife"
    "ZeroAddress" "GroupSize" "LogicSigVersion"
    "Round" "LatestTimestamp"
    "CurrentApplicationID"))

(defvar teal-asset-holding-fields
  '("AssetBalance" "AssetFrozen"))

(defvar teal-asset-params-fields
  '("AssetTotal" "AssetDecimals" "AssetDefaultFrozen"
    "AssetUnitName" "AssetName" "AssetURL"
    "AssetMetadataHash" "AssetManager"
    "AssetReserve" "AssetFreeze" "AssetClawback"
    ))

(defvar teal-on-completions
  '("NoOp" "OptIn" "CloseOut" "UpdateApplication" "DeleteApplication" "ClearState"))

(defvar teal-constants
  `(,@teal-txn-fields
    ,@teal-txn-types
    ,@teal-global-fields
    ,@teal-asset-holding-fields
    ,@teal-asset-params-fields
    ,@teal-on-completions))

(defvar teal-branch-opcodes '("bnz" "bz" "b"))
(defvar teal-opcodes
  `("err"
    "sha256" "keccak256" "sha512_256" "ed25519verify"
    "+" "-" "/" "*" "<" ">" "<=" ">=" "&&" "||" "==" "!=" "!"
    "len"
    "itob" "btoi"
    "%" "|" "&" "^" "~"
    "mulw" "addw"
    "int" "intcblock" "intc" "intc_0" "intc_1" "intc_2" "intc_3"
    "byte" "bytecblock" "bytec" "bytec_0" "bytec_1" "bytec_2" "bytec_3"
    "arg" "arg_0" "arg_1" "arg_2" "arg_3"
    "txn" "gtxn"
    "global"
    "load" "store"
    "txna" "gtxna"
    ,@teal-branch-opcodes
    "return"
    "pop"
    "dup"
    "dup2"
    "concat"
    "substring"
    "substring3"
    "balance"
    "app_opted_in"
    "app_local_get" "app_local_get_ex"
    "app_global_get" "app_global_get_ex"
    "app_local_put" "app_global_put"
    "app_local_del" "app_global_del"
    "asset_holding_get" "asset_params_get"
    ))

(defconst teal-font-lock-defaults
  `((
     ("^\\(\\(\\sw\\|\\s_\\)+\\)\\>:" (1 font-lock-function-name-face)) ; labels
     ("\\<\\(b\\|bz\\|bnz\\)\\>\\s +\\(\\(\\sw\\|\\s_\\)+\\)" (2 font-lock-function-name-face)) ; jump targets
     ( ,(regexp-opt teal-constants 'words) . font-lock-constant-face)
     ( ,(regexp-opt teal-opcodes 'words) . font-lock-keyword-face)
     ,@cpp-font-lock-keywords)))        ; handles #pragma


(defun teal-indent-line ()
  "Auto-indent the current line."
  (interactive)
  (let* ((savep (point))
	 (indent (condition-case nil
		     (save-excursion
		       (forward-line 0)
		       (skip-chars-forward " \t")
		       (if (>= (point) savep) (setq savep nil))
		       (max (teal-calculate-indentation) 0))
		   (error 0))))
    (if savep
	(save-excursion (indent-line-to indent))
      (indent-line-to indent))))

(defun teal-calculate-indentation ()
  "Return the column for indentation of current line."
  (or
   ;; Flush labels to the left margin.
   (and (looking-at "\\(\\sw\\|\\s_\\)+:") 0)
   ;; Same thing for #
   (and (looking-at "#") 0)
   ;; The rest goes at the first tab stop.
   (indent-next-tab-stop 0)))

;; Interesting place one can be in a TEAL program
;; In a pragma
;; Typing an opcode
;; After an opcode
;; Typing an arg to opcode
;; After an arg to an opcode
;; typing a label


(defun teal-simple-completions (bounds words)
  "Bundle BOUNDS and a word list WORDS into format needed by capf."
  (list (car bounds) (cdr bounds) words :exclusive 'yes))

(defun teal-functional-completions (bounds wordfunc)
  "Bundle BOUNDS and a words generated by WORDFUNC into format needed by capf."
  ;; 
  (teal-simple-completions bounds (completion-table-dynamic
                                   (lambda (_)
                                     (funcall wordfunc)))))

(defun teal-matches-in-buffer (regexp &optional re-group buffer)
  "Return RE-GROUP of each match of REGEXP in BUFFER."
  (let ((matches))
    (save-match-data
      (save-excursion
        (with-current-buffer (or buffer (current-buffer))
          (save-restriction
            (widen)
            (goto-char (point-max))
            ;; search backwards since resulting list will be reversed
            (while (search-backward-regexp regexp nil t 1)
              (push (match-string-no-properties (or re-group 0)) matches)))))
      matches)))

(defun teal-find-branch-targets ()
  "Find tokens being used as labels are arguments to branch instructions."
  (append
   (teal-matches-in-buffer "^\\(\\(\\sw\\|\\s_\\)+\\):" 1)
   (teal-matches-in-buffer (concat (regexp-opt teal-branch-opcodes 'symbols) " \\(\\(\\sw\\|\\s_\\)+\\)") 2)))

(defun teal-completion-at-point ()
  "Provide completions for opcode names and/or arguments to opcodes."
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
         (in-symbol-p (not (not bounds)))
         (bounds (or bounds (cons (point) (point))))
         (bolp (= (line-beginning-position) (car bounds)))
         (prefix (buffer-substring-no-properties (line-beginning-position) (point)))
         (tokens (split-string prefix))
         (position (- (length tokens) (if in-symbol-p 1 0))))
    (cond
     (bolp (teal-functional-completions bounds #'(lambda () (mapcar (lambda (s) (concat s ":")) (teal-find-branch-targets)))))
     ((and (= 0 position)) (teal-simple-completions bounds teal-opcodes))
     ((and (= 1 position) (string= (car tokens) "txn"))  (teal-simple-completions bounds teal-txn-fields))
     ((and (= 1 position) (string= (car tokens) "txna"))  (teal-simple-completions bounds teal-txna-fields))
     ((and (= 2 position) (string= (car tokens) "gtxn"))  (teal-simple-completions bounds teal-txn-fields))
     ((and (= 2 position) (string= (car tokens) "gtxna"))  (teal-simple-completions bounds teal-txna-fields))
     ((string= (car tokens) "global")  (teal-simple-completions bounds teal-global-fields))
     ((string= (car tokens) "int")  (teal-simple-completions bounds teal-constants))
     ((string= (car tokens) "asset_holding_get")  (teal-simple-completions bounds teal-asset-holding-fields))
     ((string= (car tokens) "asset_params_get")  (teal-simple-completions bounds teal-asset-params-fields))
     ((and (= 1 position) (string= (car tokens) "#pragma"))  (teal-simple-completions bounds teal-pragmas))
     ((member (car tokens) teal-branch-opcodes)  (teal-functional-completions bounds #'teal-find-branch-targets))
     (t nil))))

(define-derived-mode teal-mode prog-mode "TEAL Mode"
  "Major mode for editing Algorand's TEAL code.
\\{teal-mode-map}"
  (setq-local font-lock-defaults teal-font-lock-defaults)
  (setq-local indent-line-function #'teal-indent-line)
  (setq-local tab-always-indent 'complete)
  (setq-local backward-delete-char-untabify-method 'hungry)
  (setq-local electric-indent-chars (append electric-indent-chars '(?: ?#)))
  (add-hook 'completion-at-point-functions #'teal-completion-at-point)
  (setq comment-start "// ")
  (setq comment-end "")
  )


(provide 'teal-mode)

;;; teal-mode.el ends here
