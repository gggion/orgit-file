;;; orgit-file.el --- Support for links to files in Git repositories  -*- lexical-binding:t -*-

;; Author: Gino Cornejo
;; Mantainer: Gino Cornejo <gggion123@gmail.com>
;; Homepage: https://github.com/gggion/orgit-file
;; Keywords: hypermedia vc

;; Package-Version: 0.3.0
;; Package-Requires: ((emacs  "29.1") (compat "30.1") (magit "4.3") (org "9.7") (orgit "2.0"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package defines the Org link type `orgit-file', which can be
;; used to link to files in Git repositories at specific revisions.
;;
;; The package integrates with Magit to allow storing links from both
;; regular file buffers and Magit blob-mode buffers (when viewing
;; historical file revisions).
;;
;; Format
;; ------
;;
;; The link type defined here takes this form:
;;
;;    orgit-file:/path/to/repo/::REV::FILE  links to FILE at REV
;;
;; You can optionally add a search option after FILE:
;;
;;    orgit-file:/path/to/repo/::REV::FILE::SEARCH
;;
;; Where SEARCH is any Org search string (e.g., a heading, custom ID,
;; line number, or regex).
;;
;; Export
;; ------
;;
;; When an Org file containing such links is exported, then the url of
;; the remote configured with `orgit-remote' is used to generate a web
;; url according to `orgit-export-alist'.  That webpage should present
;; the file content at the specified revision.
;;
;; Both the remote to be considered the public remote, as well as the
;; actual web urls can be defined in individual repositories using Git
;; variables.
;;
;; To use a remote different from `orgit-remote' but still use
;; `orgit-export-alist' to generate the web urls, use:
;;
;;    git config orgit.remote REMOTE-NAME
;;
;; To explicitly define the web url for files, use something like:
;;
;;    git config orgit.file http://example.com/repo/blob/%r/%f
;;
;; Where %r is replaced with the revision and %f with the file path.

;;; Code:

(require 'compat)
(require 'orgit)
(require 'org-element)

(eval-when-compile (require 'subr-x))

;;; Options

(defgroup orgit-file nil
  "Org links to files in Git repositories."
  :group 'magit-extensions
  :group 'org-link)

(defcustom orgit-file-abbreviate-revisions nil
  "Whether to use abbreviated revision hashes in stored links.

When non-nil, use `magit-rev-abbrev' to shorten revision hashes
in both the link path.  When nil, use full
40-character SHA-1 hashes.

Abbreviated hashes are more readable but may become ambiguous in
very large repositories with many commits.  The abbreviated form
is sufficient for most uses and matches the behavior of similar
tools."
  :group 'orgit-file
  :type 'boolean
  :package-version '(orgit-file . "0.1.0"))

(defcustom orgit-file-link-to-file-use-orgit nil
  "Non-nil means storing a link to a file in a Git repo will use orgit-file links.

The variable can have the following values:

prefix-to-disable
      Always create orgit-file links when in a Git repository,
      unless `org-store-link' is called with a prefix argument.

prefix-to-enable
      Only create orgit-file links when `org-store-link' is called
      with a prefix argument.  Without prefix, fall back to file:
      links.

blob-buffers-only
      Only create orgit-file links in `magit-blob-mode' buffers
      (when viewing historical file revisions).  Regular file
      buffers will fall back to file: links.

nil   Never create orgit-file links automatically.  Users can still
      call `orgit-file-store' interactively to create orgit-file
      links explicitly."
  :group 'orgit-file
  :type '(choice
          (const :tag "Always use orgit-file links (C-u to disable)"
                 prefix-to-disable)
          (const :tag "Only with prefix argument (C-u to enable)"
                 prefix-to-enable)
          (const :tag "Only in magit-blob-mode buffers" blob-buffers-only)
          (const :tag "Never use orgit-file links automatically" nil))
  :package-version '(orgit-file . "0.2.0"))

(defcustom orgit-file-export-alist
  `(("github.com[:/]\\(.+?\\)\\(?:\\.git\\)?$"
     "https://github.com/%n/blob/%r/%f")
    ("gitlab.com[:/]\\(.+?\\)\\(?:\\.git\\)?$"
     "https://gitlab.com/%n/-/blob/%r/%f")
    ("codeberg.org[/:@]\\(.+?\\)\\(?:\\.git\\)?$"
     "https://codeberg.org/%n/src/commit/%r/%f")
    ("git.sr.ht[:/]\\(.+?\\)\\(?:\\.git\\)?$"
     "https://git.sr.ht/%n/tree/%r/item/%f")
    ("bitbucket.org[:/]\\(.+?\\)\\(?:\\.git\\)?$"
     "https://bitbucket.org/%n/src/%r/%f"))
  "Alist used to translate Git URLs to web URLs for file links.

Each entry has the form (REMOTE-REGEXP FILE-TEMPLATE).  If
REMOTE-REGEXP matches the URL of the chosen remote, then
FILE-TEMPLATE is used to generate the web URL.

The first submatch of REMOTE-REGEXP must match the repository
identifier.  The template must contain %n (repository name),
%r (revision), and %f (file path).

Line numbers or ranges are automatically appended as URL fragments
in the format #L43 or #L43-L58 after the template is expanded.

This can be overridden per-repository using:
    git config orgit.file http://example.com/repo/blob/%r/%f"
  :group 'orgit-file
  :type '(repeat (list :tag "Remote template"
                       (regexp :tag "Remote regexp")
                       (string :tag "File URL format")))
  :package-version '(orgit-file . "0.1.0"))

(defcustom orgit-file-export-text-fragments nil
  "Whether to export text search patterns as URL text fragments.

When non-nil, orgit-file links with text search patterns (not line
numbers) are exported with #:~:text= URL fragments.  This enables
browsers to scroll to and highlight the matching text.

Text fragments are part of the WICG Text Fragments specification,
currently supported by Chromium-based browsers (Chrome, Edge, Brave)
and Safari.  Firefox support is in development.  Browsers without
support ignore the fragment, showing the file without highlighting.

When nil, text search patterns in links are not included in exported
URLs.  Only line numbers and line ranges generate URL fragments.

This setting does not affect link following behavior within Emacs,
which always uses Org's search mechanism regardless of this setting."
  :group 'orgit-file
  :type 'boolean
  :package-version '(orgit-file . "0.2.0"))

(defcustom orgit-file-export-preview-format 'url-only
  "Default export format for `orgit-file-export-link-at-point'.

Valid values are symbols corresponding to Org export backends:
  `html'     - Export as HTML link
  `md'       - Export as Markdown link
  `latex'    - Export as LaTeX link
  `ascii'    - Export as plain text
  `url-only' - Export URL only without markup

When `orgit-file-export-link-at-point' is called with a prefix
argument, prompt for format interactively."
  :group 'orgit-file
  :type '(choice
          (const :tag "HTML link" html)
          (const :tag "Markdown link" md)
          (const :tag "LaTeX link" latex)
          (const :tag "Plain text" ascii)
          (const :tag "URL only" url-only))
  :package-version '(orgit-file . "0.3.0"))

(defcustom orgit-file-description-use-readable-revisions t
  "Whether to use human-readable revision names in link descriptions.

When non-nil, show tags and branch names in descriptions when
available.  For example, \"v1.0.0\" or \"main\" instead of
abbreviated hashes like \"a0f3651\".

When nil, always show abbreviated hashes in descriptions,
regardless of whether the revision is a tag or branch.

This setting only affects link descriptions, not the links
themselves.  Link format is controlled by
`orgit-file-abbreviate-revisions'.

Examples with non-nil (default):
  \"emacs (v29.1): lisp/org.el\"
  \"emacs (main): lisp/org.el\"

Examples with nil:
  \"emacs (a0f3651): lisp/org.el\"
  \"emacs (73347ef): lisp/org.el\""
  :package-version '(orgit-file . "0.4.0")
  :group 'orgit-file
  :type 'boolean)

(defcustom orgit-file-description-format "%%N (%%R): %%F%%S"
  "Format string for `orgit-file' link descriptions.

The format uses two-pass expansion:

First pass: git-show(1) pretty format specs (%s, %an, %ad, etc.).
Quote specs for second pass with double percent: %%N.

Second pass: Custom specs for orgit-file components:
  %%N  Repository name or path
  %%R  Revision (format controlled by
       `orgit-file-description-use-readable-revisions')
  %%F  File path relative to repository root
  %%S  Search option (line numbers or text) with separator

Examples:
  \"%%N (%%R): %%F%%S\"  → \"emacs (v29.1): lisp/org.el (line 42)\"
  \"%%F at %%R\"         → \"lisp/org.el at main\"
  \"[%%N] %s - %%F%%S\"  → \"[emacs] Add feature - lisp/org.el\"

See git-show(1) manpage for available first-pass specs."
  :package-version '(orgit-file . "0.4.0")
  :group 'orgit-file
  :type 'string)



;;; File links

;;;###autoload
(with-eval-after-load 'org
  (with-eval-after-load 'magit
    (org-link-set-parameters "orgit-file"
                             :store    #'orgit-file-store
                             :follow   #'orgit-file-open
                             :export   #'orgit-file-export
                             :complete #'orgit-file-complete-link)))

;;;; Helper Functions
(defun orgit-file--format-revision-for-description (rev)
  "Return revision string for REV according to user preference.

When `orgit-file-description-use-readable-revisions' is non-nil,
prefer tags, then branches, then abbreviated hashes.

When nil, always return abbreviated hash."
  (if orgit-file-description-use-readable-revisions
      (or (and (magit-ref-p rev) rev)
          (magit-name-tag rev)
          (magit-name-branch rev)
          (magit-rev-abbrev rev))
    (magit-rev-abbrev rev)))

(defun orgit-file--format-line-fragment (url line-start line-end)
  "Format line number fragment for URL based on hosting service.

URL is the base URL without fragment.  LINE-START and LINE-END are
integers representing the line range.  If LINE-END equals LINE-START,
format as single line.

Different hosting services use different fragment syntaxes:
- GitHub, GitLab, Codeberg: #L43-L58
- Sourcehut: #L43-58
- Bitbucket: #lines-43:58

Return fragment string including leading #."
  (cond
   ;; Bitbucket uses #lines-43:58 format
   ((string-match-p "bitbucket\\.org" url)
    (if (and line-end (not (= line-start line-end)))
        (format "#lines-%d:%d" line-start line-end)
      (format "#lines-%d" line-start)))
   ;; Sourcehut uses #L43-58 format (no L prefix on end line)
   ((string-match-p "git\\.sr\\.ht" url)
    (if (and line-end (not (= line-start line-end)))
        (format "#L%d-%d" line-start line-end)
      (format "#L%d" line-start)))
   ;; GitHub, GitLab, Codeberg use #L43-L58 format
   (t
    (if (and line-end (not (= line-start line-end)))
        (format "#L%d-L%d" line-start line-end)
      (format "#L%d" line-start)))))

(defun orgit-file--url-encode-text-fragment (text)
  "Encode TEXT for use in URL text fragment.

Text fragments use percent-encoding following RFC 3986. This function
encodes TEXT with a restricted set of allowed characters appropriate
for the :~:text= fragment syntax.

Only unreserved characters (A-Z, a-z, 0-9, and the marks . ~) plus
space and parentheses are preserved. All other characters, including
parentheses, are percent-encoded.

The Text Fragments specification requires this encoding to ensure
proper parsing and matching across different browsers."
  (let ((allowed-chars (url--allowed-chars
                        '(?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M
                          ?N ?O ?P ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z
                          ?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m
                          ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z
                          ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9
                          ?\( ?\) ?. ?~ ?\s))))
    (url-hexify-string text allowed-chars)))

(defun orgit-file--detect-search-option ()
  "Detect appropriate search option from active region.

When region is active, return one of:
- Line number string \"N\" if single complete line selected
- Line range string \"N-M\" if multiple complete lines selected
- Selected text if partial line selection

Return nil if no region is active.

Complete line selection means region starts at beginning of line
and ends at beginning of next line (or end of buffer).  This
matches the behavior of similar line-wise selection modes."
  (when (use-region-p)
    (let ((beg (region-beginning))
          (end (region-end)))
      (save-excursion
        (goto-char beg)
        (let ((beg-at-bol (bolp))
              (beg-line (line-number-at-pos beg)))
          (goto-char end)
          (let ((end-at-bol (bolp))
                (end-line (line-number-at-pos end)))
            ;; Check if selection spans complete lines
            (if (and beg-at-bol end-at-bol)
                ;; Complete line(s) selected
                (let ((last-line (if (= end (point-max))
                                     end-line
                                   (1- end-line))))
                  (if (= beg-line last-line)
                      ;; Single line
                      (number-to-string beg-line)
                    ;; Multiple lines
                    (format "%d-%d" beg-line last-line)))
              ;; Partial line selection - use text as search string
              (buffer-substring-no-properties beg end))))))))

;;;; Main Functions

;;;###autoload
(defun orgit-file-store ()
  "Store link to file in current Magit buffer.

In `magit-blob-mode' buffers, store link to the displayed revision.
In regular file buffers within Git repositories, store link to HEAD.

When region is active:
- Complete lines (from bol to bol): store line number or range
- Partial selection: store selected text as search pattern

Link format is controlled by `orgit-file-abbreviate-revisions'.
Description format is controlled by `orgit-file-description-format'.

Storage behavior depends on `orgit-file-link-to-file-use-orgit':
- nil: never store automatically (call this function explicitly)
- `blob-buffers-only': store only in `magit-blob-mode'
- `prefix-to-enable': store only with \\[universal-argument]
- `prefix-to-disable': always store (\\[universal-argument] to disable)

When called interactively, always store link regardless of settings.
Link is added to `org-stored-links' for `org-insert-link'.

Return non-nil if link was stored."
  (interactive)
  ;; Only proceed if we're in a Git repository
  (when-let* ((repo (magit-toplevel)))
    ;; Determine storage context
    (let* ((in-blob-buffer (bound-and-true-p magit-blob-mode))
           (called-interactively (called-interactively-p 'interactive))
           (has-prefix current-prefix-arg)
           ;; Decide whether to store based on context and settings.
           ;; Interactive calls always store. Non-interactive calls
           ;; check orgit-file-link-to-file-use-orgit and prefix arg.
           (should-store
            (cond
             ;; When called interactively, always try to store
             (called-interactively t)
             ;; When called from org-store-link, check settings
             (t
              (cond
               ;; Skip if prefix arg given (only for prefix-to-disable
               ;; and blob-buffers-only modes)
               ((and has-prefix
                     (memq orgit-file-link-to-file-use-orgit
                           '(prefix-to-disable blob-buffers-only)))
                nil)

               ;; Require prefix arg to enable
               ((eq orgit-file-link-to-file-use-orgit 'prefix-to-enable) has-prefix)
               ;; Always store unless prefix arg disables
               ((eq orgit-file-link-to-file-use-orgit 'prefix-to-disable) t)
               ;; Store only in magit-blob-mode buffers
               ((eq orgit-file-link-to-file-use-orgit 'blob-buffers-only) in-blob-buffer)
               ;; nil: don't store automatically
               (t nil))))))

      (when should-store
        ;; Extract file path relative to repository root
        (let ((file (cond
                     ;; In blob buffers, use magit-buffer-file-name
                     ((bound-and-true-p magit-blob-mode)
                      (magit-file-relative-name magit-buffer-file-name))
                     ;; In regular file buffers, use buffer-file-name
                     (buffer-file-name
                      (magit-file-relative-name buffer-file-name))))
              ;; Determine revision: use magit-buffer-revision in blob
              ;; buffers, otherwise use HEAD
              (rev (or (and (bound-and-true-p magit-blob-mode)
                            magit-buffer-revision)
                       (and buffer-file-name
                            (magit-rev-parse "HEAD")))))
          ;; Only proceed if we have both file and revision
          (when (and file rev)
            (let* ((repo-id (orgit--current-repository))
                   ;; Revision for description: format based on
                   ;; orgit-file-description-use-readable-revisions.
                   ;; When t: tag > branch > abbreviated hash.
                   ;; When nil: always abbreviated hash.
                   (rev-for-link (if orgit-file-abbreviate-revisions
                                     (magit-rev-abbrev rev)
                                   rev))
                   ;; Revision for description: human-readable form
                   ;; (tag > branch > abbreviated hash)
                   (rev-for-desc (orgit-file--format-revision-for-description rev))
                   ;; Detect search option from active region
                   (search-option (orgit-file--detect-search-option))
                   ;; Variables for parsed line numbers
                   (line-start nil)
                   (line-end nil)
                   ;; Formatted search description for link description
                   (search-desc nil))

              ;; Parse search option to detect line numbers and format
              ;; description. Line numbers get special formatting like
              ;; " (line 42)" or " (lines 10-20)". Text searches get
              ;; formatted as " (search text)".
              (when search-option
                (cond
                 ;; Line range: "43-58"
                 ((string-match "\\`\\([0-9]+\\)-\\([0-9]+\\)\\'" search-option)
                  (setq line-start (string-to-number (match-string 1 search-option))
                        line-end (string-to-number (match-string 2 search-option))
                        search-desc (format " (lines %d-%d)" line-start line-end)))
                 ;; Single line: "43"
                 ((string-match "\\`\\([0-9]+\\)\\'" search-option)
                  (setq line-start (string-to-number (match-string 1 search-option))
                        line-end line-start
                        search-desc (format " (line %d)" line-start)))
                 ;; Text search: anything else
                 (t
                  (setq search-desc (format " (%s)" search-option)))))

              ;; Construct the orgit-file link
              (let ((link (if search-option
                              (format "orgit-file:%s::%s::%s::%s"
                                      repo-id rev-for-link file search-option)
                            (format "orgit-file:%s::%s::%s"
                                    repo-id rev-for-link file))))
                ;; Store link properties for org-mode
                (org-link-store-props
                 :type "orgit-file"
                 :link link
                 ;; Generate description using two-pass format-spec.
                 ;; First pass: magit-rev-format expands git-show specs.
                 ;; Second pass: format-spec expands our custom specs.
                 :description (format-spec
                               (magit-rev-format orgit-file-description-format rev)
                               `((?N . ,repo-id)
                                 (?R . ,rev-for-desc)
                                 (?F . ,file)
                                 (?S . ,(or search-desc "")))))
                ;; When called interactively, add to org-stored-links
                ;; so user can insert with org-insert-link
                (when called-interactively
                  (org-link--add-to-stored-links link nil)
                  (message "Stored: %s" link))
                ;; Return non-nil to indicate success
                t))))))))

;;;###autoload
(defun orgit-file-open (path)
  "Open orgit-file link at PATH.

PATH format: REPO::REV::FILE-PATH or REPO::REV::FILE-PATH::SEARCH

Navigate to the specified file at the given revision in the
repository.  If SEARCH is provided, search for that string or
pattern in the file after opening.  If SEARCH is a line number
or line range, jump to that line."
  (pcase-let* ((`(,repo ,rev ,file-path ,search-option ,line-start ,_line-end)
                (orgit-file--parse-path path))
               (default-directory (orgit--repository-directory repo)))
    (magit-find-file rev file-path)
    (when search-option
      (if line-start
          ;; Jump to line number
          (progn
            (goto-char (point-min))
            (forward-line (1- line-start)))
        ;; Use Org's search mechanism for other patterns
        (org-link-search search-option)))))

;;;###autoload
(defun orgit-file-export (path desc format)
  "Export orgit-file link at PATH to web URL if possible.

DESC is the link description.  FORMAT is the export backend.

Use `orgit-file-export-alist' to generate web URLs for known Git
hosting services.  The remote used is determined by `orgit-remote'
or the Git variable `orgit.remote'.

Revisions are automatically expanded to full 40-character hashes
before URL generation to ensure compatibility with hosting services
that require full hashes (e.g., Codeberg).

If PATH includes a line number or range, append appropriate fragment
identifier to the URL.  Fragment syntax varies by hosting service:
- GitHub, GitLab, Codeberg use #L43-L58
- Sourcehut uses #L43-58
- Bitbucket uses #lines-43:58

If PATH includes a text search pattern (not a line number) and
`orgit-file-export-text-fragments' is non-nil, append a text fragment
using the :~:text= syntax for browsers supporting the Text Fragments
specification.

Return formatted link for FORMAT, or signal `org-link-broken' if
the URL cannot be determined."
  (pcase-let* ((`(,repo ,rev ,file-path ,search-option ,line-start ,line-end)
                (orgit-file--parse-path path))
               (dir (orgit--repository-directory repo)))
    (if (file-exists-p dir)
        (let* ((default-directory dir)
               ;; Expand revision to full hash for URL compatibility
               (full-rev (condition-case nil
                             (magit-rev-parse rev)
                           (error rev)))
               (remotes (magit-git-lines "remote"))
               (remote  (magit-get "orgit.remote"))
               (remote  (cond ((length= remotes 1) (car remotes))
                              ((member remote remotes) remote)
                              ((member orgit-remote remotes) orgit-remote))))
          (if remote
              (if-let ((link
                        (or (and-let* ((url (magit-get "orgit" "file")))
                              (format-spec url `((?r . ,full-rev)
                                                 (?f . ,file-path))))
                            (and-let* ((url (magit-get "remote" remote "url"))
                                       (template (cl-find-if
                                                  (lambda (elt)
                                                    (string-match (car elt) url))
                                                  orgit-file-export-alist)))
                              (format-spec (nth 1 template)
                                           `((?n . ,(match-string 1 url))
                                             (?r . ,full-rev)
                                             (?f . ,file-path)))))))
                  (let ((link-with-fragment
                         (cond
                          ;; Line number or range takes precedence
                          (line-start
                           (concat link
                                   (orgit-file--format-line-fragment
                                    link line-start line-end)))
                          ;; Text search pattern when enabled
                          ((and search-option
                                orgit-file-export-text-fragments)
                           (concat link
                                   "#:~:text="
                                   (orgit-file--url-encode-text-fragment search-option)))
                          ;; No fragment
                          (t link))))
                    (orgit--format-export link-with-fragment desc format))
                (signal 'org-link-broken
                        (list (format "Cannot determine public url for %s"
                                      path))))
            (signal 'org-link-broken
                    (list (format "Cannot determine public url for %s"
                                  default-directory)))))
      (signal 'org-link-broken
              (list (format "Cannot determine public url for %s %s"
                            path "(which itself does not exist)"))))))

;;;###autoload
(defun orgit-file-complete-link (&optional arg)
  "Complete orgit-file link with repository, revision, and file selection.

Optional ARG is passed to `magit-read-repository'.

Prompt for a repository, then a revision (branch, tag, or commit),
then a file from that revision.  Return a complete orgit-file link."
  (let* ((default-directory (magit-read-repository arg))
         (repo (orgit--current-repository))
         (rev (magit-read-branch-or-commit "Revision"))
         (file (magit-read-file-from-rev rev "File")))
    (format "orgit-file:%s::%s::%s" repo rev file)))

;;; Utilities

(defun orgit-file--parse-path (path)
  "Parse orgit-file link PATH.

PATH format: REPO::REV::FILE-PATH or REPO::REV::FILE-PATH::SEARCH

SEARCH can be:
- A line number (integer as string like \"43\")
- A line range (\"43-58\")
- Any other Org search string (heading, custom ID, regex, or text)

When SEARCH is a line number or range, it is used for line-based
navigation and exported as #L43 or #L43-L58 fragments.

When SEARCH is any other string, it is treated as a text search
pattern for Org's search mechanism and exported as a :~:text=
URL fragment for web browsers supporting Text Fragments.

Return (REPO REV FILE-PATH SEARCH-OPTION LINE-START LINE-END) as a list.
LINE-START and LINE-END are integers when SEARCH-OPTION is a line
number or range, nil otherwise."
  (let* ((parts (split-string path "::" t))
         (repo (nth 0 parts))
         (rev (nth 1 parts))
         (file-path (nth 2 parts))
         (search-option (nth 3 parts))
         (line-start nil)
         (line-end nil))
    (when search-option
      (cond
       ;; Line range: "43-58"
       ((string-match "\\`\\([0-9]+\\)-\\([0-9]+\\)\\'" search-option)
        (setq line-start (string-to-number (match-string 1 search-option))
              line-end (string-to-number (match-string 2 search-option))))
       ;; Single line: "43"
       ((string-match "\\`\\([0-9]+\\)\\'" search-option)
        (setq line-start (string-to-number (match-string 1 search-option))
              line-end line-start))))
    (list repo rev file-path search-option line-start line-end)))

;;;###autoload
(defun orgit-file-export-link-at-point (&optional format)
  "Export orgit-file link at point and copy result to kill ring.

Find the orgit-file link at point (or in active region if region
is active), export it according to FORMAT, and copy the result to
the kill ring.

FORMAT determines the export style:
  `html'     - HTML anchor tag: <a href=\"URL\">DESC</a>
  `md'       - Markdown link: [DESC](URL)
  `latex'    - LaTeX hyperref: \\href{URL}{DESC}
  `ascii'    - Plain text: DESC (URL)
  `url-only' - Just the URL without any markup

When called interactively without prefix argument, use
`orgit-file-export-preview-format'.  With prefix argument, prompt
for format.

Display the exported result in the echo area and add it to the
kill ring for yanking.

Signal `user-error' if no orgit-file link is found at point or in
region."
  (interactive
   (list (if current-prefix-arg
             (intern (completing-read
                      "Export format: "
                      '("html" "md" "latex" "ascii" "url-only")
                      nil t nil nil
                      (symbol-name orgit-file-export-preview-format)))
           orgit-file-export-preview-format)))
  (let* ((format (or format orgit-file-export-preview-format))
         (element (if (org-region-active-p)
                      ;; Parse region for link
                      (save-excursion
                        (goto-char (region-beginning))
                        (org-element-link-parser))
                    ;; Parse link at point
                    (org-element-lineage (org-element-context) 'link t)))
         (type (org-element-property :type element))
         ;; Extract path without "orgit-file:" prefix
         (path (org-element-property :path element))
         (desc (when (org-element-contents element)
                 (org-element-interpret-data
                  (org-element-contents element)))))
    (unless (and element (equal type "orgit-file"))
      (user-error "No orgit-file link at point"))
    ;; Create a minimal info plist for export
    (let* ((url (orgit-file-export path desc format))
           (result
            (pcase format
              ('url-only
               ;; Extract just the URL from exported result
               (cond
                ((string-match "href=[\"']\\([^\"']+\\)[\"']" url)
                 (match-string 1 url))
                ((string-match "](\\([^)]+\\))" url)
                 (match-string 1 url))
                ((string-match "{\\([^}]+\\)}" url)
                 (match-string 1 url))
                (t url)))
              (_ url))))
      ;; Copy to kill ring
      (kill-new result)
      ;; Display result
      (message "Exported link copied to kill ring: %s"
               (if (> (length result) 100)
                   (concat (substring result 0 97) "...")
                 result))
      result)))

;;; _
(provide 'orgit-file)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; orgit-file.el ends here
