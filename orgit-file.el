;;; orgit-file.el --- Support for links to files in Git repositories  -*- lexical-binding:t -*-

;; Author: Gino Cornejo
;; mantainer: Gino Cornejo <gggion123@gmail.com>
;; Homepage: https://github.com/gggion/orgit-file
;; Keywords: hypermedia vc

;; Package-Version: 0.1.0
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

(unless (fboundp 'org-link-store-props)
  (defalias 'org-link-store-props 'org-store-link-props))

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
  :type 'boolean)

(defcustom orgit-file-link-to-file-use-orgit nil
  "Non-nil means storing a link to a file in a Git repo will use orgit-file links.

The variable can have the following values:

t     Always create an orgit-file link when in a Git repository.

create-if-interactive
      If `org-store-link' is called directly (interactively, as a user
      command), create an orgit-file link.  But when doing the job for
      capture or other non-interactive callers, only use orgit-file if
      explicitly requested.

use-existing
      Only create orgit-file links in magit-specific contexts (like
      `magit-blob-mode').  Regular file buffers will fall back to file:
      links.

nil   Never create orgit-file links, instead allow file: links."
  :group 'orgit-file
  :type '(choice
          (const :tag "Always use orgit-file links in Git repos" t)
          (const :tag "Create if storing link interactively"
                 create-if-interactive)
          (const :tag "Only in magit contexts" use-existing)
          (const :tag "Never use orgit-file links" nil)))

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
                       (string :tag "File URL format"))))

;;; File links

;;;###autoload
(with-eval-after-load 'org
  (with-eval-after-load 'magit
    (org-link-set-parameters "orgit-file"
                             :store    #'orgit-file-store
                             :follow   #'orgit-file-open
                             :export   #'orgit-file-export
                             :complete #'orgit-file-complete-link)))

;;;###autoload
(defun orgit-file-store (&optional interactive?)
  "Store a link to the file in a Magit file or blob buffer.

The link includes the repository, revision, and file path.

When in a `magit-blob-mode' buffer (viewing a historical
revision), store a link to that specific revision.

The behavior is controlled by `orgit-file-link-to-file-use-orgit'.
When that variable is nil or doesn't match the current context,
return nil to allow file: links as an alternative.

The revision format in the link is controlled by
`orgit-file-abbreviate-revisions'.  When non-nil, use abbreviated
hashes; when nil, use full 40-character hashes.

With a `\\[universal-argument]' prefix argument, skip storing
orgit-file link and allow fallback to file: link instead.

Argument INTERACTIVE? indicates whether `org-store-link' was
called interactively.

Return non-nil if a link was stored, nil otherwise."
  (when-let* ((repo (magit-toplevel)))
    ;; Skip if prefix arg given
    (unless current-prefix-arg
      (let ((in-magit-context (or (bound-and-true-p magit-blob-mode)
                                  (derived-mode-p 'magit-mode))))
        (when (or (eq orgit-file-link-to-file-use-orgit t)
                  (and (eq orgit-file-link-to-file-use-orgit 'create-if-interactive)
                       interactive?)
                  (and (eq orgit-file-link-to-file-use-orgit 'use-existing)
                       in-magit-context))
          (let ((file (or (and (bound-and-true-p magit-blob-mode)
                               magit-buffer-file-name)
                          (and buffer-file-name
                               (magit-file-relative-name))))
                (rev (or (and (bound-and-true-p magit-blob-mode)
                              magit-buffer-revision)
                         (and buffer-file-name
                              (magit-rev-parse "HEAD")))))
            (when (and file rev)
              (let* ((repo-id (orgit--current-repository))
                     (rev-for-link (if orgit-file-abbreviate-revisions
                                       (magit-rev-abbrev rev)
                                     rev))
                     (link (format "orgit-file:%s::%s::%s"
                                   repo-id rev-for-link file)))
                (org-link-store-props
                 :type "orgit-file"
                 :link link)
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
identifier to the URL (e.g., #L43 or #L43-L58 for GitHub).

Return formatted link for FORMAT, or signal `org-link-broken' if
the URL cannot be determined."
  (pcase-let* ((`(,repo ,rev ,file-path ,_search-option ,line-start ,line-end)
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
                  (let ((link-with-lines
                         (if line-start
                             (concat link
                                     (if (and line-end (not (= line-start line-end)))
                                         (format "#L%d-L%d" line-start line-end)
                                       (format "#L%d" line-start)))
                           link)))
                    (orgit--format-export link-with-lines desc format))
                (signal 'org-link-broken
                        (list (format "Cannot determine public url for %s"
                                      path))))
            (signal 'org-link-broken
                    (list (format "Cannot determine public remote for %s"
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
- Any other Org search string (heading, custom ID, regex)

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

;;; _
(provide 'orgit-file)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; orgit-file.el ends here
